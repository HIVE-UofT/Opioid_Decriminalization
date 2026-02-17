
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(stringr)
  library(scales)
  library(broom)
  library(lmtest)
  library(sandwich)
  library(systemfonts)
  library(ggplot2)
  library(grid)
})


LABELED <- "labelled.csv"


raw <- readr::read_csv(LABELED, show_col_types = FALSE) |>
  dplyr::rename_with(~ tolower(gsub("\\s+", "", .x)))

pick1 <- function(d, choices) { x <- intersect(choices, names(d)); if (length(x)) x[1] else NA_character_ }

body_col      <- pick1(raw, c("body","comment","text","message"))
subreddit_col <- pick1(raw, c("communityname","subreddit","community","forum"))
created_col   <- pick1(raw, c("createdat","created_at","created","timestamp","datetime","date"))
replies_col   <- pick1(raw, c("numberofreplies","replies","replycount","numreplies","nreplies"))
writing_col   <- pick1(raw, c("writing","labels","labelstring","misconceptionlabels"))

stopifnot(!is.na(body_col), !is.na(subreddit_col), !is.na(created_col),
          !is.na(replies_col), !is.na(writing_col))

parse_created <- function(x) {
  p <- suppressWarnings(lubridate::parse_date_time(as.character(x),
                                                   orders = c("Y-m-d H:M:S","Y-m-d H:M","Y-m-d",
                                                              "m/d/Y H:M:S","m/d/Y H:M","m/d/Y",
                                                              "d/m/Y H:M:S","d/m/Y H:M","d/m/Y"),
                                                   tz = "UTC"))
  need <- is.na(p)
  if (any(need)) {
    nums <- suppressWarnings(as.numeric(as.character(x)[need]))
    ok <- !is.na(nums)
    if (any(ok)) {
      secs <- ifelse(nums[ok] > 1e12, nums[ok]/1000, nums[ok])
      p[which(need)[ok]] <- as_datetime(secs, tz = "UTC")
    }
  }
  as_datetime(p)
}

df <- tibble(
  body      = as.character(raw[[body_col]]),
  subreddit = as.character(raw[[subreddit_col]]),
  created   = parse_created(raw[[created_col]]),
  replies   = as.integer(suppressWarnings(as.numeric(raw[[replies_col]]))),
  writing   = tolower(as.character(replace_na(raw[[writing_col]], "")))
) |>
  dplyr::filter(!is.na(subreddit), !is.na(created), !is.na(replies)) |>
  dplyr::mutate(week = floor_date(created, "week", week_start = 1))


bc_local <- c("r/vancouver","r/britishcolumbia","r/victoriabc","r/bcpolitics","r/ubc")
df <- df |>
  dplyr::mutate(region = dplyr::if_else(tolower(subreddit) %in% bc_local, "BC_local", "National"),
                region = factor(region, levels = c("BC_local","National")))

# ---------------- Harm-reduction detector --------
wb <- function(x) paste0("\\b(?:", paste(x, collapse="|"), ")\\b")

pat_overdose <- c("naloxon\\w*","narcan\\w*","take\\s*home\\s*naloxone","rescue\\s*breath")
pat_checks   <- c("fentanyl\\s*test\\s*strip\\w*","\\bfts\\b","drug[-\\s]*checking")
pat_sites    <- c("supervised\\s*consumption\\s*site\\w*","\\bscs\\b",
                  "safe\\s*consumption\\s*site\\w*","supervised\\s*injection\\s*site\\w*","\\bsis\\b",
                  "overdose\\s*prevention\\s*site\\w*","\\bops\\b","needle\\s*exchange",
                  "syringe\\s*service\\s*program\\w*","\\bssp\\b")
pat_oat      <- c("opioid\\s*agonist\\s*therap\\w*","\\boat\\b","\\bioat\\b",
                  "methadone","buprenorphine","suboxone","subutex",
                  "hydromorphone","dilaudid","diacetylmorphine",
                  "slow\\s*release\\s*oral\\s*morphine","\\bsrom\\b","kadian")
pat_safe_sup <- "(?:\\bsafe\\s*supply\\b|\\bsafe\\b\\W+\\bsupply\\b)"
pat_info     <- c("good\\s*samaritan","\\bbccdc\\b","\\bbccsu\\b")

txt <- tolower(df$body)
df <- df |>
  dplyr::mutate(hr_flag = as.integer(
    str_detect(txt, wb(c(pat_overdose, pat_checks))) |
      str_detect(txt, wb(pat_sites)) |
      str_detect(txt, paste(wb(pat_oat), pat_safe_sup, sep="|")) |
      str_detect(txt, wb(pat_info))
  ))

# ----------------  Misconception flags ------------
labs <- df$writing
df <- df |>
  dplyr::mutate(
    m1_policy  = as.integer(str_detect(labs, "\\b1\\b")),
    m2_legal   = as.integer(str_detect(labs, "\\b2\\b")),
    m3_enforce = as.integer(str_detect(labs, "\\b3\\b")),
    m4_impact  = as.integer(str_detect(labs, "\\b4\\b"))
  )

# ---------------- HR share by subreddit × week ---
hr_sw <- df |>
  dplyr::group_by(subreddit, region, week) |>
  dplyr::summarise(
    n_week         = dplyr::n(),
    n_hr           = sum(hr_flag == 1, na.rm = TRUE),
    hr_rate_per100 = 100 * n_hr / n_week,
    .groups = "drop"
  )

# ----------------  Reply premium by class ----------
premium_one <- function(d, flag_col, cname) {
  m <- d[[flag_col]] == 1
  o <- !m
  tibble(
    n_pos        = sum(m, na.rm = TRUE),
    n_neg        = sum(o, na.rm = TRUE),
    mean_log_pos = if (any(m)) mean(log1p(d$replies[m]), na.rm = TRUE) else NA_real_,
    mean_log_neg = if (any(o)) mean(log1p(d$replies[o]), na.rm = TRUE) else NA_real_,
    class        = cname
  ) |>
    dplyr::mutate(premium_pct = (exp(mean_log_pos - mean_log_neg) - 1) * 100)
}

prem_byclass <- df |>
  dplyr::group_by(subreddit, region, week) |>
  dplyr::group_modify(\(.x, .y) {
    dplyr::bind_rows(
      premium_one(.x, "m2_legal",   "Legal-status confusion"),
      premium_one(.x, "m3_enforce", "Enforcement misconceptions")
    )
  }) |>
  dplyr::ungroup() |>
  dplyr::filter(is.finite(premium_pct), n_pos >= 3, n_neg >= 3)

# ---------------- Merge with HR share -------------
# NOTE: we keep ONE weight column: n_week
dat <- prem_byclass |>
  dplyr::inner_join(hr_sw, by = c("subreddit","region","week")) |>
  dplyr::transmute(
    subreddit, region, week, class,
    hr_rate_per100,
    premium_pct,
    n_week, n_pos, n_neg
  ) |>
  dplyr::filter(is.finite(hr_rate_per100), is.finite(premium_pct), n_week > 0)

# Save analysis points
readr::write_csv(dat, "hr_vs_premium_subreddit_week_byclass.csv")

# ---------------- Stats for annotation ------------
dat2 <- dat |>
  dplyr::filter(class %in% c("Legal-status confusion","Enforcement misconceptions"))

fit_draw <- lm(premium_pct ~ hr_rate_per100, data = dat2, weights = n_week)
fit_adj  <- lm(premium_pct ~ hr_rate_per100 + region + class, data = dat2, weights = n_week)

ct   <- lmtest::coeftest(fit_adj, vcov = sandwich::vcovHC(fit_adj, type = "HC3"))
b    <- unname(ct["hr_rate_per100","Estimate"])
se   <- unname(ct["hr_rate_per100","Std. Error"])
pval <- unname(ct["hr_rate_per100","Pr(>|t|)"])
ciL  <- b - 1.96*se
ciH  <- b + 1.96*se
r2   <- summary(fit_draw)$r.squared

stats_tbl <- tibble(
  model         = "overall_two_classes",
  beta_per_10pp = 10*b,
  ci_lo_10pp    = 10*ciL,
  ci_hi_10pp    = 10*ciH,
  p_value       = pval,
  r2            = r2,
  n_points      = nrow(dat2)
)

by_class <- dat |>
  dplyr::group_by(class) |>
  dplyr::group_modify(\(.x, .y) {
    m  <- lm(premium_pct ~ hr_rate_per100 + region, data = .x, weights = n_week)
    ct <- lmtest::coeftest(m, vcov = sandwich::vcovHC(m, type = "HC3"))
    est <- unname(ct["hr_rate_per100","Estimate"])
    se  <- unname(ct["hr_rate_per100","Std. Error"])
    tibble(beta_per_10pp = 10*est,
           ci_lo_10pp = 10*(est - 1.96*se),
           ci_hi_10pp = 10*(est + 1.96*se),
           p_value = unname(ct["hr_rate_per100","Pr(>|t|)"]),
           r2 = summary(m)$r.squared,
           n_points = nrow(.x))
  }) |>
  dplyr::ungroup()

readr::write_csv(dplyr::bind_rows(stats_tbl, by_class), "hr_scatter_regression_summaries.csv")



safe_has_font <- function(family) {
  if (!requireNamespace("systemfonts", quietly = TRUE)) return(FALSE)
  ok <- tryCatch({
    systemfonts::match_font(family)  # returns font info or errors if not found
    TRUE
  }, error = function(e) FALSE)
  isTRUE(ok)
}

# Try Arial first, then common substitutes; else fall back to generic sans
basefam <- dplyr::case_when(
  safe_has_font("Arial")            ~ "Arial",
  safe_has_font("Helvetica")        ~ "Helvetica",
  safe_has_font("Liberation Sans")  ~ "Liberation Sans",
  safe_has_font("DejaVu Sans")      ~ "DejaVu Sans",
  TRUE                              ~ "sans"
)


ggplot2::theme_set(
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      text = ggplot2::element_text(family = basefam),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "top"
    )
)

save_pdf <- function(plot, filename, width, height) {
  ggplot2::ggsave(
    filename, plot,
    width = width, height = height, dpi = 300,
    device = grDevices::cairo_pdf,   
    family = basefam
  )
}



ann <- sprintf("β = %.2f  95%% CI [%.2f, %.2f]  p = %.3f  ·  R² = %.02f",
               b, ciL, ciH, pval, r2)

p_clean <- ggplot2::ggplot(dat2, aes(x = hr_rate_per100, y = premium_pct)) +
  ggplot2::geom_hline(yintercept = 0, colour = "grey80") +
  ggplot2::geom_point(aes(color = class, shape = region, size = n_week),
                      alpha = 0.80, stroke = 0.2) +
  ggplot2::geom_smooth(method = "lm", se = FALSE, colour = "firebrick", linewidth = 1.1) +
  ggplot2::annotate("text",
                    x = max(dat2$hr_rate_per100, na.rm = TRUE),
                    y = min(dat2$premium_pct, na.rm = TRUE),
                    hjust = 1, vjust = -0.3, label = ann,
                    fontface = "bold", colour = "firebrick") +
  ggplot2::scale_color_manual(values = c("Legal-status confusion" = "#1f78b4",
                                         "Enforcement misconceptions" = "#e31a1c"),
                              name = "Misperception class") +
  ggplot2::scale_shape_manual(values = c(16, 17), name = "Region") +
  ggplot2::scale_size_area(max_size = 9, breaks = c(20,100,300,700), name = "Weekly comments") +
  ggplot2::scale_x_continuous("Harm-reduction share in subreddit-week (%)",
                              breaks = pretty_breaks(n = 8), limits = c(0, NA)) +
  ggplot2::scale_y_continuous("Reply premium for the class vs other comments (%)",
                              breaks = pretty_breaks(n = 8)) +
  ggplot2::labs(title = "Harm-reduction visibility and attention to legal/enforcement errors",
                subtitle = "Subreddit×week points · color = class · shape = region · size = weekly volume · red line = weighted OLS")

save_pdf(p_clean, "scatter.pdf", width = 15.8, height = 5.2)


p_facets <- ggplot2::ggplot(dat2, aes(x = hr_rate_per100, y = premium_pct)) +
  ggplot2::geom_hline(yintercept = 0, colour = "grey85") +
  ggplot2::geom_point(aes(color = region, size = n_week), alpha = 0.80, stroke = 0) +
  ggplot2::geom_smooth(method = "lm", se = FALSE, colour = "firebrick", linewidth = 1.0) +
  ggplot2::facet_wrap(~ class, nrow = 1) +
  ggplot2::scale_color_manual(values = c("BC_local" = "#4E79A7","National" = "#E15759"),
                              name = "Region") +
  ggplot2::scale_size_area(max_size = 9, breaks = c(20,100,300,700), name = "Weekly comments") +
  ggplot2::scale_x_continuous("Harm-reduction share in subreddit-week (%)", breaks = pretty_breaks(n = 8)) +
  ggplot2::scale_y_continuous("Reply premium for the class (%)", breaks = pretty_breaks(n = 8)) +
  ggplot2::labs(title = "Harm-reduction visibility vs reply premium by class",
                subtitle = "Per-class panels with volume-weighted OLS")

save_pdf(p_facets, "scatter2.pdf", width = 9.2, height = 4.8)

message("Saved CSVs: hr_vs_premium_subreddit_week_byclass.csv, hr_scatter_regression_summaries.csv")
message("Saved figures: fig_hr_scatter_clean_by2classes.pdf, fig_hr_scatter_facets_by2classes.pdf")


# ---------- Facet-level stats (weighted OLS, HC3 robust SE) ----------
get_stats <- function(d) {
  fit <- lm(premium_pct ~ hr_rate_per100, data = d, weights = n_week)
  ct  <- lmtest::coeftest(fit, vcov = sandwich::vcovHC(fit, type = "HC3"))
  b   <- unname(ct["hr_rate_per100","Estimate"])
  se  <- unname(ct["hr_rate_per100","Std. Error"])
  p   <- unname(ct["hr_rate_per100","Pr(>|t|)"])
  r2  <- summary(fit)$r.squared
  tibble(beta = b,
         ci_lo = b - 1.96*se,
         ci_hi = b + 1.96*se,
         p_value = p,
         r2 = r2,
         n_points = nrow(d))
}

# One row per class with stats + formatted label
class_stats <- dat2 %>%
  dplyr::group_by(class) %>%
  dplyr::summarise(stats = list(get_stats(dplyr::cur_data())),
                   .groups = "drop") %>%
  tidyr::unnest_wider(stats) %>%
  dplyr::mutate(
    label = sprintf("β = %.2f  [%.2f, %.2f]\n p = %.3f  ·  R² = %.02f  ·  N = %d",
                    beta, ci_lo, ci_hi, p_value, r2, n_points)
  )

# Coordinates for the in-panel labels (top-right of each facet)
span_x <- diff(range(dat2$hr_rate_per100, na.rm = TRUE))
span_y <- diff(range(dat2$premium_pct,    na.rm = TRUE))

ranges <- dat2 %>%
  dplyr::group_by(class) %>%
  dplyr::summarise(
    x_min = min(hr_rate_per100, na.rm = TRUE),
    x_max = max(hr_rate_per100, na.rm = TRUE),
    y_min = min(premium_pct,    na.rm = TRUE),
    y_max = max(premium_pct,    na.rm = TRUE),
    .groups = "drop"
  )

labels_df <- dplyr::left_join(class_stats, ranges, by = "class") %>%
  dplyr::mutate(
    x_lab = x_max - 0.02*span_x,
    y_lab = y_max - 0.05*span_y
  )


pooled_label <- "β= 0.24  95% CI [-0.20, 0.67]  p = 0.285  ·  R² = 0.00"
pooled_df <- ranges %>%
  dplyr::transmute(
    class,
    x = x_max - 0.02*span_x,
    y = y_min + 0.06*span_y,
    lab = pooled_label
  )

# ---------- Figure facet-level + pooled stats ----------
p_facets <- ggplot2::ggplot(dat2, aes(x = hr_rate_per100, y = premium_pct)) +
  ggplot2::geom_hline(yintercept = 0, colour = "grey85") +
  ggplot2::geom_point(aes(color = region, size = n_week), alpha = 0.45, stroke = 1) +
  ggplot2::geom_smooth(aes(weight = n_week), method = "lm", formula = y ~ x,
                       se = FALSE, colour = "#80527c", linewidth = 0.7) +
  ggplot2::facet_wrap(~ class, nrow = 1) +
  ggplot2::geom_text(
    data = labels_df,
    aes(x = x_lab, y = y_lab, label = label),
    inherit.aes = FALSE, hjust = 1.5, vjust = 1,
    colour = "#80527c", fontface = "bold", size = 3.2, lineheight = 1.08
  ) +
  # ggplot2::geom_text(
  #   data = pooled_df,
  #   aes(x = x, y = y, label = lab),
  #   inherit.aes = FALSE, hjust = 1, vjust = 0,
  #   colour = "grey25", size = 3.0
  # ) +
  ggplot2::scale_color_manual(values = c("BC_local" = "#a81ba6","National" = "#314e99"),
                              name = "Region") +
  ggplot2::scale_size_area(max_size = 9, breaks = c(20,100,300,700), name = "Weekly comments") +
  ggplot2::scale_x_continuous("Harm-reduction share in subreddit-week (%)",
                              breaks = scales::pretty_breaks(n = 8)) +
  ggplot2::scale_y_continuous("Reply premium for the class (%)",
                              breaks = scales::pretty_breaks(n = 8)) +
  ggplot2::labs(
    title = "Harm-reduction visibility vs reply premium by class",
    subtitle = "Per-class panels with volume-weighted OLS",
    caption = expression(beta ~ " from weighted OLS with HC3 robust SE. Points sized by weekly volume")
  )

p_facets <- p_facets +
  ggplot2::theme(
    plot.caption = ggplot2::element_text(hjust = 0.5)
  )

# p_facets <- p_facets +
#   annotation_custom(
#     grob = textGrob(
#       label = pooled_df$lab,
#       x = 0.5, y = 0.52,  # tweak y based on where you want it between facets
#       gp = gpar(col = "grey25", fontsize = 10)
#     )
#   )


save_pdf(p_facets, "scatter3.pdf", width = 9.2, height = 4.8)

