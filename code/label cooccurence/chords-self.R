library(dplyr)
library(tidyverse)
library(tidygraph)
library(chorddiag)
library(igraph)
library(webshot) # Required to export HTML widgets to PDF

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 


library(tidyverse)
library(circlize)


models <- list(
  gpt_s = "gpt_cooccurrence.csv",
  gemini_s = "gemini_cooccurrence.csv",
  llama_s = "llama_cooccurrence.csv"
)


grid_colors <- c(
  "Label 1" = "#22223b", 
  "Label 2" = "#c9ada7", 
  "Label 3" = "#6b9080", 
  "Label 4" = "#f4d35e", 
  "Label 5" = "#adb5bd"
)

for (name in names(models)) {
  # Load data
  df <- read_csv(models[[name]])
  
  # Prepare matrix - SELF-LOOPS ARE NOW INCLUDED
  df_matrix <- df %>%
    spread(Diagnosis_2, freq, fill = 0) %>%
    column_to_rownames("Diagnosis_1") %>%
    as.matrix()
  
  # Set up PDF output
  pdf(paste0(name, ".pdf"), width = 10, height = 10)
  
  # Initialize circular layout
  circos.clear()
  circos.par(start.degree = 90, gap.degree = 5)
  
  # Create the chord diagram
  chordDiagram(
    df_matrix,
    grid.col = grid_colors,
    transparency = 0.25,
    annotationTrack = "grid", # Keeps the edge clean
    self.link = 1,           # Ensures self-links are drawn as loops
    preAllocateTracks = list(track.height = 0.05)
  )
  
  
  title(paste(toupper(name), "Thematic Label Distribution"), cex.main = 1.8)
  
  # Add the Legend to each plot
  legend("bottomright", 
         legend = names(grid_colors), 
         fill = grid_colors, 
         cex = 1.0, 
         inset = c(0.02, 0.02),
         title = "Thematic Labels",
         box.col = "white")
  
  dev.off()
}

circos.clear()

