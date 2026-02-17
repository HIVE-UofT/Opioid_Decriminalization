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
  gpt = "gpt_cooccurrence.csv",
  gemini = "gemini_cooccurrence.csv",
  llama = "llama_cooccurrence.csv"
)

# Define consistent colors for the labels
grid_colors <- c(
  "Label 1" = "#22223b", 
  "Label 2" = "#c9ada7", 
  "Label 3" = "#6b9080", 
  "Label 4" = "#f4d35e", 
  "Label 5" = "#adb5bd"
)

for (name in names(models)) {
  # Load the co-occurrence data
  df <- read_csv(models[[name]])
  
  # Create matrix (excluding self-links to highlight thematic overlap)
  df_matrix <- df %>%
    filter(Diagnosis_1 != Diagnosis_2) %>%
    spread(Diagnosis_2, freq, fill = 0) %>%
    column_to_rownames("Diagnosis_1") %>%
    as.matrix()
  
  # Initialize PDF device
  pdf(paste0(name, ".pdf"), width = 10, height = 10)
  
  # Setup circular parameters
  circos.clear()
  circos.par(start.degree = 90, gap.degree = 5)
  
  # Generate Chord Diagram
  chordDiagram(
    df_matrix,
    grid.col = grid_colors,
    transparency = 0.25,
    annotationTrack = "grid", # Removes labels from the circle edge
    preAllocateTracks = list(track.height = 0.05)
  )
  
  # Add Title
  title(paste(toupper(name), "Thematic Co-occurrence"), cex.main = 1.8)
  
  # Add Legend
  legend("bottomright", 
         legend = names(grid_colors), 
         fill = grid_colors, 
         cex = 1.0, 
         inset = c(0.02, 0.02),
         title = "Thematic Labels",
         box.col = "white")
  
  dev.off()
}

# Clear settings
circos.clear()

