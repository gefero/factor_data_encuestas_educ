required_pkgs <- c(
  "shiny", "bslib", "dplyr", "tidyr", "purrr", "stringr",
  "readxl", "plotly", "DT"
)
new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs) > 0) {
  message("Instalando paquetes faltantes: ", paste(new_pkgs, collapse = ", "))
  install.packages(new_pkgs, repos = "https://cloud.r-project.org")
}

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readxl)
library(plotly)
library(DT)

HAS_WORDCLOUD2 <- requireNamespace("wordcloud2", quietly = TRUE)
HAS_SYUZHET    <- requireNamespace("syuzhet",    quietly = TRUE)

if (HAS_WORDCLOUD2) library(wordcloud2)

source("R/column_map.R")
source("R/load_data.R")
source("R/compute_dimensions.R")
source("R/text_analysis.R")
source("R/plot_helpers.R")

DATA_PATH <- "../data/raw/"
