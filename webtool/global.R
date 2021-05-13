#-------------------------------------
# This script sets out to load all
# things required to build the app
#-------------------------------------

#-------------------------------------
# Author: Trent Henderson, 13 May 2021
#-------------------------------------

library(shiny)
library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(readr)
library(ggplot2)
library(reshape2)
library(tibble)
library(stats)
library(scales)
library(broom)
library(Rcatch22)
#library(theft) # Manual implementation for now until on CRAN
library(readxl)
library(foreign)
library(plotly)
library(shinyWidgets)
library(shinycssloaders)
library(feasts)
library(tsfeatures)

# Load in any HTML imports

import_files <- list.files("imports", full.names = TRUE, pattern = "\\.html")
for(f in import_files){
  object_name <- gsub("imports/", "", f)
  object_name <- gsub("\\.html", "", object_name)
  assign(object_name, readLines(f, warn = FALSE))
}

# Load in any R functions

r_files <- list.files("R", full.names = TRUE, pattern = "\\.[Rr]")
for(f in r_files){
  source(f)
}

# Define tab names

navtab0 <- "HOME"
navtab1 <- "LOW DIMENSION VISUALISATION"
navtab2 <- "CLASSIFICATION PERFORMANCE"
navtab3 <- "FEATURE CALCULATION QUALITY"
navtab4 <- "FEATURE MATRIX VISUALISATION"
navtab5 <- "ABOUT"

# Turn off scientific notation

options(scipen = 999)

# Helper vectors

all_scalers <- c("z-score", "Sigmoid", "RobustSigmoid", "MinMax", "MeanSubtract")
