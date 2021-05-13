#-------------------------------------
# This script sets out to load all
# things required to build the app
#-------------------------------------

#-------------------------------------
# Author: Trent Henderson, 13 May 2021
#-------------------------------------

library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(Rcatch22)
library(theft)
library(foreign)
library(plotly)

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
