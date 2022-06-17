#-------------------------------------
# This script sets out to load all
# things required to build the app
#-------------------------------------

#-------------------------------------
# Author: Trent Henderson, 13 May 2021
#-------------------------------------

library(shiny)
library(data.table)
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
library(readxl)
library(foreign)
library(plotly)
library(shinyWidgets)
library(shinycssloaders)
library(feasts)
library(tsfeatures)
library(Rtsne)
library(R.matlab)
library(e1071)
library(kernlab)
library(caret)
library(purrr)
library(RColorBrewer)
library(janitor)
library(markdown)

# Load in any HTML imports

import_files <- list.files("imports", full.names = TRUE, pattern = "\\.html")
for(f in import_files){
  object_name <- gsub("imports/", "", f)
  object_name <- gsub("\\.html", "", object_name)
  assign(object_name, readLines(f, warn = FALSE))
}

# Load in caret model list

data_files <- list.files("data", full.names = TRUE, pattern = "\\.Rda")
for(d in data_files){
  load(d)
}

# Load in any R functions

r_files <- list.files("R", full.names = TRUE, pattern = "\\.[Rr]")
for(f in r_files){
  source(f)
}

# Define tab names

navtab0 <- "HOME"
navtab1 <- "LOW DIMENSIONAL PROJECTION"
navtab2 <- "FEATURE CALCULATION QUALITY"
navtab3 <- "DATA MATRIX VISUALISATION"
navtab4 <- "TIME-SERIES CLASSIFICATION"
navtab5 <- "ABOUT"

# Turn off scientific notation

options(scipen = 999)

# Define "not in" operator

"%ni%" <- Negate("%in%")

# Helper vectors

all_scalers <- c("z-score", "Sigmoid", "RobustSigmoid", "MinMax")
featuresets <- c("catch22", "feasts", "tsfeatures")
binaries <- c("No", "Yes")
lowdims <- c("PCA", "t-SNE")
cor_methods <- c("pearson", "spearman")
clust_methods <- c("average", "ward.D", "ward.D2", "single", "complete", "mcquitty", "median", "centroid")
classifiers <- unique(caretModels$model)
null_testing_methods <- c("ModelFreeShuffles", "NullModelFits")
p_value_methods <- c("gaussian", "empirical")
