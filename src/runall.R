# ~~~~~~~~~~~~~~ Script overview ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~----
#' This script runs the analyses for the following study:
#' 
#' <Anonymized>
#'  
#' Purpose: 
#'      Code is provided for the following steps in the analyses:
#'      1. Drone photograph processing
#'      2. Satellite image processing
#'      3. Reproduction of figures and tables in manuscript
#'      
#'      Sentinel-2 data extraction is conducted using code from the RGEE 
#'      repository available here: <Anonymized>
#'
#' Instructions to run:
#'      Install packages: 
#'      This project uses Renv for package management. To install the correct 
#'      version of all R packages required, run `renv::restore()` in the console
#'      
#'      
#' Notes: 
#'      This analysis was conducted using R version 4.3.3
# ~~~~~~~~~~~~~~ Analysis code ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~----
# To install all packages:
renv::restore()

library(here)
library(glue)
library(rmarkdown)

# Check that the working directory is rooted in the project folder
here::i_am("src/runall.R")

## ---- Process drone images ----
# Get %cover for every drone photograph, and export results as .csv file
source(here("src", "process-drone-images.R"))

## ---- Download satellite images ----
# Satellite datasets are provided in the data repository for this project. 
# However, the code used to download these datasets can be seen in the python 
# script `get-sat-data-batch.py`

## ---- Process satellite images ----
source(here("src", "process-sat-images.R"))

## ---- Reproduce analyses that appear in manuscript ----
# Creates a .html with all output in the `./outputs/notebook-output` directory
render(here("src", "analysis-manuscript.Rmd"), 
       output_dir = here("outputs", "notebook-output"),
       output_file = glue("analysis-manuscript.html"))
