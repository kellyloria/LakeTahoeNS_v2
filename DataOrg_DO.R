## ---------------------------
## Data aggregation of all miniDOT data from the Near shore of Lake Tahoe
##
## Author: Kelly A. Loria
## Date Created: 2021-10-26
## Email: kelly.loria@nevada.unr.edu

## ---------------------------
#Load packages
library(tidyverse)
library(ggplot2)
library(cowplot)
library(lubridate)
library(reshape2)

## ---------------------------
# Local file path setup:
if (dir.exists('/Users/kellyloria/Documents/UNR/SummerResearch2021')){
  inputDir<- '/Users/kellyloria/Documents/UNR/SummerResearch2021/DO_downloads'
  outputDir<- '/Users/kellyloria/Documents/UNR/SummerResearch2021/DO_downloads/PrelimClean' 
}
