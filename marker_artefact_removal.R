# This script is only for cleaning the data for further processing by Ledalab
library(tidyverse)
library(magrittr)
library(readxl)
source("clean_markers.R")

## Define paths
path_raw = paste(getwd(), "data", "raw", sep = "/")
path_clean = paste(getwd(), "data", "clean", sep = "/")
path_xlsx = paste(getwd(), "data", "xlsx", sep = "/")

raw_files <- list.files(path_raw, pattern = ".*_SCR.txt")
xlsx_files <- list.files(path_xlsx, pattern = ".*.xlsx")

# file_name <- paste0(path_raw,"/",raw_files[15])
file_name <- paste0(path_raw,"/12_recall_SCR.txt")

# This file contains the manual change requests
manual_requests <- read_csv(paste0(path_raw, "/manual_requests.csv")) %>% 
        mutate(file =  paste0(path_raw, "/", file))

# Cleans all false markers and removes applies the manual requests
for (f in raw_files) clean_markers(paste0(path_raw, "/", f), treshold = 320, manual_requests)



