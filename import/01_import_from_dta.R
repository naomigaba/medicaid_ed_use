## Creator: Naomi Gaba
## Date: 2022-11-06
## Purpose: Import dta file and save as RDS 

# Set Environment  ------------------------------------------------------

rm(list = ls())

# point R towards directory 
directory<- '/Users/naomi/Library/Mobile Documents/com~apple~CloudDocs/Documents/Applications/2022-2023/medicaid_lottery/'

# load the libraries needed to run R script 
library(dplyr)
library(tidyverse)

## read in data

raw <- haven::read_dta(paste0(directory,'data/oregon.dta'))

## save data as r file 

saveRDS(raw, paste0(directory,'data/raw_oregon.rds'))


