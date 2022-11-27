## Creator: Naomi Gaba
## Date:    2022-11-06
## Purpose: 

# Set Environment  ------------------------------------------------------

rm(list = ls())

# point R towards directory 
directory<- '/Users/naomi/Library/Mobile Documents/com~apple~CloudDocs/Documents/Applications/2022-2023/medicaid_lottery/'

# load the libraries needed to run R script 
library(dplyr)
library(tidyverse)
library(rlang)
library(AER)


## read in rds file 

raw <- readRDS(paste0(directory,'data/raw_oregon.rds'))

samples <- c(quo(all)
             , quo(zero)
             , quo(one)
             , quo(twoplus)
             , quo(fiveplus)
             , quo(twoplus_out))

sample <- raw %>% 
  filter(sample_ed == 1)

## function for filtering sub-sample

sub_sample_fun <- function(pre_visit_subgroup) {  
  sample_group <- pre_visit_subgroup
  
  if (quo_name(sample_group) == "all") {
    df <- sample %>% 
      mutate(sample_cut = quo_name(sample_group))
    
  } else  {
    df <- sample %>% 
      filter(!! sample_group == 1) %>% 
      mutate(sample_cut = quo_name(sample_group))
  }
  
  df
  
}

# Create function to calculate sample size and control means 

sample_size_fun <- function(sample_group) {
  
  # compute sample size 
  sample_size <- sub_sample_fun(sample_group) %>% # use sub-sample filter function from above 
    summarise(sample_cut = unique(sample_cut)
      , n_obs = n() # count number of observations
  
              )
  
  # compute control mean
  control_mean <- sub_sample_fun(sample_group) %>% 
    filter(lottery == 0
           ) %>% 
    summarise(sample_cut = unique(sample_cut)
              , contol_average = mean(ed_visit_post*100))
  
  # join the sample size and control mean as output   
  output <-  left_join(sample_size, control_mean, by = "sample_cut")  
  
  
}


# Create function to compute impact of Medicaid treatment using iv regression

ivreg_fun <- function(sample_group_input) {
  
  
  # loop through sample groups
  df    <- sub_sample_fun(sample_group_input) 
  
  # set regression parameters 
  y1    <- cbind(df$ed_visit_post)       # outcome
  d     <- cbind(df$enrolled_post_ever)  # treatment
  x1    <- cbind(df$ed_visit_pre
              , df$ed_visit_pre_miss)    # exogenous variables
  z     <- cbind(df$lottery)             # instrument
  strat <- cbind(df$numhh)               # stratification
  
  
  # run 2sls iv regression
  ivreg1 <- ivreg(y1 ~ d + x1 + factor(strat)
             | x1  + factor(strat) + z
    
             
  )
  

  # summarize results with cluster on household 
  summary <-  coeftest(ivreg1, cluster = ~id_household)
  
  # prepare output 
  tibble(sample_cut = paste(unique(df$sample_cut))
    , estimate = summary[2, 1]*100
         , std_error = summary[2, 2]*100
         , p_val = summary[2, 4]
  )
  
}


# Create Table 2

table_2 <- left_join(map_dfr(samples, sample_size_fun)
                     , map_dfr(samples, ivreg_fun), by = "sample_cut")


# Export table ------------------------------------------------------------

write_csv(table_2, paste0(directory, "output/table_2.csv"))

