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


sample <- raw %>% 
  filter(sample_ed == 1)


outcomes <- list(quo(tr_ed_edcnnp_post)
                 , quo(tr_ed_edcnpa_post)
                 , quo(tr_ed_epct_post)
                 , quo(tr_ed_ne_post)
                 , quo(tr_ed_unclas_post)
)

pre_trial <- list(quo(tr_ed_edcnnp_pre)
                 , quo(tr_ed_edcnpa_pre)
                 , quo(tr_ed_epct_pre)
                 , quo(tr_ed_ne_pre)
                 , quo(tr_ed_unclas_pre)
)

pre_trial_miss <- list(quo(tr_ed_edcnnp_pre_miss)
                  , quo(tr_ed_edcnpa_pre_miss)
                  , quo(tr_ed_epct_pre_miss)
                  , quo(tr_ed_ne_pre_miss)
                  , quo(tr_ed_unclas_pre_miss)
)


table_4_fun <- function(outcome, pre, pre_miss){

  #outcome <- quo(tr_ed_edcnnp_post)
  
  control_stats <- sample %>% 
    filter(lottery == 0) %>% 
    summarise(mean = mean(!!outcome, na.rm = T )
              , st_dev = sd(!!outcome, na.rm = T)
              )
  
  
  
  # set regression parameters 
  y1    <- cbind(sample %>% select(!!outcome) %>% pull())       # outcome
  d     <- cbind(sample %>% select(enrolled_post_ever) %>% pull())  # treatment
  x1    <- cbind(sample %>% select(!!pre) %>% pull()
                 , sample %>% select(!!pre_miss) %>% pull())    # exogenous variables
  z     <- cbind(sample %>% select(lottery) %>% pull())             # instrument
  strat <- cbind(sample %>% select(numhh) %>% pull())               # stratification
  
  ivreg1 <- ivreg(y1 ~ d + x1 + factor(strat)
                  | x1  + factor(strat) + z
                  )
  
  
  # summarize results with cluster on household 
  summary <-  coeftest(ivreg1, cluster = ~id_household)
  
  # select results for table
  
  tibble(
    outcome = quo_name(outcome)
    , control_mean = control_stats$mean 
    , std_dev = control_stats$st_dev
    , effect_mdcd = summary[2,1]
    , std_error = summary[2,2]
    , p_val = summary[2,4]
  )

}

table_4 <- pmap_dfr(list(outcomes
              , pre_trial
              , pre_trial_miss)
          , table_4_fun)



# Export table ------------------------------------------------------------


write_csv(table_4, paste0(directory, "output/table_4.csv"))
