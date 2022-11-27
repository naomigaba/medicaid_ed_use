## Creator: Naomi Gaba
## Date:    2022-11-06
## Purpose: Check covariate balance between the control and test group to rule out potential bias 

# Set Environment  ------------------------------------------------------

rm(list = ls())

# point R towards directory 
directory<- '/Users/naomi/Library/Mobile Documents/com~apple~CloudDocs/Documents/Applications/2022-2023/medicaid_lottery/'

# load the libraries needed to run R script 
library(dplyr)
library(tidyverse)
library(miceadds)
library(sandwich)
library(rlang)

## read in rds file 

raw <- readRDS(paste0(directory,'data/raw_oregon.rds'))



# Set inputs --------------------------------------------------------------


### list of covariates on which to check balance between sample and control group 
covariates <- c('sample_ed'
                , 'birthyear'
                , 'female'
                , 'english'
                , 'self'
                , 'first_day'
                , 'have_phone'
                , 'pobox')

### list binary variables to scale 
binary_vars <- c('sample_ed'
                 , 'female'
                 , 'english'
                 , 'self' 
                 , 'first_day'
                 , 'have_phone'
                 , 'pobox')


sample <- raw %>% 
  filter(sample_ed == 1)

# Set functions -----------------------------------------------------------

### scaling function to be apply to binary variables 
scale <-  function(x) {
  x * 100
}

### function for the clustered covariate model check
covariate_cluster_model <-  function(y, d) {
  lm.cluster(y ~ lottery + factor(numhh)
             , cluster = 'id_household'
             , data = d
  )
}


### function for the entire covariate check
covariate_check_fun <- function(x) {

  # select the covariate variable 
  if(x == 'sample_ed') {
    
    y <- raw %>% 
      select(!!x) %>% 
      mutate_all(~scale(.)) %>% 
      pull()
    
    data_in <- raw
    
  } else {
    
    y <- sample %>% 
      select(!!x) %>% 
      mutate_if(x%in%binary_vars, ~scale(.)) %>% 
      pull()
    
    data_in <- sample
    
  }
  
  # run the regression on the sample data 
  model <-  covariate_cluster_model(y = y, d = data_in)
  
  
  # generate summary of regression output        
  summary <- model %>% 
    summary()
  
  # output the relevant estimates 
  output <- tibble(
    covariate      = x            # label the covariate
    , diff_in_mean = summary[2,1] # beta 
    , sd_error     = summary[2,2] # p value 
  )
  
  
}


# Control mean ------------------------------------------------------------

# summarize control means of covariates to compare against test group

control_mean <- raw %>% 
  filter(sample_ed == 1, lottery == 0) %>% # subset to ed_sample and control
  mutate_at(binary_vars, ~scale(.)) %>% # scale the binary variables 
  select(covariates) %>% 
  select(-sample_ed) %>% # sample_ed must be done separately on the raw data, not the sample
  summarise_all(~mean(.)
  ) %>% 
  pivot_longer(cols =        birthyear:pobox
               , names_to =  'covariate'
               , values_to = 'control_average')

control_mean_all <- raw %>% 
  mutate_at("sample_ed", ~scale(.)) %>% 
  filter(lottery == 0) %>% 
  select(sample_ed) %>% 
  summarise_all(~mean(.)
  ) %>% 
  pivot_longer(cols =        sample_ed
               , names_to =  'covariate'
               , values_to = 'control_average') %>% 
  bind_rows(control_mean)
  



# Test impact of covariates on selection into the lottery -----------------

## Run the function over all covariates of interest 
covariate_check <- map_dfr(covariates, covariate_check_fun)


# Replicate analysis for sample_ed selection ------------------------------



# Create table 1 - Covariate Check ------------------------------------------

table_1 <- left_join(control_mean_all, covariate_check
                     , by = 'covariate') 
# Export table

write_csv(table_1, paste0(directory, "output/table_1.csv"))
