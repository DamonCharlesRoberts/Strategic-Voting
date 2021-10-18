#### 2008 ANES Cleaning: Set up ####

#### Notes: ####
### Description: Set up document to load original 2008 ANES dta file ###
### Files: ###
## Input: anes_timeseries_2008_dta/anes_timeseries_2008.dta ##
## Output: NA ##

#### Load Dependencies ####
setup <- function() { # Defining the setup function
  library(haven)
  library(tidyverse)
  library(here)
  here::here()
}
setup() # Execute setup function

anes08Orig <- read_dta("Data/anes_timeseries_2008_dta/anes_timeseries_2008.dta") # Load the original (unaltered) 2008 ANES data
