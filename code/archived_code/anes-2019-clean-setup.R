#### 2019 ANES Cleaning: Set Up ####

#### Notes: ####
#### Description: Set up document to load original 2019 ANES dta file ####
#### Files: ####
### Input: anes_pilot_2019_dta/anes_pilot_2019.dta
### Output: None

# Load dependencies
setup <- function(){
  library(haven)
  library(tidyverse)
  library(here)
  here::here()
}
setup()

anes_2019_orig <- read_dta('Data/anes_pilot_2019_dta/anes_pilot_2019.dta')
