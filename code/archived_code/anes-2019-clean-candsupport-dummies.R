#### 2019 ANES Cleaning: Candidate Dummies ####

#### Notes: ####
### Description: Create dummies for intended candidate support in the 2020 Democratic Primary ###

#### Files: ####
### Input: anes_pilot_2019_dta/anes-2019-cleaned.dta ###
### Output: anes_pilot_2019_dta/anes-2019-cleaned.dta ###

#### Setup ####
library(haven)
library(tidyverse)

here::here()

dat <- read_dta('Data/anes_pilot_2019_dta/anes-2019-cleaned.dta')

#### Create the Dummies ####

dat <- dat %>%
  mutate(biden_sup = 
           (case_when(vote20cand == 1 ~ 1,
                      vote20cand >= 2 ~ 0))) %>%
  mutate(bloomberg_sup = 
           (case_when(vote20cand == 2 ~ 1,
                      vote20cand == 1 ~ 0,
                      vote20cand >= 3 ~ 0))) %>%
  mutate(booker_sup = 
           (case_when(vote20cand == 3 ~ 1,
                      vote20cand <= 2 ~ 0,
                      vote20cand >= 4 ~ 0))) %>%
  mutate(buttigieg_sup = 
           (case_when(vote20cand == 4 ~ 1,
                      vote20cand <= 3 ~ 0,
                      vote20cand >= 5 ~ 0))) %>%
  mutate(klobuchar_sup = 
           (case_when(vote20cand == 5 ~ 1, 
                      vote20cand <= 4 ~ 0,
                      vote20cand >= 6 ~ 0))) %>%
  mutate(sanders_sup =
           (case_when(vote20cand == 6 ~ 1,
                      vote20cand <= 5 ~ 0,
                      vote20cand >= 7 ~ 0))) %>%
  mutate(warren_sup =
           (case_when(vote20cand == 7 ~ 1, 
                      vote20cand <= 6 ~ 0,
                      vote20cand >=8 ~ 0))) %>%
  mutate(yang_sup = 
           (case_when(vote20cand == 8 ~ 1,
                      vote20cand <= 7 ~ 0,
                      vote20cand == 9 ~ 0))) %>%
  mutate(other_sup = 
           (case_when(vote20cand == 9 ~ 1,
                      vote20cand <= 8 ~ 0)))


#### Save it####
write_dta(dat, 'Data/anes_pilot_2019_dta/anes-2019-cleaned.dta')
