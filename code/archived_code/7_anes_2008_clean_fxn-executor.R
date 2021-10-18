#### 2008 ANES Cleaning: Funciton Executor ####

#### Notes: ####
### Description: Sources functions from all of the 2008-anes cleaning files and executes those functions to actually clean the dataset. ###

#### Files: ####
### Input: anes-2008-clean-setup.R, anes-2019-clean-missingness.R,
### Output: anes_timeseries_2008_dta/anes-2008-clean-dataset.dta

{
  here::here()
  source("code/1_anes_2008_clean_setup.R")
  source("code/2_anes_2008_clean_missingness.R")
}

#### Run the Functions sourced from the other files ####
anes.cleaned.0.1 <- rm.nas(anes08Orig)

#### Code for One-Off Variables ####
### PID ### Set Strong Democrat == -3 and Strong Republican == 3
### IDEO7 ### Set Extremely Liberal == -3 and Extremely Conservative == 3
### BLACK ### Set Black and Black and Other Race == 1, Otherwise == 0
### PLEASED ### Flip scale so that Extremely Pleased == 5; Not Pleased at all == 1
### FEMALE ### R Female == 1, R Male == 0
### VOTEOBAMA ### R vote for Obama in Primary == 1, 0 Otherwise
### VOTECLINTON ### R Vote for Clinton in Primary == 1, 0 Otherwise
anes.cleaned.0.1 <- anes.cleaned.0.1 %>% 
  mutate(pid = ifelse(V083098x == 0, -3, # Recoding PID 
    ifelse(V083098x == 1, -2,
      ifelse(V083098x == 2, -1,
        ifelse(V083098x == 3, 0,
          ifelse(V083098x == 4, 1,
            ifelse(V083098x == 5, 2,
              ifelse(V083098x == 6, 3, NA)
            )
          )
        )
      )
    )
  )) %>%
  mutate(ideo7 = ifelse(V083069 == 1, -3,  # Recoding Ideology
    ifelse(V083069 == 2, -2,
      ifelse(V083069 == 3, -1,
        ifelse(V083069 == 4, 0,
          ifelse(V083069 == 5, 1,
            ifelse(V083069 == 6, 2,
              ifelse(V083069 == 7, 3, NA)
            )
          )
        )
      )
    )
  )) %>%
  mutate(black = ifelse(V081102 == 2 | V081102 == 6, 1, # Creating Dummy variable for Black
    ifelse(V081102 == 1 | V081102 == 3 | V081102 == 4 | V081102 == 5 | V081102 == 7, 0, NA)
  )) %>%
  mutate(pleased = ifelse(V083171a == 1, 5, # Flipping Likert Scale for whether pleased by having Black President
    ifelse(V083171a == 2, 4,
      ifelse(V083171a == 3, 3,
        ifelse(V083171a == 4, 2,
          ifelse(V083171a == 5, 1, NA)
        )
      )
    )
  )) %>%
  mutate(female = ifelse(V081101 == 1, 0, # Dichotomizing Female
    ifelse(V081101 == 2, 1, NA)
  )) %>%
  mutate(voteObama = ifelse(V083077a == 12, 1, 0)) %>% #Creating Dummy measure of whether voted for Obama
  mutate(voteClinton = ifelse(V083077a == 2, 1, 0)) %>% #Creating Dummy measure of whether voted for Clinton
  rename(
    ftClinton = V083040, # Renaming to feeling thermometer Clinton
    ftObama = V083037a, # Renaming to feeling thermometer Obama
    age = V083215x # Renaming to age variable
  ) %>%
  mutate(stratBO = ifelse(ftClinton > ftObama & voteObama == 1, 1, 0)) %>%  # Creating Measure of Strategically voting for Obama
  mutate(stratHC = ifelse(ftObama > ftClinton & voteClinton == 1, 1, 0))  # Creating Measure of Strategically voting for Clinton

#### Saving The Data ####

write_dta(anes.cleaned.0.1, "Data/anes_timeseries_2008_dta/anes-2008-cleaned.dta")
