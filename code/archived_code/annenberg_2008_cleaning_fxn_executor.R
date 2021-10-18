#### 2008 Annenberg Cleaning: Fxn Executor ####

#### Notes: ####
  ### Description: Setup file for cleaning Annenberg dataset ###

#### Files: ####
  ### In: 
  ### Out: 

#### Setup ####
here::here()
source("code/annenberg_2008_cleaning_setup.R")
source("code/1_annenberg_2008_cleaning_remove_nas.R")
source("code/2_annenberg_2008_cleaning_black.R")
source("code/3_annenberg_2008_cleaning_female.R")
source("code/4_annenberg_2008_cleaning_ideology.R")


##### Executing Functions
ann08.01 <- remove.nas(annenberg2008)
ann08.02 <- black(ann08.01)
ann08.03 <- female(ann08.02)
ann08.04 <- ideology(ann08.03)


#### Strategic Voting Measure ####
ann08.05 <- ann08.04 %>%
  mutate(ABc02_2 = ifelse(is.na(ABc02_2), 0, ABc02_2), 
         ABe02_2 = ifelse(is.na(ABe02_2), 0, ABe02_2), 
         ABo02_2 = ifelse(is.na(ABo02_2), 0, ABo02_2)) %>%
  mutate(preference = case_when(ABc02_2 > ABe02_2 & ABc02_2 > ABo02_2 ~ 1,
                          ABe02_2 > ABc02_2 & ABe02_2 > ABo02_2 ~ 2,
                          ABo02_2 > ABc02_2 & ABo02_2 > ABe02_2 ~ 3))
ann08.06 <- ann08.05 %>%
  mutate(stratvote = ifelse(preference == 1 & RBa03_2 == 2, 0, # prefer clinton and vote clinton
                     ifelse(preference == 2 & RBa03_2 == 4, 0, # prefer edwards and vote edwards
                     ifelse(preference == 3 & RBa03_2 == 7, 0, # prefer obama and vote obama
                     ifelse(preference == 1 & RBa03_2 != 2, 1,
                     ifelse(preference == 2 & RBa03_2 != 4, 1,
                     ifelse(preference == 3 & RBa03_2 != 7, 1, NA)))))))
ann08.07 <- ann08.06 %>%
  mutate(stratvote2 = ifelse(preference == 1 & NB03_2 == 2, 0, 
                      ifelse(preference == 2 & NB03_2 == 4, 0, 
                      ifelse(preference == 3 & NB03_2 == 7, 0, 
                      ifelse(preference == 1 & NB03_2 != 2, 1, 
                      ifelse(preference == 2 & NB03_2 != 4, 1, 
                      ifelse(preference == 3 & NB03_2 != 7, 1, NA)))))))
ann08.07 <- ann08.07 %>%
  mutate(across(where(is.character), ~ substr(., 1, 2045)))
write_dta(ann08.07, "Data/annenberg_2008_wave2/annenberg_2008_wave2_updated.dta")
