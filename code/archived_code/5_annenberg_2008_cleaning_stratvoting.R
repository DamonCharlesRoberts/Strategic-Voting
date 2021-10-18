#### 2008 Annenberg Cleaning: Strategic Voting ####

#### Notes: ####
  ### Description: Setup file for cleaning Annenberg dataset ###

#### Files: ####
  ### In: 
  ### Out: 

#### Setup ####

#### Strategic Voting Measure ####
preference <- function(x){
    x <- mutate(x, 
                preference = case_when(ABc02_2 > ABe02_2 & ABc02_2 > ABo02_2 ~ 1,
                          ABe02_2 > ABc02_2 & ABe02_2 > ABo02_2 ~ 2,
                          ABo02_2 > ABc02_2 & ABo02_2 > ABe02_2 ~ 3))
}

stratvote <- function(x){
    x <- mutate(x,
                stratvote = ifelse(preference == 1 & RBa03_2 == 2, 0,
                            ifelse(preference == 2 & RBa03_2 == 4, 0,
                            ifelse(preference == 3 & RBa03_2 == 7, 0, 1))))
}