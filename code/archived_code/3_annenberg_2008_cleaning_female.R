#### 2008 Annenberg Cleaning: Female ####

#### Notes: ####
  ### Description: Setup file for cleaning Annenberg dataset ###

#### Files: ####
  ### In: 
  ### Out: 

#### Setup ####


#### Cleaning ####
female <- function(x){
    x <- mutate(x, 
                female = ifelse(WA01_a == 2, 1, 0))
}