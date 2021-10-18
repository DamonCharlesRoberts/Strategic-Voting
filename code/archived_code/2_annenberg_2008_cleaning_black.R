#### 2008 Annenberg Cleaning: White ####

#### Notes: ####
  ### Description: Setup file for cleaning Annenberg dataset ###

#### Files: ####
  ### In: 
  ### Out: 

#### Setup ####


#### Cleaning ####
black <- function(x){
    x <- mutate(x,
                black = ifelse(WC01_a == 2, 1, 0))
}