#### 2008 Annenberg Cleaning: Female ####

#### Notes: ####
  ### Description: Setup file for cleaning Annenberg dataset ###

#### Files: ####
  ### In: 
  ### Out: 

#### Setup ####


#### Cleaning ####
ideology <- function(x){
    x <- mutate(x,
                ideo = ifelse(MA06_2 == 1, 7, 
                        ifelse(MA06_2 == 2, 6,
                        ifelse(MA06_2 == 3, 5,
                        ifelse(MA06_2 == 4, 4,
                        ifelse(MA06_2 == 5, 3,
                        ifelse(MA06_2 == 6, 2,
                        ifelse(MA06_2 == 7, 1, NA))))))))
}