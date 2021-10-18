#### 2008 Annenberg Cleaning: Remove NAs ####

#### Notes: ####
  ### Description: Setup file for cleaning Annenberg dataset ###

#### Files: ####
  ### In: 
  ### Out: 

#### Setup ####


#### Cleaning ####
remove.nas <- function(x){
    x <- mutate_at(x, 
                vars(ABc02_2, 
                ABe02_2, 
                ABo02_2,
                NB03_2,
                RBa03_2,
                WC01_a,
                WA02_a,
                WA01_a,
                MA01_2,
                MA06_2),
                list(~ ifelse(. == 999, NA,
                        ifelse(. == 101, NA,
                        ifelse(. == 102, NA, .)
                        )
                        )
                )
                )
}
