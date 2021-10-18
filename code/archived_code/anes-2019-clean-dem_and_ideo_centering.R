#### 2019 ANES Cleaning: Put Dems on Otherside of zero #####

#### Notes: ####
### Description: For all variables that discusses democrats, put those items on other side of zero ###

#### Files: ####
### Input: None ###
### Output: None ###

otherside.dems.1 <- function(x){
  x <- mutate_at(x,
    vars(vote20dem,),
    list(~ ifelse(. == 1, -1, 
                  ifelse(. == 2, 1, 
                         ifelse(. == 3, 0, .)
                         )
                  )
         )
  )
}

otherside.dems.2 <- function(x){
  x <- mutate_at(x,
    vars(vote20jb, vote20ew, vote20bs),
    list(~ ifelse(. == 2, -1, 
                  ifelse(. == 3 & . == 4, 0, .)
                  )
         )
  )
}

otherside.dems.3 <- function(x){
  x <- mutate_at(x,
    vars(lcself, lcd, lcr,pid7),
    list(~ ifelse(. == 1, -3, 
                  ifelse(. == 2, -2, 
                         ifelse(. == 3, -1,
                                ifelse(. == 4, 0,
                                       ifelse(. == 5, 1, 
                                              ifelse(. == 6, 2,
                                                     ifelse(. == 7, 3,.)
                                                     )
                                              )
                                       )
                                )
                         )
                  )
         )
  )
}
