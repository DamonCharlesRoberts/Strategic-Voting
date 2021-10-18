#### 2019 ANES Cleaning: Dummies ####

#### Notes: ####
### Description: Make dummies 0&1 instead of 1&2 ###

#### Files: ####
### Input: none ###
### Output: none ###

dummies.switch <- function(x){
  x <- mutate_at(x,
    vars(att1,gender),
    list(~ ifelse(. == 2, 0, .)
         )
  )
}

dummies.white <- function(x){
  x <- mutate(x,
              ifelse(white !=1,0,white)
              )
}

dummies.black <- function(x){
  x <- mutate(x,
              ifelse(black == 2,1,black)
              )
}