#### 2019 ANES Cleaning: Likert Flip ####

#### Notes: ####
### Description: Flip likert scales to be Disagree to Agree ###

#### Files: ####
### Input: none ###
### Output: none ###
flip.four <- function(x){
  x <- mutate_at(x,
    vars(newsint,pew_religimp,pew_churatd),
    list(~ ifelse(. == 1, 4,
                 ifelse(. == 2, 3,
                        ifelse(. == 3, 2,
                               ifelse(. == 4, 1,
                                      ifelse(. == 7, NA, .)
                                      )
                               )
                        )
                 )
          )
  )
}
flip.five.likert <- function(x){
  x <- mutate_at(x,
    vars(pop1, pop2, pop3),
    list(~ ifelse(. == 1, 5,
                  ifelse(. == 2, 4,
                         ifelse(. == 3, 3, 
                                ifelse(. == 4, 2,
                                       ifelse(. == 5, 1, .)
                                       )
                                )
                         )
                  )
         )
  )
}


