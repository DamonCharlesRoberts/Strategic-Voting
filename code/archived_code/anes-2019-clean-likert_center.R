#### 2019 ANES Cleaning: Likert Center ####

#### Notes: ####
### Description: center likert scales around zero ###

#### Files: ####
### Input: none ###
### Output: none ###
center.five.likert <- function(x){
  x <- mutate_at(x,
    vars(pop1, pop2, pop3),
    list(~ ifelse(. == 5, 2,
                  ifelse(. == 4, 1,
                         ifelse(. == 3, 0,
                                ifelse(. == 2, -1,
                                       ifelse(. == 1, -2,.)
                                       )
                                )
                         )
                  )
         )
  )
}

