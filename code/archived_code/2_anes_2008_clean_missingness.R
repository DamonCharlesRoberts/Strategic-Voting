#### 2008 ANES Cleaning: removing NA's ####

#### Notes: ####
### Description: Remove No answers and inapplicable, responses from vars of interest ###
### Input Files: None - just a set of functions ###
rm.nas <- function(x) { #Defining the removing NA's (rm.nas) function
  x <- mutate_at(
    x,
    vars(V081102, V083040, V083037a, V083077a, V083071a, V083098x, V083069, V083215x, V081101), #specifying what variables to include
    list(~ ifelse(. == -1 | . == -2 | . == -3 | . == -4 | . == -5 | . == -6 | . == -7 | . == -8 | . == -9, NA, .)) # conditional statement specifying to convert variables from list above to have their values converted to NA if meets any of the conditions
  )
}
