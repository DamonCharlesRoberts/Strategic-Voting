#### 2019 ANES Cleaning: removing NA's ####

#### Notes: ####
### Description: Remove No answers and inapplicable, responses from vars of interest ###
### Input Files: None - just a set of functions ###

rm.nas <- function(x){
  x <- mutate_at(x,
    vars(fttrump,ftobama,ftbiden,ftwarren,ftsanders,ftbuttigieg,ftharris,ftblack,ftwhite,vote20dem, vote20cand, electable, vote20jb, vote20ew, vote20bs,lcself,lcd,lcr,pop1,pop2,pop2,pid7,att1,att2,raceid,racework,birthyr,gender,educ,race,ideo5,newsint,pew_religimp,pew_churatd,sentence),
    list(~ ifelse(. == -7 | . == -1 | . == 997 , NA, .))
  )
}
