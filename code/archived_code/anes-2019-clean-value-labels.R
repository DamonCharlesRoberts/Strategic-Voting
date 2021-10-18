#### 2019 ANES Cleaning: Value Labels ####

#### Notes: ####
### Description: Adding Value Labels to categorical variables ####
### Challenges: Tried doing this in Stata then importing the new dataset to R and did not work. ###

#### Files: ####
### Input: 'Data/anes_pilot_2019_dta/anes-2019-cleaned.dta'
### Output: 

#### Dependencies: ####
### Input file cleaned using anes-cleaned-*.R files. ###

dat$vote20dem <- labelled(dat$vote20dem,
                        c(-1, 0, 1), c('Democrat', 'Neither', 'Republican')
                        )
dat$vote20cand <- labelled(dat$vote20cand,
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                         labels = c('Biden', 'Bloomberg', 'Booker', 'Buttigieg', 'Klobuchar', 'Sanders', 'Warren', 'Yang', 'Someone Else')
                         )
dat$electable <- labelled(dat$electable,
                        levels = c(1, 2, 3, 4),
                        labels = c('Shares Positions', 'Beat Trump', 'Neither', 'Dont Know')
                        )
dat$vote20jb <- labelled(dat$vote20jb,
                       levels = c(-1, 0, 1),
                       labels = c('Biden', 'Neither', 'Trump')
                       )
dat$vote20ew <- labelled(dat$vote20ew, 
                       levels = c(-1, 0, 1),
                       labels = c('Warren', 'Neither', 'Trump')
                       )
dat$vote20bs <- labelled(dat$vote20bs,
                       levels = c(-1,0,1),
                       labels = c('Sanders', 'Neither', 'Trump')
                       )
dat$lcself <- labelled(dat$lcself,
                     levels = c(-3, -2, -1, 0, 1, 2, 3),
                     labels = c('Very Liberal', 'Liberal', 'Somewhat Liberal', 'Neither', 'Somewhat Conservative', 'Conservative', 'Very Conservative')
                     )
dat$lcr <- labelled(dat$lcr,
                     levels = c(-3, -2, -1, 0, 1, 2, 3),
                     labels = c('Very Liberal', 'Liberal', 'Somewhat Liberal', 'Neither', 'Somewhat Conservative', 'Conservative', 'Very Conservative')
)

dat$lcd <- labelled(dat$lcd,
                     levels = c(-3, -2, -1, 0, 1, 2, 3),
                     labels = c('Very Liberal', 'Liberal', 'Somewhat Liberal', 'Neither', 'Somewhat Conservative', 'Conservative', 'Very Conservative')
)

dat$ideo5 <- labelled(dat$ideo5,
                    levels = c(-2, -1, 0, 1, 2),
                    labels = c('Very Liberal', 'Liberal', 'Neither', 'Conservative', 'Very Conservative')
                    )
dat$pop1 <- labelled(dat$pop1,
                   levels = c(-2, -1, 0, 1, 2),
                   labels = c('Strongly Disagree', 'Disagree', 'Neither', 'Agree', 'Strongly Disagree')
                   )
dat$pop2 <- labelled(dat$pop2,
                   levels = c(-2, -1, 0, 1, 2),
)
dat$pop3 <- labelled(dat$pop3,
                   levels = c(-2, -1, 0, 1, 2),
                   labels = c('Strongly Disagree', 'Disagree', 'Neither', 'Agree', 'Strongly Disagree')
)
dat$pid7 <- labelled(dat$pid7,
                   levels = c(-3, -2, -1, 0, 1, 2, 3),
                   labels = c('Strong Democrat', 'Democrat', 'Lean Democrat', 'Neither', 'Lean Republican', 'Republican', 'Strong Republican')
                   )
dat$att1 <- labelled(dat$att1,
                   levels = c(0, 1),
                   labels = c('No', 'Yes')
                   )
dat$male <- labelled(dat$gender,
                   levels = c(0, 1),
                   labels = c('Female', 'Male')
                   )
dat$black <- labelled(dat$black,
                    levels = c(0, 1),
                    labels = c('non-Black', 'Black')
                    )
dat$white <- labelled(dat$white,
                    levels = c(0,1),
                    labels = c('non-White', 'White')
                    )
dat$newsint <- labelled(dat$newsint,
                      levels = c(1, 2, 3, 4),
                      labels = c('Hardly', 'Now and then', 'Sometimes', 'Most of the Time')
                      )
