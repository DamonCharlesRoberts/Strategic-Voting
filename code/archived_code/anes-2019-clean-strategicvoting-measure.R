#### 2019 ANES Cleaning: Create Strategic Voting Measure ####

#### Notes: ####
  ### Description: Create measure for strategic voting ###

#### Files: ####
  ### In: anes-2019-cleaned.dta
  ### Out: anes-2019-cleaned.dta

#### Setup ####
library(haven)
library(dplyr)
here::here()
dat <- read_dta('Data/anes_pilot_2019_dta/anes-2019-cleaned.dta')


#### Create measure ####
dat <- dat %>%
  mutate(ft_jb =
           (case_when(ftharris > ftbiden | ftwarren > ftbiden | ftsanders > ftbiden | ftbuttigieg > ftbiden ~ 1,
                      ftharris <= ftbiden & ftwarren <= ftbiden & ftsanders <= ftbiden & ftbuttigieg <= ftbiden ~ 0)
            )
         )
dat <- dat %>%
  mutate(strat_jbharris = 
           (case_when(ftharris > ftbiden ~ 1,
                      ftharris <= ftbiden ~ 0)
            )
         )

dat <- dat %>%
  mutate(strat_jb = 
           (case_when(ft_jb == 1 & biden_sup == 1 ~ 1,
                      ft_jb == 0 & biden_sup == 1 ~ 0,
                      ft_jb == 1 & biden_sup == 0 ~ 0,
                      ft_jb == 0 & biden_sup == 0 ~ 0)))

write_dta(dat, 'Data/anes_pilot_2019_dta/anes-2019-cleaned.dta')


