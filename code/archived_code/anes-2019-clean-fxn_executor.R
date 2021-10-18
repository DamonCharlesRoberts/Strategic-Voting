#### 2019 ANES Cleaning: Funciton Executor ####

#### Notes: ####
### Description: Sources functions from all of the 2019-anes cleaning files and executes those functions to actually clean the dataset. ###

#### Files: ####
### Input: anes-2019-clean-setup.R, anes-2019-clean-missingness.R,
### Output: anes_pilot_2019_dta/anes-2019-clean-dataset.dta
here::here()
source('code/anes-2019-clean-setup.R')
source('code/anes-2019-clean-missingness.R')
source('code/anes-2019-clean-dem_and_ideo_centering.R')
source('code/anes-2019-clean-likert_flip.R')
source('code/anes-2019-clean-likert_center.R')
source('code/anes-2019-clean-dummies.R')

#### Run the functions ####
anes.cleaned.0.1 <- rm.nas(anes_2019_orig)
anes.cleaned.0.2 <- otherside.dems.1(anes.cleaned.0.1)
anes.cleaned.0.3 <- otherside.dems.2(anes.cleaned.0.2)
anes.cleaned.0.4 <- otherside.dems.3(anes.cleaned.0.3)
anes.cleaned.0.5 <- flip.four(anes.cleaned.0.4)
anes.cleaned.0.6 <- flip.five.likert(anes.cleaned.0.5)
anes.cleaned.0.7 <- center.five.likert(anes.cleaned.0.6)
anes.cleaned.0.9 <- dummies.switch(anes.cleaned.0.7)


anes.cleaned.0.9$white <- NA
anes.cleaned.0.9$white[anes.cleaned.0.9$race == 1] <- 1
anes.cleaned.0.9$white[anes.cleaned.0.9$race == 2] <- 0
anes.cleaned.0.9$white[anes.cleaned.0.9$race == 3] <- 0
anes.cleaned.0.9$white[anes.cleaned.0.9$race == 4] <- 0
anes.cleaned.0.9$white[anes.cleaned.0.9$race == 5] <- 0
anes.cleaned.0.9$white[anes.cleaned.0.9$race == 6] <- 0
anes.cleaned.0.9$white[anes.cleaned.0.9$race == 7] <- 0
anes.cleaned.0.9$white[anes.cleaned.0.9$race == 8] <- 0

### Make Black Dummy ###
anes.cleaned.0.9$black <- NA
anes.cleaned.0.9$black[anes.cleaned.0.9$race == 1] <- 0
anes.cleaned.0.9$black[anes.cleaned.0.9$race == 2] <- 1
anes.cleaned.0.9$black[anes.cleaned.0.9$race == 3] <- 0
anes.cleaned.0.9$black[anes.cleaned.0.9$race == 4] <- 0
anes.cleaned.0.9$black[anes.cleaned.0.9$race == 5] <- 0
anes.cleaned.0.9$black[anes.cleaned.0.9$race == 6] <- 0
anes.cleaned.0.9$black[anes.cleaned.0.9$race == 7] <- 0
anes.cleaned.0.9$black[anes.cleaned.0.9$race == 8] <- 0

#### Calculate Political Knowledge ####
anes.cleaned.0.9$polknow <- rowMeans(anes.cleaned.0.9[,c("pk_cjus_correct", "pk_germ_correct")])
### Make Strategic Voting Measure ###
anes.cleaned.09 <- anes.cleaned.0.9 %>%
    mutate(ftharris = ifelse(is.na(ftharris), 0, ftharris), 
           ftbiden = ifelse(is.na(ftbiden), 0, ftbiden), 
           ftwarren = ifelse(is.na(ftwarren), 0, ftwarren), 
           ftsanders = ifelse(is.na(ftsanders), 0, ftsanders), 
           ftbuttigieg = ifelse(is.na(ftbuttigieg), 0, ftbuttigieg)) %>%
    mutate(preference = ifelse(ftharris > ftbiden & ftharris > ftwarren & ftharris > ftsanders & ftharris > ftbuttigieg, 1, 
                        ifelse(ftbiden > ftharris & ftbiden > ftwarren & ftbiden > ftsanders & ftbiden > ftbuttigieg, 2, 
                        ifelse(ftwarren > ftharris & ftwarren > ftbiden & ftwarren > ftsanders & ftwarren > ftbuttigieg, 3, 
                        ifelse(ftsanders > ftharris & ftsanders > ftbiden & ftsanders > ftwarren & ftsanders > ftbuttigieg, 4, 
                        ifelse(ftbuttigieg > ftharris & ftbuttigieg > ftbiden & ftbuttigieg > ftwarren & ftbuttigieg > ftsanders, 5, NA))))))
anes.cleaned.09 <- anes.cleaned.09 %>%
    mutate(stratvote = ifelse(preference == 1, 1,
                       ifelse(preference == 2 & vote20cand == 1, 0, 
                       ifelse(preference == 3 & vote20cand == 7, 0, 
                       ifelse(preference == 4 & vote20cand == 6, 0, 
                       ifelse(preference == 5 & vote20cand == 4, 0, 
                       ifelse(preference == 2 & vote20cand != 4, 1, 
                       ifelse(preference == 3 & vote20cand != 7, 1, 
                       ifelse(preference == 4 & vote20cand != 6, 1, 
                       ifelse(preference == 5 & vote20cand != 4, 1, NA)))))))))) %>%
    mutate(stratvote2 = ifelse(electable == 2, 1, 0))
anescheck <- anes.cleaned.09 %>%
    select(ftharris, ftbiden, ftwarren, ftsanders, ftbuttigieg, preference, vote20cand, stratvote,stratvote2)
#### Save Cleaned Dataset ####
anes.cleaned.1.0 <- as_tibble(anes.cleaned.09)

write_dta(anes.cleaned.1.0, 'Data/anes_pilot_2019_dta/anes-2019-cleaned.dta')

source('code/anes-2019-clean-candsupport-dummies.R')
source('code/anes-2019-clean-strategicvoting-measure.R')
source('code/anes-2019-clean-age.R')
