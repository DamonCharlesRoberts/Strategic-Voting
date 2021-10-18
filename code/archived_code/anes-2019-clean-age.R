#### 2019 ANES Cleaning: Age ####

library(haven)
library(dplyr)
here::here()

dat <- read_dta('Data/anes_pilot_2019_dta/anes-2019-cleaned.dta')
#### Create Age Variable ####
dat <- dat %>%
  mutate(age = 2020-birthyr)
write_dta(dat, 'Data/anes_pilot_2019_dta/anes-2019-cleaned.dta')