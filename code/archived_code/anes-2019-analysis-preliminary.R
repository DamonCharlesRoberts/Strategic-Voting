#### 2019 ANES Analysis: Preliminary ####

#### Notes: ####
### Description: Preforming some preliminary analyses on 2019 ANES ####

#### Files: ####
### Input: 'Data/anes_pilot_2019_dta/anes-2019-cleaned.dta'
### Output: 

#### Dependencies: ####
### Input file cleaned using anes-cleaned-*.R files. ###

#### Set up ####
{
  library(dplyr)
  library(haven)
  library(glmnet)
  library(pglm)
  library(ggplot2)
  library(ggeffects)
  library(BayesTree)
  library(gginference)
}
here::here()
dat.2019 <- read_dta('../Data/anes_pilot_2019_dta/anes-2019-cleaned.dta')

#### Diff of Means test among Black R's only####
#bdat <- bdat <- dat[ which(dat$black == 1), ]
  #two tailed
#diffMu.biden.booker <- t.test(bdat$biden_sup, bdat$booker_sup, alternative = 'two.sided', var.equal = FALSE)
#diffMu.biden.warren <- t.test(bdat$biden_sup, bdat$warren_sup, alternative = 'two.sided', var.equal = FALSE)
#diffMu.biden.sanders <- t.test(bdat$biden_sup, bdat$sanders_sup, alternative = 'two.sided', var.equal = FALSE)
#diffMu.biden.booker
#diffMu.biden.warren
#diffMu.biden.sanders
  #one tailed
#diffMu.biden.booker.one <- t.test(bdat$biden_sup, #bdat$booker_sup, alternative = 'greater', var.equal = FALSE)

#biden.booker.plot.uni <- ggttest(diffMu.biden.booker.one)
#biden.booker.plot.uni

#diffMu.biden.warren.one <- t.test(bdat$biden_sup, bdat$warren_sup, alternative = 'greater', var.equal = FALSE)
#biden.warren.plot.uni <- ggttest(diffMu.biden.warren.one, colaccept = 'lightgreen', colreject = 'grey84', colstat = 'green')
#biden.warren.plot.uni

#diffMu.biden.sanders.one <- t.test(bdat$biden_sup, bdat$sanders_sup, alternative = 'greater', var.equal = FALSE)
#biden.sanders.plot.uni <- ggttest(diffMu.biden.sanders.one, colaccept = 'dodgerblue1', colreject = 'grey84',colstat='dodgerblue4')
#biden.sanders.plot.uni
#### Running Some Models ####
#reg1 <- lm(vote20cand ~ black + pid7 + lcself + age + gender, data = dat)
#summary(reg1)

#int1 <- lm(vote20cand ~ black*raceid + pid7 + lcself + age + gender, data = dat)
#summary(int1)

### Dummies for candidate support ###

#reg2 <- lm(biden_sup ~ black + pid7 + lcself + age + gender, data = dat)
#summary(reg2)
#reg3 <- lm(biden_sup ~ black + pid7 + lcself + age + gender + raceid, data = dat)
#summary(reg3)

#int2 <- lm(biden_sup ~ black*racework + pid7 + lcself + age + gender + raceid, data = dat)
#summary(int2)

#stargazer::stargazer(reg2, reg3, int2, type = 'text')

#reg4 <- lm(biden_sup ~ raceid + pid7 + lcself + age + gender, data = bdat)
#reg5 <- lm(biden_sup ~ racework + lcself + pid7 + age + gender, data = bdat)
#int3 <- lm(biden_sup ~ raceid*racework + pid7 + lcself + age + gender, data = bdat)


### Strategic Voting ###
#reg6 <- lm(biden_sup ~ black + strat_jb + fttrump +  pid7 + lcself + age + gender + newsint, data = dat)
#reg7 <- lm(biden_sup ~ black + +strat_jb + raceid + fttrump + pid7 + lcself + age + gender + newsint, data = dat)
#reg8 <- lm(biden_sup ~ black + strat_jb + raceid + racework + fttrump + pid7 + lcself + age + gender + newsint, weights = weight, data = dat)

#reg9 <- lm(strat_jb ~ raceid + pid7 + lcself + age + gender + newsint, data = bdat)
#reg10 <- lm(strat_jb ~ racework + pid7 + lcself + age + gender + newsint, data = bdat)
#int4 <- lm(strat_jb ~ racework*raceid + pid7 + lcself + age + gender + newsint, data = bdat)

#int1.2019 <- lm(biden_sup ~ black*strat_jb + raceid + racework + fttrump + pid7 + lcself + age + gender + newsint, data = dat)
#summary(int1.2019)



#### Corrected Analyses ####

corr2019 <- dat.2019 %>%
  select(biden_sup, black, raceid, racework, lcself, pid7) %>%
  mutate(Support = biden_sup, Black = black, Identity = raceid, Work = racework, Ideology = lcself, PID = pid7)
corr2019.1 <- corr2019 %>%
  select(Support, Black, Identity, Work, Ideology, PID)
ggcorr(corr2019.1,label = TRUE) + ggplot2::labs(title = "Figure 2. 2019 Pearson's r correlations") + ggplot2::theme(legend.position = 'bottom')
reg.2019 <- lm(strat_jb ~ black + raceid + racework + pid7 + lcself + age + gender, data = dat.2019)
