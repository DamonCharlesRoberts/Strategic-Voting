# Title: 2004 ANES Cleaning ----

# Notes: ----
  #* Updated: 2021-05-13 ----
  #* Updated By: dcr ----
  #* Description: Script for Cleaning of 2004 ANES

# Setup ----
  #* Load Libraries ----
box::use(
  haven = haven[read_dta, write_dta],
  dplyr = dplyr[mutate, case_when, rename],
  magrittr = magrittr[...],
  anesr = anesr[...]
)
  #* Working Directory ----
here::here()
  #* Load Data ----
library(anesr)
data(package = "anesr")
data(timeseries_2004)
data(timeseries_2004_doc)
anes04 = timeseries_2004
# Cleaning ----
  #* Black ----
    #** Coded as: V043299 - 10 Black, 12 Black and asian, 13 Black and Native American, 14 Black and Hispanic, 15 Black and White, 20 Asian, 23 Asian and Native American, 24 Asian and Hispanic, 25 Asian and White, 30 Native American, 34 Native American and Hispanic, 35 Native American and White, 40 Hispanic, 45 Hispanic and White, 50 White, 70 Other.
    #** Recoded to: 0 Not Black, 1 Black
anes04 = anes04 %>% 
  mutate(black = ifelse(V043299 == 10, 1, 0))

  #* PID ----
    #** Coded as: 0 Strong Democrat, 1 Weak Democrat, 2 Independent - Democrat, 3 Independent, 4 Independent - Republican, 5 Weak Republican, 6 Strong Republican, 7 Other; Minor Party, 8 Apolitical, 9 Don't Know
    #** Recoded to: -3 Strong Democrat, -2 Weak Democrat, -1 Independent - Democrat, 0 Independent, 1 Independent - Republican, 2 Weak Democrat, 3 Strong Republican
anes04 = anes04 %>% 
  mutate(pid = ifelse(V043116 == 0, -3,
                      ifelse(V043116 == 1, -2, 
                             ifelse(V043116 == 2, -1,
                                    ifelse(V043116 == 3, 0,
                                           ifelse(V043116 == 4, 1,
                                                  ifelse(V043116 == 5, 2,
                                                         ifelse(V043116 == 6, 3,
                                                                ifelse(V043116 == 7, 0,
                                                                       ifelse(V043116 == 8, 0, 
                                                                              ifelse(V043116 == 9, 0, NA)))))))))))

  #* Ideo ----
    #** Coded as: V045117 - 1 Extremely Liberal, 2 Liberal, 3 slightly liberal, 4 moderate, 5 slightly conservative, 6 conservative, 7 extremely conservative, 80 haven't thought much, 88 don't know, 89 refused
    #** Recode to: -3 extremely liberal, -2 liberal, -1 slightly liberal, 0 moderate/haven't thought much/don't know/refused, 1 slightly conservative, 2 conservative, 3 extremely conservative ----
  anes04 = anes04 %>% 
    mutate(ideo = ifelse(V045117 == 1, -3,
                         ifelse(V045117 == 2, -2,
                                ifelse(V045117 == 3, -1,
                                       ifelse(V045117 == 4, 0,
                                              ifelse(V045117 == 5, 1,
                                                     ifelse(V045117 == 6, 2,
                                                            ifelse(V045117 == 7, 3,
                                                                   ifelse(V045117 == 80, 0,
                                                                          ifelse(V045117 == 88, 0,
                                                                                 ifelse(V045117 == 89, 0, NA)))))))))))

  #* Age ----
    #** Coded as: V043250 - Numeric
    #** Recode to: rename
  anes04 = anes04 %>% 
    rename(age = V043250)

  #* Female ----
    #** Coded as: V043411 - 1 Male, 2 Female
    #** Recode to: 0 Male, 1 Female
  anes04 = anes04 %>% 
    mutate(female = ifelse(V043411 == 2, 1, 0))

  #* Candidate Primary Dummies ----
    #** Coded as: 