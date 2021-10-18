# Title: NAES 1988 Cleaning ----

# Notes: ----
  #* Updated: 2021-05-12 ----
  #* Updated By: dcr ----
  #* Description: Script for Cleaning of 1984 NBES

# Setup ----
  #* Load Libraries ----
  box::use(
    haven = haven[read_dta, write_dta],
    dplyr = dplyr[mutate, case_when, rename,  rowwise],
    magrittr = magrittr[...],
  )
  #* Working Directory ----
  here::here()
  #* Load Data ----
  nes88 = read_dta("Data/anes_1984_1988/NES1988.dta")

# Cleaning ----
  #* Black ----
    #** Coded as: V880412 - 1 White, 2 Black, 3 American Indian or Alaskan Native, 4 Asian or Pacific Islander, 7 Other, 9 NA ----
    #** Recode to: black - 0 Not Black, 1 Black ----
  nes88 = nes88 %>%
    mutate(black = ifelse(V880412 == 2, 1, 0))
  
  #* PID ----  
    #** Coded as: V880274 - 0 Strong Democrat, 1 Weak Democrat, 2 Independent-Democat, 3 Independent, 4 Independent-Republican, 5 Weak Republican, 6 Strong Republican, 7 Other-Minor Party, Refuses to Say, 8 Apolitical, 9 NA
    #** Recode to: -3 Strong Democrat, -2 Weak Democrat, -1 Independent-Democrat, 0 Independent/Other-Minor Party/Refuses to say/Apolitical, 1 Independent Republican, 2 Weak Republican, 3 Strong Republican
  nes88 = nes88 %>%
    mutate(pid = ifelse(V880274 == 0, -3,
                        ifelse(V880274 == 1, -2, 
                               ifelse(V880274 == 2, -1,
                                      ifelse(V880274 == 3, 0,
                                             ifelse(V880274 == 4, 1,
                                                    ifelse(V880274 == 5, 2,
                                                           ifelse(V880274 == 6, 3,
                                                                  ifelse(V880274 == 7, 0,
                                                                         ifelse(V880274 == 8, 0, NA))))))))))

  #* IDEO ----
    #** Coded As: V880228 - 1 Extremely Liberal, 2 Liberal, 3 Slightly Liberal, 4 Moderate, 5 Slightly Conservative, 6 Conservative, 7 Extremely Conservative, 8 Don't Know, 9 NA, 0 Haven't Thought Much ----
    #** Recode to: -3 Extremely Liberal, -2 Liberal, -1 Slightly Liberal, 0 Moderate/Don't Know/Haven't Thought Much, 1 Slightly Conservative, 2 Conservative, 3 Extremely Conservative ----
  nes88 = nes88 %>% 
    mutate(ideo = ifelse(V880228 == 1, -3,
                         ifelse(V880228 == 2, -2,
                                ifelse(V880228 == 3, -1,
                                       ifelse(V880228 == 4, 0,
                                              ifelse(V880228 == 5, 1,
                                                     ifelse(V880228 == 6, 2,
                                                            ifelse(V880228 == 7, 3,
                                                                   ifelse(V880228 == 8, 0,
                                                                         ifelse(V880228 == 0, 0, NA))))))))))
   #* Education ----
    #** Coded as: 1 8 grades or less: 7 advanced degree, 98 Don't know, 99 NA
    #** Recoded to: 1 8 grades or less: 7 advanced degree  
   nes88 = nes88 %>%
    mutate(educ = ifelse(V880422 == 98, NA, ifelse(V880422 == 99, NA, V880422)))
  #* Income ----
    #** Coded as: 0 None or less than 2,999: 22 75,000 and Over, 88 DK, 98 Refused to anser, 99 NA
    #** Recoded to: 0 None or less than 2,999: 22 75,000 and Over
  nes88 = nes88 %>%
   mutate(income = ifelse(V880520 == 88, NA, ifelse(V880520 == 98, NA, ifelse(V880520 == 99, NA, V880520)))) 
  #* Age ----
    #** Coded as: V880414 - Numeric ----
    #* Recode to: Rename to `age` ----
  nes88 = nes88 %>% 
    rename(age = V880228)

  #* Female ----
    #** Coded as: V880413 - 1 Male, 2 Female ----
    #** Recode to: 0 Male, 1 Female ----
  nes88 = nes88 %>% 
    mutate(female = ifelse(V880413 == 2, 1, 0))

  #* Candidate Primary Dummies ----
    #** Coded as: V880151 - 10 Dukakis, 11 Babbitt, 12 Biden, 13 Gephardt, 14 Gore, 15 Hart, 16 Jackson, 17 Simon, 96 Don't Like Any of Them, 97 Other, 98 Can't Remember, 99 NA, 00 NA ----
    #** Recode to: 0 Did Not Support Candidate, 1 Support Candidate ----
  nes88 = nes88 %>% 
    mutate(dukakisSupport = ifelse(V880151 == 10, 1, 0)) %>% 
    mutate(babbittSupport = ifelse(V880151 == 11, 1, 0)) %>% 
    mutate(bidenSupport = ifelse(V880151 == 12, 1, 0)) %>% 
    mutate(gephardtSupport = ifelse(V880151 == 13, 1, 0)) %>% 
    mutate(goreSupport = ifelse(V880151 == 14, 1, 0)) %>% 
    mutate(hartSupport = ifelse(V880151 == 15, 1, 0)) %>% 
    mutate(jacksonSupport = ifelse(V880151 == 16, 1, 0)) %>% 
    mutate(simonSupport = ifelse(V880151 == 17, 1, 0))
  #* Democratic Party Feeling Thermometers ----
    #** Coded as: 0-100, 888 NA, 997 Doesn't recognize name, 998 Can't judge, 999 NA ----
  nes88 = nes88 %>% 
    mutate(demFT = ifelse(V880164 >= 888, NA, V880164))
  
    #** Recode to: 0-100, NA ---- 
  #* Candidate Feeling Thermometers ----
    #** Coded as: 0-100, 888 NA, 997 Doesn't recognize name, 998 Can't judge, 999 NA ----
    #** Recode to: 0-100, NA ---- 
  nes88 = nes88 %>% 
    mutate(dukakisFT = ifelse(V880593 >= 888, NA, V880593)) %>% 
    mutate(babbitFT = ifelse(V880594 >= 888, NA, V880594)) %>%
    mutate(jacksonFT = ifelse(V880162 >= 888, NA, V880162))

  #* Voted for Feeling thermometers ----
    #** Coded as: Feeling thermometer for candidate respondent reported to vote for
  nes88 = nes88 %>% 
    mutate(stratvote = case_when(dukakisSupport == 1 ~ dukakisFT,
                                 babbittSupport == 1 ~ babbitFT,
                                 jacksonSupport == 1 ~ jacksonFT))
  mean = function(x) base::mean(x, na.rm=TRUE)
  nes88 = nes88 %>% 
    mutate(stratvote2 = case_when(dukakisSupport == 1 ~ (dukakisFT - mean(dukakisFT)),
                                 babbittSupport == 1 ~ (babbitFT - mean(babbitFT)),
                                 jacksonSupport == 1 ~ (jacksonFT - mean(jacksonFT))))
  nes88 = nes88 %>%
    rowwise() %>%
    mutate(stratvote3 = case_when(dukakisSupport == 1 ~ (dukakisFT - mean(c(babbitFT, jacksonFT))),
        babbittSupport == 1 ~ (babbitFT - mean(c(dukakisFT, jacksonFT))),
        jacksonSupport == 1 ~ (jacksonFT - mean(c(dukakisFT, babbitFT)))))
# Save Data ----
write_dta(nes88, "Data/anes_1984_1988/nes_1988_updated_2021-05-12.dta")
  