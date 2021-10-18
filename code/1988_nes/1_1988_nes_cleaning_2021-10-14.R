# Title: NAES 1988 Cleaning ----

# Notes: ----
  #* Updated: 2021-10-14 ----
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
nes88 = nes88 %>%
  mutate(
  #* Black ----
    #** Coded as: V880412 - 1 White, 2 Black, 3 American Indian or Alaskan Native, 4 Asian or Pacific Islander, 7 Other, 9 NA ----
    #** Recode to: black - 0 Not Black, 1 Black ----
    black = ifelse(V880412 == 2, 1, 0),
  #* PID ----  
    #** Coded as: V880274 - 0 Strong Democrat, 1 Weak Democrat, 2 Independent-Democat, 3 Independent, 4 Independent-Republican, 5 Weak Republican, 6 Strong Republican, 7 Other-Minor Party, Refuses to Say, 8 Apolitical, 9 NA ----
    #** Recode to: -3 Strong Democrat, -2 Weak Democrat, -1 Independent-Democrat, 0 Independent/Other-Minor Party/Refuses to say/Apolitical, 1 Independent Republican, 2 Weak Republican, 3 Strong Republican ----
    pid = ifelse(V880274 == 0, -3,
            ifelse(V880274 == 1, -2,
              ifelse(V880274 == 2, -1,
                ifelse(V880274 == 3, 0,
                  ifelse(V880274 == 4, 1,
                    ifelse(V880274 == 5, 2,
                      ifelse(V880274 == 6, 3,
                        ifelse(V880274 == 7, 0,
                          ifelse(V880274 == 8, 0, NA))))))))),
  #* IDEO ----
    #** Coded As: V880228 - 1 Extremely Liberal, 2 Liberal, 3 Slightly Liberal, 4 Moderate, 5 Slightly Conservative, 6 Conservative, 7 Extremely Conservative, 8 Don't Know, 9 NA, 0 Haven't Thought Much ----
    #** Recode to: -3 Extremely Liberal, -2 Liberal, -1 Slightly Liberal, 0 Moderate/Don't Know/Haven't Thought Much, 1 Slightly Conservative, 2 Conservative, 3 Extremely Conservative ----
    ideo = ifelse(V880228 == 1, -3,
            ifelse(V880228 == 2, -2,
              ifelse(V880228 == 3, -1,
                ifelse(V880228 == 4, 0,
                  ifelse(V880228 == 5, 1,
                    ifelse(V880228 == 6, 2,
                      ifelse(V880228 == 7, 3,
                        ifelse(V880228 == 8, 0,
                          ifelse(V880228 == 0, 0, NA))))))))),
  #* Education ----
    #** Coded as: 1 8 grades or less: 7 advanced degree, 98 Don't know, 99 NA ----
    #** Recoded to: 1 8 grades or less: 7 advanced degree ----
    educ = ifelse(V880422 == 98, NA,
            ifelse(V880422 == 99, NA, V880422)),
  #* Income ----
    #** Coded as: 0 None or less than 2,999: 22 75,000 and Over, 88 DK, 98 Refused to anser, 99 NA ----
    #** Recoded to: 0 None or less than 2,999: 22 75,000 and Over ----
    income = ifelse(V880520 == 88, NA, 
              ifelse(V880520 == 98, NA, 
                ifelse(V880520 == 99, NA, V880520))),
  #* Female ----
    #** Coded as: V880413 - 1 Male, 2 Female ----
    #** Recode to: 0 Male, 1 Female ----
    female = ifelse(V880413 == 2, 1, 0),
  #* Candidate Primary Dummies ----
    #** Coded as: V880151 - 10 Dukakis, 11 Babbitt, 12 Biden, 13 Gephardt, 14 Gore, 15 Hart, 16 Jackson, 17 Simon, 96 Don't Like Any of Them, 97 Other, 98 Can't Remember, 99 NA, 00 NA ----
    #** Recode to: 0 Did Not Support Candidate, 1 Support Candidate ----
    dukakis_support = ifelse(V880151 == 10, 1, 0),
    babbitt_support = ifelse(V880151 == 11, 1, 0),
    biden_support = ifelse(V880151 == 12, 1, 0),
    gephardt_support = ifelse(V880151 == 13, 1, 0),
    gore_support = ifelse(V880151 == 14, 1, 0),
    hart_support = ifelse(V880151 == 15, 1, 0),
    jackson_support = ifelse(V880151 == 16, 1, 0),
    simon_support = ifelse(V880151 == 17, 1, 0),
  #* Democratic Party Feeling Thermometers ----
    #** Coded as: 0-100, 888 NA, 997 Doesn't recognize name, 998 Can't judge, 999 NA ----
    #** Recode to: 0-100, NA ----
    dem_ft = ifelse(V880164 >= 888, NA, V880164),
  #* Candidate Feeling Thermometers ----
    #** Coded as: 0-100, 888 NA, 997 Doesn't recognize name, 998 Can't judge, 999 NA ----
    #** Recode to: 0-100, NA ---- 
    dukakis_ft = ifelse(V880593 >= 88, 0, V880593),
    babitt_ft = ifelse(V880594 >= 88, 0, V880594),
    jackson_ft = ifelse(V880162 >= 88, 0, V880162),
  #* Most Preferred Candidate ----
    #** Coded as: Individual rating of most preferred candidate based on highest feeling thermometer; 1 - Dukakis, 2 - Babbitt, 3 - Jackson ----
    preference = case_when(dukakis_ft >= babitt_ft & dukakis_ft >= jackson_ft ~ 1,
      babitt_ft >= dukakis_ft & babitt_ft >= jackson_ft ~ 2,
      jackson_ft >= dukakis_ft & jackson_ft >= babitt_ft ~ 3),
  #* Strategic Vote ----
    #** Coded as: 0 - Voted for candidate they most prefer, 1 - Voted for a candidate that they did not most prefer ----
    strat_vote = ifelse(preference == 1 & dukakis_support == 1, 0,
                  ifelse(preference == 2 & babbitt_support == 1, 0,
                    ifelse(preference == 3 & jackson_support == 1, 0,
                      ifelse(preference == 1 & dukakis_support != 1, 1,
                        ifelse(preference == 2 & babbitt_support != 1, 1, 
                          ifelse(preference == 3 & jackson_support != 1, 1, NA))))))
    ) %>%
  #* Age ----
    #** Coded as: V880414 - Numeric ----
    #* Recode to: Rename to `age` ----
    rename(age = V880228)
# Save Data ----
write_dta(nes88, "Data/anes_1984_1988/nes_1988_updated_2021-10-14.dta")