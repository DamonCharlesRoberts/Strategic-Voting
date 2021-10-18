# Title: NAES 1984 Cleaning ----

# Notes: ----
  #* Updated: 2021-10-14 ----
  #* Updated By: dcr ----
  #* Description: Script for Cleaning of 1984 NBES
# Setup ----
  #* Load Libraries ----
  box::use(
    haven = haven[read_dta, write_dta],
    dplyr = dplyr[mutate, case_when, rename, rowwise],
    magrittr = magrittr[...],
  )
  #* Working Directory ----
  here::here()
  #* Load Data ----
  nes84 = read_dta("Data/anes_1984_1988/NES1984.dta")

# Cleaning ----
nes84 = nes84 %>%
  mutate(
  #* Black ----
    #** Coded as: VAR 840708 - 1 White, 2 Black, 3 American Indian or Alaskan Native, 4 Asian or Pacific Islander, 7 other, 9 NA ----
    #** Recode to: black - 0 Not-Black, 1 Black ----
    black = ifelse(V840708 == 2, 1, 0),
  #* PID ----
    #** Coded as: V840318 - 0 Strong Dem, 1 Weak Dem, 2 Independent Dem, 3 Independent-Independent, 4, Independent Republican, 5 Weak Republican, 6 Strong Republican, 7 Other - Refuse to say, 8 Apolitical, 9 NA ----
    #** Recode to: pid - -3 Strong D, -2 Weak Dem, -1 Lean Dem, 0 Independent/Other-Refuse to Say/Apolitical, 1 Lean Rep, 2 Weak Rep, 3 Strong Rep ----
    pid = ifelse(V840318 == 0, -3,
            ifelse(V840318 == 1, -2,
              ifelse(V840318 == 2, -1,
                ifelse(V840318 == 3, 0,
                  ifelse(V840318 == 4, 1,
                    ifelse(V840318 == 5, 2,
                      ifelse(V840318 == 6, 3,
                        ifelse(V840318 == 7, 0,
                          ifelse(V840318 == 8, 0, NA))))))))),
  #* IDEO ----
    #** Coded as: V840369 1 Extremely Liberal, 2 Liberal, 3 Slightly liberal, 4 Moderate, 5 Slightly Conservative, 6 Conservative,  7 Extremely Conservative, 8 Dont Know, 9 NA, 0 Haven't Thought Much ----
    #** Recode to: -3 Extremely liberal, -2 Liberal, -1 Slightly liberal, 0 Moderate/Don't Know/Haven't Thought Much, 1 Slightly Conservative, 2 Conservative, 3 Extremely Conservative ----
    ideo = ifelse(V840369 == 1, -3,
            ifelse(V840369 == 2, -2,
              ifelse(V840369 == 3, -1,
                ifelse(V840369 == 4, 0,
                  ifelse(V840369 == 5, 1,
                    ifelse(V840369 == 6, 2,
                      ifelse(V840369 == 7, 3,
                        ifelse(V840369 == 8, 0,
                          ifelse(V840369 == 0, 0, NA))))))))),
  #* Education ----
    #** Coded as: 0 No grades completed: 17 Years or more, 98 Don't know, 99 NA ----
    #** Recoded to: 0 No grades completed: 17 years or more ----
    educ = ifelse(V840431 == 98, NA,
            ifelse(V840431 == 99, NA, V840431)),
  #* Income ----
    #** Coded as: 0 None or less than 2,999: 22 75,000 and Over, 88 DK, 98 Refused to anser, 99 NA ----
    #** Recoded to: 0 None or less than 2,999: 22 75,000 and Over ----
    income = ifelse(V840680 == 88, NA,
              ifelse(V840680 == 98, NA,
                ifelse(V840680 == 99, NA, V840680))),
  #* Female ----
    #** Coded as: V840707 - 1 Male, 2 Female ----
    #** Recode to: female - 0 Male, 1 Female ----
    female = ifelse(V840707 == 1, 0,
              ifelse(V840707 == 2, 1, NA)),
  #* Candidate Primary Dummies ----
    #** Coded as: V840211 - 1 Reuben Askew, 2 Alan Cranston, 3 John Glenn, 4 Gary Hart, 5 Ernest Hollings, 6 Jesse Jackson, 7 George McGovern, 8 Walter Mondale, 96 Didn't like anyone, 97 Other, 98 Don't Know or Didn't Care, 99 NA ----
    #** Recode to: candnameSupport - 0 Didn't Support Candidate, 1 Supported Candidate ----
    askew_support = ifelse(V840211 == 1, 1, 0),
    cranston_support = ifelse(V840211 == 2, 1, 0),
    glenn_support = ifelse(V840211 == 3, 1, 0),
    hart_support = ifelse(V840211 == 4, 1, 0),
    holling_support = ifelse(V840211 == 5, 1, 0),
    jackson_support = ifelse(V840211 == 6, 1, 0),
    mcgovern_support = ifelse(V840211 == 7, 1, 0),
    mondale_support = ifelse(V840211 == 8, 1, 0),
  #* Party Feeling Thermometers ----
    #** Coded as: 1-100, 997 Doesn't Recognize Name, 998 Can't Judge, 999 NA ----
    #** Recode to: 1-100, NA NA/Doesn't Recognize Name, 998 Can't Judge ----
    demFT = ifelse(V840305 >= 997, NA, V840305),
  #* Candidate Feeling Thermometers ----
    #** Coded as: 1-100, 997 Doesn't Recognize Name, 998 Can't Judge, 999 NA ----
    #** Recode to: 1-100, NA NA/Doesn't Recognize Name, 998 Can't Judge ----
      #** Note: Askew, Cranston, and Hollings FT's not collected ----
    glenn_ft = ifelse(V840296 >= 997, 0, V840296),
    hart_ft = ifelse(V840297 >= 997, 0, V840297),
    jackson_ft = ifelse(V840298 >= 997, 0, V840298),
    mcgovern_ft = ifelse(V840300 >= 997, 0, V840300),
    mondale_ft = ifelse(V840301 >= 997, 0, V840301),
  #* Most Preferred Candidate ----
    #** Coded as: Individual rating of most preferred candidate based on highest feeling thermometer; 1 - Glenn, 2 - Hart, 3 - Jackson, 4 - McGovern, 5 - Mondale ----
    preference = case_when(glenn_ft >= hart_ft & glenn_ft >= jackson_ft & glenn_ft >= mcgovern_ft & glenn_ft >= mondale_ft ~ 1,
      hart_ft >= glenn_ft & hart_ft >= jackson_ft & hart_ft >= mcgovern_ft & hart_ft >= mondale_ft ~ 2,
      jackson_ft >= glenn_ft & jackson_ft >= hart_ft & jackson_ft >= mcgovern_ft & jackson_ft >= mondale_ft ~ 3,
      mcgovern_ft >= glenn_ft & mcgovern_ft >= hart_ft & mcgovern_ft >= jackson_ft & mcgovern_ft >= mondale_ft ~ 4,
      mondale_ft >= glenn_ft & mondale_ft >= hart_ft & mondale_ft >= jackson_ft & mondale_ft >= mcgovern_ft ~ 5),
  #* Strategic Vote ----
    #** Coded as: 0 - Voted for candidate they most prefer, 1 - Voted for a candidate that they did not most prefer ----
    strat_vote = ifelse(preference == 1 & glenn_support == 1, 0,
                  ifelse(preference == 2 & hart_support == 1, 0,
                    ifelse(preference == 3 & jackson_support == 1, 0,
                      ifelse(preference == 4 & mcgovern_support == 1, 0,
                        ifelse(preference == 5 & mondale_support == 1, 0,
                          ifelse(preference == 1 & glenn_support != 1, 1,
                            ifelse(preference == 2 & hart_support != 1, 1,
                              ifelse(preference == 3 & jackson_support != 1, 1,
                                ifelse(preference == 4 & mcgovern_support != 1, 1,
                                  ifelse(preference == 5 & mondale_support != 1, 1, NA))))))))))
    ) %>%
  #* Age ----
    #** Coded as: V840429 - Numeric ----
    #** Recode to: age - Numeric ----
    rename(age = V840429)
# Save Data ----
write_dta(nes84, "Data/anes_1984_1988/nes_1984_updated_2021-10-14.dta")