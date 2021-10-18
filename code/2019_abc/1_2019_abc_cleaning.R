# Title: ABC 2019 Cleaning ----

# Notes: ----
  #* Updated: 2021-05-12 ----
  #* Updated By: dcr ----
  #* Description: Script for Cleaning of 1984 NBES

# Setup ----
  #* Load Libraries ----
  box::use(
    haven = haven[read_dta, write_dta],
    dplyr = dplyr[mutate, case_when, rowwise],
    magrittr = magrittr[...],
  )
  #* Working Directory ----
  here::here()
  #* Load Data ----
  abc19 = read_dta("Data/2019_abc_poll/abc_poll.dta")

# Cleaning ----
  #* Black ----
    #** Coded as: 918 - 1 White, 2 Black, 3 White Hispanic, 4 Black Hispanic, 5 Hispanic ----
    #** Recode to: 0 Not Black, 1 Black/Black Hispanic ----
  abc19 = abc19 %>% 
    mutate(black = ifelse(Q918 == 2, 1, 0))

  #* PID ----
    #** Coded as: 901 and 904: 901 -  1 Democrat, 2 Republican, 3 Independent, 4 Other, 8 Don't Know/No Opinion, 9 NA; 904 - 1 Lean Democrat, 2 Lean republican, 3 Neither, 8 Don't Know/No Opinion, 9 NA  ----
    #** Recode to: -2 Democrat, -1 Lean Democrat, 0 Independent/Don't Know/No Opinion, 1 Lean Republican, 2 Republican ----
  abc19 = abc19 %>% 
    mutate(pid = ifelse(Q901 == 1, -2,
                        ifelse(Q904 == 1, -1,
                               ifelse(Q904 == 3, 0,
                                      ifelse(Q904 == 2, 1,
                                             ifelse(Q901 == 2, 3,
                                                    ifelse(Q904 == 8, 0, NA)))))))

  #* Ideo ----
    #** Coded as: 908a, 908b, 908c - 1 Liberal, 2 Moderate, 3 Conservative, 4 Don't think in those terms, 8 Don't Know/No Opinion, 9 NA; 908b - 1 Very Liberal, 2 Somewhat Liberal, 8 Dont Know/No Opinion, 9 NA; 908c - 1 Very Conservative, 2 Somewhat Conservative, 8 Don't know/no opinion, 9 NA ----
    #** Recode to: -2 Very Liberal, -1 Somewhat liberal, 0 Moderate/Don't think in those terms/Don't know (908a only), 1 somewhat conservative, 2 very conservative ----
  abc19 = abc19 %>%
    mutate(ideo = ifelse(Q908A == 1 & Q908B == 1, -2,
                         ifelse(Q908A == 1 & Q908B == 2, -1,
                                ifelse(Q908A == 1 & Q908B == 8, -1,
                                     ifelse(Q908A == 1 & Q908B == 9, -1,
                                            ifelse(Q908A == 2, 0,
                                                   ifelse(Q908A == 4, 0,
                                                          ifelse(Q908A == 8, 0,
                                                               ifelse(Q908A == 3 & Q908C == 2, 1, 
                                                                    ifelse(Q908A == 3 & Q908C == 8, 1, 
                                                                           ifelse(Q908A == 3 & Q908C == 9, 1,
                                                                                  ifelse(Q908A == 3 & Q908C == 1, 2, NA))))))))))))

  #* Education ----
    #** Coded as: 1 8th grade or less: 6 post-graduate
    #** Rename
  abc19 = abc19 %>%
    rename(educ = Q909)
  #* Income ----
    #** Coded as: 1 under 20k: 6 100k or more
  #* Age ----
    #** Coded as: Q910 - Numeric ----
    #** Recode to: Rename ----
  abc19 = abc19 %>% 
    rename(age = Q910)

  #* Female ----
    #** Coded as: Q921 - 1 Male, 2 Female ----
    #** Recode to: 0 Male, 1 Female ----
  abc19 = abc19 %>%
    mutate(female = ifelse(Q921 == 2, 1, 0))
  
  #* Candidate Primary Dummies ----
    #** Coded as: 1 Bennet, 2 Biden, 3 Booker, 4 Bullock, 5 Buttigieg, 6 Castro, 7 de Blasio, 8 Delaney, 9 Gabbard, 11 Harris, 12 Klobuchar, 13 O'Rourke, 14 Ryan, 15 Sanders, 16 Steyer, 17 Warren, 18 Williamson, 19 Yang, 95 Other, 96 None of these, 97 Would not vote ----
    #** Recode to: 0 Do not support candidate, 1 Support candidate ----
  abc19 = abc19 %>% 
    mutate(bennetSupport = ifelse(Q10 == 1, 1, 0)) %>% 
    mutate(bidenSupport = ifelse(Q10 == 2, 1, 0)) %>% 
    mutate(bookerSupport = ifelse(Q10 == 3, 1, 0)) %>% 
    mutate(bullockSupport = ifelse(Q10 == 4, 1, 0)) %>% 
    mutate(buttigiegSupport = ifelse(Q10 == 5, 1, 0)) %>% 
    mutate(castroSupport = ifelse(Q10 == 6, 1, 0)) %>% 
    mutate(deblasioSupport = ifelse(Q10 == 7, 1, 0)) %>% 
    mutate(delaneySupport = ifelse(Q10 == 8, 1, 0)) %>% 
    mutate(gabbardSupport = ifelse(Q10 == 9, 1, 0)) %>% 
    mutate(gillibrandSupport = ifelse(Q10 == 10, 1, 0)) %>%
    mutate(harrisSupport = ifelse(Q10 == 11, 1, 0)) %>% 
    mutate(klobucharSupport = ifelse(Q10 == 12, 1, 0)) %>% 
    mutate(orourkeSupport = ifelse(Q10 == 13, 1, 0)) %>% 
    mutate(ryanSupport = ifelse(Q10 == 14, 1, 0)) %>% 
    mutate(sandersSupport = ifelse(Q10 == 15, 1, 0)) %>% 
    mutate(steyerSupport = ifelse(Q10 == 16, 1, 0)) %>% 
    mutate(warrenSupport = ifelse(Q10 == 17, 1, 0)) %>% 
    mutate(williamsonSupport = ifelse(Q10 == 18, 1, 0)) %>% 
    mutate(yangSupport = ifelse(Q10 == 19, 1, 0))

  #* Candidate Feeling Thermometers ----
    #** Coded as: Q20a-Q20f- 1 Strongly Favorable, 2 Somewhat Favorable, 3 Somewhat Unfavorable, 4 Strongly Unfavorable ----
    #** Recode to: 1 Strongly Unfavorable, 2 Somewhat Unfavorable, 3 Somewhat Favorable, 4 Strongly Favorable ----
  abc19 = abc19 %>%
    mutate(bidenFT = ifelse(Q20_2 == 1, 4,
                            ifelse(Q20_2 == 2, 3,
                                   ifelse(Q20_2 ==3, 2,
                                          ifelse(Q20_2 == 4, 1, NA))))) %>% 
    mutate(sandersFT = ifelse(Q20_3 == 1, 4,
                              ifelse(Q20_3 == 2, 3,
                                     ifelse(Q20_3 == 3, 2,
                                            ifelse(Q20_3 == 4, 1, NA))))) %>% 
    mutate(warrenFT = ifelse(Q20_4 == 1, 4,
                             ifelse(Q20_4 == 2, 3,
                                    ifelse(Q20_4 == 3, 2,
                                           ifelse(Q20_4 == 4, 1, NA))))) %>% 
    mutate(harrisFT = ifelse(Q20_5 == 1, 4,
                             ifelse(Q20_5 == 2, 3,
                                    ifelse(Q20_5 == 3, 2,
                                           ifelse(Q20_5 == 4, 1, NA))))) %>%
    mutate(buttigiegFT = ifelse(Q20_6 == 1, 4,
                                     ifelse(Q20_6 == 2, 3, 
                                            ifelse(Q20_6 == 3, 2,
                                                   ifelse(Q20_6 == 4, 1, NA)))))
  #* Voted for Feeling thermometers ----
    #** Coded as: Feeling thermometer for candidate respondent reported to vote for
abc19 = abc19 %>% 
  mutate(stratvote = case_when(bidenSupport == 1 ~ bidenFT,
                               sandersSupport == 1 ~ sandersFT,
                               warrenSupport == 1 ~ warrenFT,
                               harrisSupport == 1 ~ harrisFT,
                             buttigiegSupport == 1 ~ buttigiegFT))
mean = function(x) base::mean(x, na.rm=TRUE)
abc19 = abc19 %>% 
  mutate(stratvote2 = case_when(bidenSupport == 1 ~ (bidenFT - mean(bidenFT)),
                                sandersSupport == 1 ~ (sandersFT - mean(sandersFT)),
                                harrisSupport == 1 ~ (harrisFT - mean(harrisFT)),
                                buttigiegSupport == 1 ~ (buttigiegFT - mean(buttigiegFT))))
abc19 = abc19 %>%
    rowwise() %>%
    mutate(
        stratvote3 = case_when(bidenSupport == 1 ~ (bidenFT - mean(c(sandersFT, warrenFT, harrisFT, buttigiegFT))),
            sandersSupport == 1 ~ (sandersFT - mean(c(bidenFT, warrenFT, harrisFT, buttigiegFT))),
            warrenSupport == 1 ~ (warrenFT - mean(c(bidenFT, sandersFT, harrisFT, buttigiegFT))),
            harrisSupport == 1 ~ (harrisFT - mean(c(bidenFT, sandersFT, warrenFT, buttigiegFT))),
            buttigiegSupport == 1 ~ (buttigiegFT - mean(c(bidenFT, sandersFT, warrenFT, harrisFT)))))
# Save Data ----
write_dta(abc19, "Data/2019_abc_poll/2019_abc_poll_updated_2021-05-12.dta")