# Title: NAES 1984 Cleaning ----

# Notes: ----
  #* Updated: 2021-05-12 ----
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
  #* Black ----
    #** Coded as: VAR 840708 - 1 White, 2 Black, 3 American Indian or Alaskan Native, 4 Asian or Pacific Islander, 7 other, 9 NA ----
    #** Recode to: black - 0 Not-Black, 1 Black ----
  nes84 = nes84 %>%
    mutate(black = ifelse(V840708 == 2, 1, 0))
  #* PID ----
    #** Coded as: V840318 - 0 Strong Dem, 1 Weak Dem, 2 Independent Dem, 3 Independent-Independent, 4, Independent Republican, 5 Weak Republican, 6 Strong Republican, 7 Other - Refuse to say, 8 Apolitical, 9 NA ----
    #** Recode to: pid - -3 Strong D, -2 Weak Dem, -1 Lean Dem, 0 Independent/Other-Refuse to Say/Apolitical, 1 Lean Rep, 2 Weak Rep, 3 Strong Rep ----
  nes84 = nes84 %>%
    mutate(pid = ifelse(V840318 == 0, -3,
                        ifelse(V840318 == 1, -2,
                               ifelse(V840318 == 2, -1,
                                      ifelse(V840318 == 3, 0,
                                             ifelse(V840318 == 4, 1,
                                                    ifelse(V840318 == 5, 2,
                                                           ifelse(V840318 == 6, 3,
                                                                  ifelse(V840318 == 7, 0,
                                                                         ifelse(V840318 == 8, 0, NA))))))))))
  #* IDEO ----
    #** Coded as: V840369 1 Extremely Liberal, 2 Liberal, 3 Slightly liberal, 4 Moderate, 5 Slightly Conservative, 6 Conservative,  7 Extremely Conservative, 8 Dont Know, 9 NA, 0 Haven't Thought Much ----
    #** Recode to: -3 Extremely liberal, -2 Liberal, -1 Slightly liberal, 0 Moderate/Don't Know/Haven't Thought Much, 1 Slightly Conservative, 2 Conservative, 3 Extremely Conservative ----
  nes84 = nes84 %>%
    mutate(ideo = ifelse(V840369 == 1, -3,
                         ifelse(V840369 == 2, -2,
                                ifelse(V840369 == 3, -1,
                                       ifelse(V840369 == 4, 0,
                                              ifelse(V840369 == 5, 1,
                                                     ifelse(V840369 == 6, 2,
                                                            ifelse(V840369 == 7, 3,
                                                                   ifelse(V840369 == 8, 0,
                                                                         ifelse(V840369 == 0, 0, NA))))))))))
      #* Education ----
        #** Coded as: 0 No grades completed: 17 Years or more, 98 Don't know, 99 NA
        #** Recoded to: 0 No grades completed: 17 years or more
    nes84 = nes84 %>%
      mutate(educ = ifelse(V840431 == 98, NA, ifelse(V840431 == 99, NA, V840431)))
      #* Income ----
        #** Coded as: 0 None or less than 2,999: 22 75,000 and Over, 88 DK, 98 Refused to anser, 99 NA
        #** Recoded to: 0 None or less than 2,999: 22 75,000 and Over
    nes84 = nes84 %>%
      mutate(income = ifelse(V840680 == 88, NA, ifelse(V840680 == 98, NA, ifelse(V840680 == 99, NA, V840680))))
  #* Age ----
    #** Coded as: V840429 - Numeric ----
    #** Recode to: age - Numberic ----
  nes84 = nes84 %>%
    rename(age = V840429)

  #* Female ----
    #** Coded as: V840707 - 1 Male, 2 Female ----
    #** Recode to: female - 0 Male, 1 Female ----
  nes84 = nes84 %>%
    mutate(female = ifelse(V840707 == 1, 0, 
                           ifelse(V840707 == 2, 1, NA)))
  
  #* Candidate Primary Dummies ----
    #** Coded as: V840211 - 1 Reuben Askew, 2 Alan Cranston, 3 John Glenn, 4 Gary Hart, 5 Ernest Hollings, 6 Jesse Jackson, 7 George McGovern, 8 Walter Mondale, 96 Didn't like anyone, 97 Other, 98 Don't Know or Didn't Care, 99 NA ----
    #** Recode to: candnameSupport - 0 Didn't Support Candidate, 1 Supported Candidate ----
  nes84 = nes84 %>% 
    mutate(askewSupport = ifelse(V840211 == 1, 1, 0)) %>% 
    mutate(cranstonSupport = ifelse(V840211 == 2, 1, 0)) %>% 
    mutate(glennSupport = ifelse(V840211 == 3, 1, 0)) %>%
    mutate(hartSupport = ifelse(V840211 == 4, 1, 0)) %>% 
    mutate(hollingsSupport = ifelse(V840211 == 5, 1, 0)) %>% 
    mutate(jacksonSupport = ifelse(V840211 == 6, 1, 0)) %>% 
    mutate(mcgovernSupport = ifelse(V840211 == 7, 1, 0)) %>% 
    mutate(mondaleSupport = ifelse(V840211 == 8, 1, 0))
    
    #* Party Feeling Thermometers ----
      #** Coded as: 1-100, 997 Doesn't Recognize Name, 998 Can't Judge, 999 NA ----
      #** Recode to: 1-100, NA NA/Doesn't Recognize Name, 998 Can't Judge ----
    nes84 = nes84 %>% 
      mutate(demFT = ifelse(V840305 >= 997, NA, V840305))
    
    #* Candidate Feeling Thermometers ----
      #** Coded as: 1-100, 997 Doesn't Recognize Name, 998 Can't Judge, 999 NA ----
      #** Recode to: 1-100, NA NA/Doesn't Recognize Name, 998 Can't Judge ---
      
      
    nes84 = nes84 %>% 
      mutate(glennFT = ifelse(V840296 >= 997, NA, V840296)) %>% 
      mutate(hartFT = ifelse(V840297 >= 997, NA, V840297)) %>% 
      mutate(jacksonFT = ifelse(V840298 >= 997, NA, V840298)) %>% 
      mutate(mcgovernFT = ifelse(V840300 >= 997, NA, V840300)) %>% 
      mutate(mondaleFT = ifelse(V840301 >= 997, NA, V840301))
        #*** Note: Askew, Cranston, and Hollings FT not collected ----
  

    #* Voted for Feeling thermometers ----
      #** Coded as: Feeling thermometer for candidate respondent reported to vote for
    nes84 = nes84 %>% 
      mutate(stratvote = case_when(glennSupport == 1 ~ glennFT,
                                   hartSupport == 1 ~ hartFT,
                                   jacksonSupport == 1 ~ jacksonFT,
                                   mcgovernSupport == 1 ~ mcgovernFT,
                                   mondaleSupport == 1 ~ mondaleFT))
mean = function(x) base::mean(x, na.rm=TRUE)
  nes84 = nes84 %>% 
    mutate(stratvote2 = case_when(glennSupport == 1 ~ (glennFT - mean(glennFT)),
                                  hartSupport == 1 ~ (hartFT - mean(hartFT)),
                                  jacksonSupport == 1 ~ (jacksonFT - mean(jacksonFT)),
                                  mcgovernSupport == 1 ~ (mcgovernFT - mean(mcgovernFT)),
                                  mondaleSupport == 1 ~ (mondaleFT - mean(mondaleFT))))
  nes84 = nes84 %>%
    rowwise() %>%
    mutate(stratvote3 = case_when(glennSupport == 1 ~ (glennFT - mean(c(hartFT, jacksonFT, mcgovernFT, mondaleFT))),
            hartSupport == 1 ~ (hartFT - mean(c(glennFT, jacksonFT, mcgovernFT, mondaleFT))),
            jacksonSupport == 1 ~ (jacksonFT - mean(c(glennFT, hartFT, mcgovernFT, mondaleFT))),
            mcgovernSupport == 1 ~ (mcgovernFT - mean(c(glennFT, hartFT, jacksonFT, mondaleFT))),
            mondaleSupport == 1 ~ (mondaleFT - mean(c(glennFT, hartFT, jacksonFT, mcgovernFT)))))
# Save Data ----
write_dta(nes84, "Data/anes_1984_1988/nes_1984_updated_2021-05-12.dta")
    