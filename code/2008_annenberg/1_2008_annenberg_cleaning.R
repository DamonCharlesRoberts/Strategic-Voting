# Title: Annenberg 2008 Cleaning ----

# Notes: ----
  #* Updated: 2021-05-12 ----
  #* Updated By: dcr ----
  #* Description: Script for Cleaning of 1984 NBES

# Setup ----
  #* Load Libraries ----
  box::use(
    haven = haven[write_dta],
    dplyr = dplyr[mutate, case_when, rowwise, rename],
    magrittr = magrittr[...],
    readr = readr[read_delim]
  )
  #* Working Directory ----
  here::here()
  #* Load Data ----
  annen = read_delim("Data/annenberg_2008_wave2/annenberg_2008_wave2.txt", delim = "\t")
  annenweight = read_delim("Data/annenberg_2008_wave2/naes08-online-weights.txt", delim = "\t")

# Cleaning ----
  #* Black ----
    #** Coded as: WC01_a- 1 White or white Hispanic, 2 Black, AA, Black Hispanic, 3 Asian, American Indian, 5 Hispanic, no race given, 6 Mixed Race, 7 Other, 998 don't Know, 999 No Answer ----
    #** Recode to: 0 Not Black, 1 Black ----
  annen = annen %>%
    mutate(black = ifelse(WC01_a == 2, 1, 0))

  #* PID ----
    #** Coded as: MA01_2 - 1 Strong Republican, 2 Not Strong Republican, 3 Leans Republican, 4 Independent/other/undecided, 5 Leans Democrat, 6 Not strong Democrat, 7 Strong Democrat ----
    #** Recode to: -3 Strong Democrat, -2 Not strong Democrat, -1 Lean Democrat, 0 Independent/other/undecided, 1 Lean Republican, 2 Not strong Republican, 3 Strong Republican ----
  annen = annen %>% 
    mutate(pid = ifelse(MA01_2 == 7, -3, 
                        ifelse(MA01_2 == 6, -2,
                               ifelse(MA01_2 == 5, -1,
                                      ifelse(MA01_2 == 4, 0,
                                             ifelse(MA01_2 == 3, 1,
                                                    ifelse(MA01_2 == 2, 2,
                                                           ifelse(MA01_2 == 1, 3, NA))))))))

  #* Ideo ----
    #** Coded as: MA06_2 - 1 Extremely Liberal, 2 Liberal, 3 Slightly Liberal, 4 Moderate, 5 Slightly Conservative, 6 Conservative, 7 Extremely Conservative, 999 Skipped ----
    #** Recode to: -3 Extremely Liberal, -2 Liberal, -1 Slightly Liberal, 0 Moderate, 1 Slightly Conservative, 2 Conservative, 3 Extremely Conservative ----
  annen = annen %>% 
    mutate(ideo = ifelse(MA06_2 == 1, -3,
                         ifelse(MA06_2 == 2, -2,
                                ifelse(MA06_2 == 3, -1, 
                                       ifelse(MA06_2 == 4, 0,
                                              ifelse(MA06_2 == 5, 1,
                                                     ifelse(MA06_2 == 6, 2,
                                                            ifelse(MA06_2 == 7, 3, NA))))))))

  #* Education ----
    #** Coded as: 1 Grade 8 or lower: 9 Doctorate degree_a
    #** Renamed
  annen = annen %>%
    rename(educ = WA03_a)
  #* Income ----
    #** Coded as: 1 Less than 5000: 19 175000 or more
    #** Renamed
  annen = annen %>%
    rename(income = WA05_a)
  #* Age ----
    #** Coded as: WA02_a - Numeric ----
    #** Recode to: Rename ----
  annen = annen %>%
    rename(age = WA02_a)

  #* Female ----
    #** Coded as: WA01_a - 1 Male, 2 Female
    #** Recode to: 0 Male, 1 Female
  annen = annen %>%
    mutate(female = ifelse(WA01_a == 2, 1, 0))

  #* Candidate Primary Dummies ----
    #* Coded as: RBa03_2 - 1 Biden, 2 Clinton, 3 Dodd, 4 Edwards, 5 Gravel, 6 Kucinich, 7 Obama, 8 Richardson, 999 NA ----
    #* Recode to: 0 Did not support candidate, 1 Support candidate ----
  annen = annen %>% 
    mutate(bidenSupport = ifelse(RBa03_2 == 1, 1, 0)) %>% 
    mutate(clintonSupport = ifelse(RBa03_2 == 2, 1, 0)) %>% 
    mutate(doddSupport = ifelse(RBa03_2 == 3, 1, 0)) %>% 
    mutate(edwardsSupport = ifelse(RBa03_2 == 4, 1, 0)) %>% 
    mutate(gravelSupport = ifelse(RBa03_2 == 5, 1, 0)) %>% 
    mutate(kucinichSupport = ifelse(RBa03_2 == 6, 1, 0)) %>% 
    mutate(obamaSupport = ifelse(RBa03_2 == 7, 1, 0)) %>% 
    mutate(richardsonSupport = ifelse(RBa03_2 == 8, 1, 0))

  #* Candidate Feeling Thermometer ----
    #** Coded as: 0-100, 101 Don't Know, 102 Don't know enough, 999 Skipped ----
    #** Recode to: 0 - 100 ----
  annen = annen %>% 
    mutate(clintonFT = ifelse(ABc02_2 >= 101, NA, ABc02_2)) %>% 
    mutate(edwardsFT = ifelse(as.numeric(ABe02_2) >= 101, NA, as.numeric(ABe02_2))) %>% 
    mutate(obamaFT = ifelse(ABo02_2 >= 101, NA, ABo02_2))
    #** Does not include Biden, Dodd, Gravel, Kucinich, or Richardson
  #* Voted for Feeling thermometers ----
    #** Coded as: Feeling thermometer for candidate respondent reported to vote for
  annen = annen %>% 
    mutate(stratvote = case_when(clintonSupport == 1 ~ clintonFT,
                                 edwardsSupport == 1 ~ edwardsFT,
                                 obamaSupport == 1 ~ obamaFT))
  mean = function(x) base::mean(x, na.rm=TRUE)
  annen = annen %>% 
    mutate(stratvote2 = case_when(clintonSupport == 1 ~ (clintonFT - mean(clintonFT)),
                                 edwardsSupport == 1 ~ (edwardsFT - mean(edwardsFT)),
                                 obamaSupport == 1 ~ (obamaFT - mean(obamaFT))))
  annen = annen %>%
    rowwise() %>%
    mutate(
      stratvote3 = case_when(clintonSupport == 1 ~ (clintonFT - mean(c(edwardsFT, obamaFT))),
        edwardsSupport == 1 ~ (edwardsFT - mean(c(clintonFT, obamaFT))),
        obamaSupport == 1 ~ (obamaFT - mean(c(clintonFT, edwardsFT)))))
# Merge Survey Weights ----
annen = merge(annen, annenweight, by = "RKEY")
# Save Data ----
write.csv(annen, "Data/annenberg_2008_wave2/annenberg_2008_updated.csv")
  