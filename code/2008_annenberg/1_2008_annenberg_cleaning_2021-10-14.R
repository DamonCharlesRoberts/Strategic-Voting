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
  ann08 = read_delim("Data/annenberg_2008_wave2/annenberg_2008_wave2.txt", delim = "\t")
  annenweight = read_delim("Data/annenberg_2008_wave2/naes08-online-weights.txt", delim = "\t")

# Cleaning ----
ann08 = ann08 %>%
  mutate(
  #* Black ----
    #** Coded as: WC01_a- 1 White or white Hispanic, 2 Black, AA, Black Hispanic, 3 Asian, American Indian, 5 Hispanic, no race given, 6 Mixed Race, 7 Other, 998 don't Know, 999 No Answer ----
    #** Recode to: 0 Not Black, 1 Black ----
    black = ifelse(WC01_a == 2, 1, 0),
  #* PID ----
    #** Coded as: MA01_2 - 1 Strong Republican, 2 Not Strong Republican, 3 Leans Republican, 4 Independent/other/undecided, 5 Leans Democrat, 6 Not strong Democrat, 7 Strong Democrat ----
    #** Recode to: -3 Strong Democrat, -2 Not strong Democrat, -1 Lean Democrat, 0 Independent/other/undecided, 1 Lean Republican, 2 Not strong Republican, 3 Strong Republican ----
    pid = ifelse(MA01_2 == 7, -3,
            ifelse(MA01_2 == 6, -2,
              ifelse(MA01_2 == 5, -1,
                ifelse(MA01_2 == 4, 0,
                  ifelse(MA01_2 == 3, 1,
                    ifelse(MA01_2 == 2, 2,
                      ifelse(MA01_2 == 1, 3, NA))))))),
  #* Ideo ----
    #** Coded as: MA06_2 - 1 Extremely Liberal, 2 Liberal, 3 Slightly Liberal, 4 Moderate, 5 Slightly Conservative, 6 Conservative, 7 Extremely Conservative, 999 Skipped ----
    #** Recode to: -3 Extremely Liberal, -2 Liberal, -1 Slightly Liberal, 0 Moderate, 1 Slightly Conservative, 2 Conservative, 3 Extremely Conservative ----
    ideo = ifelse(MA06_2 == 1, -3,
            ifelse(MA06_2 == 2, -2,
              ifelse(MA06_2 == 3, -1,
                ifelse(MA06_2 == 4, 0,
                  ifelse(MA06_2 == 5, 1,
                    ifelse(MA06_2 == 6, 2, 
                      ifelse(MA06_2 == 7, 3, NA))))))),
  #* Female ----
    #** Coded as: WA01_a - 1 Male, 2 Female ----
    #** Recode to: 0 Male, 1 Female ----
    female = ifelse(WA01_a == 2, 1, 0),
  #* Candidate Primary Dummies ----
    #** Coded as: RBa03_2 - 1 Biden, 2 Clinton, 3 Dodd, 4 Edwards, 5 Gravel, 6 Kucinich, 7 Obama, 8 Richardson, 999 NA ----
    #** Recode to: 0 Did not support candidate, 1 Support candidate ----
    biden_support = ifelse(RBa03_2 == 1, 1, 0),
    clinton_support = ifelse(RBa03_2 == 2, 1, 0),
    dodd_support = ifelse(RBa03_2 == 3, 1, 0),
    edwards_support = ifelse(RBa03_2 == 4, 1, 0),
    gravel_support = ifelse(RBa03_2 == 5, 1, 0),
    kucinich_support = ifelse(RBa03_2 == 6, 1, 0),
    obama_support = ifelse(RBa03_2 == 7, 1, 0),
    richardson_support = ifelse(RBa03_2 == 8, 1, 0),
  #* Candidate Feeling Thermometer ----
    #** Coded as: 0-100, 101 Don't Know, 102 Don't know enough, 999 Skipped ----
    #** Recode to: 0 - 100 ----
    clinton_ft = ifelse(ABc02_2 >= 101, 0, ABc02_2),
    edwards_ft = ifelse(ABe02_2 >= 101, 0, ABe02_2),
    obama_ft = ifelse(ABo02_2 >= 101, 0, ABo02_2),
  #* Most Preferred Candidate ----
    #** Coded as: Individual rating of most preferred candidate based on highest feeling thermometer; 1 - Clinton, 2 - Edwards, 3 - Obama ----
    preference = case_when(clinton_ft >= edwards_ft & clinton_ft >= obama_ft ~ 1,
                           edwards_ft >= clinton_ft & edwards_ft >= obama_ft ~ 2,
                           obama_ft >= clinton_ft & obama_ft >= edwards_ft ~ 3),
  #* Strategic Vote ----
    #** Coded as: 0 - Voted for candidate they most prefer, 1 - Voted for a candidate that they did not most prefer ----
    strat_vote = ifelse(preference == 1 & clinton_support == 1, 0,
                  ifelse(preference == 2 & edwards_support == 1, 0,
                    ifelse(preference == 3 & obama_support == 1, 0,
                      ifelse(preference == 1 & clinton_support != 1, 1,
                        ifelse(preference == 2 & edwards_support != 1, 1,
                          ifelse(preference == 3 & obama_support != 1, 1, NA))))))
    ) %>%
  rename(
  #* Education ----
    #** Coded as: 1 Grade 8 or lower: 9 Doctorate degree ----
    educ = WA03_a,
  #* Income ----
    #** Coded as: 1 Less than 5000: 19 175000 or more ----
    income = WA05_a,
  #* Age ----
    #** Coded as: WA02_a - Numeric ----
    age = WA02_a
    )
# Merge Survey Weights ----
ann08 = merge(ann08, annenweight, by = "RKEY")
# Save Data ----
write.csv(ann08, "Data/annenberg_2008_wave2/annenberg_2008_updated_2021-10-14.csv")