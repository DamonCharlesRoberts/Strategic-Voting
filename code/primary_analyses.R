# Title: Viability Primary analyses ----

# Notes: -----
  #* Description: Script for primary analyses in viability project ----
  #* Updated: 2021/10/07
  #* Updated by: dcr

# Setup ----
  #* Modularly load packages -----
box::use(
  haven = haven[read_dta],
  tables = tables[...],
  ggplot2 = ggplot2[...],
  rstanarm = rstanarm[stan_glm],
  bayesplot = bayesplot[mcmc_areas],
  patchwork = patchwork[...]
)
  #* Source cleaning files
source('code/1984_nes/1_1984_nes_cleaning_2021-10-14.R') # 1984 NES Cleaning File
source('code/1988_nes/1_1988_nes_cleaning_2021-10-14.R') # 1988 NES Cleaning File
source('code/2008_annenberg/1_2008_annenberg_cleaning_2021-10-14.R') # 2008 Annenberg Cleaning File
source('code/2019_abc/1_2019_abc_cleaning_2021-10-14.R') # 2019 ABC Poll Cleaning File
  #* Load data ----
nes84 = read_dta('Data/anes_1984_1988/nes_1984_updated_2021-10-14.dta')
nes88 = read_dta('Data/anes_1984_1988/nes_1988_updated_2021-10-14.dta')
ann08 = read.csv('Data/annenberg_2008_wave2/annenberg_2008_updated_2021-10-14.csv')
abc19 = read_dta('Data/2019_abc_poll/2019_abc_poll_updated_2021-10-14.dta')

# Descriptive analyses ----
  #* Set up things so that you can present some descriptive statistics ----
mean = function(x) base::mean(x, na.rm=TRUE) # Make a new mean function so that it removes NA's as a default
nes84stratsum = nes84 %>% # Create an object for the NES84 to use as a way to build descriptive stats table
  mutate(glennstrat = ifelse(glenn_support == 1, glenn_ft, NA), # Establish conditionality for variables to be used in descriptive statistics
         hartstrat = ifelse(hart_support == 1, hart_ft, NA), # " "
         jacksonstrat = ifelse(jackson_support == 1, jackson_ft, NA), # " "
         mcgovernstrat = ifelse(mcgovern_support == 1, mcgovern_ft, NA), # " "
         mondalestrat = ifelse(mondale_support == 1, mondale_ft, NA)) # " "
nes88stratsum = nes88 %>%  # Create an object for the NES88 to use as a way to build descriptive stats table
  mutate(dukakisstrat = ifelse(dukakis_support == 1, dukakis_ft, NA), # Establish conditionality for variables to be used in descriptive statistics
         babbittstrat = ifelse(babbitt_support == 1, babbitt_ft, NA), # " "
         jacksonstrat = ifelse(jackson_support == 1, jackson_ft, NA)) # " "
ann08stratsum = ann08 %>%  # Create an object for the 2008 Annenberg to use as a way to build descriptive stats table
  mutate(clintonstrat = ifelse(clinton_support == 1, clinton_ft, NA), # Establish conditionality for variables to be used in descriptive statistics
         edwardsstrat = ifelse(edwards_support == 1, edwards_ft, NA), # " "
         obamastrat = ifelse(obama_support == 1, obama_ft, NA)) # " "
abc19stratsum = abc19 %>%  # Create an object for the abc19 to use as a way to build descriptive stats table
  mutate(bidenstrat = ifelse(biden_support == 1, biden_ft, NA), # Establish conditionality for variables to be used in descriptive statistics
         sandersstrat = ifelse(sanders_support == 1, sanders_ft, NA), # " "
         harrisstrat = ifelse(harris_support == 1, harris_ft, NA), # " "
         buttigiegstrat = ifelse(buttigieg_support == 1, buttigieg_ft, NA)) # " "

  #*  Using objects from above, build some tables ----
nes84stratsumtab = tabular(((Heading('1984') * ((Glenn = glennstrat) + (Hart = hartstrat) + (Jackson = jacksonstrat) + (McGovern = mcgovernstrat) + (Mondale = mondalestrat)))) ~ (((Black = (black == 1)) + (`non-Black` = (black == 0)))*(Mean = mean)), data = nes84stratsum)
nes88stratsumtab = tabular(((Heading('1988') * ((Dukakis = dukakisstrat) + (Babbitt = babbittstrat) + (Jackson = jacksonstrat)))) ~ (((Black = (black == 1)) + (`non-Black` = (black == 0)))*(Mean = mean)), data = nes88stratsum)
ann08stratsumtab = tabular(((Heading('2008') * ((Clinton = clintonstrat) + (Edwards = edwardsstrat) + (Obama = obamastrat)))) ~ (((Black = (black == 1)) + (`non-Black` = (black == 0)))*(Mean = mean)), data = ann08stratsum)
abc19stratsumtab = tabular(((Heading('2020') * ((Biden = bidenstrat) + (Sanders = sandersstrat) + (Harris = harrisstrat) + (Buttigieg = buttigiegstrat)))) ~ (((Black = (black == 1)) + (`non-Black` = (black == 0)))*(Mean = mean)), data = abc19stratsum)

  #* Convert those tables into latex code ----
toLatex(nes84stratsumtab) # Table 1
toLatex(nes88stratsumtab) # Table 2
toLatex(ann08stratsumtab) # Table 3
toLatex(abc19stratsumtab) # Table 4

# OLS Analyses ----
  #* 1984 ----
lm84 = lm(strat_vote ~ black + pid + ideo + age + female + educ + income, data = nes84)
  #* 1988
lm88 = lm(strat_vote ~ black + pid + ideo + age + female + educ + income, data = nes88)
  #* 2008
lm08 = lm(strat_vote ~ black + pid + ideo + age + female + educ + income, data = ann08, weights = WT_2)
  #* 2020
lm20 = lm(strat_vote ~ black + pid + ideo + age + female + educ + income, data = abc19, weights = weight)

  #* Table 
olstab = stargazer::stargazer(lm84, lm88, lm08, lm20,
                   type = 'html',
                   style = 'apsr',
                   keep.stat = c('n', 'adj.rsq', 'f'),
                   star.cutoffs = c(0.10),
                   notes.append = FALSE,
                   notes = c('Ordinary Least Squares Coefficients.', 'Standard Errors in parentheses.', '* p < 0.10'),
                   covariate.labels = c('Black', 'Party ID', 'Ideology', 'Age', 'Female', 'Education', 'Income'),
                   column.labels = c('1984', '1988', '2008', '2019'),
                   dep.var.labels = 'Vote for non-most preferred candidate',
                   out = here::here('figures/primary_reg.html'))

# Bayesian Robustness Check (gives us uncertainty around our estimates given our relatively low N for 1988 and 2019 models) ----
  #* 1984 ----
bay84 = stan_glm(strat_vote ~ black + pid + ideo + age + female, data = nes84, iter = 3000, chains = 4, seed = 717) # 3000 iterations with 4 chains
posterior84 = as.matrix(bay84) # Convert stan object to a matrix
bay84posteriorplot = mcmc_areas(posterior84, pars = c('black'), prob = 0.95) +
labs(title = "1984") # Make a plot showing the posterior predictions for the 'black' estimates
  #* 1988 ----
bay88 = stan_glm(strat_vote ~ black + pid + ideo + age + female, data = nes88, iter = 3000, chains = 4, seed = 717) # 3000 iterations with 4 chains
posterior88 = as.matrix(bay88) # Convert stan object to a matrix
bay88posteriorplot = mcmc_areas(posterior88, pars = c('black'), prob = 0.95) +
labs(title = "1988") # Make a plot showing the posterior predictions for the 'black' estimates
  #* 2008 ----
bay08 = stan_glm(strat_vote ~ black + pid + ideo + age + female, data = ann08, iter = 3000, chains = 4, seed = 717) # 3000 iterations with 4 chains
posterior08 = as.matrix(bay08) # Convert stan object to a matrix
bay08posteriorplot = mcmc_areas(posterior08, pars = c('black'), prob = 0.95) +
labs(title = "2008") # Make a plot showing the posterior predictions for the 'black' estimates
  #* 2019 ----
bay19 = stan_glm(strat_vote ~ black + pid + ideo + age + female, data = abc19, iter = 3000, chains = 4, seed = 717) # 3000 iterations with 4 chains
posterior19 = as.matrix(bay19) # Convert stan object to a matrix
bay19posteriorplot = mcmc_areas(posterior19, pars = c('black'), prob = 0.95) +
labs(title = "2019") # Make a plot showing the posterior predictions for the 'black' estimates

bayplot = (bay84posteriorplot | bay88posteriorplot)/(bay08posteriorplot | bay19posteriorplot) # Use patchwork to put all of these plots in one figure that I can load into LaTeX