#### Primary Analyses ####

#### Notes: ####
  ### Description: Central file for all the analyses ###
    ## Most cleaning done in seperate files ##

#### Analysis Set up ####
{
  library(tidyverse) #Helpful tidyverse fxns
  library(haven) #Loading .dta (STATA) files
  library(stargazer) #Package for table making
  library(GGally) #Package for creating correlation plots
  library(ggthemes) #Package to make theme for graphs
  library(ggpubr) #Package for ggarrange - put multiple figures in same image  
  here::here() #Shortcut to setting WD to project directory
}

#### Load the Data ####
  ### 2019 ANES ###
anes19 <- read_dta("Data/anes_pilot_2019_dta/anes-2019-cleaned.dta")# Loading 2019 ANES data
anes19 <- anes19 %>%
  rename(pid = pid7, 
         strategicvote = strat_jb) %>%
  mutate(female = ifelse(gender == 1, 0, 
                         ifelse(gender == 0, 1, NA))) #creating a dichotomous variable from gender titled female. Female self-identified respondents == 1, otherwise == 0
  ### 2008 Annenberg ###
ann08 <- read_dta("Data/annenberg_2008_wave2/annenberg_2008_wave2_updated.dta")

#### Correlation Plots ####
#
#  ### 2019 ANES ###
#corrplotMatrix19 <- anes19 %>%
#  select(stratvote, black, raceid, racework, lcself, pid) %>%
#  mutate(Strategic = stratvote, Black = black, Identity = raceid, Work = racework, Ideology = lcself, PID = pid) %>%
#  select(Strategic, Black, Identity, Work, Ideology, PID)
#ggcorr(corrplotMatrix19,label = TRUE) + 
#  ggplot2::labs(title = "Figure 2. 2019 Pearson's r correlations") + 
#  ggplot2::theme(legend.position = 'bottom') +
#  ggsave("Figures/anes-2019-corrplot.png")
#
  ### 2008 Annenberg ###
#corrplotMatrix08 <- ann08 %>%
#  select(stratvote, black, MA01_2, MA06_2) %>%
#  mutate(Strategic = stratvote, Black = black, PartyID = MA01_2, Ideology = MA06_2) %>%
#  select(Strategic, Black, PartyID, Ideology)
#ggcorr(corrplotMatrix08, label = TRUE) +
#    ggplot2::labs(title = "Figure 1. 2008 Pearson's r correlations") +
#    ggplot2::theme(legend.position = 'bottom') +
#    ggsave("Figures/annenberg-2008-corrplot.png")

#### Motivations for Voting - Descriptives ####
  ### 2019 ###
    ## Filter to only look at why Black voters chose their primary/caucus candidate and create value labels for it##
label <- c('Position', 'Defeat Trump', 'Neither', 'Do not know')

motivationanes19black <- anes19 %>%
  mutate(Electability = ifelse(electable == 2, 1, 0)) %>%
  filter(black == 1) %>%
  select(Electability)
motivationanes19nonblack <- anes19 %>%
  mutate(Electability = ifelse(electable == 2, 1, 0)) %>%
  filter(black == 0) %>% 
  select(Electability)
motivationanes19black <- as.data.frame(motivationanes19black)
motivationanes19nonblack <- as.data.frame(motivationanes19nonblack)


    ## Look at break down ##
motivationanes19blackTable <- stargazer(motivationanes19black,
                                        type = 'html', 
                                        style = 'apsr',
                                        title = 'Electability among Black Respondents - 2019 ANES',
                                        notes = c('Source: 2019 American National Election Study', 'Descriptive statistics of supporter responses that Biden can beat Trump.'),
                                        covariate.labels = c('Electability - Black Respondents'),
                                        notes.append = FALSE,
                                        out = 'figures/descriptive-electability-black-2019.html')

motivationanes19nonblackTable <- stargazer(motivationanes19nonblack,
                                           type = 'html',
                                           style = 'apsr',
                                           title = 'Electability among non-Black Respondents - 2019 ANES',
                                           notes = c('Source: 2019 American National Election Study', 'Descriptive statistics of supporter responses that Biden can beat Trump.'),
                                           covariate.labels = c('Electability - Non-Black Respondents'),
                                           notes.append = FALSE,
                                           out = 'figures/descritpive-electability-non-black-2019.html')
  ### 2008 Annenberg ###
    ## Thinks Clinton has best chance of winning general election ##
ann08 <- ann08 %>%
  mutate(ClintonWin = ifelse(NB03_2 == 2, 1, 0), 
        EdwardsWin = ifelse(NB03_2 == 4, 1, 0),           
        ObamaWin = ifelse(NB03_2 == 7, 1, 0))
ann08black <- ann08 %>%
  filter(black == 1) %>%
  select(ClintonWin, EdwardsWin, ObamaWin)
ann08nonblack <- ann08 %>%
  filter(black == 0) %>%
  select(ClintonWin, EdwardsWin, ObamaWin)

ann08black <- as.data.frame(ann08black)
ann08blacktable <- stargazer(ann08black, 
                                        type = 'html', 
                                        style = 'apsr',
                                        title = 'Electability among Black Respondents - 2008 Annenberg Study',
                                        notes = c('Source: 2008 Annenberg Study - Wave 2', 'Descriptive statistics of those who say Clinton, Edwards, or Obama can win general election'),
                                        covariate.labels = c('Clinton', 'Edwards', 'Obama'),
                                        notes.append = FALSE,
                                        out = 'figures/descriptive-electability-black-2008.html')
ann08nonblack <- as.data.frame(ann08nonblack)
ann08nonblacktable <- stargazer(ann08nonblack, 
                                        type = 'html', 
                                        style = 'apsr',
                                        title = 'Electability among non-Black Respondents - 2008 Annenberg Study',
                                        notes = c('Source: 2008 Annenberg Study - Wave 2', 'Descriptive statistics of those who say Clinton, Edwards, or Obama can win general election'),
                                        covariate.labels = c('Clinton', 'Edwards', 'Obama'),
                                        notes.append = FALSE,
                                        out = 'figures/descriptive-electability-nonblack-2008.html')

##### Plots - Descriptives ####
  ### Motivations for Voting ###
    ## 2019 ##
      # Data Management #
pal19 <- c("#000000", "#56B4E9")
motivationanes19black <- anes19 %>%
  mutate(Electability = ifelse(electable == 2, 1, 0)) %>%
  filter(black == 1, !is.na(Electability)) %>%
  select(Electability)
motivationanes19nonblack <- anes19 %>%
  mutate(Electability = ifelse(electable == 2, 1, 0)) %>%
  filter(black == 0, !is.na(Electability)) %>% 
  select(Electability)
motivationanes19black$Electability <- factor(motivationanes19black$Electability,
  levels = c(0, 1),
  labels = c("Other Motivation", "Electable"))
motivationanes19nonblack$Electability <- factor(motivationanes19nonblack$Electability,
  levels = c(0, 1),
  labels = c("Other Motivation", "Electable"))
        # Plot #
anes19MotivationsPlotblack <- ggplot(data = motivationanes19black, mapping = aes(x = Electability, fill = Electability)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  geom_bar(stat = "count", aes(y = ..count../sum(..count..))) +
  scale_fill_manual(values = c("#000000", "#56B4E9")) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylim(0,0.6) +
  labs(y = "Proportion", x = "Motivation", title = "Black Voters")
anes19MotivationsPlotnonblack <- ggplot(data = motivationanes19nonblack, mapping = aes(x = Electability, fill = Electability)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  geom_bar(stat = "count", aes(y = ..count../sum(..count..))) +
  scale_fill_manual(values = c("#000000", "#56B4E9")) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylim(0,0.6) +
  labs(y = "Proportion", x = "Motivation", title = "Non-Black Voters")
anes19MotivationsPlot <- ggarrange(anes19MotivationsPlotblack, NULL, anes19MotivationsPlotnonblack, NULL,
  widths = c(1, 0.15, 1, 0.15), nrow = 1)
anes19MotivationsPlot.a <- annotate_figure(anes19MotivationsPlot,
  bottom = text_grob("Data Source: 2019 American National Election Study.\n Note: Proportion of vote choice motivation for respondents by race."),
  top = text_grob("2019 Vote Choice Motivations"))
ggsave(plot = anes19MotivationsPlot.a, "figures/2019-motivations-plot.png", height = 10.1, width = 10.1, units = "in")

    ## 2008 ##
      # Data Management #
ann08plot <- ann08 %>%
  mutate(ClintonWin = ifelse(NB03_2 == 2, 1, 0), 
        EdwardsWin = ifelse(NB03_2 == 4, 1, 0),           
        ObamaWin = ifelse(NB03_2 == 7, 1, 0)) %>%
  filter(!is.na(ClintonWin), !is.na(EdwardsWin), !is.na(ObamaWin))
ann08plot$ClintonWin <- factor(ann08plot$ClintonWin,
  levels = c(0, 1),
  labels = c("Other Motivation", "Electable"))
ann08plot$EdwardsWin <- factor(ann08plot$EdwardsWin,
  levels = c(0, 1),
  labels = c("Other Motivation", "Electable"))
ann08plot$ObamaWin <- factor(ann08plot$ObamaWin,
  levels = c(0, 1),
  labels = c("Other Motivation", "Electable"))
ann08black <- ann08plot %>%
  filter(black == 1)
ann08nonblack <- ann08plot %>%
  filter(black == 0)
      # Plot #
ann08ClintonMotivationsPlotblack <- ggplot(data = ann08black, mapping = aes(x = ClintonWin, fill = ClintonWin)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  geom_bar(stat = "count", aes(y = ..count../sum(..count..))) +
  scale_fill_manual(values = c("#999999", "#E69F00")) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylim(0, 1.0) +
  labs(y = "Proportion", x = "Clinton Win", title = "Black Voters")
ann08ClintionMotivationsPlotnonblack <- ggplot(data = ann08nonblack, mapping = aes(x = ClintonWin, fill = ClintonWin)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  geom_bar(stat = "count", aes(y = ..count../sum(..count..))) +
  scale_fill_manual(values = c("#999999", "#E69F00")) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylim(0, 1.0) +
  labs(y = "Proportion", x = "Clinton Win", title = "Non-Black Voters")

ann08EdwardsMotivationsPlotblack <- ggplot(data = ann08black, mapping = aes(x = EdwardsWin, fill = EdwardsWin)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  geom_bar(stat = "count", aes(y = ..count../sum(..count..))) +
  scale_fill_manual(values = c("#999999", "#E69F00")) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylim(0, 1.0) +
  labs(y = "Proportion", x = "Edwards Win", title = "Black Voters")
ann08EdwardsMotivationsPlotnonblack <- ggplot(data = ann08nonblack, mapping = aes(x = EdwardsWin, fill = EdwardsWin)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  geom_bar(stat = "count", aes(y = ..count../sum(..count..))) +
  scale_fill_manual(values = c("#999999", "#E69F00")) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylim(0, 1.0) +
  labs(y = "Proportion", x = "Edwards Win", title = "Non-Black Voters")

ann08ObamaMotivationsPlotblack <- ggplot(data = ann08black, mapping = aes(x = ObamaWin, fill = ObamaWin)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  geom_bar(stat = "count", aes(y = ..count../sum(..count..))) +
  scale_fill_manual(values = c("#999999", "#E69F00")) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylim(0, 1.0) +
  labs(y = "Proportion", x = "Obama Win", title = "Black Voters")
ann08ObamaMotivationsPlotnonblack <- ggplot(data = ann08nonblack, mapping = aes(x = ObamaWin, fill = ObamaWin)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  geom_bar(stat = "count", aes(y = ..count../sum(..count..))) +
  scale_fill_manual(values = c("#999999", "#E69F00")) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylim(0, 1.0) +
  labs(y = "Proportion", x = "Obama Win", title = "Non-Black Voters")

ann08MotivationPlot <- ggarrange(ann08ClintonMotivationsPlotblack, NULL, ann08ClintionMotivationsPlotnonblack, NULL, ann08ObamaMotivationsPlotblack, NULL, ann08ObamaMotivationsPlotnonblack, NULL, ann08EdwardsMotivationsPlotblack, NULL, ann08EdwardsMotivationsPlotnonblack, NULL,
  widths = c(1, 0.15, 1, 0.15), nrow = 3, ncol = 4, align = "v")
ann08MotivationsPlot.a <- annotate_figure(ann08MotivationPlot,
  bottom = text_grob("Data Source: 2008 Annenberg Survey.\n Note: Proportion of voters believing candidate can win."),
  top = text_grob("2008 Vote Choice Motivations"))
ggsave(plot = ann08MotivationsPlot.a, "figures/2008-motivations-plot.png", height = 10.1, width = 10.1, units = "in")

  ### Count of Respondents per measure ###
    ## 2019 ##
      # Data Management #
anes19stratblack <- anes19 %>%
  filter(black == 1, !is.na(strategicvote), !is.na(stratvote), !is.na(stratvote2))
anes19stratblack$strategicvote <- factor(anes19stratblack$strategicvote,
  levels = c(0, 1),
  labels = c("No", "Yes"))
anes19stratblack$stratvote <- factor(anes19stratblack$stratvote,
  levels = c(0, 1),
  labels = c("No", "Yes"))
anes19stratblack$stratvote2 <- factor(anes19stratblack$stratvote2,
  levels = c(0, 1),
  labels = c("No", "Yes"))
anes19stratnonblack <- anes19 %>%
  filter(black == 0, !is.na(strategicvote), !is.na(stratvote), !is.na(stratvote2))
anes19stratnonblack$strategicvote <- factor(anes19stratnonblack$strategicvote,
  levels = c(0, 1),
  labels = c("No", "Yes"))
anes19stratnonblack$stratvote <- factor(anes19stratnonblack$stratvote,
  levels = c(0, 1),
  labels = c("No", "Yes"))
anes19stratnonblack$stratvote2 <- factor(anes19stratnonblack$stratvote2,
  levels = c(0, 1),
  labels = c("No", "Yes"))
      # Plot #
anes19StratPlotBlack <- ggplot(data = anes19stratblack, mapping = aes(x = strategicvote, fill = strategicvote)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  geom_bar(stat = "count", aes(y = ..count../sum(..count..))) +
  scale_fill_manual(values = c("#000000", "#56B4E9")) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylim(0,1.0) +
  labs(y = "Proportion", x = "Strategic Vote - Measure 1", title = "Black Voters")
anes19StratPlotNonBlack <- ggplot(data = anes19stratblack, mapping = aes(x = strategicvote, fill = strategicvote)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  geom_bar(stat = "count", aes(y = ..count../sum(..count..))) +
  scale_fill_manual(values = c("#000000", "#56B4E9")) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylim(0,1.0) +
  labs(y = "Proportion", x = "Strategic Vote - Measure 1", title = "Non-Black Voters")

anes19Strat2PlotBlack <- ggplot(data = anes19stratblack, mapping = aes(x = stratvote, fill = stratvote)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  geom_bar(stat = "count", aes(y = ..count../sum(..count..))) +
  scale_fill_manual(values = c("#000000", "#56B4E9")) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylim(0,1.0) +
  labs(y = "Proportion", x = "Strategic Vote - Measure 2", title = "Black Voters")
anes19Strat2PlotNonBlack <- ggplot(data = anes19stratblack, mapping = aes(x = stratvote, fill = stratvote)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  geom_bar(stat = "count", aes(y = ..count../sum(..count..))) +
  scale_fill_manual(values = c("#000000", "#56B4E9")) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylim(0,1.0) +
  labs(y = "Proportion", x = "Strategic Vote- Measure 2", title = "Non-Black Voters")

anes19Strat3PlotBlack <- ggplot(data = anes19stratblack, mapping = aes(x = stratvote2, fill = stratvote2)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  geom_bar(stat = "count", aes(y = ..count../sum(..count..))) +
  scale_fill_manual(values = c("#000000", "#56B4E9")) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylim(0,1.0) +
  labs(y = "Proportion", x = "Strategic Belief - Measure 3", title = "Black Voters")
anes19Strat3PlotNonBlack <- ggplot(data = anes19stratblack, mapping = aes(x = stratvote2, fill = stratvote2)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  geom_bar(stat = "count", aes(y = ..count../sum(..count..))) +
  scale_fill_manual(values = c("#000000", "#56B4E9")) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylim(0,1.0) +
  labs(y = "Proportion", x = "Strategic Belief - Measure 3", title = "Non-Black Voters")



anes19stratPlot <- ggarrange(anes19StratPlotBlack, NULL, anes19StratPlotNonBlack, NULL, anes19Strat2PlotBlack, NULL, anes19Strat2PlotNonBlack, NULL, anes19Strat3PlotBlack, NULL, anes19Strat3PlotNonBlack, NULL,
  widths = c(1, 0.15, 1, 0.15), nrow = 3, ncol = 4)
anes19stratPlot.a <- annotate_figure(anes19stratPlot,
  bottom = text_grob("Data Source: 2019 American National Election Study.\n Note: Proportion of respondents categorized as strategic voters by measure."),
  top = text_grob("2019 Strategic Voting"))
ggsave(plot = anes19stratPlot.a, "figures/2019-strat-plot.png", height = 10.1, width = 10.1, units = "in")

    ## 2008 ##
      # Data Management #
ann08StratBlack <- ann08 %>%
  filter(black == 1, !is.na(stratvote), !is.na(stratvote2))
ann08StratBlack$stratvote <- factor(ann08StratBlack$stratvote,
  levels = c(0,1),
  labels = c("No", "Yes"))
ann08StratBlack$stratvote2 <- factor(ann08StratBlack$stratvote2,
  levels = c(0,1),
  labels = c("No", "Yes"))
ann08StratNonBlack <- ann08 %>%
  filter(black == 0, !is.na(stratvote), !is.na(stratvote2))
ann08StratNonBlack$stratvote <- factor(ann08StratNonBlack$stratvote,
  levels = c(0,1),
  labels = c("No", "Yes"))
ann08StratNonBlack$stratvote2 <- factor(ann08StratNonBlack$stratvote2,
  levels = c(0,1),
  labels = c("No", "Yes"))
        # Plot #
ann08StratPlotBlack <- ggplot(data = ann08StratBlack, mapping = aes(x = stratvote, fill = stratvote)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  geom_bar(stat = "count", aes(y = ..count../sum(..count..))) +
  scale_fill_manual(values = c("#999999", "#E69F00")) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylim(0,1.0) +
  labs(y = "Proportion", x = "Strategic Vote - Measure 1", title = "Black Voters")
ann08StratPlotNonBlack <- ggplot(data = ann08StratNonBlack, mapping = aes(x = stratvote, fill = stratvote)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  geom_bar(stat = "count", aes(y = ..count../sum(..count..))) +
  scale_fill_manual(values = c("#999999", "#E69F00")) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylim(0,1.0) +
  labs(y = "Proportion", x = "Strategic Vote - Measure 1", title = "Non-Black Voters")

ann08Strat2PlotBlack <- ggplot(data = ann08StratBlack, mapping = aes(x = stratvote2, fill = stratvote2)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  geom_bar(stat = "count", aes(y = ..count../sum(..count..))) +
  scale_fill_manual(values = c("#999999", "#E69F00")) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylim(0,1.0) +
  labs(y = "Proportion", x = "Strategic Vote - Measure 2", title = "Black Voters")
ann08Strat2PlotNonBlack <- ggplot(data = ann08StratNonBlack, mapping = aes(x = stratvote2, fill = stratvote2)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  geom_bar(stat = "count", aes(y = ..count../sum(..count..))) +
  scale_fill_manual(values = c("#999999", "#E69F00")) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylim(0,1.0) +
  labs(y = "Proportion", x = "Strategic Vote- Measure 2", title = "Non-Black Voters")

ann08StratPlot <- ggarrange(ann08StratPlotBlack, NULL, ann08StratPlotNonBlack, NULL, ann08Strat2PlotBlack, NULL, ann08Strat2PlotNonBlack, NULL, 
  widths = c(1, 0.15, 1, 0.15), nrow = 2, ncol = 4, align = "v")
ann08StratPlot.a <- annotate_figure(ann08StratPlot,
  bottom = text_grob("Data Source: 2008 Annenberg Survey.\n \n Note: Proportion of respondents categorized as strategic voters by measure."),
  top = text_grob("2008 Strategic Voting"))
ggsave(plot = ann08StratPlot.a, "figures/2008-strat-plot.png", height = 10.1, width = 10.1, units = "in")

#### Regression Analyses ####
  ### 2019 ANES ###
  ## First Operationalization of the Strategic Vote: Do people that are less supportive of Biden still support him? 
stratvote1.19 <- lm(strategicvote ~ black + raceid + racework + pid + lcself + age + female, data = anes19)
texreg::screenreg(stratvote1.19)
  ## Second Operationalization of the Strategic Vote: Mismatch between preference from feeling thermometer to who they end up voting for?
stratvote2.19 <- lm(stratvote ~ black + raceid + racework + pid + lcself + age + female, data = anes19)
texreg::screenreg(stratvote2.19)


  ## Third Operationalization of the Strategic Vote: Do they support the candidate because they think they can beat Trump? 
stratvote3.19 <- lm(stratvote2 ~ black + raceid + racework + pid + lcself + age + female, data = anes19)
texreg::screenreg(stratvote3.19)


  ## Create Table ##
anes19regtable <- stargazer(stratvote1.19, stratvote2.19, stratvote3.19, 
                            covariate.labels = c('Black', 'Racial Identity', 'Linked Fate', 'Party Identification', 'Ideology', 'Age', 'Female'),
                            dep.var.labels = c("Strategic Vote", "Strategic Vote", "Strategic Vote"), 
                            type = 'html',
                            style = 'apsr',
                            title = 'Regression comparing Black to non-Black voters on the use of the strategic vote',
                            notes = c('Source: 2019 American National Election Study', 'Ordinary least squares coefficients.', 'Standard errors in parentheses.', '* p < 0.05'), 
                            notes.append = FALSE,
                            keep.stat = c('n', 'adj.rsq'),
                            star.cutoffs = c(0.05),
                            out = 'figures/Regression-2019.html'
                            )
  ## Robustness
stratvote1robust.19 <- lm(strategicvote ~ black + raceid + racework + polknow + pid + lcself + age + female, data = anes19)
texreg::screenreg(stratvote1robust.19)
stratvote2robust.19 <- lm(stratvote ~ black + raceid + racework + polknow + pid + lcself + age + female, data = anes19)
texreg::screenreg(stratvote2robust.19)
stratvote3robust.19 <- lm(stratvote2 ~ black + raceid + racework + polknow + pid + lcself + age + female, data = anes19)
texreg::screenreg(stratvote3robust.19)
anes19robustregtable <- stargazer(stratvote1robust.19, stratvote2robust.19, stratvote3robust.19, 
                            covariate.labels = c('Black', 'Racial Identity', 'Linked Fate', 'Political Knowledge', 'Party Identification', 'Ideology', 'Age', 'Female'),
                            dep.var.labels = c("Strategic Vote", "Strategic Vote", "Strategic Vote"), 
                            type = 'html',
                            style = 'apsr',
                            title = 'Regression comparing Black to non-Black voters on the use of the strategic vote',
                            notes = c('Source: 2019 American National Election Study', 'Ordinary least squares coefficients.', 'Standard errors in parentheses.', '* p < 0.05'), 
                            notes.append = FALSE,
                            keep.stat = c('n', 'adj.rsq'),
                            star.cutoffs = c(0.05),
                            out = 'figures/regression-robust-2019.html'
                            )
  ### 2008 Annenberg ###
  ## First Operationalization of the Strategic Vote: Mismatch between preference from feeling thermometer to who they end up voting for?
  stratvote1.08 <- lm(stratvote ~ black + MA01_2 + MA06_2 + WA02_a + female, data = ann08)
  texreg::screenreg(stratvote1.08)
  ## Second Operationalization of the Strategic Vote: Mismatch between preference from feeling thermometer to who they think has best chance of winning?
  stratvote2.08 <- lm(stratvote2 ~ black + MA01_2 + MA06_2 + WA02_a + female, data = ann08)
texreg::screenreg(stratvote2.08)

  ## Table ##
ann08regtable <- stargazer(stratvote1.08, stratvote2.08, 
                            covariate.labels = c('Black', 'Party Identification', 'Ideology', 'Age', 'Female'),
                            dep.var.labels = c("Strategic Vote", "Strategic Vote"), 
                            type = 'html',
                            style = 'apsr',
                            title = 'Regression comparing Black to non-Black voters on the use of the strategic vote',
                            notes = c('Source: 2008 Annenberg study', 'Ordinary least squares coefficients.', 'Standard errors in parentheses.', '* p < 0.05'), 
                            notes.append = FALSE,
                            keep.stat = c('n', 'adj.rsq'),
                            star.cutoffs = c(0.05),
                            out = 'figures/Regression-2008.html'
                            )

