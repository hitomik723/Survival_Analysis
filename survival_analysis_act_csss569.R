##################################################
# title: "final_poster_survival_analysis_csss569"
# author: "Hitomi Kariya"
# date: "3/11/2020"
# output: html_document
##################################################

##### Prepare for the analysis #####
rm(list = ls())

library(foreign)
library(haven)
library(tidyverse)
library(survival)
library(survminer)
library(RColorBrewer)
library(ggrepel)
library(ggstance)
source("http://staff.washington.edu/kpleung/vis/theme/theme_cavis.R")

# Get nice colors
brewer <- brewer.pal(9, "Set1")
blue <- brewer[2]
orange <- brewer[5]

##### 0.1 Check the data set #####
colnames(master848)
head(master848$bln_flag_dead_asof_30sep2016)
head(master848$dbl_dementia_years)
head(master848$bln_onsetdate)
class(master848$bln_onsetdate)

##### 0.2 Create a variable of survival years for the censored #####
master848$end_date <- "2016-09-30"
head(master848$end_date)
master848$end_date <- as.Date(master848$end_date)
class(master848$end_date)

master848$censored_dementia_years <- master848$end_date - master848$bln_onsetdate #time differnece in days
head(master848$censored_dementia_years)

master848$censored_dementia_years <- master848$censored_dementia_years/365 #time difference in years

master848$bln_flag_dead_asof_30sep2016 <- as.factor(master848$bln_flag_dead_asof_30sep2016)

master848$surv_dementia_years <- ifelse (master848$bln_flag_dead_asof_30sep2016 == 0, master848$censored_dementia_years, master848$dbl_dementia_years)

head(master848$surv_dementia_years) #use this as a variable for survival years


##### 0.3 Check those who were dienrolled during the study #####
#Since there is no missing value for bln_flag_dead_asof_30sep2016 (1 = dead, 0 = alive as of 30sep2016), there is no loss to follow-up.
#We don't need to account for the number of loss to follow-up.
master848$bln_flag_discontinued_from_act <- as.factor(master848$bln_flag_discontinued_from_act)

table(master848$bln_flag_discontinued_from_act)
#1 = discontinued from act, 0 = otherwise

master848 %>% filter(bln_flag_dead_asof_30sep2016 == 1) %>% filter (bln_flag_discontinued_from_act == 1)

#check how bln_flag_discontinued_from_act is coded
#check how dbl_discont_years is coded
table(master848$bln_flag_dead_asof_30sep2016)
#1 = dead, 0 = alive as of 30sep2016

##### 0.4 Recoding exposure variables and confounders #####
#recode the variables (elder orphans, living alone, gender, charlson) to factor
master848$dql_true_elder_orphan <- as.factor(master848$dql_true_elder_orphan)

master848$dei_livingalone <- as.factor(master848$dei_livingalone)

master848$bln_gender <- as.factor(master848$bln_gender)

#keep the charlson score as numeric
class(master848$ein_charlson)
table(master848$ein_charlson)

#FEMALE as the REFERENCE group
#recode the gender variable from 1/2 to 0/1
#1 = male, 2 = female -> 0 = female, 1 = male
str(master848$bln_gender)
sum(is.na(master848$bln_gender)) #check NA
table(master848$bln_gender) #1 (male) = 312, 2 (female) = 536
master848$bln_gender <- ifelse(master848$bln_gender == 2, 0, 1)
table(master848$bln_gender) #0 (female) = 312, 1 (male) = 536

#recode the education variable from continuous to binary
master848$bln_education_bin <- ifelse(master848$bln_education <= 12, 0, 1)
master848$bln_education_bin <- as.factor(master848$bln_education_bin)
class(master848$bln_education_bin)
table(master848$bln_education_bin)
# 13 or more than 13 years = college or more = 1
# less than 13 years = high school graduate or less = 0

##### 1.1 KM Curve for elder orphans/non-elder orphans #####
#recode the event variable (has to be numeric or boolean)
master848$bln_flag_dead_asof_30sep2016 <- as.numeric(master848$bln_flag_dead_asof_30sep2016)

#recode the time variable (has to be numeric)
master848$surv_dementia_years <- as.numeric(master848$surv_dementia_years)

#survival function
master848$tte <- Surv(master848$surv_dementia_years, master848$bln_flag_dead_asof_30sep2016) #to make tte, see slides15 @14 BIOST513

km <- survfit(master848$tte ~ master848$dql_true_elder_orphan)
#ggsurvplot(km, data = master848)

# survival plot only
ggsurv_only <- ggsurvplot(
  km, 
  data = master848, 
  title = FALSE,
  # subtittle = "Based on the Kaplan-Meier Estimator",
  surv.plot.height = 1,
  size = 1,                 # change line size
  palette = 
    c("#5e3c99","#00AB84"),# custom color palettes
  conf.int = TRUE,          # Add confidence interval
  pval = TRUE,             # Add p-value
  pval.size = 6,
  risk.table = FALSE,        # No risk table
  risk.table.fontsize = 3,
  risk.table.col = "strata", # Risk table color by groups
  risk.table.y.text = FALSE,
  risk.table.height = 0.25,
  legend.labs = 
    c("Elders with Family", "Elders without Family"),    # Change legend labels
  legend.font.size = 10,
  xlab = "Time in years",
  ncensor.plot = FALSE,
  ncensor.plot.height = 0.25, # Useful to change when you have multiple groups
  ggtheme = theme_bw()      # Change ggplot2 theme
)

ggpar(
  ggsurv_only,
  # font.title = c(16, "bold", "#2E056A"),
  # font.subtitle = c(15, "bold", "#2E056A"),
  font.x = c(14, "bold.italic", "#2E056A"),
  font.y = c(14, "bold.italic", "#2E056A"),
  font.tickslab = c(15, "plain")
)


ggsurv <- ggsurvplot(
  km, 
  data = master848, 
  title = "Survival Curves",
  subtittle = "Based on the Kaplan-Meier Estimator",
  surv.plot.height = 1,
  size = 1,                 # change line size
  palette = 
    c("#5e3c99","#00AB84"),# custom color palettes
  conf.int = TRUE,          # Add confidence interval
  pval = TRUE,             # Add p-value
  pval.size = 6,
  risk.table = TRUE,        # Add risk table if you want %, "abs_pct" instead of TRUE
  risk.table.fontsize = 6,
  risk.table.col = "strata", # Risk table color by groups
  risk.table.y.text = FALSE,
  risk.table.height = 0.30,
  legend.labs = 
    c("Elders with Family", "Elders without Family"),    # Change legend labels
  xlab = "Time in years",
  ncensor.plot = TRUE,
  ncensor.plot.height = 0.30, # Useful to change when you have multiple groups
  ggtheme = theme_bw()      # Change ggplot2 theme
)

ggpar(
  ggsurv,
  font.title = c(16, "bold", "#2A025C"),
  font.subtitle = c(15, "bold", "#2A025C"),
  font.x = c(14, "bold.italic", "#2A025C"),
  font.y = c(14, "bold.italic", "#2A025C"),
  font.tickslab = c(15, "plain")
)

##### 1.2 Fit a cox model for elder orphans (adjusted for living alone) #####
elder.living <- coxph(tte ~ dql_true_elder_orphan + dei_livingalone + bln_age_at_onset + bln_education_bin + bln_gender + ein_charlson, data = master848)
elder.living
exp(confint(elder.living))

##### 1.1 Ropeladder for estimates #####
sa_rope <- read.csv("survival_analysis_ropeladder_act.csv")

sa_rope

sa_rope <- sa_rope %>%
  mutate(signif = case_when(
    lower > 1 & upper > 1 ~ TRUE, # Both bounds are above zero -> signif
    lower < 1 & upper < 1 ~ TRUE, # Both bounds are below zero -> signif
    TRUE ~ FALSE)) # Everything else is not signif

str(sa_rope$signif)
sa_rope$signif <- ifelse(sa_rope$signif == TRUE, "Significant", "Insignificant")

rope_plot <- ggplot(sa_rope, aes(y = covariate, x = hr, xmax = upper, xmin = lower, shape = signif, fill = signif)) +
  geom_vline(xintercept = 1) +
  geom_pointrangeh(aes(colour = signif), position = position_dodge2v(height = 0.7)) + 
  scale_colour_manual(values = c("#85754d", "#00AB84")) +
  scale_shape_manual(values = c(19, 19)) + #Non-signif-> fillable circle(21)
  scale_fill_manual(values = c(NA, NA)) + #Non-signif-> fill it w. white
  scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  guides(shape = "none", fill = "none") +
  labs(y = "", x = "") +
  theme_cavis_vgrid +
  theme(legend.position = c(0.79, 0.125), axis.ticks.x = element_blank(), legend.text = element_text(size = 13)) +
  scale_y_discrete(labels = c("gender" = "Male", "elder_orphans" = "Without Family", "education" = "College Educated", "cci" = "CCI\n(1 score diff)", "age" = "Age at onset\n(1 age diff)"))

rope_plot <- rope_plot +
  theme(axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16))

rope_plot


