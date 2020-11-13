# Ethan vanderWilden
# Replication Example for PS811, Exercise 10
# Date: 13 November 2020

# NOTE: I think that Jess, Clint, and I are going to work on this paper for our 812
#       Project, so it's possible that they also might use this for the 811 exercise


# WORKING FROM PAPER:
  # Siegel, Alexandra A. and Vivienne Badaa. 2020. "#No2Sectarianism: Experimental 
  #      Approaches to Reducing Sectarian Hate Speech Online." American Journal 
  #      of Political Science 114:3, 837-855.

# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/KQJKY0


# packages
library("magrittr")
library("tidyverse")
library("haven")
library("here")
library("stargazer")
library("modelsummary")
library("sjlabelled")
library(coefplot)

#Load in Data
data <- read.csv("twitter_data.csv")

# FOR CONTEXT:
#   This is an experiment in which the authors create a fake twitter account and message people who
#   tweet sectarian hate-speech. In their messages, they test different primes (either common
#   Arab or Religious Identity and with/without elite support). They are looking at the difference
#   in amount of hate-tweets before and after treatment.

# MODEL:
#   I think that the data for treatment_num is not just numeric data. I think that the regression
#   is being run such that the DV is difference in tweets, while the IV is a binary measure between
#   no treatment (treatment_num = 0) and whatever treatment the person gets.

month<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=data)

two_weeks<-lm(two_weeks_anti_shia_post-two_weeks_anti_shia_pre ~ treatment_num, data=data)

week<-lm(week_anti_shia_post-week_anti_shia_pre ~ treatment_num, data=data)

day<-lm(tpd_anti_shia_post-tpd_anti_shia_pre ~ treatment_num, data=data)

stargazer(day, week, two_weeks, month,
          covariate.labels = c("Arab ID", "Religious ID",
                               "Arab ID (elite)", "Religious ID (elite)",
                               "No ID"),
          column.labels = c("One Day", "One Week", "Two Weeks", "One Month"),
          dep.var.caption = "Difference in Anti-Shiea Tweets",
          dep.var.labels.include = FALSE,
          type = "text",
          title = "Effect of Treatment on Volume of Anti-Shia Tweets")


# This is the plot that the authors show in their paper. It is just the CIs for the beta coefficients
# from the regression table that I created above
multiplot(day, week, two_weeks, month, coefficients=c("treatment_num1", "treatment_num2", "treatment_num3", "treatment_num4", "treatment_num5"), 
          newNames=c(treatment_num1="Arab ID ", treatment_num2="Religious ID ", treatment_num3="Arab ID (Elite)", 
                     treatment_num4="Religious ID (Elite)", treatment_num5=" No ID "), 
          names=c("     Day", "   Week", "  Two Weeks ", "Month    "), title="", scales="free_x",
          sort="alphabetical", innerCI=1.645, outerCI=1.96, single=FALSE,  zeroType = 0,legend.position="none") +
  scale_color_manual(values=c("red", "blue", "seagreen", "black")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none", axis.line = element_line(colour = "black"), text = element_text(size=15))+
  ylab("Treatments") + xlab("Difference in Anti-Shia Tweet Count")+ geom_vline(aes(xintercept = 0), size = .5, linetype = "dashed")









