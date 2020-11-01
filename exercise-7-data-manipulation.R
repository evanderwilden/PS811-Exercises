library(dplyr)
library(here)
library(tidyverse)
library(haven)

  ###Task 1
#Creating dataframe

Justice <- c("Clarence Thomas", "Ruth Bader Ginsburg",
             "Stephen Breyer", "John Robers", "Samuel Alito",
             "Sonia Sotomayor", "Elena Kagan", "Neil Gorsuch",
             "Brett Kavanaugh")
State <- c("GA","NY","MA","MD","NJ","NY","MA","CO","MD")
Position <- c("Associate Justice", "Associate Justice",
              "Associate Justice", "Chief Justice",
              "Associate Justice","Associate Justice",
              "Associate Justice","Associate Justice",
              "Associate Justice")
Replacing <- c("Thurgood Marshall", "Byron White",
               "Harry Blackmun", "William Rehnquist",
               "Sandra Day O'Connor", "David Souter",
               "John Paul Stevens", "Antonin Scalia",
               "Anthony Kennedy")
Year <- c(1991,1993,1994,2005,2006,2009,2010,2017,2018)
Senate_vote <- c("52-48","96-3","87-9","78-22","58-42",
                 "68-31","63-37","54-45","50-48")
Nominated_by <- c("George HW Bush", "Bill Clinton", 
                  "Bill Clinton", "George W Bush",
                  "George W Bush", "Barack Obama",
                  "Barack Obama", "Donald Trump", 
                  "Donald Trump")
SCJustices <- data.frame(Justice,State,Position,Replacing,
                         Year, Senate_vote, Nominated_by)



  ###Task 2
#downloading justices.csv (and loading it)
# ms: in the future, consider putting all your datasets in a "data" folder and using the "here" package to load them in
justices <- read.csv("justices.csv")

  ###Task 3
#Merging Datasets
scotus <- read_dta("SCDB_2020_01_justiceCentered_Citation.dta")
head(scotus)
as.data.frame(scotus)

#Checking to see that the variables for justices and term have the same label
names(scotus)
names(justices) #they do ("justiceName" and "term")


joined_justices <- full_join(scotus, justices, by=c("justiceName", "term"))

table(joined_justices$justiceName)
#Making sure that actual justices names were compatible across datasets
#Note: I had visually done this just by looking at the dataframes
#before merging, this was just to double-check

  ###Task 4
#Filter to justices with Martin-Quinn Scores
filter(joined_justices, post_mn == TRUE)
# ms: consider this:
# filter(joined_justices, !is.na(post_mn))

  ###Task 5
#Find mean martin quinn score for each term
mq_byterm<- joined_justices %>%
  group_by(term) %>%
  summarise(mean = mean(post_mn, na.rm = TRUE))

print(mq_byterm, n = nrow(mq_byterm))


  ###Task 6
#Find mean of decision for each term

#mutating data so the values work like mq scores
joined_justices <- mutate(joined_justices,
                          decisionDirection = case_when(
                            decisionDirection == 1 ~ 1, #conservative
                            decisionDirection == 2 ~ -1, #liberal
                            decisionDirection == 3 ~ 0)) #unspecified


decision_byterm <- joined_justices %>%
  group_by(term) %>%
  summarise(mean = mean(decisionDirection, na.rm = TRUE))

print(decision_byterm, n = nrow(decision_byterm))



  ###Task 7
#Compare mq scores and vote direction
mq_decision_compare <- inner_join(mq_byterm, decision_byterm,
                                  by="term")
#QUESTION: Why is this keeping the NAs?
# ms: the reason why it's keeping everything from 1937-1945 is because when you created the "decision_byterm" tibble, you used the "joined_justices,"
# which includes all years because it was created via a full_join
# since you are joining by term, the the year value also exists in "decision_byterm" even though it doesn't have a decisionDirection value
colnames(mq_decision_compare) <- c("term", "MQ Score", "Vote Direction")
view(mq_decision_compare)

plot(mq_decision_compare$`MQ Score`, mq_decision_compare$`Vote Direction`)
#Just quickly looking at the data, it appears that the scores match well
#with vote directions
