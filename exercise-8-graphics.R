library(dplyr)
library(tidyverse)
library(ggplot2)

USArrests



          ####   BASE R   ####

# ms: the question asks for assault vs. murder arrests but otherwise, this looks good!

#Scatterplot for Rape arrests vs Murder Arrests
plot(USArrests$Rape, USArrests$Murder, 
     main = "Relationship between \n Rape and Murder Arrests",
     xlab = "Rape Arrests per 100,000", ylab = "Murder Arrests per 100,000")

#Boxplot for Rape Arrests by State
boxplot(USArrests$Rape, main = "Boxplot for US Rape Arrests by State", 
        ylab = "Number of Arrests (per 100,000)")

#Barplot for Rape Arrests by State
barplot(USArrests$Rape, main = "Barplot for US Rape Arrests by State",
        ylab = "Number of Arrests (per 100,000)")

### ms: I would label the states for the above base R plot

barplot(USArrests$Rape,
        names = row.names(USArrests),
        main = "Barplot for U.S. Rape Arrests by State",
        ylab = "Number of Arrests per 100,000",
        las = 2,
        cex.names = 0.5
        )

###

#Histogram for % Urban Population
hist(USArrests$UrbanPop, main = "Histogram of % Urban Population",
     xlab = "Percent Urban Population", prob = TRUE)


      ####    GGPLOT    ####

#Scatterplot for Rape arrests vs Murder Arrests
ggplot(data=USArrests, aes(x=Rape, y = Murder)) +
  geom_point(col = 10) + geom_smooth(method = "lm") +
  labs(title = "Relationship between Rape and Murder Arrests", 
       y = "Murder Arrests(per 100,000)",
       x= "Rape Arrests (per 100,000)", 
       subtitle = "By US State", caption = "Data from USArrests in R")

#Boxplot for Rape Arrests by State
ggplot(data=USArrests, aes(x=Rape))+
  geom_boxplot(col = 1) + 
  labs(title = "Boxplot for US Rape Arrests", 
       subtitle = "By US State", caption = "Data from USArrests in R",
       x= "Number of Arrests (per 100,000)" )

#Barplot for Rape Arrests by State
ggplot(data=USArrests, aes(x = row.names(USArrests), y = Rape)) +
  geom_bar(stat= "Identity") +
  theme(axis.text.x= element_text(angle=90, hjust=1, size = 5)) +
  labs(title = "Barplot for US Rape Arrests", 
       subtitle = "By US State", caption = "Data from USArrests in R",
       x= "Number of Arrests (per 100,000)" )


#Histogram for % Urban Population
ggplot(data=USArrests, aes(x=UrbanPop)) +
  geom_histogram(color = "Black", fill = "lightblue", bins = 10)+
  labs(title = "Histogram For % Urban Population", 
       subtitle = "By US State", caption = "Data from USArrests in R",
       x= "Percent Urban Pppulation" )
  

