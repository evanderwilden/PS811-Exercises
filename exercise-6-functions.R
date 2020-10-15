#loading in the data
national <- read.csv("national.csv")

library(tidyverse)

#Task 1. Loop to see how many variables there are per observation
columns_loop <- numeric() #creates a place to store the number of variables

for (row in 1:nrow(national)) {
  columns_loop[row] <- length(national[row, ])
} #for loop that gives the number of variables (columns) per observation
columns_loop #print that and see that there are 70 variables per observation


#Task 2. Find mean number of protestants per country
tapply(
  X = national$christianity_protestant, 
  INDEX = list(national$state), 
  FUN = mean, 
  na.rm = TRUE  
) #in base R


national %>%
  group_by(state) %>%
  summarize(
    mean_nom = mean(christianity_protestant, na.rm = TRUE)
  )  #in tidyverse


#Task 3. Check the column type for each variable that is just characters
sapply(national, class) == "character" #prints "true" for columns that are characters
select_if(national, is.character) #not sure which we want here?
  
#Task 4. Log the Buddhism variables
log(national$buddhism_all) #logs the buddhism_all variable

national %>% #This logs all of the buddhism variables, not sure which we want
  mutate_at(
    .vars = vars(starts_with("buddhism")),
    .funs = function(x) log(x)
  ) %>% 
  select(starts_with("buddhism"))


#Task 5. function that lists unique years with more that 300000 Christians

many_christians_years <- rep(NA, nrow(national)) #Creating a vector to hold years
for (row in 1:nrow(national)) { #checks each row
  if (national$christianity_all[row] > 300000) { #if Christians is greater than 300000
    many_christians_years[row] <- national$year[row] #put that year into my vector
  }
}
unique_years <- unique(many_christians_years) #Keeps only unique years
unique_years<-unique_years[!is.na(unique_years)] #Removes NAs
unique_years


#Task 6, grouping data by code
nested_national <- national %>%
  group_by(code) %>%
  nest() %>%
  print()

#Task 7, creating a model column
nested_models <- nested_national %>%
  mutate(
    model = map(
      .x = data, 
      .f = ~ lm(dual_religion ~ judaism_percent, data = .x)
      )
    ) %>%
    print()

#cracking open nested data frame
nested_models$code
nested_models$data
nested_models$model

#Task 8,extract the coefficients into a new column
nested_coefs <- nested_models %>%
  mutate(coefs = map(model, coefficients)) %>%
  print()

#Task 9, look at coefficients
nested_coefs$coefs

#Task 10, pull out model column
nested_coefs$model

#Task 11, unnest coefficients column
coefs <- nested_coefs %>%
  unnest(coefs) %>%
  print()
