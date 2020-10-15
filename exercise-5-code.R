
          ###BASE R

food <- read.csv("food_coded.csv")

#Getting the first 95 entries
food_95 <- food[1:95, ]

#Taking certain columns 
food_95[ , c("GPA", "calories_chicken", "drink",
           "fav_cuisine", "father_profession", "mother_profession")]
food_95[ , c(1, 4, 16, 26, 25, 45)]

# Making the healthy variable between 1-100
food_95$healthy100 <- food_95$healthy_feeling*10

# filters female and gpa > 3 people
# Organizing alphabetically by favorite cuisine
highgpa_females <- subset(food_95, GPA > 3.0 & Gender==1)
highgpa_females <- highgpa_females[order(highgpa_females$fav_cuisine), ]

#Putting means and sd's of certain variables in a df
data.frame(chicken.mean = mean(food$calories_chicken, na.rm = TRUE),
           chicken.sd = sd(food$calories_chicken, na.rm = TRUE),
           tortilla.mean = mean(food$tortilla_calories, na.rm = TRUE),
           tortilla.sd = sd(food$tortilla_calories, na.rm = TRUE),
           turkey.mean = mean(food$turkey_calories, na.rm = TRUE),
           turkey.sd = sd(food$turkey_calories, na.rm = TRUE),
           waffle.mean = mean(food$waffle_calories, na.rm = TRUE),
           waffle.sd = sd(food$waffle_calories, na.rm = TRUE))

# Summarize GPA and Weight within gender and cuisine variables

food$GPA <- as.numeric(food$GPA) #need to be numbers, not characters to get means
food$weight <- as.numeric(food$weight)

aggregate(formula = cbind(GPA, weight) ~ Gender + cuisine, 
          data = food, 
          FUN = function(x){
            c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
          }) #not sure on this one because I get a lot of NAs for means


      ###TIDYVERSE

facebook <- read.csv("facebook-fact-check.csv")

#taking bottom 500 entries
facebook500 <- facebook %>%
  top_n(-500)

# Identifying every even column (also by name)
select(facebook500, 2,4,6,8,10,12)
labels(facebook500) #just to look at the labels
select(facebook500, "post_id", "Page", "Date.Published",
       "Rating", "share_count", "comment_count")

#Adding column that puts post type into a numeric variable
facebook500 <- 
  mutate(facebook500,
         post_type_coded = 
           if_else(Post.Type == "link", 1,
                   if_else (Post.Type == "photo", 2,
                            if_else (Post.Type == "text", 3,4))))


#arranging by reverse alphabetical order of page name
facebook500 <- arrange(facebook500, desc(facebook500$Page))

# Create a dataframe with mean and standard deviation information
summarise(facebook,
          share_count.mean = mean(share_count, na.rm = TRUE),
          share_count.sd = sd(share_count, na.rm = TRUE),
          reaction_count.mean = mean(reaction_count, na.rm = TRUE),
          reaction_count.sd = sd(reaction_count, na.rm = TRUE),
          comment_count.mean = mean(comment_count, na.rm = TRUE),
          comment_count.sd = sd(comment_count, na.rm = TRUE))

#Finding these values for Mainstream Variables
#Can do this way, which give values for left, mainstream, and right
facebook %>% 
  group_by(Category) %>% 
  summarise(share_count.mean = mean(share_count, na.rm = TRUE),
            share_count.sd = sd(share_count, na.rm = TRUE),
            reaction_count.mean = mean(reaction_count, na.rm = TRUE),
            reaction_count.sd = sd(reaction_count, na.rm = TRUE),
            comment_count.mean = mean(comment_count, na.rm = TRUE),
            comment_count.sd = sd(comment_count, na.rm = TRUE)) %>% 
  ungroup()

#can also filter the data and then get the values
mainstream_facebook <- filter(facebook,
                              Category == "mainstream")
summarise(mainstream_facebook,
          share_count.mean = mean(share_count, na.rm = TRUE),
          share_count.sd = sd(share_count, na.rm = TRUE),
          reaction_count.mean = mean(reaction_count, na.rm = TRUE),
          reaction_count.sd = sd(reaction_count, na.rm = TRUE),
          comment_count.mean = mean(comment_count, na.rm = TRUE),
          comment_count.sd = sd(comment_count, na.rm = TRUE))



