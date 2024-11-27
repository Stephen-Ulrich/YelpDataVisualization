library(readr)
business <- read_csv("RStudio/OA 11.7 - yelp_academic_dataset_business.json.csv")
View(business)

library(ggplot2)

### 1. ###

head(business, 5) 
# prints first 5 rows

### 2. Bar Graph of Businesses in each State ###

ggplot(business) + geom_bar(aes(x = state), fill = "lightblue") + 
  labs("Businesses in each State", y = "Number of Businesses")

### 3. Pie graph of Star Ratings ###

cont_table <- table(business$stars)

pie(cont_table, main = "Number of each Review Level (Stars)", col = rainbow(9))

# 3.5 star ratings are the most common while 1 stars are the least

### 4. Box plots for review ratings ###

business$stars = as.factor(business$stars)
# as.factor makes things categorical

review <- subset(business, review_count <= 50)
# Mask to limit number of reviews

ggplot(review, aes(x = stars, y = review_count, fill = stars)) + 
  geom_boxplot(show.legend = FALSE)

### 5. Chi-Square Test ###

five_stars <- subset(business, stars == 5)

five_star_cont_table <- table(five_stars$stars, five_stars$review_count)

chisq.test(five_star_cont_table)

one_stars <- subset(business, stars == 1)

one_star_cont_table <- table(one_stars$stars, one_stars$review_count)

chisq.test(one_star_cont_table)

# In both instances the review count is significant in terms of a review
# being a one star or a five star. Both p-values are < 2.2e-16

###############
###USER DATA###
###############

library(readr)
user_data <- read_csv("RStudio/OA 11.6 - yelp_academic_dataset_user.json.csv")
View(user_data)

### 1. Printing Columns ###

names(user_data)

### 2. Pearson's R Correlation ###

#corr_votes <- cor(user_data$cool_votes, user_data$funny_votes, 
                      #user_data$useful_votes)
corr_votes <- cor(user_data[,c("cool_votes", "funny_votes", 
                               "useful_votes")])
print(corr_votes)

### 3. Linear Regression on Cool and Useful ###

corr_cool_useful <- cor(user_data$cool_votes, user_data$useful_votes)

linear_model <- lm(user_data$cool_votes ~ user_data$useful_votes)
#print(linear_model)

coefs_cool_useful <- coef(linear_model)
print(coefs_cool_useful)
lm_y_intercept <- coefs_cool_useful[1]
lm_slope <- coefs_cool_useful[2]
cat("Slope:", lm_slope, "Y-Incercept:", lm_y_intercept)

ggplot(user_data) + geom_point(aes(x = cool_votes, y = useful_votes)) +
  geom_smooth(aes(x = cool_votes, y = useful_votes), method = "lm", se = F)

### 4. Review Count and Fans, Correlated? ###

corr_review_fans <- cor(user_data$review_count, user_data$fans)

linear_model_fans <- lm(user_data$review_count ~ user_data$fans)

coefs_review_fans <- coef(linear_model_fans)
lm_y_intercept <- coefs_review_fans[1]
lm_slope <- coefs_review_fans[2]
cat("Slope:", lm_slope, "Y-Intercept:", lm_y_intercept)

ggplot(user_data) + geom_point(aes(x = review_count, y = fans)) +
  geom_smooth(aes(x = review_count, y = fans), method = "lm", se = F)

print(corr_review_fans)
# The review count and number of fans are loosely positively correlated cor=.58
# Meaning that sometimes an increase in the number of reviews 
# will constitute and increase in fans

### 5. Another variable and fans, Correlated? ###

corr_funny_fans <- cor(user_data$funny_votes, user_data$fans)

linear_model_funny <- lm(user_data$funny_votes ~ user_data$fans)

coefs_funny_fans <- coef(linear_model_funny)
lm_y_intercept <- coefs_funny_fans[1]
lm_slope <- coefs_funny_fans[2]
cat("Slope:", lm_slope, "Y-Intercept:", lm_y_intercept)

ggplot(user_data) + geom_point(aes(x = funny_votes, y = fans)) +
  geom_smooth(aes(x = funny_votes, y = fans), method = "lm", se = F)

print(corr_funny_fans)
# However, the number of funny votes received compared to the number of fans
# is more positively correlated with a score of .73.
# Meaning that there is a likely chance that and increase in the number of
# funny votes will result in an increase in overall fans

### 6. KMeans Review Count and Fans ###

library(class)
library(corrplot) 
# Makes non numeric columns go bye bye
user_data$friends <- NULL
user_data$elite <- NULL
user_data$yelping_since <- NULL
user_data <- na.omit(user_data)

user_cluster <- kmeans(user_data[3:8], 4)
print(table(user_cluster$cluster, user_data$fans))

user_data$cluster <- user_cluster$cluster

# Scatter Plot
ggplot(user_data) + geom_point(aes(x = fans, y = review_count,
                                   color = user_data$cluster))

user_x <- user_data[3:8]
wcss <- function(k){
  kmeans(user_x, centers = k)$tot.withinss
}

k_values <- 1:10
# Elbow plot to determine ideal number of k values
wcss_values <- sapply(k_values, wcss)

elbow_plot <- data.frame(k = k_values, wcss = wcss_values)
print(elbow_plot)

ggplot(elbow_plot, aes(x = k, y = wcss)) + geom_line() + geom_point()

# The ideal k value for the number of clusters was 4.
# The first cluster is pretty separated from the rest however,
# most of the points remain on top of eachother and very overlaped
# and intermigled with one another, no real division.

### Other Variables ###

user_cluster <- kmeans(user_data[3:8], 4)
print(table(user_cluster$cluster, user_data$fans))

user_data$cluster <- user_cluster$cluster

# Scatter plot
ggplot(user_data) + geom_point(aes(x = fans, y = funny_votes,
                                   color = user_data$cluster))

user_x <- user_data[3:8]
wcss <- function(k){
  kmeans(user_x, centers = k)$tot.withinss
}

k_values <- 1:10

wcss_values <- sapply(k_values, wcss)
# Elbow macarroni plot
elbow_plot <- data.frame(k = k_values, wcss = wcss_values)
print(elbow_plot)

ggplot(elbow_plot, aes(x = k, y = wcss)) + geom_line() + geom_point()
# Once again the ideal k value is 4.
# The clusters are visable and divided,
# however there is still a decent bit of overlap on a futher inspection.


