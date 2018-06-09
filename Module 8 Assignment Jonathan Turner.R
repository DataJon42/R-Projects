#Install packages
install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)

# view data
data("Adult")
str(Adult)
class(Adult)
summary(Adult)
View(Adult)

# form and sort with rules
filter_trans <- Adult[size(Adult) >= 12]
inspect(filter_trans[1:20])

itemFrequencyPlot(Adult, topN=20, type = "absolute")

rules <- apriori(Adult, parameter = list(supp = 0.1, conf = 0.8 ))

inspect(head(sort(rules, by = "lift"), 100))
inspect(head(sort(rules, by = "lift"), 500))
inspect(head(sort(rules, by = "lift"), 1000))

# observations
### If your a white male in the United States of any background you are very likely to be married
### If your young you are very likely to not be married

# Vizualize
plot(rules)

# jester5k dataset
install.packages("recommenderlab")
library(recommenderlab)

data("Jester5k")
class(Jester5k)
summary(Jester5k)
length(Jester5k)
colnames(Jester5k)
Jester5k

# number of ratings
nratings(Jester5k)

# number of ratings per user
summary(rowCounts(Jester5k))

# rating distribution
hist(getRatings(Jester5k), main="Distribution of ratings", col = "blue")

# best joke with highest average rating
best <- which.max(colMeans(Jester5k))
cat(JesterJokes[best])

# check distribution of ratings
vector_ratings <- as.vector(Jester5k@data)
as.matrix(Jester5k@data[1:4, 1:4])

unique(vector_ratings)

table_ratings <- table(vector_ratings)
table_ratings

vector_ratings <- vector_ratings[vector_ratings !=0]
table(vector_ratings)

library(ggplot2)
qplot(vector_ratings) + ggtitle("Distrobution of Ratings")

#check most popular jokes
rated_jokes <- colCounts(Jester5k)
table_jokes <- data.frame(joke = names(rated_jokes), rates = rated_jokes)
table_jokes

table_jokes[order(table_jokes$rates, decreasing = TRUE),]
head(table_jokes)
colnames(table_jokes)
ggplot(table_jokes[1:5, ], aes(x=joke, y=rates)) +geom_bar(stat = "identity")

# distrobution of ratings
average_ratings <- colMeans(Jester5k)
qplot(average_ratings) + stat_bin(binwidth = 0.1) + ggtitle("Distrobution of Average Joke Rating")

average_ratings_relevent <- average_ratings[rated_jokes>100]
qplot(average_ratings_relevent) + stat_bin(binwidth = 0.1) + ggtitle("Distrobution of Average Joke Rating")

image(Jester5k, main = "heatmap of rating matrix")

# decisions for a good recomendation system
min_n_jokes <- quantile(rowCounts(Jester5k), 0.99)
min_n_jokes

min_n_users <- quantile(colCounts(Jester5k), 0.99)
min_n_users

image(Jester5k[rowCounts(Jester5k), min_n_jokes, colCounts(Jester5k), min_n_users], main ="heatmap of top users and jokes")

ratings_jokes <- Jester5k[rowCounts(Jester5k) > 50, colCounts(Jester5k) > 2500]

min_jokes <- quantile(rowCounts(ratings_jokes), 0.98)
min_users <- quantile(colCounts(ratings_jokes), 0.98)
image(ratings_jokes[rowCounts(ratings_jokes) > min_jokes, colCounts(ratings_jokes)  > min_users], main = "heatmap of the top jokes")

#split data into test and train
which_train <- sample(x = c(TRUE, FALSE), size = nrow(ratings_jokes), replace = TRUE, prob = c(0.8, 0.2))
recc_data_train <- ratings_jokes[which_train, ]
recc_data_test <- ratings_jokes[!which_train, ]

#calling the recommender function
recc_model_ubcf <- Recommender(data = recc_data_train, method = "UBCF")
n_recommended_ubcf <- 6
recc_predicted_ubcf <- predict(object = recc_model_ubcf, newdata = recc_data_test, n= n_recommended_ubcf)
recc_predicted_ubcf

#getting predictions for a user
recc_predicted_ubcf@items[[1]]
recc_user_1_ubcf <- recc_predicted_ubcf@items[[1]]
jokes_user_1_ubcf <- recc_predicted_ubcf@itemsLabels[recc_user_1_ubcf]
jokes_user_1_ubcf