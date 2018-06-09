#  Get Data
install.packages("party")
library(party)
data(iris)

summary(iris)

#making branches
Sepal_L <- ifelse(iris$Sepal.Length >=5.8, "Long", "Short")
Sepal_W <- ifelse(iris$Sepal.Width >=3, "Wide", "Slim")
Petal_L <- ifelse(iris$Petal.Length >=3.7, "Long", "Short")
Petal_W <- ifelse(iris$Petal.Width >=1.2, "Wide", "Slim")

#add branches to DF
iris <- data.frame(iris, Sepal_L)
iris <- data.frame(iris, Sepal_W)
iris <- data.frame(iris, Petal_L)
iris <- data.frame(iris, Petal_W)

#drop numeric cols
iris <- iris[ ,c(-1, -2, -3, -4)]

#tree time
library(caTools)
set.seed(3)
split <- sample.split(iris$Species, SplitRatio = 0.7)
train <- subset(iris, split == TRUE)
test  <- subset(iris, split == FALSE)



# fit the tree model using training data
Decision_Tree <- ctree(iris$Species ~ ., data = train)

# plot tree
plot(Decision_Tree)


