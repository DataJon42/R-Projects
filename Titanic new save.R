# Jonathan Turner Titanic: Machine Learning from Disaster 11/5/2016
setwd("C:/Users/Jonathan/Desktop/Programing (R)")
library(readr)
train <- read_csv("C:/Users/Jonathan/Desktop/Programing (R)/train.csv")
Parsed with column specification:
  cols(
    PassengerId = col_integer(),
    Survived = col_integer(),
    Pclass = col_integer(),
    Name = col_character(),
    Sex = col_character(),
    Age = col_double(),
    SibSp = col_integer(),
    Parch = col_integer(),
    Ticket = col_character(),
    Fare = col_double(),
    Cabin = col_character(),
    Embarked = col_character()
  )
View(train)
library(readr)
test <- read_csv("C:/Users/Jonathan/Desktop/Programing (R)/test.csv")
Parsed with column specification:
  cols(
    PassengerId = col_integer(),
    Pclass = col_integer(),
    Name = col_character(),
    Sex = col_character(),
    Age = col_double(),
    SibSp = col_integer(),
    Parch = col_integer(),
    Ticket = col_character(),
    Fare = col_double(),
    Cabin = col_character(),
    Embarked = col_character()
  )
View(test)
# set working directory and import data files
str(train)
table(train$Survived)

0   1 
549 342 
prop.table(table(train$Survived))

0         1 
0.6161616 0.3838384 
test$survived <- rep(0, 418)
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)
#Gender-CalssModel
summary(train$Sex)
Length     Class      Mode 
891 character character 
ftable(train$Sex)
female male

314  577
prop.table(table(train$Sex, train$Survived))

0          1
female 0.09090909 0.26150393
male   0.52525253 0.12233446
prop.table(table(train$Sex, train$Survived),1)

0         1
female 0.2579618 0.7420382
male   0.8110919 0.1889081
test$survived <- 0
test$survived[test$Sex == 'female'] <- 1
submit1 <- data.frame(PassengerId = test$PassengerId, Survived = test$survived)
write.csv(submit1, file = "theyallperish1.csv", row.names = FALSE)
setwd("C:/Users/Jonathan/Desktop/Programing (R)")
#AgeVariable
summary(train$Age)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
0.42   20.12   28.00   29.70   38.00   80.00     177
train$Child <- 0
train$Child[train$Age < 18] <- 1
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
  Child    Sex Survived
1     0 female      195
2     1 female       38
3     0   male       86
4     1   male       23
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
  Child    Sex Survived
1     0 female      259
2     1 female       55
3     0   male      519
4     1   male       58
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
  Child    Sex  Survived
1     0 female 0.7528958
2     1 female 0.6909091
3     0   male 0.1657033
4     1   male 0.3965517
#FareVariable
train$fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$fare2[train$Fare < 10] <- '10'
aggregate(Survived ~ fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
   fare2 Pclass    Sex  Survived
1  20-30      1 female 0.8333333
2    30+      1 female 0.9772727
3  10-20      2 female 0.9142857
4  20-30      2 female 0.9000000
5    30+      2 female 1.0000000
6     10      3 female 0.5937500
7  10-20      3 female 0.5813953
8  20-30      3 female 0.3333333
9    30+      3 female 0.1250000
10    10      1   male 0.0000000
11 20-30      1   male 0.4000000
12   30+      1   male 0.3837209
13    10      2   male 0.0000000
14 10-20      2   male 0.1587302
15 20-30      2   male 0.1600000
16   30+      2   male 0.2142857
17    10      3   male 0.1115385
18 10-20      3   male 0.2368421
19 20-30      3   male 0.1250000
20   30+      3   male 0.2400000
test$survived <- 0
test$survived[test$Sex == 'female'] <- 1
test$survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0
submit2 <- data.frame(PassengerId = test$PassengerId, Survived = test$survived)
write.csv(submit2, file = "theyallperish2.csv", row.names = FALSE)
#DescionTree
library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
plot(fit)
text(fit)
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class", control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class", control=rpart.control(minsplit=2, cp=0.005))
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)
train$Name[1]
test$Survived <-NA
combi$Name <- as.character(combi$Name)
combi$Name[1]
strsplit(combi$Name[1], split='[,.]')
[[1]]
[1] "Braund"       " Mr"          " Owen Harris"
strsplit(combi$Name[1], split='[,.]')[[1]]
[1] "Braund"       " Mr"          " Owen Harris"
strsplit(combi$Name[1], split='[,.]')[[1]][2]
[1] " Mr"
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
table(combi$Title)
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'

library(rpart)
install.packages('randomForest')
library(randomForest)
install.packages('party')
library(party)

# Join together the test and train sets for easier feature engineering
test$Survived <- NA
combi <- rbind(train, test)

# Convert to a string
combi$Name <- as.character(combi$Name)

# Engineered variable: Title
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
# Combine small title groups
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
# Convert to a factor
combi$Title <- factor(combi$Title)

# Engineered variable: Family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Engineered variable: Family
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
# Delete erroneous family IDs
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
# Convert to a factor
combi$FamilyID <- factor(combi$FamilyID)

# Fill in Age NAs
summary(combi$Age)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, 
data=combi[!is.na(combi$Age),], method="anova")

combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
# Check what else might be missing
summary(combi)
# Fill in Embarked blanks
summary(combi$Embarked)
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)
# Fill in Fare NAs
summary(combi$Fare)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

# New factor for Random Forests, only allowed <32 levels, so reduce number
combi$FamilyID2 <- combi$FamilyID
# Convert back to string
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
# And convert back to factor
combi$FamilyID2 <- factor(combi$FamilyID2)

# Split back into test and train sets
train <- combi[1:891,]
test <- combi[892:1309,]

# Build Random Forest Ensemble
set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2,
data=train, importance=TRUE, ntree=2000)
# Look at variable importance
varImpPlot(fit)
# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)

# Build condition inference tree Random Forest
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3)) 
# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "ciforest.csv", row.names = FALSE)
