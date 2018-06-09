#Box plot the Titanic Data

View(Titanic)
Titanic <- as.data.frame(Titanic)
str(Titanic)
Titanic$Class <- as.numeric(Titanic$Class)
Titanic$Survived <- as.numeric(Titanic$Survived)
Titanic$Age <- as.numeric(Titanic$Age)
Titanic$Sex <- as.numeric(Titanic$Sex)
boxplot(Titanic$Age ~ Titanic$Sex,  main = "Age Vs. Sex", xlab = "Sex", ylab = "Age", col = c("red", "Blue"))
boxplot(Titanic$Class ~ Titanic$Survived, main = "Class Vs. Survived", xlab = "Survived", ylab = "Class", col = c("red", "Blue"))

#Correlation Matrix mtcars

View(mtcars)
cor(mtcars$mpg, mtcars$disp) #check correlation
plot(mtcars$disp, mtcars$mpg)

cor(mtcars) #correlation matrix
par(mfrow = c(2, 2))
plot(mtcars$disp, mtcars$mpg)
plot(mtcars$disp, mtcars$wt)
plot(mtcars$disp, mtcars$carb)
plot(mtcars$disp, mtcars$drat)

# MPG and DISP have a negative linear correlation
# WT ad DISP have a positive linear correlation

install.packages("corrgram")
library(corrgram)
corrgram(mtcars)

head(mtcars)
summary(mtcars)

install.packages("Hmisc")
library(Hmisc)
describe(mtcars)

A1 <- summarize(mtcars$mpg, mtcars$cyl, mean)

summarize(mtcars$mpg, llist(mtcars$cyl, mtcars$gear), mean)

par(mfrow = c(1,1))
hist(mtcars$mpg)
hist(mtcars$mpg, labels = TRUE)
hist(mtcars$mpg, labels = TRUE, breaks = 10, col = "red")





