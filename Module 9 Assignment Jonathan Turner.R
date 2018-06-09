# Get data set
library(readr)
library(MASS) 
library(caTools)
Computer_Data <- read_csv("C:/Users/Jonathan/Desktop/Edureka/Computer_Data.csv")
View(Computer_Data)
summary(Computer_Data)

# Split my dataset into two parts
set.seed(1000)
split <- sample.split(Computer_Data$price, SplitRatio = 0.7)
split

# Split up the dataset 
train <- subset(Computer_Data, split == TRUE)
test  <- subset(Computer_Data, split == FALSE)
nrow(Computer_Data)
nrow(train)
nrow(test)

# Linear regression model 
Reg1 <- lm(price ~ . , data = train)
summary(Reg1)

# Reject the Null all variables are *** significant


# explore Reg1 
names(Reg1)
Reg1$coefficients
Reg1$fitted.values
Reg1$residuals

# prediction test 
predicttest <- predict(Reg1, newdata = test)
predicttest[1:5]

#plot preddicttest
plot(test$price, col = "red", type = "l", lty=1.8)
lines(predicttest, col = "blue", type = "l", lty = 1.5)

# Get Data
install.packages("AER")
library(AER)
data(Affairs)
View(Affairs)

# factor affairs
Affairs$affairs <- factor(Affairs$affairs)

# fit linear model
lrmod <- glm(affairs ~ ., data = Affairs, family = "binomial")

confint(lrmod)
exp(coef(lrmod))

# Predict
predict(lrmod, type = "response")

#plot
cdplot(affairs ~ gender, data = Affairs)
cdplot(affairs ~ age, data = Affairs)
cdplot(affairs ~ yearsmarried, data = Affairs)
cdplot(affairs ~ children, data = Affairs)
cdplot(affairs ~ religiousness, data = Affairs)
cdplot(affairs ~ education, data = Affairs)
cdplot(affairs ~ occupation, data = Affairs)
cdplot(affairs ~ rating, data = Affairs)

