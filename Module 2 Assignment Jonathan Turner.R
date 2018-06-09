#Install packages
install.packages("DAAG")
library(DAAG)

install.packages("lattice")
library(lattice)

#Import Dataframe
data("jobs")
head(jobs)
View(jobs)

#Create an Alberta and BC dataframe and combine it with 'jobs' dataset

#Split columns
Alberta <- jobs[[2]]
BC <- jobs[[1]]

#Make Dataframes
Alb <- data.frame(Alberta)
BC <- data.frame(BC)

#Combine Dataframes
jobs1 <- cbind(jobs, Alb, BC)

#Find highest total employment across states

#Drop Date column
jb1 <- jobs[ ,1:6, drop = FALSE]

#make new dataframe
rSum <- data.frame(rowSums(jb1))

#rename column
colnames(rSum) <- ("Max")

#find max employment number across states 
max(rSum)

#find the month that has the max employment
which.max(rSum$Max)

#Find months in which employment figures drop below 950 in Atlantic

#Months in which jobs in Atlantic dropped below 950 
AtLessThan <- jobs[jobs$Atlantic < 950, ]

#Sort figures for Quebec in ascending order

#Quebec asc order
Qasc <- jobs[order(jobs$Quebec, decreasing = FALSE), ]



  