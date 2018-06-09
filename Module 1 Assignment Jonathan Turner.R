#install and load 3 packages

install.packages("plyr")
library(plyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("RColorBrewer")
library(RColorBrewer)

#Create average program and print results

val <- c("A" = 22, "B" = 34, "C" = 102)
mean(val)
avg <- mean(val)




