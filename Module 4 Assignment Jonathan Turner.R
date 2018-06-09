#Import data and calculate mean
install.packages("openxlsx")
library(openxlsx)

my_txt <- read.xlsx("C:/Users/Jonathan/Desktop/Edureka/height_weight.xlsx")
View(my_txt)

subw <- subset(my_txt, Height > 4.7, select = "Weight")
mean(subw$Weight)

#Change Bobby's weight
my_txt["2", "Weight"] = 45

#Import Cricket data
install.packages(XML)
library(XML)

url="http://stats.espncricinfo.com/ci/engine/stats/index.html?class=11;template=results;type=batting"
tables=readHTMLTable(url,stringsAsFactors = F)

#convert to numeric
table2=tables$"Overall figures"
rm(tables)

#Creating new variables from Span
table2$Debut=as.numeric(substr(table2$Span,1,4))
table2$LastYr=as.numeric(substr(table2$Span,6,10))
table2$YrsPlayed=table2$LastYr-table2$Debut

#Creating new variables
table2$HSNotOut=grepl("\\*",table2$HS)
table2$HS2=gsub("\\*","",table2$HS)

#Creating a FOR Loop (!) to convert variables to numeric variables
for (i in 3:17) {table2[, i] <- as.numeric(table2[, i])}

str(table2)
