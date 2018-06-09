#Q1
VADeaths <- (VADeaths)
View(VADeaths)
mean(VADeaths[ ,2])

#Q2
str(VADeaths)
gsub(11.7, 12, VADeaths)

pattern <- 20
grep(pattern, VADeaths)
VADeaths[grep(pattern, VADeaths)] <- 20
str(VADeaths)

substr(VADeaths, 2, 3)

#Q3
mtcars <- (mtcars)
str(mtcars)
apply(mtcars, 2, mean)
apply(mtcars, 2, length)
apply(mtcars, 2,  function(x) length(x[x<3]))

sapply(1:5, function(x) x^5)
sapply(mtcars, function(x) x<5)

lapply(mtcars, function(x) x<5)
