# Check out the data
View(diamonds)
dia <- diamonds
class(dia)
str(dia)
which(is.na(dia))

# install packeges
install.packages("RcmdrPlugin.KMggplot2")
library(RcmdrPlugin.KMggplot2)
install.packages("ggplot2")
library(ggplot2)

# visualized diamonds using RcmdrPlugin.KMggplot2
require("ggplot2")
.df <- dia[c("carat", "depth", "price")]
.grid <- expand.grid(x = 1:ncol(.df), y = 1:ncol(.df))
.grid <- subset(.grid, x != y)
.all <- do.call("rbind", lapply(1:nrow(.grid), function(i) {
  xcol <- .grid[i, "x"]; 
  ycol <- .grid[i, "y"]; 
  data.frame(xvar = names(.df)[ycol], yvar = names(.df)[xcol],
             x = .df[, xcol], y = .df[, ycol], .df)
}))
.all$xvar  <- factor(.all$xvar, levels = names(.df))
.all$yvar  <- factor(.all$yvar, levels = names(.df))
.densities <- do.call("rbind", lapply(1:ncol(.df), function(i) {
  .tmp <- as.data.frame(density(x = .df[, i])[c("x", "y")]); 
  .tmp$y <- .tmp$y/max(.tmp$y)*diff(range(.tmp$x)) + min(.tmp$x); 
  data.frame(xvar = names(.df)[i], yvar = names(.df)[i],
             x = .tmp$x, y = .tmp$y)
}))
.all <- data.frame(.all, z = rep(dia$clarity, length = nrow(.all)))
.densities$z <- NA
.plot <- ggplot(.all, aes(x = x, y = y, colour = z, shape = z)) + 
  facet_grid(xvar ~ yvar, scales = "free") + 
  geom_point() + 
  geom_line(aes(x = x, y = y), data = .densities, colour = "black") + 
  scale_y_continuous(expand = c(0.01, 0)) + 
  xlab(NULL) + 
  ylab(NULL) + 
  labs(colour = "clarity", shape = "clarity") + 
  theme_bw(base_size = 14, base_family = "sans") + 
  theme(legend.position = "right")
print(.plot)
rm(.grid, .all, .densities, .plot)

# mtcars graphs
mtcars <- mtcars

# barplot
mtcars2 <- table(mtcars$gear)
par(bg="yellow")
barplot(mtcars2, main="Car Distribution", col = "green", 
        xlab="Number of Gears")

#scatterplot
attach(mtcars)
par(bg = "yellow")
plot(wt, mpg, main="Scatterplot Mtcars", 
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)
abline(lm(mtcars$mpg~mtcars$wt), col="green") 
lines(lowess(mtcars$wt,mtcars$mpg), col="blue")

#scatterplot matrix
pairs(~mpg+disp+drat+wt,data=mtcars, 
      main="Mtcars Matrix")


#lineplot
mtcars <- mtcars
par(bg="yellow", col="green")  
par(mfrow = (1:11)) 
for(i in 1:length(mtcars)){ 
  heading = paste("MtCars=",mtcars[i]) 
  plot(mtcars$mpg, mtcars$cyl, type = "o", main="Cars go Fast") 
  lines(mtcars$mpg, mtcars$cyl, type=mtcars[i]) 
}

#Pie chart
mtcars1 <- (mtcars$mpg)
lbls <- paste(names(mtcars1), "\n", mtcars, sep="")
pie(mtcars1, labels = lbls, 
    main="Pie Chart Mtcars")

#VADeaths histogram
VADeaths <- VADeaths
library(RColorBrewer)
brewer.pal
color <- brewer.pal(5,"Blues")
par(bg = "yellow")
hist(VADeaths, breaks=5, col= color)


