# Multivariate Regression
multivariate = read.csv("~/Downloads/multivariate.csv")
head(multivariate)
attach(multivariate)
help(lm)
mm = lm(Homeowners~Immigrant)
summary(mm)$coef

plot(Homeowners~Immigrant)
help(abline)
abline(mm)
abline(mm,col=2,lwd=3)

newImmigrantdata = data.frame(Immigrant = c(0,20))
# mm %>% predict(newImmigrantdata)

abline(mm)
abline(mm,col=3,lwd=3) # line color = green, line width = 3
attributes(mm)
mm$coefficients

# Creating Plots
# Chapter 2 -- R Graphics Cookbook
plot(mtcars$wt, mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt, mtcars$mpg)
qplot(wt, mpg, data = mtcars)
ggplot(mtcars, aes(x=wt, y=mpg))+geom_pont()
plot(pressure$temperature, pressure$pressure, type = 'l')
points(pressure$temperature, pressure$pressure)

lines(pressure$temperature, pressure$pressure/2, col = 'red')
points(pressure$temperature, pressure$pressure/2, col = 'blue')
library(ggplot2)
qplot(pressure$temperature, pressure$pressure, geom = 'line')
qplot(temperature, pressure, data = pressure, geom = 'line')
ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line() +geom_point()

# Creating Bar graphs
barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl)) # generate a table of counts.
qplot(mtcars$cyl) # cyl is continuos here
qplot(factor(mtcars$cyl)) # treat cyl as distance 
# Bar graph of counts
qplot(factor(cyl), data = mtcars)
ggplot(mtcars, aes(x = factor(cyl))) + geom_bar()

# Creating Histogram 
# View the distribution of one-dimentional data with a histogram
hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10)
hist(mtcars$mpg, breaks = 5)
hist(mtcars$mpg, breaks = 12)
qplot(mpg, data = mtcars, binwidth = 4)
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth = 4)
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth = 5)

# Creating Box-plot
plot(ToothGrowth$supp, ToothGrowth$len)
# Formula Syntax
boxplot(len ~ supp, data = ToothGrowth)
boxplot(len ~ supp + dose, data = ToothGrowth)
library(ggplot2)
qplot(ToothGrowth$supp, ToothGrowth$len, geom = 'boxplot')
qplot(supp, len, data = ToothGrowth, geom = 'boxplot')
ggplot(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot()
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom = 'boxplot')
qplot(interaction(supp, dose), len, data = ToothGrowth, geom = 'boxplot')
ggplot(ToothGrowth, aes(x = interaction(supp, dose), y = len)) + geom_boxplot()
##Plotting a function cureve

