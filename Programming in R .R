# Install ecdat package

install.packages("Ecdat")
library(Ecdat)
library(dplyr)

# View cigarette data frame

head(Cigarette)

# Create box plot of the average number of packs per capita by state. 

library("ggplot2")

ggplot(Cigarette, aes(x = state, y = packpc)) + geom_boxplot() 

summary(Cigarette) 

# KY has the highest packs per capita and UT has t 
Cigarette %>% group_by(state) %>% summarise(Mean = (mean(packpc))) %>% arrange(desc(Mean))
Cigarette %>% group_by(state) %>% summarise(Mean = (mean(packpc))) %>% arrange(Mean)

#	Find the median over all the states of the number of packs per capita for each year. 

MedianDF <- Cigarette %>% group_by(year) %>% summarise(Median = (median(packpc))) 
MedianDF

# Plot this median value for the years from 1985 to 1995. 

ggplot(MedianDF, aes(x=year, y=Median)) + geom_point()

# I can see that the usage of packs per capita decreased over the years. 

# Create a scatter plot of price per pack vs number of packs per capita for all states and years.

ggplot(Cigarette, aes (x=avgprs, y=packpc)) + geom_point() +geom_smooth(method=lm)

# Are the price and the per capita packs positively correlated, negatively correlated, or uncorrelated?

# Yes, they are negatively correlated,The line is trending downwards and correlation is -0.58. 

cor.test(Cigarette$avgprs, Cigarette$packpc, method = "pearson", "use complete.obs")

#Change your scatter plot to show the points for each year in a different color. 

ggplot(Cigarette, aes (x=avgprs, y=packpc, color=year)) + geom_point() +geom_smooth(method=lm)

#Does the relationship between the two variable change over time?
#Yes, price goes down with packs per year.

#Do a linear regression for these two variables. 

linereg <- lm(packpc~avgprs, Cigarette)
summary(linereg)

#How much variability does the line explain?
#34.15% of the variability

#Adjust the price of a pack of cigarettes for inflation

inflationprs <- Cigarette %>% mutate(inflation = avgprs /cpi)

#Re-do your scatter plot and linear regression using this adjusted price.

ggplot(inflationprs, aes (x=inflation, y=packpc, color=year)) + geom_point() +geom_smooth(method=lm)

lineregadj <- lm(packpc~inflation, inflationprs)
summary(lineregadj)

#Create a data frame with just the rows from 1985

Packs1985 <- Cigarette %>% filter(year == 1985)

#Create a data frame with just the rows from 1995

Packs1995 <- Cigarette %>% filter(year == 1995)

#From each of these data frames, get a vector of the number of packs per capita

Packs1985subset<- Packs1985[1:48,]

#Use a paired t-test to see if the number of packs per capita in 1995 was significantly different than the number of packs per capita in 1985.

t.test(Packs1985subset$packpc, Packs1995$packpc, paired = TRUE)

#The number of packs per capita in 1995 was significantly different than the number of packs per capita in 1985.