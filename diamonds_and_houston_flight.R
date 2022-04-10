library(ggplot2)
str(diamonds) ##describes data columns and datatypes
table(diamonds$color)
# output of 3rd line
#D     E     F     G     H     I     J 
#6775  9797  9542 11292  8304  5422  2808 
#Since color is a factor variable with 7 levels, we can plot carat vs. price with color represented by various colors. 
#Also, since the diamonds data frame is large, lets take a random sample of 100 records.
set.seed(123)
dsample <- diamonds[sample(nrow(diamonds), 100), ]
#Plot the points from the sample.
cpc0 <- ggplot(data = dsample, aes(carat, price, color)) 
cpc1 <- cpc0 + geom_point(aes(color = color), size = 3)
cpc1 
#Now we add a differentiable curve and regression line.
cpc2 <- cpc1 + geom_smooth()
cpc2 + ggtitle("Differentiable Curve")
##Regression Line
cpc3 <- cpc1 + geom_smooth(method = "lm", se = FALSE)
cpc3 + ggtitle("Regression Line")
#We can observe an apparent linear relationship between price and carat but it 
#is difficult to discern if any relationship exists between price and color with the above graphs. 
#Therefore,we will explore the impact of color using geom_jitter using the full diamonds data frame.
# for color and carat.
cpc_carat <- ggplot(data = diamonds, aes(x = color, y = carat)) 
cpc_carat + geom_jitter(alpha = 0.1)
#boxplot
cpc_carat + geom_boxplot(aes(fill = color))
#From both of the above plots, we notice that there may be a relationship between color and carat with higher carats boasting a higher color. 
#Density plots for carat with each color.
cpc_carat_den <- ggplot(data = diamonds, aes(x = carat))
cpc_carat_den + geom_density(aes(color = color)) + scale_x_continuous(limits = c(0,3))
#The density plot is consistent with our earlier observation suggesting a relationship.
#For color and price
cpc_price <- ggplot(data = diamonds, aes(x = color, y = price)) 
cpc_price + geom_jitter(alpha = 0.2, color = "blue")
cpc_price + geom_boxplot(aes(fill = color))
#Similar to our observations of color and carat, we notice that higher prices 
#are associated with the colors further away from the start of the alphabet.
#Density plot of price consistent with density plot of carat.
cpc_price_den <- ggplot(data = diamonds, aes(x = price))
cpc_price_den + geom_density(aes(color = color)) + scale_x_continuous(limits = c(0,17000))
#we see that colors D, E, F, and G are peaked at a lower price than the remaining colors of H, I, and J, 
#suggesting a relationship between color and price.
#Since carat and price appear to have a linear relationship, we expect price/carat to 
#illustrate a similar relationship.
#For color and price/carat:
cpc_pc <- ggplot(data = diamonds, aes(x = color, y = price/carat)) 
cpc_pc_den <- ggplot(data = diamonds, aes(x = price/carat))
cpc_pc_den + geom_density(aes(color = color)) + scale_x_continuous(limits = c(0,12000))
#Based on all of the above plots, we observe that color does have a relationship with price and carat.
head(diamonds)
##For above r codes taken help of https://rpubs.com/krask/98655 
install.packages("dplyr")
library(dplyr)
head(diamonds,10)## to get first 10 records.
ggplot(data = diamonds, aes(price,fill= cut))+
  scale_fill_brewer(type = 'qual')+
  geom_histogram()+
  scale_x_log10()+
  facet_wrap(~color, ncol=3)
## From above code , Different plots for colors in 3 rows and used colorbrewer and facetwrap.
ggplot(data=diamonds, aes(y=price, x=table, color=cut))+
  scale_fill_brewer(type = 'qual')+
  geom_jitter(position = position_jitter(height = 0))+
  scale_x_continuous(limits = c(50,80), breaks = seq(50,80,2))
summary(diamonds)

## HOUSTON FLIGHT DATA ANALYSIS from hflights Package ###
install.packages("hflights")
library(hflights)
str(hflights)
#For DayOfWeek
d <- ggplot(data = hflights, aes(x = factor(DayOfWeek), y = ArrDelay)) + geom_point()
d + xlab("Day of Week") + ylab("Arrival Delay")
##It stores the data as a vector of integer values. 
#Factor in R is also known as a categorical variable that stores both string and integer data values as levels.
# Boxplot below--
d_box <- ggplot(data = hflights, aes(x = factor(DayOfWeek), y = ArrDelay)) + geom_boxplot()
d_box + xlab("Day of Week") + ylab("Arrival Delay")
d_den <- ggplot(data = hflights, aes(x = ArrDelay))
d_den + geom_density(aes(color = factor(DayOfWeek))) + scale_x_continuous(limits = c(0,250))
# Based on above 3 plots it seems to confirm that there is not a relationship between arrival delays and day of the week.
#For Arrival Time
t_jitter <- ggplot(data = hflights, aes(x = ArrTime, y = ArrDelay)) + geom_jitter(alpha=0.1)
t_jitter + xlab("Arrival Time") + ylab("Arrival Delay")
##Making linear regression model from given data
t <- ggplot(data = hflights, aes(x = ArrTime, y = ArrDelay))
t + geom_point() + geom_smooth(method = "lm")
# We notice that the regression line is flat and both of the above graphs suggest 
# that there is no relationship between arrival times and arrival delays.
# We will take a sample to declutter the graphs and confirm the lack of relationship betweent the variables.
set.seed(111)
fsample <- hflights[sample(nrow(hflights), 1000), ]
t_jitter_sample <- ggplot(data = fsample, aes(x = ArrTime, y = ArrDelay)) + geom_jitter()
t_jitter_sample + xlab("Arrival Time") + ylab("Arrival Delay") + ggtitle("Random Sample")
##Again drawing linear regression line for the sample taken.
t_sample <- ggplot(data = fsample, aes(x = ArrTime, y = ArrDelay))
t_sample + geom_point() + geom_smooth(method = "lm")
#The sample confirms the appearance of no relationship between arrival delays and arrival times.
# Taken help from https://rpubs.com/krask/98655
