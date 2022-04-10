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
cpc_pc + geom_jitter(alpha = 0.2, color = "green")
cpc_pc + geom_boxplot(aes(fill = color))
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

