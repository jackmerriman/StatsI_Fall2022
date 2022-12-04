install.packages("car")
install.packages("carData")
library(car)
data(Prestige)
help(Prestige)

library(stargazer)
library(tidyverse)
install.packages("GGally")
library(GGally)

###QUESTION 1

##(a)
head(Prestige)
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)

##(b)
model <- lm(prestige ~ income*professional, data = Prestige)
summary(model)
stargazer(model)

ggplot(data = Prestige, aes(x = income, y = prestige, color = professional))+
  geom_point()+
  geom_smooth(method = lm)

ggplot(data = Prestige, aes(x = as.factor(professional), y = income))+
  geom_boxplot()

ggpairs(data.frame(Prestige$prestige, Prestige$income, Prestige$professional))

##(c)
##(d)
##(e)
##(f)
##(g) - all dealt with in LaTeX

###QUESTION 2

##a - Calculate test statistic with coefficient and se, find P-value
##95% confidence interval
0.042/0.016
2*pt(2.625, df = 128, lower.tail = FALSE)

##b
0.042/0.013
2*pt(0.042/0.013, df = 128, lower.tail = FALSE)
##c
##d - overall F-test
0.094/2
1-0.094
(0.094/2)/((1-0.094)/128)

df((0.094/2)/((1-0.094)/128), 2, 128)
