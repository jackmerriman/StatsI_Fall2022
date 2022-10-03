#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set working directory
setwd("~/Documents/GitHub/QTM200Spring2021/problem_sets/PS1")
install.packages("tidyverse")
library(tidyverse)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

n <- length(y) #sample size
ybar <- mean(y) #sample mean
s <- sd(y) #sample standard deviation
cc <- 0.9 #confidence coefficient

n>30#t-distribution because CLT doesn't apply unless n>30

#not finding the 90th percentile, because we exclude the first five percentiles, as
#this is a two tailed confidence interval, so (1+cc)/2 gives us 0.95 rather than 0.9
#degrees of freedom = sample size - 1
z <- qt((1+cc)/2,df=n-1) #z-score
z

se <- (s/sqrt(n)) #sample standard error
se

md <- z*se #distance of mean (md) from interval boundary
md

ll <- ybar-md
ul <- ybar+md
ll
ul


t.test(y, conf.level = 0.9, alternative = "two.sided")


#step 1 = formulate hypotheses
#null hypothesis - ybar <= 100
#alternative hypothesis - ybar > 100
#alpha is significance level

#step 2 = state variance
#variance, or alpha = 0.05
alp <- 0.05

#step 3 = calculate degrees of freedom
dgfr <- n-1

#step 4 = determine critical value
crit <- qt(alp, dgfr, lower.tail=FALSE)
crit

qt(abs(t), dgfr, lower.tail=FALSE)
#if t is greater than crit, then we reject null hypothesis

#step 5 = calculate test statistic
#t formula = sample mean - population mean divided by sample sd over square root n
ybar <- mean(y)
mu0 <- 100

t <- (ybar-mu0)/(s/sqrt(n)) #find t-score for this set of hyoptheses
pt(t, 24, lower.tail = FALSE)
#p value is greater than 0.05, therefore we do not reject the null hypothesis


t.test(y, mu = 100, alternative = "greater")


#if p is greater than alpha then accept the null hypothesis


#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2022/main/datasets/expenditure.txt", header=T)
expenditure$Region[expenditure$Region == 1] <- "Northeast"
expenditure$Region[expenditure$Region == 2] <- "North Central"
expenditure$Region[expenditure$Region == 3] <- "South"
expenditure$Region[expenditure$Region == 4] <- "West"

head(expenditure)
expenditure %>%
  ggplot(aes(x=X1, y=Y, colour = X2, size = X3))+
  geom_point()+
  labs(x = "per capita personal income in state",
       y = "per capita expenditure on shelters/housing assistance in state",
       colour =
       "Number of residents
       per 100,000 that
       are 'financially insecure'
       in state",
       size =
       "Number of people
       per thousand residing
       in urban areas in state")
expenditure %>%
  ggplot(aes(x=Region, y=Y))+
  geom_boxplot()+
  labs(x = "Region",
       y = "per capita expenditure on shelters/housing assistance in state")

expenditure %>%
  ggplot(aes(x=X1, y=Y, colour=Region))+
  geom_point()+
  labs(x = "per capita personal income in state",
       y = "per capita expenditure on shelters/housing assistance in state",
       colour = "Region")
