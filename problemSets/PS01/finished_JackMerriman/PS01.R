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
install.packages(tidyverse)
library(tidyverse)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#part 1

n <- length(y) #sample size
ybar <- mean(y) #sample mean
s <- sd(y) #sample standard deviation
cc <- 0.9 #confidence coefficient

n>30#t-distribution because CLT doesn't apply unless n>30

z <- qt((1+cc)/2,df=n-1) #z-score
se <- (s/sqrt(n)) #sample standard error
md <- z*se #distance of mean (md) from interval boundary
ybar - md #lower interval
ybar + md #upper interval

t.test(y, conf.level = 0.9, alternative = "two.sided") #checking my answer

#part 2

#null hypothesis = mean is less than or equal to 100
#alternative hypothesis = mean is greater than 100
mu0 <- 100 #null hypothesis population mean
alp <- 0.05 #alpha
dgfr <- n-1 #degrees of freedom
t <- (ybar-mu0)/se #t-score
p <- pt(t, dgfr, lower.tail = FALSE) #p value for right tailed t-test
p > alp
#p value is greater than 0.05, therefore we do not reject the null hypothesis

t.test(y, mu = 100, alternative = "greater") #checking my answer


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
       in urban areas in state",
       title = "Relationships between all discrete variables")

expenditure %>%
  ggplot(aes(x=Region, y=Y))+
  geom_boxplot()+
  labs(x = "Region",
       y = "per capita expenditure on shelters/housing assistance in state",
       title = "Shelter spending by region")

expenditure %>%
  ggplot(aes(x=X1, y=Y))+
  geom_point()+
  labs(x = "per capita personal income in state",
       y = "per capita expenditure on shelters/housing assistance in state",
       title = "Relationship between personal income and shelter spending")

expenditure %>%
  ggplot(aes(x=X1, y=Y, colour=Region, shape=Region))+
  geom_point()+
  labs(x = "per capita personal income in state",
       y = "per capita expenditure on shelters/housing assistance in state",
       colour = "Region",
       shape = "Region",
       title = "Relationship between personal income and shelter spending with
       regions")


