library(tidyverse)


#Create tabulated data
corruption <- matrix(c(14, 6, 7, 7, 7, 1), ncol=3, byrow=TRUE)
colnames(corruption) <- c("notStopped", "bribeRequested", "stoppedWarned")
rownames(corruption) <- c("upperClass", "lowerClass")
corruption
#find expected values
expCorruption <- matrix(, ncol = 3, nrow = 2)
for (i in 1:nrow(corruption)){
  expCorruption[i, 1] <- (sum(corruption[i,])*sum(corruption[,1]))/sum(corruption)
  expCorruption[i, 2] <- (sum(corruption[i,])*sum(corruption[,2]))/sum(corruption)
  expCorruption[i, 3] <- (sum(corruption[i,])*sum(corruption[,3]))/sum(corruption)
}
expCorruption
#find squared residuals
chiCorruption <- ((corruption-expCorruption)^2)/expCorruption
chiCorruption

#sum for chi squared test statistic and check using the chisq.test() function
chi <- sum(chiCorruption)
chi
#I can then check my workings by running the chisq.test() function on the original data
chisq.test(corruption)


#question 2, use p function
pchisq(chi, df = 2, lower.tail = FALSE)


#question 3, use residuals formula
resCorruption <- matrix(, ncol = 3, nrow = 2)
for (i in 1:nrow(corruption)){
  resCorruption[i, 1] <- (corruption[i,1]-expCorruption[i,1])/(
    sqrt(expCorruption[i,1]*(1-(sum(corruption[,1])/sum(corruption)))*
           (1-(sum(corruption[i,])/sum(corruption)))))
  resCorruption[i, 2] <- (corruption[i,2]-expCorruption[i,2])/(
    sqrt(expCorruption[i,2]*(1-(sum(corruption[,2])/sum(corruption)))*
           (1-(sum(corruption[i,])/sum(corruption)))))
  resCorruption[i, 3] <- (corruption[i,3]-expCorruption[i,3])/(
    sqrt(expCorruption[i,3]*(1-(sum(corruption[,3])/sum(corruption)))*
           (1-(sum(corruption[i,])/sum(corruption)))))
}
resCorruption

#question 4, none of the residuals are larger than 3, so no outliers


#Exercise 2

#part a, read in data
policyData <- read.csv(url("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv"))
policyData
head(policyData)
policyData %>% #create boxplot to explore data, not particularly useful given outliers
  ggplot(aes(x=as.logical(reserved), y=water))+
  geom_boxplot()+
  labs(x="reserved",
       y="water")

#hypothesis should be rho = 0, alternative hypothesis rho != 0
#two variables are reserved and water

#part b

polModel <- lm(formula = water ~ reserved, data = policyData)
confint(polModel)
summary(polModel)

#confidence interval shows that rho is between 1.5 and 17 which means we can reject
#the null that rho equals 0

#part c
summary(polModel)
#we interpret the coefficient as saying that where there is a policy mandating
#a female council head, that there is an average increase of 9.252 repaired water
#pumps


