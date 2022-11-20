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
library(tidyverse)
library(stargazer)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2022/main/datasets/incumbents_subset.csv")


#QUESTION 1

#1
lm_vd <- lm(formula = voteshare ~ difflog, data = inc.sub)
stargazer(lm_vd, title = "Linear Regression: Vote Share - Spending Difference")
summary(lm_vd)

#2
inc.sub %>%
  ggplot(aes(x = difflog, y = voteshare))+
  geom_point()+
  geom_smooth(method = lm)
#3
res_vd <- unlist(residuals(lm_vd))
res_vd
#4
#Y = a+bx
#voteshare = 0.58 + 0.04(difflog)

#QUESTION 2

#1
lm_pd <- lm(formula = presvote ~ difflog, data = inc.sub)
stargazer(lm_pd, title = "Linear Regression: Presidential Vote Share - Spending Difference")
summary(lm_pd)
#2
inc.sub %>%
  ggplot(aes(x = difflog, y = presvote))+
  geom_point()+
  geom_smooth(method = lm)
#3
res_pd <- unlist(residuals(lm_pd))
res_pd
#4
#Y = a+bx
#presvote = 0.51 + 0.02(difflog)

#QUESTION 3

#1
lm_vp <- lm(formula = voteshare ~ presvote, data = inc.sub)
stargazer(lm_vp, title = "Linear Regression: Vote Share - Presidential Vote Share")
summary(lm_vp)
#2
inc.sub %>%
  ggplot(aes(x = presvote, y = voteshare))+
  geom_point()+
  geom_smooth(method = lm)
#3
#Y = a+bx
#voteshare = 0.44 + 0.39(presvote)

#QUESTION 4

#1
lm_res <- lm(formula = res_vd ~ res_pd)
stargazer(lm_res, title = "Linear Regression: Residual Model")
summary(lm_res)
#2
tib_res <- tibble(res_vd, res_pd)
tib_res %>%
  ggplot(aes(x = res_pd, y = res_vd))+
  geom_point()+
  geom_smooth(method = lm)
#3
#res_vd = -4.86e-18 - 2.569e-01(res_pd)

?lm
#QUESTION 5
#1
final_model <- lm(formula = voteshare ~ difflog + presvote, data = inc.sub)
stargazer(final_model, title = "Linear Regression: Vote Share - Spending Difference and Presidential Vote")
summary(final_model)
#2
# voteshare = 0.44 + 0.04(difflog) + 0.26(presvote)
#3
#The residuals of the two models are the same. The residual model investigated the
#confounding effects between presvote and difflog's effect on voteshare, so a model
#including them both as input variables will have the same residuals as the residual model

