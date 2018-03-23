# R-Studio logistic regression with Retention.csv data

setwd("C:/Users/anjal/Desktop/Big Data Analytics")
Target <- read.csv("RETENTION.csv", header=TRUE)

TargetClean <- na.omit(Target)

# set refrence value for Gender
TargetClean$GENDER <- relevel(TargetClean$GENDER, ref ="M")

# fit base model
logitbase <- glm(Target ~ 1, data = TargetClean, family = binomial)

summary(logitbase)

# fit full model
logitfull <- glm(Target ~ AGE + Avg_income + 
                   Att_hrs_fall + Fall_GPA + Perc_hrs_comp_fall +
                   Need_pct_met + GENDER + Stu_worker_ind , 
                 data = TargetClean, family = binomial)

summary(logitfull)

# odds ratio
exp(coef(logitfull))



# for classification table
predprob <- fitted(logitfull)

probTable <- data.frame(predprob)

table(predprob>.5, TargetClean$Target)



# run stepwise selection with "forward" method
finalmodel <- step(logitbase, scope=list(upper=logitfull, lower=~1), direction="forward", trace=TRUE)

summary(finalmodel)

#odds ratio for forward selection 
exp(coef(finalmodel))

# model fit stats for logistic regression
# first find number of row values
n <- nrow(TargetClean)

# log likelyhood
logLik(finalmodel)


# Likelihood ratio test
install.packages('lmtest', repos="https://cran.r-project.org")
library(lmtest)

lrtest(logitbase, finalmodel)


# McFadden R2
McfadR2 <- 1-((finalmodel$deviance/-2)/(finalmodel$null.deviance/-2))
cat("mcFadden R2=",McfadR2,"\n")

# AIC
AIC<- finalmodel$deviance+2*2
cat("AIC=",AIC,"\n")


# package for logistic regression model fit stats
install.packages('pscl', repos="https://cran.r-project.org")
library(pscl)
pR2(finalmodel)

# for classification table
predprob <- fitted(finalmodel)

probTable <- data.frame(predprob)

table(predprob>.5, TargetClean$Target)

