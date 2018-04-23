ls()
rm(list = ls())
library(psych)
library(tidyverse)
library(ggplot2)
library(plyr)
library(Matrix)
library(lmtest)
library(sandwich)
#to do====
#structure and write
#1.6 interpretation
#1.8 bootstrap 
#2.1 summarize paper 
#2.7 robust and clustered errors same?? 
#2.7 clustered errors manually?
#2.8 explanation 


#Part 1====
#1 summary
#Alternativ nur skim(evs) berücksichtigt die Datenstruktur 

evs = read.csv("ps2_EVS.csv")
describe(evs)
summary(evs$country)
str(evs$country)
str(evs$Unemployed)
summary(evs$Unemployed)
str(evs$Sex)
summary(evs$Education)
hist(evs$Education)

#No missing observations. Besides this, data look plausible. ESS samples persons from 15 years or older, regardless of nationality and citizenship, language or legal status. 
# Latter fact may explain why there are some observations with no elementary-education. Also in accordance, with the sampling structure reported ages look also plausible spreading from 15 to 
#108 years. With fewer observations at the lower and upper tails. 
#No apparent mistakes in data. Self-reported values of happiness and satisfaction lie within the 1-4 and 1-10 range. 
#Dummy variables take on two values - 0,1. 
#Hannah Gender distribution, education distribution etc. 

#2 Conditional Expectation
# Alternativ könnte man abkürzen  cond_exp_age = aggregate(evs$Satisfaction, list(evs$Age), mean) 
# plot(cond_exp_age$Group.1, cond_exp_age$x, "p") # oder "b" für Punkte und Linien
#Gleiche Logik für andere Conditional Expectation

cond_exp_age = matrix(nrow=length(unique(evs$Age)), ncol = 2)
cond_exp_age = data.frame(cond_exp_age)
cond_exp_age$X1 = unique(evs$Age)
for (i in unique(cond_exp_age$X1)) {
  cond_exp_age$X2[cond_exp_age$X1 == i] = mean(evs$Satisfaction[evs$Age == i])
} 
cond_exp_age = rename(cond_exp_age, c("X1"="age", "X2"="exp_satisfation"))
plot(cond_exp_age$age, cond_exp_age$exp_satisfation)
#looks slightly decreasing
#now for unemployed
cond_exp_u = matrix(nrow=length(unique(evs$Unemployed)), ncol = 2)
cond_exp_u = data.frame(cond_exp_u)
cond_exp_u$X1 = unique(evs$Unemployed)
for (i in unique(cond_exp_u$X1)) {
  cond_exp_u$X2[cond_exp_u$X1 == i] = mean(evs$Satisfaction[evs$Unemployed == i])
} 
cond_exp_u = rename(cond_exp_u, c("X1"="unemployed", "X2"="exp_satisfation"))
plot(cond_exp_u$unemployed, cond_exp_u$exp_satisfation)
#perfectly linear as there are only two values for unemployed. Plot of two points always linear

#3 linear regression
reg1 = lm(Satisfaction ~ Unemployed, data = evs)
summary(reg1)
#being unemployed decreases satisfaction on scale 1-10 by 0.87 points on average (highly significant)

#4
#manual regression and computation of heteroskedastic standard errors
X = model.matrix(~., data = evs %>% select(Unemployed))
y = evs$Satisfaction
betahat = solve(t(X)%*%X) %*% t(X) %*% y
betahat
epsilonhat = y - X %*% betahat
View(reg1$residuals) #just checking whether manually computed residuals are correct
diagonal = diag(length(epsilonhat))#diagonal matrix 64160x64160 too large for R, thus can't compute robust standard errors manually.

coeftest(reg1, vcov = vcovHC(reg1, type = "HC0")) #HC0 uses white standard errors. Robust SEs are smaller.

#5
reg2 = lm(Satisfaction ~ Unemployed + PartnerUnemployed + Unemployed:PartnerUnemployed, data = evs)
summary(reg2)
#Effect how much unemployment decreases self-reported satisfaction depends on whether the partner is unemployed or not.
#If partner is unemployed, unemployment only decreases satisfaction by  (0.8725 - 0.3976) = 0.4749 points ceteris paribus.
#If partner isn't unemployed, unemployment decreases satisfaction by 0.8725 points ceteris paribus. 
#Hannah: ceteris paribus um deutlich zu machen, dass occupation status beim parnter konstant gehalten wird.  
# Hannah To Do: Beta1 und co Beziehung einfügen. 

#6
coeftest(reg2, vcov = vcovHC(reg2, type = "HC0")) #To Do Hannah: Check this command produces same results, used sandwich command
#interpretation? 
#If the error terms are heteroskedastic, OLS standard errors are biased. White standard errors are, however, consistent.  
#Hannah To Do: Argumentieren, grosse Abweichungen?

#7
#H0 : beta1*beta2 = beta3 using delta method
# Rearrange H0 to beta1*beta2 - beta3 = 0
gradient = c(reg2$coefficients[3], reg2$coefficients[2], -1) 
t(gradient)
View(gradient)
View(t(gradient))
#get variance betahat
vcov(reg2) #schritt notwendig? 
vcov=vcov(reg2)
vcov=vcov[-1,]
vcov=vcov[,-1]
#standard error for hypothesis by delta method:
se = sqrt(t(gradient) %*% vcov %*% gradient)
se
tstat = (reg2$coefficients[2]*reg2$coefficients[3] - reg2$coefficients[4])/se
tstat
crit = qt(0.95, (nrow(evs) - length(reg2$coefficients)))
if (abs(tstat)>crit) {
  print("H_0 that beta1*beta2 = beta3 is rejected")
} else {print("H_0 that beta1*beta2 = beta3 is not rejected")}
#Alternativ p-Wert ausrechnen, mehr Information zu welchem Signifikanzniveau verwerfen
#p.value = dt(tstat, df=length(evs)-length(reg$2coefficients))


#8 bootstrap
bootstrap = matrix(nrow = length(reg2$coefficients), ncol = 10)
for (i in 1:10) {
  evs[i] = sample_n(evs, size = nrow(evs), replace = TRUE)
  reg[i] = lm(Satisfaction ~ Unemployed + PartnerUnemployed + Unemployed:PartnerUnemployed, data = evs[i])
  bootstrap[,i] = reg[i]$coefficients
}


evs_sample = sample_n(evs, size = nrow(evs), replace = TRUE)
length(reg2$coefficients)

#Part 2====
ls()
rm(list = ls())
penn = read.csv("ps2_Penn.csv")
summary(penn$country)

#1 
#Investigate how macroeconomic variables influence human well-being

#2 
evs = read.csv("ps2_EVS.csv")
merged = merge(evs, penn, by = c("country", "year"))
# less observations. for some country in evs there was no information in penn
# reason 1: evs confined to European countries, penn not. Some countries such as Northern Ireland (Hannah include uni notes) not included in penn.

#3 Conditional Expectation
#unemployed

#Alternative wie oben
cond_exp_un = matrix(nrow=length(unique(merged$empPopRatio)), ncol = 2)
cond_exp_un = data.frame(cond_exp_un)
cond_exp_un$X1 = unique(merged$empPopRatio)
for (i in unique(cond_exp_un$X1)) {
  cond_exp_un$X2[cond_exp_un$X1 == i] = mean(merged$Satisfaction[merged$empPopRatio == i & merged$Unemployed == 1])
} 
cond_exp_un = rename(cond_exp_un, c("X1"="empPopRatio", "X2"="exp_satisfation_un"))
plot(cond_exp_un$empPopRatio, cond_exp_un$exp_satisfation_un)
#hardly any relationship visually detectable #Hannah: causal/linear, more or less spreads randomly?
#employed
cond_exp_em = matrix(nrow=length(unique(merged$empPopRatio)), ncol = 2)
cond_exp_em = data.frame(cond_exp_em)
cond_exp_em$X1 = unique(merged$empPopRatio)
for (i in unique(cond_exp_em$X1)) {
  cond_exp_em$X2[cond_exp_em$X1 == i] = mean(merged$Satisfaction[merged$empPopRatio == i & merged$Unemployed == 0])
} 
cond_exp_em = rename(cond_exp_em, c("X1"="empPopRatio", "X2"="exp_satisfation_em"))
plot(cond_exp_em$empPopRatio, cond_exp_em$exp_satisfation_em)
#difficult to see relationship, satisfaction might be increasing in empPopRatio

#4
reg1 = lm(Satisfaction ~ Unemployed + empPopRatio, data = merged)
summary(reg1)
coeftest(reg1, vcov = vcovHC(reg1, type = "HC0"))

#5 
#empPopRatio only varies at level of country. Within country-year-clusters each observation has the same empPopRatio 

#6
#Moulton factor. need average cluster size, rho_eps, rho_xj
#average cluster size
length(unique(merged$country))
str(merged$country)
summary(merged$country)
summary(as.numeric(merged$country))
clustersize = matrix(nrow = length(unique(as.numeric(merged$country))), ncol = 3)
clustersize[,1] = c(1:41)
clustersize[,2] = unique(as.numeric(merged$country))
clustersize = data.frame(clustersize)
for (i in 1:41) {
  clustersize[i, 3] = length(merged$country[as.numeric(merged$country) == clustersize[i, 2]])
}
clustersize = rename(clustersize, c("X1"="index", "X2"="as.numeric(country)", "X3"="clustersize"))
Nbar = mean(clustersize$clustersize)
#Hannah alternative aus Uni einfuegen.

#rho_eps: 
resid = reg1$residuals
sigmahatsq = (t(resid)%*%resid)/(length(resid) - length(reg1$coefficients))
sigmahatsq
merged$residuals = resid
clustersize$meanresid = mean(merged$residuals[as.numeric(merged$country) == clustersize$as.numeric(country)])
mean(merged$residuals)
mean(merged$residuals[as.numeric(merged$country) == 1])
for (i in 1:41) {
  clustersize[i, 4] = mean(merged$residuals[as.numeric(merged$country) == clustersize[i, 2]])
}
merged$countrynumeric = as.numeric(merged$country)
test = merge(merged, clustersize, by.x = "countrynumeric", by.y = "as.numeric(country)")
merged = test
merged$residdemeaned = merged$residuals - merged$meanresid
sigmatildesq = (t(merged$residdemeaned) %*% merged$residdemeaned)/(length(merged$residuals) - length(unique(merged$empPopRatio)) - length(reg1$coefficients))
sigmatildesq
rho_eps = (sigmahatsq - sigmatildesq)/sigmatildesq
rho_eps
#rho_x equals one
Moulton = sqrt(1 + (Nbar - 1)*rho_eps)
Moulton
X = model.matrix(~., data = merged %>% select(Unemployed, empPopRatio))
Vbetahat = c(sigmahatsq) * solve(t(X) %*% X) * c(1 + (Nbar - 1)*rho_eps)
y = merged$Satisfaction
betahat = solve(t(X)%*%X) %*% t(X) %*% y
betahat
cbind(betahat, sqrt(diag(Vbetahat)))


#7
coeftest(reg1, vcov=vcovHC(reg1, type = "HC0", cluster = "index"))
#comparison:
clusteredSE = matrix(nrow = 3, ncol = 1)
for (i in 1:3) {
  clusteredSE[i,1] = coeftest(reg1, vcov=vcovHC(reg1, type = "HC0", cluster = "index"))[i,2]
}
robustSE = matrix(nrow = 3, ncol = 1)
robustSE[,1] = coeftest(reg1, vcov = vcovHC(reg1, type = "HC0"))[,2]
robustSE
moultonSE = matrix(nrow = 3, ncol = 1)
moultonSE[,1] = sqrt(diag(Vbetahat))
moultonSE
comparison = data.frame(robust=robustSE, moulton=moultonSE, clustered=clusteredSE)
comparison

#manually
#need for each cluster g: X_g and Psi_g
merged = arrange(merged, index)
reg1 = lm(Satisfaction ~ Unemployed + empPopRatio, data = merged)
resid = reg1$residuals
X = model.matrix(~., data = merged %>% select(Unemployed, empPopRatio, index))
X[,-4]
X = data.frame(X)
sum = matrix(0,nrow = 3, ncol = 3)
for (i in 1:41) {
  sum = sum + 
    t(cbind(X$X.Intercept.[X$index == i], X$Unemployed[X$index == i], X$empPopRatio[X$index ==i])) %*%
    ((length(unique(X$index))/(length(unique(X$index)) - 1)) * (merged$residuals[merged$index == i] %*% t(merged$residuals[merged$index == i]))) %*%
    cbind(X$X.Intercept.[X$index == i], X$Unemployed[X$index == i], X$empPopRatio[X$index == i])
}
#sum should be sum over g of (X_g)' times Psi_hat times X_g in formula from slide 23
clustervhatbeta = solve(t(X[,-4]) %*% X[,-4]) %*% sum %*% solve(t(X[,-4]) %*% X[,-4])
#not sure why it isn't working
#Hannah: Check uni 

#8
reg2 = lm(Satisfaction ~ Unemployed + empPopRatio + Unemployed:empPopRatio, data = merged)
summary(reg2)
coeftest(reg2, vcov=vcovHC(reg2, type = "HC0", cluster = "index"))
# coefficient on unemployment positive
#Include Interpretation 

#9
curve(reg2$coefficients[1] + reg2$coefficients[2] + reg2$coefficients[3]*x + reg2$coefficients[4]*x, from = 0, to = 1)
# for unemployed people, empPopRatio negatively affects Satisfaction
# include code for two-line graph to illustrate difference between employed and unemployed.
