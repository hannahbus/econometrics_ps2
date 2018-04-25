ls()
rm(list = ls())
library(psych)
library(tidyverse)
library(ggplot2)
library(plyr)
library(Matrix)
library(lmtest)
library(sandwich)
library(skimr)
#to do====
#2.7 clustered errors manually?
#part3(optional)


#Part 1====
#1 summary
#Alternativ nur skim(evs) berücksichtigt die Datenstruktur 

evs = read.csv("ps2_EVS.csv")
describe(evs)
skim(evs)

#No missing observations. Besides this, data look plausible. ESS samples persons from 15 years or older, regardless of nationality and citizenship, language or legal status. 
# Latter fact may explain why there are some observations with no elementary-education. Also in accordance, with the sampling structure reported ages look also plausible spreading from 15 to 
#108 years. With fewer observations at the lower and upper tails. 
#No apparent mistakes in data. Self-reported values of happiness and satisfaction lie within the 1-4 and 1-10 range. 
#Dummy variables take on two values - 0,1, year only 2008. 
#Hannah Gender distribution, education distribution etc. 

#2 Conditional Expectation
cond_exp_age = aggregate(evs$Satisfaction, list(evs$Age), mean)
plot(cond_exp_age$Group.1, cond_exp_age$x, "b")
#looks slightly decreasing
#now for unemployed
cond_exp_u = aggregate(evs$Satisfaction, list(evs$Unemployed), mean)
plot(cond_exp_u$Group.1, cond_exp_u$x, "b")
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
diagonal = diag(length(epsilonhat))#this diagonal matrix 64160x64160 would be needed to compute robust standard errors manually (following the formula on slide 12). It is however too large for R, thus can't compute robust standard errors manually.
#We can also get robust standard errors using the coeftest command:
coeftest(reg1, vcov = vcovHC(reg1, type = "HC0")) #HC0 uses White standard errors. Robust SEs are smaller than the ones assuming homoskedasticity.

#5
reg2 = lm(Satisfaction ~ Unemployed + PartnerUnemployed + Unemployed:PartnerUnemployed, data = evs)
summary(reg2)
#Effect how much unemployment decreases self-reported satisfaction depends on whether the partner is unemployed or not.
#If partner is unemployed, unemployment only decreases satisfaction by  (0.8725 - 0.3976) = 0.4749 points ceteris paribus.
#If partner isn't unemployed, unemployment decreases satisfaction by 0.8725 points ceteris paribus. 
# Hannah To Do: Beta1 und co Beziehung einfügen. 

#6
coeftest(reg2, vcov = vcovHC(reg2, type = "HC0")) #To Do Hannah: Check this command produces same results, used sandwich command 
#If the error terms are heteroskedastic, OLS standard errors are incorrect. White standard allow for heteroskedasticity and yield correctly computed standard errors and thus correct conclusions about the significance of estimated coefficients.  
#When interpreting estimated coefficients one should keep in mind that the dependend variable is self-reported satisfaction in a survey and not a naturally measured variable. Thus one should for example refrain from intepretations like "a person with satisfaction 8 is twice as happy as one with satisfaction 4."
#Hannah To Do: Argumentieren, grosse Abweichungen?

#7
#H0 : beta1*beta2 = beta3 using delta method
# Rearrange H0 to beta1*beta2 - beta3 = 0
gradient = c(reg2$coefficients[3], reg2$coefficients[2], -1) 
t(gradient)
View(gradient)
View(t(gradient))
#get variance betahat
vcov=vcov(reg2)
vcov=vcov[-1,]
vcov=vcov[,-1]
#standard error for hypothesis by delta method:
se = sqrt(t(gradient) %*% vcov %*% gradient)
se
tstat = (reg2$coefficients[2]*reg2$coefficients[3] - reg2$coefficients[4])/se
tstat
p.value = dt(tstat, df=length(evs)-length(reg2$coefficients))
p.value
#reject hypothesis for any significance level smaller than the p-value

#8 bootstrap
bootstrap = matrix(nrow = length(reg2$coefficients), ncol = 10000)
for (i in 1:10000) {
  bootstrap[,i] = lm(Satisfaction ~ Unemployed + PartnerUnemployed + Unemployed:PartnerUnemployed, data = sample_n(evs, size = nrow(evs), replace = TRUE))$coefficients
}
var(bootstrap[2,])
bootstrapSE = matrix(nrow = 4, ncol = 2)
bootstrapSE[,1] = coeftest(reg2, vcov=vcovHC(reg2, type = "HC0"))[,2]
bootstrapSE[,2] = c(sqrt(var(bootstrap[1,])), sqrt(var(bootstrap[2,])), sqrt(var(bootstrap[3,])), sqrt(var(bootstrap[4,])))
bootstrapSE #compares White standard errors to standard errors achieved via bootstrap

#Part 2====
ls()
rm(list = ls())
penn = read.csv("ps2_Penn.csv")

#1 
#Contribute to the debate on the true costs of business cycles. 
#Run regressions on self-reported satisfaction of thousands of people from different countries and find: 
#(1) Broadly common structure of happiness regressions across countries, i.e. a sizable set of personal characteristics has a similar influence on self-reported satisfaction
#(2) Business cycles have a sizable effects on self-reported satisfaction. In monetary terms: Give each citizen with 200 dollars
# per year to compensate for induced losses in self-reported satisfaction. 


#2 
evs = read.csv("ps2_EVS.csv")
merged = merge(evs, penn, by = c("country", "year"))
# less observations. for some country in evs there was no information in penn
# reason 1: evs confined to European countries, penn not. Some countries such as Northern Ireland (Hannah include uni notes) not included in penn.

#3 Conditional Expectation
#unemployed
cond_exp_empU = aggregate(merged$Satisfaction[merged$Unemployed == 1], list(merged$empPopRatio[merged$Unemployed == 1]), mean)
plot(cond_exp_empU$Group.1, cond_exp_empU$x, "b")
#hardly any relationship visually detectable #Hannah: causal/linear, more or less spreads randomly?
#employed
cond_exp_empE = aggregate(merged$Satisfaction[merged$Unemployed == 0], list(merged$empPopRatio[merged$Unemployed == 0]), mean)
plot(cond_exp_empE$Group.1, cond_exp_empE$x, "b")
#difficult to see relationship, but it seems like satisfaction is increasing in empPopRatio.

#4
reg1 = lm(Satisfaction ~ Unemployed + empPopRatio, data = merged)
summary(reg1)
coeftest(reg1, vcov = vcovHC(reg1, type = "HC0"))

#5 
#empPopRatio only varies at level of country. Within country-year-clusters each observation has the same empPopRatio 

#6
#Moulton factor. need average cluster size, rho_eps, rho_xj
#average cluster size
merged = merged %>% mutate(indicator = 1)
groupsize = aggregate(merged$indicator, list(merged$country), sum)
Nbar = mean(groupsize$x)

#rho_eps: 
resid = reg1$residuals
sigmahatsq = (t(resid)%*%resid)/(length(resid) - length(reg1$coefficients))
sigmahatsq
merged$residuals = resid
clustersize = matrix(nrow = length(unique(as.numeric(merged$country))), ncol = 3)
clustersize[,1] = c(1:41)
clustersize[,2] = unique(as.numeric(merged$country))
clustersize = data.frame(clustersize)
clustersize = rename(clustersize, c("X1"="index", "X2"="as.numeric(country)", "X3"="meanresid"))
for (i in 1:41) {
  clustersize[i, 3] = mean(merged$residuals[as.numeric(merged$country) == clustersize[i, 2]])
}
merged$countrynumeric = as.numeric(merged$country)
test = merge(merged, clustersize, by.x = "countrynumeric", by.y = "as.numeric(country)")
merged = test
merged$residdemeaned = merged$residuals - merged$meanresid
sigmatildesq = (t(merged$residdemeaned) %*% merged$residdemeaned)/(length(merged$residuals) - length(unique(merged$empPopRatio)) - length(reg1$coefficients))
sigmatildesq
rho_eps = (sigmahatsq - sigmatildesq)/sigmatildesq
rho_eps
#rho_x equals one as empPopRatio is constant within each cluster.
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
#coefficient on unemployment positive. However beta1 is not the whole effect of Unemployment on self-reported satisfaction. Due to the interaction term it also depends on the value of empPopRatio. The estimated coefficient for the interaction term is negative. Taking the derivative of the regression equation with the estimated coefficients shows that the expected marginal effect of Unemployment (including the effect through the interaction term) is negative if empPopRatio is larger than beta1/(-beta3) = 1.14/5 = 0.228.

#9
curve(reg2$coefficients[1] + reg2$coefficients[2] + reg2$coefficients[3]*x + reg2$coefficients[4]*x, from = 0, to = 1)
# for unemployed people, empPopRatio negatively affects Satisfaction. Conversely, for an employed person satisfaction inreases c.p. in empPopRatio.

