ls()
rm(list = ls())
library(psych)
library(tidyverse)
library(ggplot2)
library(plyr)
library(Matrix)
library(lmtest)
library(sandwich)
library(matlib)
#to do====
#1.8 bootstrap 
#2.7 robust and clustered errors same?? 
#2.7 clustered errors manually?
#2.8 explanation 


#Part 1====
#1 summary
#Alternativ nur skim(evs) berücksichtigt die Datenstruktur 

evs = read.csv("ps2_EVS.csv")
skim(evs) 

#No missing observations. Besides this, data look plausible. ESS samples persons from 15 years or older, regardless of nationality 
#and citizenship, language or legal status. 
# Latter fact may explain why there are some observations with no elementary-education. Also in accordance, with the sampling 
#structure reported ages look plausible spreading from 15 to 108 years. With fewer observations at the lower and upper tails. 
#No apparent mistakes in data. Self-reported values of happiness and satisfaction lie within the 1-4 and 1-10 range. 
#Dummy variables take on two values - 0,1, year only 2008.  

#2 Conditional Expectation

#for age
cond_exp_age = aggregate(evs$Satisfaction, list(evs$Age), mean) 
plot(cond_exp_age$Group.1, cond_exp_age$x, "p") 

#looks slightly decreasing

#for unemployed

cond_exp_un = aggregate(evs$Satisfaction, list(evs$Unemployed), mean) 
plot(cond_exp_un$Group.1, cond_exp_un$x, "p") 

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
epsilonhat = y - X %*% betahat
View(reg1$residuals) #just checking whether manually computed residuals are correct
diagonal = diag(length(epsilonhat))#diagonal matrix 64160x64160 too large for R, thus can't compute robust standard errors manually.

coeftest(reg1, vcov=sandwich) 
#HC0 uses white standard errors. Robust SEs are smaller. 

#5
reg2 = lm(Satisfaction ~ Unemployed + PartnerUnemployed + Unemployed:PartnerUnemployed, data = evs)
summary(reg2)
#Effect how much unemployment decreases self-reported satisfaction depends on whether the partner is unemployed or not.
#If partner is unemployed, unemployment only decreases satisfaction by  (0.8725 - 0.3976) = 0.4749 points ceteris paribus.
#If partner isn't unemployed, unemployment decreases satisfaction by 0.8725 points ceteris paribus. 

#6
coeftest(reg2, vcov = sandwich) 
#If the error terms are heteroskedastic, OLS standard errors are biased. White standard errors are, however, consistent.  

#7
#H0 : beta1*beta2 = beta3 using delta method
# Rearrange H0 to beta1*beta2 - beta3 = 0
gradient = c(reg2$coefficients[3], reg2$coefficients[2], -1) 
#get variance betahat
vcov=vcov(reg2)
vcov=vcov[-1,-1]
#standard error for hypothesis by delta method:
se = sqrt(t(gradient) %*% vcov %*% gradient)
tstat = (reg2$coefficients[2]*reg2$coefficients[3] - reg2$coefficients[4])/se
crit = qt(0.95, (nrow(evs) - length(reg2$coefficients)))
if (abs(tstat)>crit) {
  print("H_0 that beta1*beta2 = beta3 is rejected")
} else {print("H_0 that beta1*beta2 = beta3 is not rejected")}
p.value = dt(tstat, df=length(evs)-length(reg2$coefficients))
paste("In fact, can be rejected at", p.value)

#8 bootstrap
bootstrap = matrix(nrow = length(reg2$coefficients), ncol = 10000)
#caution: the following command takes R quiet a while to compute
for (i in 1:10000) {
  bootstrap[,i] = lm(Satisfaction ~ Unemployed + PartnerUnemployed + Unemployed:PartnerUnemployed, data = sample_n(evs, size = nrow(evs), replace = TRUE))$coefficients
}
var(bootstrap[2,])
bootstrapSE = matrix(nrow = 4, ncol = 2)
bootstrapSE[,1] = coeftest(reg2, vcov=vcovHC(reg2, type = "HC0"))[,2]
bootstrapSE[,2] = c(sqrt(var(bootstrap[1,])), sqrt(var(bootstrap[2,])), sqrt(var(bootstrap[3,])), sqrt(var(bootstrap[4,])))


#Part 2====
ls()
rm(list = ls())
penn = read.csv("ps2_Penn.csv")
summary(penn$country)

#1 
#Contribute to the debate on the true costs of business cycles. 
#Run regressions on self-reported satisfaction of thousands of people from different countries and find: 
#(1) Broadly common structure of happiness regressions across countries, i.e. a set of personal characteristics has a similar 
#influence on self-reported satisfaction
#(2) Business cycles have a sizable effects on self-reported satisfaction. In monetary terms: Give each citizen 200 dollars
#per year to compensate for induced losses in self-reported satisfaction. 


#2 
evs = read.csv("ps2_EVS.csv")
merged = merge(evs, penn, by = c("country", "year"))
# less observations. for some country in evs there was no information in penn
# reason 1: evs confined to European countries, penn not. 
# reason 2: Kosovo, Macedonia, Moldova, Northern Cyprus, Northern Ireland in evs but not in penn. 

#3 Conditional Expectation
#unemployed

cond_exp_un = data.frame(matrix(NA, nrow=length(unique(merged$empPopRatio)), ncol =2))
cond_exp_un$X1 = unique(merged$empPopRatio)
for (i in unique(cond_exp_un$X1)) {
  cond_exp_un$X2[cond_exp_un$X1 == i] = mean(merged$Satisfaction[merged$empPopRatio == i & merged$Unemployed == 1])
} 
cond_exp_un = cond_exp_un[order(cond_exp_un$X1),]
names(cond_exp_un) = c("empPopRatio", "exp_satisfation_un")
plot(cond_exp_un$empPopRatio, cond_exp_un$exp_satisfation_un, "b")
abline(lm(cond_exp_un$exp_satisfation_un ~ cond_exp_un$empPopRatio))
#No systematic linear relationship visually detectable 

#employed
cond_exp_em = data.frame(matrix(NA, nrow=length(unique(merged$empPopRatio)), ncol = 2))
cond_exp_em$X1 = unique(merged$empPopRatio)
for (i in unique(cond_exp_em$X1)) {
  cond_exp_em$X2[cond_exp_em$X1 == i] = mean(merged$Satisfaction[merged$empPopRatio == i & merged$Unemployed == 0])
} 
names(cond_exp_em) = c("empPopRatio", "exp_satisfation_em")
plot(cond_exp_em$empPopRatio, cond_exp_em$exp_satisfation_em)
abline(lm( cond_exp_em$exp_satisfation_em ~ cond_exp_em$empPopRatio))
#Satisfaction is increasing in empPopRatio

#4
reg1 = lm(Satisfaction ~ Unemployed + empPopRatio, data = merged)
summary(reg1)
coeftest(reg1, vcov = sandwich)

#5 
#empPopRatio only varies at level of country. Within country-year-clusters each observation has the same empPopRatio 

#6
#Moulton factor. need average cluster size, rho_eps, rho_xj
#average cluster size

#compute Nbar 

length(unique(merged$country))
clustersize = data.frame(matrix(NA, nrow=length(unique(as.numeric(merged$country))), ncol = 4))
clustersize$X1 = c(1:41)
clustersize$X2 = unique(as.numeric(merged$country))
for (i in 1:41) {
  clustersize[i, 3] = length(merged$country[as.numeric(merged$country) == clustersize[i, 2]])
}
names(clustersize) = c("index", "country as number", "clustersize", "meanresid")
Nbar = mean(clustersize$clustersize)


#rho_eps: 
resid = reg1$residuals
merged$residuals = resid
sigmahatsq = (t(resid)%*%resid)/(length(resid) - length(reg1$coefficients))

for (i in 1:41) {
  clustersize[i, 4] = mean(merged$residuals[as.numeric(merged$country) == clustersize[i, 2]])
}

merged$countrynumeric = as.numeric(merged$country)
test = merge(merged, clustersize, by.x = "countrynumeric", by.y = "country as number")
merged = test
merged$residdemeaned = merged$residuals - merged$meanresid
sigmatildesq = (t(merged$residdemeaned) %*% merged$residdemeaned)/(length(merged$residuals) - length(unique(merged$country)) - length(reg1$coefficients))
rho_eps = (sigmahatsq - sigmatildesq)/sigmatildesq
rho_eps

#rho_xj, j=2 equals one
Moulton = sqrt(1 + (Nbar - 1)*rho_eps)
Moulton

X = matrix(NA, nrow=length(merged$Satisfaction), ncol=3)
X[,1] = c(1)
X[,2] = merged$Unemployed
X[,3] = merged$empPopRatio
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
moultonSE = matrix(nrow = 3, ncol = 1)
moultonSE[,1] = sqrt(diag(Vbetahat))
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

#8
reg2 = lm(Satisfaction ~ Unemployed + empPopRatio + Unemployed:empPopRatio, data = merged)
summary(reg2)
coeftest(reg2, vcov=vcovHC(reg2, type = "HC0", cluster = "index"))
# coefficient on unemployment positive
#Include Interpretation 

#9
curve(reg2$coefficients[1] + reg2$coefficients[2] + reg2$coefficients[3]*x + reg2$coefficients[4]*x, from = 0, to = 1, col= "red")
curve(reg2$coefficients[1] +  reg2$coefficients[3]*x, add=TRUE, col ="green")
legend("bottomleft", legend=c("Unemployed", "Employed"), col=c("red", "green"), rgb("red", "green"), pch = c(17,19), bty = "n", pt.cex = 2, cex = 1.2, text.col = "black", horiz =F, inset = c(0.1, 0.1))
# for unemployed people, empPopRatio negatively affects self-reported satisfaction. Conversely, for an employed person satisfaction inreases c.p. in empPopRatio.
# include code for two-line graph to illustrate difference between employed and unemployed.
