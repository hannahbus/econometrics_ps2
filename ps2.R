library(psych)

#Part 1====
#1
evs = read.csv("ps2_EVS.csv")
describe(evs)
summary(evs$country)
str(evs$country)
str(evs$Unemployed)
summary(evs$Unemployed)
str(evs$Sex)
summary(evs$Education)
hist(evs$Education)
