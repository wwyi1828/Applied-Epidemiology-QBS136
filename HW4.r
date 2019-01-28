setwd("E:/Download/QBS/AppliedEpi/Week4")
dig <- read.csv("dig.csv")
library(survival)
library(dplyr)
library(magrittr)
#Calculate the proportion
p_proportion <- nrow(dig[dig$TRTMT==0,])/nrow(dig)
t_proportion <- nrow(dig[dig$TRTMT==1,])/nrow(dig)
dig$surv <- Surv(time=dig$WHFDAYS,event=dig$WHF)
#Creat a new data frame with WHF, WHFDAYS and surv status
new_dig <- select(dig,c(WHF,WHFDAYS,surv))
head(new_dig)
#Summary 
survfit(dig$surv~dig$TRTMT)
survfit(dig$surv~dig$TRTMT) %>% summary()

#Question 9
#In treatment group, there are 3397 people who were at risk at baseline.
#8 events occured at baseline.
#There are 3386 people who were at risk at time 2.
#This is because some people censored during the study. For example, they died because of other reasons or quitted the study.


#Plot 
plot(x=survfit(dig$surv~dig$TRTMT),
     xlab="Months",
     ylab="Survival rate of hospitalized for worsening heart failure",
     col=c("red","blue"),
     xscale=30,
     main="Kaplan-Meier curves",
     sub="From the figure above, we can find that Digoxin reduces the probability of heart failure")
legend("bottomleft",
       c("Placebo Group","Digoxin Group"),
       col=c("red","blue"),
       lty=c(3,2))
#Question 11
death_data <- dig[dig$DEATH==1 & dig$REASON==1,]
dig$surv2 <- Surv(dig$DEATHDAY,dig$DEATH==1&dig$REASON==1)
dig$DEATHSW <- ifelse(dig$DEATH==1&dig$REASON==1,1,0)
new_dig2 <- select(dig,c(DEATHSW,DEATHDAY,surv2))
head(new_dig2)
survfit(dig$surv2~dig$TRTMT)
survfit(dig$surv2~dig$TRTMT) %>% summary()
#The first death because of worsening heart failure appeared in the eigth day.
#3391 people at risks and 1 event occured at that time
#What's more, many people censored from time 8 to time 18.
plot(x=survfit(dig$surv2~dig$TRTMT),
     xlab="Months",
     ylab="Survival rate of died for worsening heart failure",
     col=c("red","blue"),
     xscale=30,
     main="Kaplan-Meier curves",
     sub="From the figure above, digoxin reduce mortality rate significantly only after 60 months")
legend("bottomleft",
       c("Placebo Group","Digoxin Group"),
       col=c("red","blue"),
       lty=c(3,2))



