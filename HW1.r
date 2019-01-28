first_character <- c(18,27.5,26,24.5,28,26,26.1,28,27.5,29,26.5,26,25.8,28.5)  #Creat a vector of forearm
second_character <- c(17.5,18.5,19.5,14.5,21.5,19.7,18.6,20,20,22,17,18.5,18.4,20.5)
data <- data.frame(first_character,second_character)
str(data)
names(data)
names(data) <- c('forearm_length', 'hand_length')
data <- rbind(data,c(25.5,20.5),c(25.5,20.0),c(25.0,18.5),c(18.0,12.0),c(11.5,9.0))

adult <- rep(T,19)
adult[18:19] <- F
data <- cbind(data, adult)
str(data)
data$adult <- as.factor(data$adult)
summary(data)

library(dplyr)
data <- data %>% mutate(ratio = forearm_length/hand_length)
#ratio <- data$forearm_length/data$hand_length
#data <- cbind(data,ratio)
str(data)
dim(data)
ncol(data)
nrow(data)
str(data)

attach(data)

var <- c('forearm_length','hand_length','ratio')
min <- c(min(forearm_length),min(hand_length),min(ratio))
max <- c(max(forearm_length),max(hand_length),max(ratio))
mean <- c(mean(forearm_length),mean(hand_length),mean(ratio))
sd <- c(sd(forearm_length),sd(hand_length),sd(ratio))
median <- c(median(forearm_length),median(hand_length),sd(ratio))

stat_table <- data.frame(var, min, max, mean, sd, median)

CV <- function(x){
   100*sd(x)/mean(x)
}
forearm_length_cv <- CV(forearm_length)
hand_length_cv <- CV(hand_length)
ratio_cv <- CV(ratio)
cv <- c(forearm_length_cv,hand_length_cv,ratio_cv)
stat_table <- cbind(stat_table,cv)

write.csv(stat_table, file="summary.csv", row.names=F)
write.csv(data, file="length_data.csv", row.names=F)
csv <- read.csv("length_data.csv")

library(ggplot2)
ggplot(data,aes(forearm_length,hand_length))+
  geom_point(aes(color=adult))+
  geom_smooth(method=lm,se=F)+
  labs(title='Relationship Between Forearm and Hand',
       x='Forearm Length',
       y='Hand Length',
       caption='Figure 1. Scatter plot and regression of forearm length and hand length indicate they are strongly related ')+
  geom_text(aes(x=15,y=25,label=paste('correlation: ',cor(forearm_length,hand_length))))+
  theme(plot.title = element_text(hjust = 0.5))
cor(forearm_length,hand_length)
#plot(forearm_length,hand_length,xlab="forearm_length",ylab="hand_length")
#abline(lm(hand_length~forearm_length),col='red')
#cor(forearm_length,hand_length)
#legend('topleft',paste('correlation: ',cor(forearm_length,hand_length)),bty='n')

plot(data[which(adult=="TRUE"),]$forearm_length,
     data[which(adult=="TRUE"),]$hand_length,
     main="Relationship Between Forearm and Hand",
     xlab="adult_forearm_length",
     ylab="adult_hand_length")
abline(lm(data[which(adult=="TRUE"),]$hand_length~data[which(adult=="TRUE"),]$forearm_length),col='red')
text(20,20,paste('correlation: ',cor(data[which(adult=="TRUE"),]$forearm_length,data[which(adult=="TRUE"),]$hand_length)))
legend('topleft',"adult",pch=1,inset=.05,bty='n')
cor(data[which(adult=="TRUE"),]$forearm_length,data[which(adult=="TRUE"),]$hand_length)