#Read data
setwd("E:/Download/QBS/AppliedEpi/wk3")
library(dplyr)
data <- read.csv("frmgham2.csv")
baseline_1 <- data[data$PERIOD==1&data$PREVSTRK!=1,]
new_data <- baseline_1 %>% mutate(SAGE=AGE+TIMESTRK/365.25) %>% 
  select(c(RANDID,SEX,AGE,SAGE,STROKE)) #creat a new variable and select some variables we want to use
#Divide the data into two groups by sex
data_male <- new_data %>% filter(SEX==1) 
data_female <- new_data %>% filter(SEX==2) 
#establish a function to calculate Number of subjects contributing follow-up time and  Person years of follow-up
f <- function(data,start,end){
  leng <- c()
  evts <- c()
  for(i in 1:nrow(data)){
    if(min(data$SAGE[i],end)-max(data$AGE[i],start)>0){
      leng[i]=min(data$SAGE[i],end)-max(data$AGE[i],start)
      evts[i]=1
    }else{
      leng[i]=0
      evts[i]=0
    }
  }
  return(data.frame(leng,evts))
}
#Calculate some indicators we are interested in
age_range <- c(seq(35,85,10),max(new_data$SAGE)+1)
mpy<-c()
fpy<-c()
mes<-c()
fes<-c()
mcas<-c()
fcas<-c()
for(i in 1:length(age_range)){
  mpy[i]=sum(f(data_female,age_range[i],age_range[i+1]-1)$leng)
  fpy[i]=sum(f(data_male,age_range[i],age_range[i+1]-1)$leng)
}
for(i in 1:length(age_range)){
  mes[i]=sum(f(data_female,age_range[i],age_range[i+1]-1)$evts)
  fes[i]=sum(f(data_male,age_range[i],age_range[i+1]-1)$evts)
}
for (i in 1:length(age_range)-1){
  mcas[i]<-nrow(data_female[SEX==1&SAGE>=age_range[i]&SAGE<age_range[i+1]&STROKE==1,])
  fcas[i]<-nrow(data_female[SEX==2&SAGE>=age_range[i]&SAGE<age_range[i+1]&STROKE==1,])
}
#Creat a dataframe and save it as csv
AgeRange <- c("35-45","45-55","55-65","65-75","75-85",">85")
final <- data.frame(AgeRange,fes,fpy,fcas,mes,mpy,mcas)
final <- mutate(final,fir=fcas/fpy*1000)
final <- mutate(final,mir=mcas/mpy*1000)
write.csv(final,file="final.csv",row.names=F)
