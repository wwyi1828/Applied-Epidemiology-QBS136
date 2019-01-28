setwd("E:/Download/QBS/AppliedEpi/wk5")
library(dplyr)
library(magrittr)
toy1 <- data.frame(letters1 = c(rep("A", 4),
                            rep("B", 4),
                            rep("C", 4)),
                            numbers1 = c(1:6, 1:6))
aggregate(toy1$numbers1, by = list(toy1$letters1), sum)

aggregate(cbind(numbers1.group = toy1$numbers1),
          by = list(letters1 = toy1$letters1),
          sum)

#toy2<- cbind(toy1,letter2=c(rep(c("X","X","Y","Y"),3)),
#numbers2=c(1:12))
toy2 <- data.frame(letters1 = c(rep("A", 4), rep("B", 4), rep("C", 4)),
                 letters2 = rep(c("X", "X", "Y", "Y"), 3),
                 numbers1 = c(1:6, 1:6),
                 numbers2 = 1:12)
#tapply(toy2[,2],toy2[,1],sum)
#aggregate(toy1$numbers1,by=list(toy1$letters1),sum)
aggregate(cbind(numbers1.group = toy2$numbers1, numbers2.group = toy2$numbers2),
          by = list(letters1 = toy2$letters1, letters2 = toy2$letters2),
          sum)

toy3 <- data.frame(letters1 = c("A", "B", "C", "D"),
                   numbers3 = c(1000, 2000, 3000, 4000))
merge(x = toy2, y = toy3, by = "letters1")
merge(x = toy2, y = toy3, by = "letters1", all = T)


#Part2
data <- read.csv("pennlc.csv")
str(data)
#data_ag <- aggregate(cbind(population=data$population,cases=data$cases),
#          by=list(Sex=data$sex,Age=data$age),
#          sum)
data_ag <- data %>% group_by(sex, age) %>% summarise(cases = sum(cases), population = sum(population)) %>% mutate(CI=cases/population)
data_ag_st <- data %>% group_by(county, sex, age) %>% summarise(cases = sum(cases), population = sum(population)) %>% ungroup() %>% mutate(CI = cases / population)
#Question 9 
new_m <- merge(data_ag, data_ag_st, by = c("sex","age"))
new_m %<>% mutate(EC = CI.x * population.y) %<>% select(1:2, 6:10)
eachcty <- new_m %>% group_by(county) %>% summarise(population=sum(population.y),ob_cases=sum(cases.y),ex_cases=sum(EC)) %>% mutate(SMR=ob_cases/ex_cases)
write.csv(eachcty,"final_dataset.csv",row.names=F)

#Part(3)
library(spdep)
library(maptools)
spd <- readShapeSpatial("E:/Download/QBS/AppliedEpi/wk5/PA_Counties_clip/PA_Counties_clip.shp")
names(spd)
colnames(eachcty)[1] <- "NAME"
new_cty <- merge(x = spd, y = eachcty, by = "NAME")
spplot(new_cty, "SMR") 
new_cty$incidence <- 100000*new_cty$ob_cases/new_cty$population
spplot(new_cty, "incidence") 


