setwd("D:/Program Files (x86)/QBS Hw/Applied Epi/week6")
library(magrittr)
library(dplyr)
data <- read.csv("frmgham2.csv")
data %<>% filter(PERIOD==1)
str(data)
data %>% group_by(SEX) %>% select(CURSMOKE) %>% table() %>% prop.table()
attach(data)
chisq.test(SEX,CURSMOKE)
chisq.test(CURSMOKE,SEX)
detach(data)

freq <- data %>% group_by(SEX) %>% select(CURSMOKE) %>% table() %>% data.frame()
attach(freq)
vec_s <- as.vector(c(freq[SEX == 1 & CURSMOKE == 1,]$Freq, freq[SEX == 2 & CURSMOKE == 1,]$Freq))
vec_t <- as.vector(c(sum(freq[SEX == 1,]$Freq), sum(freq[SEX == 2,]$Freq)))
prop.test(vec_s, vec_t)
detach(freq)
#Yes, the answers of two ways above is same.
attach(data)
data$AR=0
data[AGE >= 30 & AGE <= 39,]$AR <- "[30,39]"
data[AGE >= 40 & AGE <= 49,]$AR <- "[40,49]"
data[AGE >= 40 & AGE <= 49,]$AR <- "[40,49]"
data[AGE >= 50 & AGE <= 59,]$AR <- "[50,59]"
data[AGE >= 60,]$AR <- "[60,Inf]"
#age_range <- ifelse(
#                    AGE >= 30 & AGE <= 39, "[30,39]",
#                    ifelse(AGE>=40&AGE<=49,"[40,49]",
#                           ifesle))
freq_2 <- table(data$AR,data$CURSMOKE) %>% addmargins()
prop.trend.test(as.vector(freq_2[1:4,2]),
                as.vector(freq_2[1:4,3]))
#Creat a figure
exps <- freq_2[1:4,2] %>% as.numeric()
total <- freq_2[1:4,3] %>% as.numeric()
AgeR <- c("[30,39]","[40,49]","[50,59]",">60")
fig <- cbind(AgeR,exps,total) %>% data.frame()
fig %<>% mutate(Cumulative_incidece=as.numeric(as.character(fig$exps))/as.numeric(as.character(fig$total)))
library(ggplot2)
fig$AgeR %<>% as.character()
fig %>% ggplot(aes(AgeR, Cumulative_incidece)) +
    geom_bar(stat = 'identity', col = 'olivedrab', fill = 'olivedrab', width = 0.4) +
    geom_line(aes(group=1),col='chocolate',size=1) +
  labs(x="Age Range",y="proportion(Cumulative incidence)",
       title="Barplot",
       subtitle = "4434 Samples") +
       geom_text(aes(x = AgeR, y = Cumulative_incidece + 0.06, label = round(Cumulative_incidece, digits = 3)), col="deeppink",size=10)+
  theme(plot.title = element_text(size = 30,hjust = 0.5),
        plot.caption = element_text(size = 25,hjust= 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        text=element_text(face='italic',color = 'olivedrab'))+
  theme(panel.grid.major =element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "olivedrab"))

summary(data[data$SEX==1,]$TOTCHOL)
summary(data[data$SEX==2,]$TOTCHOL)
t.test(data[data$SEX == 1,]$TOTCHOL, data[data$SEX == 2,]$TOTCHOL)
t.test(data$TOTCHOL ~ data$SEX)
plot(data$TOTCHOL ~ data$SEX)

data$SEX %<>% as.character()

ggplot(data = data, aes(x = SEX, y = log(TOTCHOL))) +
    geom_boxplot(aes(fill = SEX), notch= T) +
    geom_point(position = "jitter", color = "steelblue", alpha = .2) +
    geom_rug(color = "deeppink")+
    scale_fill_brewer(labels = c("Male", "Female")) +
    labs(x = "SEX", y = "Total Cholesterol(ln(mg/dL))",
       title = "Boxplot",
       subtitle = "SEX~Total Cholesterol at baseline") +
       scale_x_discrete(labels = c("Male", "Female")) +
       theme(plot.title = element_text(size = 30, hjust = 0.5),
        plot.caption = element_text(size = 25, hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        text = element_text(face = 'italic', color = 'olivedrab')) +
    theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "olivedrab"))
