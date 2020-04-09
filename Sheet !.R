

#preparing workspace

setwd(".....longitude/R")
DAT01<-readRDS("....longitude/R/LEON.rds")



# Packages 
install.packages("stargazer")
install.packages("haven")
install.packages("descr")
install.packages("survival")
install.packages("eha")
install.packages("dplyr")
install.packages("survminer")

# Libraries
library(haven)
library(descr)
library(survival)
library(dplyr)
library(eha)
library(stargazer)
library(survminer)

#exploring  and preparing data

#looking at distribution of DV
hist(DAT$TIME, breaks = 60)
table(DAT$TIME)
mean(DAT01$TIME)

median((DAT01$Timer))
DAT01$Timer<-as.numeric(DAT01$TIME)
sd(DAT01$TIME)

table(DAT01$TIME)


table(DAT01$syear)
table(DAT01$EVENT)
table(DAT01$EVENT,DAT01$TIME)
#there is a bulk at 60 monhts, which implies that there might be a stop of counting since people will be considered as long term unemployed and hence drop out of statistics

hist(DAT01$plh0171[DAT01$TIME==60])
table(DAT$plh0171)

hist(DAT01$plh0171, breaks=11)
mean(DAT01$plh0171)
sd(DAT01$plh0171)

#Looks normally distributed around 8 though thereare some -1 and -5 which need to be removed


DAT01<-subset(DAT, plh0171!=-5 & plh0171!=-1)
table(DAT01$plh0171)

DAT01$Health<-0

DAT01$Health[DAT01$plh0171==0]<-"Bad"
DAT01$Health[DAT01$plh0171==1]<-"Bad"
DAT01$Health[DAT01$plh0171==2]<-"Bad"
DAT01$Health[DAT01$plh0171==3]<-"Bad"
DAT01$Health[DAT01$plh0171==4]<-"Bad"
DAT01$Health[DAT01$plh0171==5]<-"Fair"
DAT01$Health[DAT01$plh0171==6]<-"Fair"
DAT01$Health[DAT01$plh0171==7]<-"Fair"
DAT01$Health[DAT01$plh0171==8]<-"Good"
DAT01$Health[DAT01$plh0171==9]<-"Good"
DAT01$Health[DAT01$plh0171==10]<-"Good"


table(DAT01$Health)
#table

table(DAT01$EDU)
table(DAT01$AGEUN)
table(DAT01$REGION)

#general survival function
SURVIVAL01 <-survfit(Surv(DAT01$TIME, DAT01$EVENT)~1)
plot(SURVIVAL01)
plot(SURVIVAL01, col = c("blue"),
     xlab="unemployment duration (in months)")

#survival based on health
SURVIVAL02 <-survfit(Surv(DAT01$TIME, DAT01$EVENT)~DAT01$Health)

plot(SURVIVAL02, col = c("blue","red","green"),
     xlab="unemployment duration (in months)", ylab="Hazard ratio")
legend("topright", legend=c("Fair", "Bad", "Good"),
       col=c("red", "blue", "green"), lty=1, cex=0.8)


MODEL01 <- coxph(Surv(TIME,EVENT) ~  Health, data=DAT01)
ggforest(MODEL01, data = NULL, main = "Hazard ratio")


MODEL02 <- coxph(Surv(TIME,EVENT) ~  Health+EDU+AGEUN, data=DAT01)
ggforest(MODEL02, data = NULL, main = "Hazard ratio")


MODEL03 <- coxph(Surv(TIME,EVENT) ~  Health+EDU+AGEUN, data=DAT01)
ggforest(MODEL03, data = NULL, main = "Hazard ratio")
stargazer(MODEL01,MODEL02,MODEL03,MODEL04)

MODEL04 <- coxph(Surv(TIME,EVENT) ~  Health+EDU+REGION+AGEUN, data=DAT01)
ggforest(MODEL04, data = NULL, main = "Hazard ratio")

MODEL05 <- coxph(Surv(TIME,EVENT) ~  Health+EDU+REGION+AGEUN+FAM, data=DAT01)
ggforest(MODEL05, data = NULL, main = "Hazard ratio")

#interaction


MODEL06 <- coxph(Surv(TIME,EVENT) ~  Health+EDU+REGION+INT, data=DAT01)
ggforest(MODEL06, data = NULL, main = "Hazard ratio")

MODEL07 <- coxph(Surv(TIME,EVENT) ~  INT, data=DAT01)
ggforest(MODEL07, data = NULL, main = "Hazard ratio")



MODEL06 <- coxph(Surv(TIME,EVENT) ~  Health+EDU+REGION+AGEUN+FAM+KID, data=DAT01)
ggforest(MODEL06, data = NULL, main = "Hazard ratio")
