library(dplyr)
library(MASS)
dat<-read.csv("C:/Users/jagad/Desktop/heat and card_anomal/01for analysis/work_ana_v1.csv",
         header=T, fileEncoding="UTF-8-BOM")
dat$yr_cat<- as.factor(dat$Year)
dat$Name<- as.factor(dat$Name)
dat$Group<- as.factor(dat$Group)
dat$LB<- as.numeric(dat$LB)
summary(dat)

central.dat<- filter(dat, Name == "SOUTHWEST" & Group == "Critical")

m1<- glm.nb(BD~frq_30C_90F+offset(log(LB)),data=central.dat)

(est <- cbind(Estimate = coef(m1), confint(m1)))
exp(est)

