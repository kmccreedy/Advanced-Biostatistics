################################################################
##### CTS 575. R code for Lab 10. Survival I             2021 ##
################################################################


library(survival)




############## KM. Simple########################
#simple example
id<-1:12
time<-c(1,2,2,2,3,5,6,7,8,16,17,34)
status<-c(1,1,1,0,1,1,1,0,1,0,1,0)
nhl<-data.frame(cbind(id, time, status))
nhl


km_nhl<-survfit(Surv(time, status)~ 1, data=nhl)

summary(km_nhl)
km_nhl

plot(km_nhl)
plot(km_nhl, mark.time=T)

#install.packages("survminer")
library(survminer)
ggsurvplot(km_nhl, risk.table=T)


############## DIG DATA ########################
setwd("C:\\Users\\arodday\\Dropbox\\Integrated Epi Biostat\\04 Labs") #change to your working directory

dig<-read.csv("dig_surv.csv", header=T)


dim(dig)
names(dig)
head(dig)
tail(dig)
summary(dig)




######### KM with DIG data ##########

#2 groups for KM
table(dig$TRTMT)
table(trt=dig$TRTMT, death=dig$DEATH)
prop.table(table(trt=dig$TRTMT, death=dig$DEATH),1)

km1<-survfit(Surv(DEATHDAY, DEATH)~ TRTMT, data=dig)
km1
summary(km1)

plot(km1)

plot(km1, main="Survival by Trt Group", col=c("blue", "red"), xlab="Days")
legend(x=100,y=.6,c("placebo","digoxin"), col=c("blue","red"),lty=13)


ggsurvplot(km1, risk.table=T)
?ggsurvplot
ggsurvplot(km1, risk.table=T, censor=F)

survdiff(Surv(DEATHDAY, DEATH)~ TRTMT, data=dig)

survdiff(Surv(DEATHDAY, DEATH)~ TRTMT, data=dig, rho=1)


## create dig by sex variable
table(male=dig$MALE,trt=dig$TRTMT)

dig$male_dig[dig$MALE==1 & dig$TRTMT==1]<-1 #male and dig
dig$male_dig[dig$MALE==1 & dig$TRTMT==0]<-2 #male and nodig
dig$male_dig[dig$MALE==0 & dig$TRTMT==1]<-3 #female and dig
dig$male_dig[dig$MALE==0 & dig$TRTMT==0]<-4 #female and nodig

table(dig$male_dig)

km2<-survfit(Surv(DEATHDAY, DEATH)~ male_dig, data=dig)
km2
summary(km2)

plot(km2, main="Survival by Sex*Trt Group", col=c("blue", "red","green","black"), xlab="Days")
legend(x=100,y=.4,c("maledig","malenodig","femaledig","femalenodig"), col=c("blue","red","green","black"),lty=1)

survdiff(Surv(DEATHDAY, DEATH)~ male_dig, data=dig)




########### COX PH model ##############
cox1<-coxph(Surv(DEATHDAY, DEATH)~ TRTMT, data=dig)
summary(cox1)

cox1a<-coxph(Surv(DEATHDAY, DEATH)~ I(1-TRTMT), data=dig)
summary(cox1a)

########### multivariable COX PH model ##############
cox2<-coxph(Surv(DEATHDAY, DEATH)~ TRTMT+AGE+MALE+EJF_PER, data=dig)
summary(cox2)


exp(-0.0349986*10)
exp(10*cox2$coef["EJF_PER"])

coef["EJF_PER"]
cox2$coef[4]


## end of lab material





































###############   exercises ######################



#death or hospitalization for HF
km4<-survfit(Surv(DWHFDAYS, DWHF )~ TRTMT, data=dig)
km4

plot(km4, main="Death or Hosp for HF by Trt Group", col=c("blue", "red"), xlab="Days")
legend(x=100,y=.4,c("placebo","digoxim"), col=c("blue","red"),lty=13)

survdiff(Surv(DWHFDAYS, DWHF )~ TRTMT, data=dig)

cox4<-coxph(Surv(DWHFDAYS, DWHF)~ TRTMT, data=dig)
summary(cox4)


#hospitalization
km5<-survfit(Surv(HOSPDAYS,HOSP)~ TRTMT, data=dig)
km5

plot(km5, main="Hosp by Trt Group", col=c("blue", "red"), xlab="Days")
legend(x=100,y=.4,c("placebo","digoxim"), col=c("blue","red"),lty=13)

survdiff(Surv(HOSPDAYS,HOSP)~ TRTMT, data=dig)

cox5<-coxph(Surv(HOSPDAYS,HOSP)~ TRTMT, data=dig)
summary(cox5)





##########  ggsurvplot ########
?ggsurvplot
ggsurvplot(km5, risk.table=T, censor=F)
ggsurvplot(km5, risk.table=T, censor=F, pval=T, surv.median.line="h")
ggsurvplot(km5, risk.table=T, censor=F, pval=T, fun="cumhaz")
ggsurvplot(km5, risk.table=T, censor=F, pval=T, legend.labs=c("Placebo","Digoxin"),
           legend.title="Treatment", legend=c(.8,.8), 
           title="Hospitalization by Treatment", xlab="Days")
