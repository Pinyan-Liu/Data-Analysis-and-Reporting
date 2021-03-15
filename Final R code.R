library(uwIntroStats)
library(geepack)

#merge data1 with data2
dat<-merge(data1,data2,by="id")

#change missing with other indicator
dat$oc2<-gsub("\\.","2",dat$oc)
dat$hrt2<-gsub("\\.","2",dat$hrt)

#question 1
mod1_rec<-regress("odds",as.factor(recvte)~as.factor(sex)+age+bmi+as.factor(oc2)+as.factor(hrt2),data=dat)
mod1_rec

mod1_death<-regress("odds",as.factor(death)~as.factor(sex)+age+bmi+as.factor(hrt2)+as.factor(oc2),data=dat)
mod1_death

#question 2
dat$pstmp<-gsub("\\.","3",dat$pstmp)
gee.fit1 <- geeglm(recvte ~ sex + pstmp,
                   data = dat, id = id, 
                   family=binomial(link="logit"), corstr = "independence")
summary(gee.fit1)
confint.default(gee.fit1)

mod2_rec<-regress("odds",as.factor(recvte)~as.factor(sex)+as.factor(pstmp),data=dat)
mod2_rec

mod2_death<-regress("odds",as.factor(death)~as.factor(sex)+as.factor(pstmp),data=dat)
mod2_death

#question 3
#split data
require(caTools)
install.packages("caTools")
library(caTools)
set.seed(101)
sample = sample.split(dat, SplitRatio = .75)
train = subset(dat, sample == TRUE)
test  = subset(dat, sample == FALSE)

#prediction
mod3<-glm(recvte~vte_type+age+sex+race+pstmp+bmi+act+smoker+priorcvd+hrt+oc,
          family=binomial(link="logit"),data=train)
summary(mod3)
mod3_1<-glm(as.factor(recvte)~as.factor(vte_type)+age+as.factor(sex)+
              as.factor(pstmp)+as.factor(priorcvd)
              +as.factor(hrt2)+as.factor(oc2),
          family=binomial(link="logit"),data=train)
summary(mod3_1)

test$predict_1<-plogis(predict(mod3_1,test,type = "response"))
#roc(test$predict,test$recvte,plot=TRUE,
    #main="ROC CURVE",xlab="1-Specificity",col="blue")
#roc curve
library(pROC)
roc(recvte~test$predict_1,data=test,plot=TRUE,
    main="ROC CURVE",xlab="1-Specificity",col="blue",print.auc=TRUE)

#death
mod3_2<-glm(as.factor(death)~as.factor(vte_type)+age+as.factor(sex)+
              as.factor(pstmp)+as.factor(priorcvd)+bmi
            +as.factor(hrt2)+as.factor(oc2)+as.factor(smoker),
            family=binomial(link="logit"),data=train)
summary(mod3_2)

test$predict_2<-plogis(predict(mod3_2,test,type = "response"))
#roc curve
par(mfrow=c(1,2))
roc(death~test$predict_2,data=test,plot=TRUE,
    main="ROC CURVE of death",xlab="1-Specificity",col="blue",print.auc=TRUE)
roc(recvte~test$predict_1,data=test,plot=TRUE,
    main="ROC CURVE of recurrence",xlab="1-Specificity",col="blue",print.auc=TRUE)

#question 4
dat2<-merge(dat,data3,by="id")
#recurrence
mod4_1<-glm(as.factor(recvte)~as.factor(vte_type)+age+as.factor(sex)+
              as.factor(pstmp)+as.factor(priorcvd)
            +as.factor(hrt2)+as.factor(oc2),
            family=binomial(link="logit"),data=train)
data3$predict_1<-plogis(predict(mod4_1,dat2,type = "response"))
#death
mod4_2<-glm(as.factor(death)~as.factor(vte_type)+age+as.factor(sex)+
              as.factor(pstmp)+as.factor(priorcvd)
            +as.factor(hrt2)+as.factor(oc2),
            family=binomial(link="logit"),data=train)
data3$predict_2<-plogis(predict(mod4_2,dat2,type = "response"))


roc(data3$recvte2~data3$predict_1,plot=TRUE,
    main="ROC CURVE of recurrence",xlab="1-Specificity",col="blue",print.auc=TRUE)
roc(data3$death2~data3$predict_2,plot=TRUE,
    main="ROC CURVE of death",xlab="1-Specificity",col="blue",print.auc=TRUE)







