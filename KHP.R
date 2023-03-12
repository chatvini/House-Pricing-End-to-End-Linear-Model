library(Amelia)
library(psych)
library(corrplot)
library(Metrics)
library(car)
library(caret)
library(dplyr)
library(ggplot2)

setwd("/Users/macmojave/Downloads")
test <- read.csv("Kolkata_House_Price_Test (2).csv",na.strings = "")
train  <- read.csv("Kolkata_House_Price_Train_v2 (2).csv",na.strings = "")

#######################
#Test Data & Structure
######################
train1 <- train
test1 <- test

View(train1)
summary(train1)
train1$ID<- as.factor(train1$ID)
#train1$RIVER_FLG<-as.factor(train1$RIVER_FLG)

 #OUTLIERS
boxplot(train1$CRIM, col = 'yellow')
boxplot(train1$ZN, col = 'yellow')
boxplot(train1$INDUS, col = 'yellow')
boxplot(train1$RIVER_FLG, col = 'yellow')
summary(train1$RIVER_FLG)
boxplot(train1$nitric.oxides.concentration, col = 'yellow')
boxplot(train1$X.rooms.dwelling, col = 'yellow')
boxplot(train1$AGE, col = 'yellow')
boxplot(train1$DIS, col = 'yellow')
boxplot(train1$RAD, col = 'yellow')
# quantile(train1$RAD, seq(0.0,0.4,by=0.01))
# quantile(train1$RAD,probs = seq(0.60,1,by=0.01))
# train1$RAD <- ifelse(train1$RAD >6.5,mean(train1$RAD),train1$RAD)
# train1$RAD <- ifelse(train1$RAD <3
#                       ,mean(train1$RAD),train1$RAD)
x<-boxplot(train1$RAD)
list_out<- x$out
list_out
# #gives the positions in the data where outliers are present
# index<-which(train1$RAD %in% list_out)
# index
# #---Shortlist the outliers from the dataset and replace
# train1$RAD[index]
# 
boxplot(train1$TAX, col = 'yellow')
boxplot(train1$PTRATIO, col = 'yellow')
boxplot(train1$B, col = 'yellow')
y<-boxplot(train1$B)
list_out1<- y$out
list_out1
summary(train1$B)
IQR(train1$B)
375.53-(1.5*24.63168)
train1$B <- ifelse(train1$B < 338.5825
                     ,mean(train1$B),train1$B)

boxplot(train1$LSTAT, col = 'yellow')
summary(train1$LSTAT)
IQR(train1$LSTAT)
z<-boxplot(train1$LSTAT)
list_out2<- z$out
list_out2
13.245+(1.5*7.1359)
train1$LSTAT <- ifelse(train1$LSTAT > 23.94885
                   ,mean(train1$LSTAT),train1$LSTAT)

boxplot(train1$MEDV,col = 'yellow')
# summary(train1$MEDV)
# IQR(train1$MEDV)
# a<-boxplot(train1$MEDV)
# list_out4<- a$out
# list_out4
# train1$MEDV <- ifelse(train1$MEDV > 41.76575
#                        ,mean(train1$MEDV),train1$MEDV)


#CRIM
boxplot(train1$CRIM, col = 'yellow')
summary(train1$CRIM)
IQR(train1$CRIM)
LOW_outlier<- 0.05879-(1.5*0.3526831)
HIGH_outlier<- 0.41147+(1.5*0.3526831)

quantile(train1$CRIM,probs = seq(0.1,1,by=0.05))
quantile(train1$CRIM,probs = seq(0.85,1,by=0.01))

train1$CRIM <- ifelse(train1$CRIM >0.9404946
                      ,mean(train1$CRIM),train1$CRIM)


#TAX
boxplot(train1$TAX, col = 'yellow')
summary(train1$TAX)
IQR(train1$TAX)
LOW_outlier1<- 195.0-(1.5*94.25)
LOW_outlier1
HIGH_outlier1<- 446.0+(1.5*94.25)
HIGH_outlier1


#B
boxplot(train1$B, col = 'yellow')
summary(train1$B)
IQR(train1$B)
LOW_outlier2<- 69.38-(1.5*24.63168)
LOW_outlier2
HIGH_outlier2<- 416.75+(1.5*24.63168)
HIGH_outlier2
377.1-(1.5*23.10195)

a<-boxplot(train1$B)
list_out3<- a$out
list_out3
quantile(train1$B,probs = seq(0.1,1,by=0.05))
quantile(train1$B,probs = seq(0.00,0.10,by=0.01))

train1$B <- ifelse(train1$B <342.4471
                   ,mean(train1$B),train1$B)


#INDUS
boxplot(train1$INDUS, col = 'yellow')
summary(train1$INDUS)
IQR(train1$INDUS)
LOW_outlier3<- 0.4508-(1.5*6.243975)
LOW_outlier3
HIGH_outlier3<- 10.2636+(1.5*6.243975)
HIGH_outlier3

10.2636+(1.5*6.243975)
19.62956
quantile(train1$INDUS,probs = seq(0.1,1,by=0.05))
quantile(train1$INDUS,probs = seq(0.90,1,by=0.01))

train1$INDUS <- ifelse(train1$INDUS >19.62956
                       ,mean(train1$INDUS),train1$INDUS)

#ZN
boxplot(train1$ZN, col = 'yellow')
summary(train1$ZN)
IQR(train1$ZN)
22+(1.5*22)
head(train1$ZN)
#train1$ZN<-as.factor(train1$ZN)
View(train1)
train1$ZN <- ifelse(train1$ZN >55
                       ,mean(train1$ZN),train1$ZN)

#X.rooms.dwelling
boxplot(train1$X.rooms.dwelling, col = 'yellow')
quantile(train1$X.rooms.dwelling,probs = seq(0.80,1,by=0.01))
summary(train1$X.rooms.dwelling)
IQR(train1$X.rooms.dwelling)
6.747+(1.5* 0.8152025)
train1$X.rooms.dwelling <- ifelse(train1$X.rooms.dwelling >7.969804
                                  ,mean(train1$X.rooms.dwelling),train1$X.rooms.dwelling)


#Modelling
mod<- lm(train1$MEDV ~.-ID ,  data = train1)
summary(mod)

step(mod,data= train1, direction = "both")

mod1<-lm(formula = train1$MEDV ~ CRIM + INDUS + RIVER_FLG + nitric.oxides.concentration + 
     X.rooms.dwelling + DIS + PTRATIO + LSTAT, data = train1)
summary(mod1)

mod2<- lm(formula = (train1$MEDV) ~ TAX + INDUS + RIVER_FLG + nitric.oxides.concentration + 
            X.rooms.dwelling + DIS + PTRATIO + LSTAT, data = train1)
summary(mod2)

library(olsrr)
ols_step_all_possible(mod1)
k=ols_step_all_possible(mod1)
plot(k)

# cooksd <- cooks.distance(mod2)
# head (cooksd)

cooksd <- cooks.distance(mod1)
head (cooksd)


plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance") 
abline(h = 3*mean(cooksd, na.rm=T), col="red") 
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd,
                                                                 na.rm=T),names(cooksd),""), col="red")
influential <- as.numeric(names(cooksd)[(cooksd > 3*mean(cooksd, na.rm=T))]) 
head(train1[influential, ])

train_exinf <- train1[-influential, ]
nrow(train_exinf)

mod3<- lm(formula = train_exinf$MEDV ~ CRIM + INDUS + RIVER_FLG + nitric.oxides.concentration + 
            X.rooms.dwelling + DIS + PTRATIO + LSTAT, data = train_exinf)
summary(mod3)


mod4<- lm(formula = train_exinf$MEDV ~ INDUS + RIVER_FLG + nitric.oxides.concentration + 
            X.rooms.dwelling + DIS + PTRATIO + LSTAT, data = train_exinf)
summary(mod4)

mod5<- lm(formula = log(train_exinf$MEDV) ~ log(CRIM) + exp(nitric.oxides.concentration) + 
            (X.rooms.dwelling) + (AGE) + sqrt(DIS) + log(TAX) + (PTRATIO) + log(B) + sqrt(LSTAT), 
          data = train_exinf)
summary(mod5)

car::vif(mod5)

#residuals

qqPlot(mod4$residuals)

#heteroskedasticity

plot(mod4$fitted.values,mod4$residuals)

#PREDICTION
pred = predict(mod1,train1,type="response") # gives hat(p_i)
summary(pred)
pred


pred = predict(mod3,train_exinf,type="response") # gives hat(p_i)
summary(pred)
pred

RMSE((pred),train1$MEDV)
RMSE(exp(pred),train_exinf$MEDV)
rmse(log(pred),train_exinf$MEDV)
rmse(sqrt(pred),train_exinf$MEDV)
rmse(train_exinf$MEDV,pred)

colnames(test1)[11]<-"TAX"

View(test1)
#Prediction on Test
#test1$RIVER_FLG<-as.factor(test1$RIVER_FLG)
pred_t = predict(mod3,test1,type="response") # gives hat(p_i)
summary(pred_t)
pred_t
RMSE((pred_t),train_exinf$MEDV)
RMSE(exp(pred_t),train_exinf$MEDV)
rmse(log(pred_t),train_exinf$MEDV)
rmse(sqrt(pred_t),train_exinf$MEDV)
pred_exp = exp(pred_t)
pred_log = log(pred_t)
pred_sqrt = sqrt(pred_t)

head(pred_t)
b<-boxplot(pred_t)
out1<- b$out
out1

head(pred_exp)
c<-boxplot(pred_exp)
out2<- c$out
out2


head(pred_log)
d<-boxplot(pred_log)
out3<- d$out
out3

head(pred_sqrt)
e<-boxplot(pred_exp)
out4<- e$out
out4

###########
#Submission 
###########

ID<-test1$ID
test2<-test1
test2$pred_t<-pred_t
MEDV<- data.frame(test2$pred_t)
submissionnew_4 <- cbind(ID,MEDV)
colnames(submissionnew_4)<- c("ID","MEDV")
write.csv(submissionnew_4,"F:\\Kolkata House Price\\Submissions\\HousepriceNew_4.csv",row.names=FALSE)

# ID<-test1$ID
# test2<-test1
# test2$pred_t<-pred_t
# MEDV<- data.frame(test2$pred_t)
# submission_8 <- cbind(ID,MEDV)
# colnames(submission_8)<- c("ID","MEDV")
# write.csv(submission_8,"F:\\Kolkata House Price\\Submissions\\Houseprice4_1.csv",row.names=FALSE)


#crim
#indus
#zn
#nitric oxide
#rooms dwelling
#rad me upper
#B
#lstat