library(ggplot2)
library(lattice)
library(corrplot)
library(tidyverse)
library(Epi)
library(MASS)
library(class)
library(factoextra)
library(ggplot2)


load("CHD.RData")


set.seed(202271442) 

z.omit<- sample(1:nrow(CHD), 10, replace = TRUE)

newCHD<- CHD[-z.omit,]

#Question 1 
#1
head(newCHD)
tail(newCHD)
dim(newCHD)
names(newCHD)


class(newCHD)
str(newCHD)

#2
#Visualisations 

summary(newCHD)
table(newCHD$CHD)

barchart(table(newCHD$CHD),horizontal = FALSE, xlab = "Coronary Heart Disease", col = "red")

par(mfrow = c(2,2))
boxplot(sbp~CHD, data= newCHD,main = "Systolic Blood Pressure") #no
boxplot(log(tobacco+1)~CHD, data= newCHD,main = "Log Tobacco + 1") #some association
boxplot(log(ldl)~CHD, data= newCHD, main ="log Low-Density Lipoprotein Cholestrol")#some
boxplot(adiposity~CHD, data= newCHD,  main= "Percentage of Body Fat")#some
boxplot(typea~CHD, data= newCHD, main = "Type-A Behaviour Pattern Score")# no 
boxplot(obesity~CHD, data= newCHD, main = "Body Mass Index")#no
boxplot(log(alcohol+1)~CHD, data= newCHD, main = "Log Current Alcohol Consumption + 1")#no 
boxplot(age~CHD, data= newCHD,  main = "Age")# yes



hist(newCHD$tobacco, main = "Tobacco", xlab = "Yearly use of tobbao (kg)") # Exponential dist
hist(newCHD$sbp, main = "Systolic Blood Pressure", xlab = "Systolic blood pressure (mmHg)") #skewed
hist(newCHD$ldl, main = "Low-Density Lipoprotein Cholestrol", xlab = "Low-density lipoprotein cholestrol (mmol/L)") #Skewed
hist(newCHD$adiposity, main= "Percentage of Body Fat", xlab = "Percentage of body (%)") #normal 
hist(newCHD$typea, main = "Type-A Behaviour Pattern Score", xlab = "Type-A behaviour pattern score") # skewd
hist(newCHD$obesity, main = "Body Mass Index", xlab= "Body mass index") #normal
hist(newCHD$alcohol, main = "Current Alcohol Consumption", xlab = "Current alcohol consumption (L)") #Exponential
hist(log(newCHD$alcohol+1), main = "Log of Current Alcohol Consumption + 1", xlab = "Log ofcurrent alcohol consumption (L) +1")
hist(newCHD$age, main = "Age", xlab = "Age (years)") #Uniform
hist(newCHD$age)

par(mfrow=c(2,2))
hist(log(newCHD$tobacco+1),main = "Log of Tobacco + 1", xlab = "Log of yearly use of tobbao (kg) +1" )
hist(log(newCHD$ldl), main  = "Log of Low-Density Lipoprotein Cholestrol",xlab = "Log Low-density lipoprotein cholestrol (mmol/L)")
hist(log(newCHD$alcohol+1), main = "Log of Current Alcohol Consumption + 1", xlab = "Log ofcurrent alcohol consumption (L) +1")

newCHD$Logtobacco <- log(newCHD$tobacco+1)
newCHD$Logldl <- log(newCHD$ldl)
newCHD$Logalcohol <- log(newCHD$alcohol+1)

#removal of tobbaco, ldl , alchochol, ind
newCHD %>% 
  dplyr::select(sbp, adiposity,typea,obesity,age,Logtobacco, Logldl, Logalcohol,CHD) -> CHDlogs



#3
par(mfrow = c(1,1))
#correlation plot
corrplot(cor(CHDlogs[,-9]), method = "color", type = "upper")
#correlation and covariance matrixs
cor(CHDlogs[,-9])
var(CHDlogs[,-9])
pairs(CHDlogs)


#PART 2

#1

set.seed(21)

row_sel <- rbinom(nrow(CHDlogs),1,0.33)

CHDtrain <- subset(CHDlogs, row_sel== 0)
CHDtest <- subset(CHDlogs, row_sel==1)

dim(CHDtrain)
dim(CHDtest)

#LOGISTIC
#FULL Model
CHDlogsFull <- glm(CHD~sbp+adiposity+typea+obesity+age
                +Logtobacco+Logldl+Logalcohol, 
                data = CHDtrain, family = binomial)

summary(CHDlogsFull)
#SELECTION

drop1(CHDlogsFull, test = "Chi")

#DROP LogAlcohol

CHDlogs1 <- glm(CHD~sbp+adiposity+typea+obesity+age
                +Logtobacco+Logldl, 
                data = CHDtrain, family = binomial)

drop1(CHDlogs1, test = "Chi")

#DROP adiposity

CHDlogs1 <- glm(CHD~sbp+typea+obesity+age
                +Logtobacco+Logldl, 
                data = CHDtrain, family = binomial)

drop1(CHDlogs1, test = "Chi")

CHDlogs1 <- glm(CHD~sbp+typea+age
                +Logtobacco+Logldl, 
                data = CHDtrain, family = binomial)

drop1(CHDlogs1, test = "Chi")

#Drop Sbp

CHDlogs1 <- glm(CHD~typea+age +Logtobacco+Logldl, 
                data = CHDtrain, family = binomial)

drop1(CHDlogs1, test = "Chi")

#Drop log tobacco

CHDlogs1 <- glm(CHD~typea+age +Logldl, 
                data = CHDtrain, family = binomial)

drop1(CHDlogs1, test = "Chi")

#SELECTION DONE


#MODEL ASSESSMENT
CHDFinal <- glm(CHD~typea+age +Logldl,data = CHDtrain, family = binomial)

#DEVIANCE = 311.19
#AIC = 319

summary(CHDFinal)

#CHD = -7.39580 + 0.04047*typea + 0.05607*age + 1.25030*logldl

#log odds and conficdence interval
cbind(CHDFinal$coefficients, confint.default(CHDFinal))

#odds ratio and confidence interval
exp(cbind(CHDFinal$coefficients, confint.default(CHDFinal)))

#deviance Test
anova(CHDFinal, CHDlogsFull, test = "Chi")
#Shows there is a significant change between first model and final model 



#Predict

CHDFinal.pred <- predict(CHDFinal, newdata = CHDtest)
CHDFinal.pred.train <- predict(CHDFinal, newdata = CHDtrain)


ROC(CHDFinal.pred,CHDtest$CHD,plot = "ROC")
table(CHDFinal.pred> -0.809, CHDtest$CHD)
#SENS = 75.4
#Spec = 68.0
#Corr =70.
ROC(CHDFinal.pred.train,CHDtrain$CHD,plot = "ROC")
table(CHDFinal.pred.train> -0.967, CHDtrain$CHD)
#SENS = 85.1
#Spec = 58.5
#corr = 66.7

#Binary classification
par(mfrow= c(1,1))
boxplot(CHDFinal.pred.train~CHDtrain$CHD, xlab= "Cononary Heart Disease Status", ylab = "linear predictor")
densityplot(~CHDFinal.pred.train|CHDtrain$CHD, layout= c(1,2), xlab= "linear predictor")

#cutoff= -0.7
cutoff <- -0.7

table(CHDFinal.pred.train> cutoff, CHDtrain$CHD)
#SENS = 0.691
#Spec = 0.675
#Correct = 0.68

#LDA
CHDFinalLDA <- lda(CHD~typea+age +Logldl,data = CHDtrain)
CHDFinalLDA

CHDFinal_pred_lda <- predict(CHDFinalLDA)$class

table(CHDFinal_pred_lda, CHDtrain$CHD)
#SENS = 0.351
#Spec = 0.885
#Correct = 0.714


#QDA

CHDFinalQDA <- qda(CHD~typea+age +Logldl,data = CHDtrain)
CHDFinalQDA

CHDFinal_pred_qda <- predict(CHDFinalQDA)$class

table(CHDFinal_pred_qda, CHDtrain$CHD)
#SENS = 0.414
#Spec = 0.84
#Correct = 0.704


##CV

#LDACV
CHDFinalLDA_CV <- lda(CHD~typea+age +Logldl,data = CHDtrain, CV=TRUE)
CHDFinalLDA_CV


table(CHDFinalLDA_CV$class, CHDtrain$CHD)
#SENS = 0.351
#Spec = 0.88
#Correct = 0.71


#QDACV

CHDFinalQDA_CV <- qda(CHD~typea+age +Logldl,data = CHDtrain, CV=TRUE)
CHDFinalQDA_CV


table(CHDFinalQDA_CV$class, CHDtrain$CHD)
#SENS = 0.414
#Spec = 0.83
#Correct = 0.697

#KNN

c_CHD <- CHDlogs[ ,c("typea", "age", "Logldl")]

summary(c_CHD)
var(c_CHD)


#needs to be scaled

c_CHD <- scale(c_CHD)

c_sel <- sample(1:nrow(c_CHD), 294, replace = FALSE)
#o train 
#1 test

c_train <- c_CHD[c_sel,]
c_test <- c_CHD[-c_sel,]

dim(c_train)
dim(c_test)

c_num <- numeric(30)

for(i in 1:30){
  c_cv <- knn.cv(c_train, cl=CHDlogs$CHD[c_sel], k=i)
  c_num[i] <- 100*(1-sum(c_cv==CHDlogs$CHD[c_sel])/nrow(c_train))
}
c_num
plot(1:30, c_num, type = "l", xlab = "k", ylab = "% Missclassified")

#k= 11

c <- knn(c_train,c_test, cl=CHDlogs$CHD[c_sel], k= 11)
c_tab <- table(c, CHDlogs$CHD[-c_sel])
c_tab

#SENS = 0.397
#Spec = 0.82
#Correct = 0.665


c_cv <- knn.cv(c_train, cl=CHDlogs$CHD[c_sel], k= 11)
c_tab_cv <- table(c_cv, CHDlogs$CHD[c_sel])
c_tab_cv

#SENS = 0.433
#Spec = 0.827
#Correct = 0.697  


#PART3 

#original Data without CHD and ind

withoutCHD <- newCHD[, 2:9]


corrplot(cor(withoutCHD),method = "color",  type="upper")
# corrlation around 0.2-0.6

var(withoutCHD)
#different units and scales use, and differnct sizes


#PCA of data 
CHDpca<- prcomp(withoutCHD, scale= TRUE)


#Cumalative proportion 
summary(CHDpca)

#Graphs scree plots
fviz_eig(CHDpca, addlabels = TRUE)
plot(CHDpca)
mtext(side = 1, "All Staff Salary Principal Component", line = 1 , font = 1)


# 4 components


CHDpca

#PC1: Average type and alcohol disregarded
#PC2 Contrast of sbp,tobacco, alcohol, age vs ldl type a, obesity
#PC3: Average type a and alchol
#PC4 : contrast of sbp obesity alcohol vs age type a ldl tobacco

CHDpc <- predict(CHDpca)

head(round(CHDpc,2))
head(withoutCHD)

par(mfrow= c(1,1))
# PC1 v PC2
plot(CHDpc[,1:2], type= "n")
points(x= CHDpc[,1], y=CHDpc[,2], col=ifelse(newCHD$CHD=="1","red","blue"), pch = 16)
legend("topright", legend = c("1","0"),col =c("red","blue"), pch= 16, cex = 0.65)
title("Coronary Heart Disease Colour Coded")

#PC1 vs PC3
plot(CHDpc[,1:3], type= "n", ylab = "PC3")
points(x= CHDpc[,1], y=CHDpc[,3], col=ifelse(newCHD$CHD=="1","red","blue"), pch = 16)
legend("topright", legend = c("1","0"),col =c("red","blue"), pch= 16, cex = 0.65)
title("Coronary Heart Disease Colour Coded")

#PC1 vs PC4
plot(CHDpc[,1:4], type= "n", ylab = "PC4")
points(x= CHDpc[,1], y=CHDpc[,4], col=ifelse(newCHD$CHD=="1","red","blue"), pch = 16)
legend("topright", legend = c("1","0"),col =c("red","blue"), pch= 16, cex = 0.65)
title("Coronary Heart Disease Colour Coded")

#PC2 vs PC3
plot(CHDpc[,2:3], type= "n")
points(x= CHDpc[,2], y=CHDpc[,3], col=ifelse(newCHD$CHD=="1","red","blue"), pch = 16)
legend("topright", legend = c("1","0"),col =c("red","blue"), pch= 16, cex = 0.65)
title("Coronary Heart Disease Colour Coded")

#PC2 vs PC4
plot(CHDpc[,2:4], type= "n", ylab = "PC4")
points(x= CHDpc[,2], y=CHDpc[,4], col=ifelse(newCHD$CHD=="1","red","blue"), pch = 16)
legend("topright", legend = c("1","0"),col =c("red","blue"), pch= 16, cex = 0.65)
title("Coronary Heart Disease Colour Coded")


#PC3 vs PC4
plot(CHDpc[,3:4], type= "n")
points(x= CHDpc[,3], y=CHDpc[,4], col=ifelse(newCHD$CHD=="1","red","blue"), pch = 16)
legend("topright", legend = c("1","0"),col =c("red","blue"), pch= 16, cex = 0.65)
title("Coronary Heart Disease Colour Coded")


#biplots

fviz_pca_biplot(CHDpca, col.var = "red")








