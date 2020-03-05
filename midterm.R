############################################
# CS440 Midterm Project: Tuan Anh Vu, Tung Truong
##########################################
library(lmtest)
library(caret)
library(Metrics)
library(leaps)

setwd("/Users/BenMeow/Desktop/Midterm/ai-midterm")
suidat <- read.csv("/Users/BenMeow/Desktop/Midterm/ai-midterm/SuiRate.csv")
head(suidat)
#Suicide Rate Data + Variables:
#https://drive.google.com/open?id=1JFalYrbit84vgHNzYlsCnm0j2AOKyz0D#

# Split data randomly into training set and test set, each with 75% and 25% of the data respectively
set.seed(3456)
trainData <- createDataPartition(suidat$SuiRate, p=.75,list=FALSE,times=1)

suiTrain <- suidat[trainData,]
suiTest <- suidat[-trainData,]
head(suiTrain)
head(suiTest)

###Linear Response
plot(suiTrain$Unemp,suiTrain$SuiRate)
plot(suiTrain$Vet,suiTrain$SuiRate)
plot(suiTrain$Arrest,suiTrain$SuiRate)
plot(suiTrain$Mental,suiTrain$SuiRate)
plot(suiTrain$Disable,suiTrain$SuiRate)
plot(suiTrain$Uninsured,suiTrain$SuiRate)
plot(suiTrain$Poverty,suiTrain$SuiRate)


############################################################
# Best-subset Method
############################################################

best_subsets <- regsubsets(SuiRate~Unemp + Vet + Arrest + Mental + Disable + Uninsured + Poverty, nbest = 1, nvmax= 7, data=suiTrain,method="exhaustive")
summary_best<- summary(best_subsets)
which.max(summary_best$adjr2)

bestsubset_model <- lm(SuiRate~Unemp + Vet + Arrest + Mental + Disable + Uninsured, data = suiTrain)


############################################################
# Cross - Validated Error
############################################################

lmFit<-train(SuiRate~Unemp + Vet + Arrest + Mental + Disable + Uninsured + Poverty, data = suidat, method = "lm")

ctrl<-trainControl(method = "cv",number = 10)
lmCVFit<-train(SuiRate~Unemp + Vet + Arrest + Mental + Disable + Uninsured + Poverty, data = suidat, method = "lm", trControl = ctrl, metric="RMSE")
summary(lmCVFit)

bestkfold_model <- lm(SuiRate~Unemp + Vet + Arrest + Mental + Disable + Uninsured, data = suidat)
summary(bestkfold_model)



############################################################
# Interaction Term Analysis
############################################################
best_uxa <- lm(SuiRate~Unemp + Vet + Arrest + Mental + Disable + Uninsured + uxa, data = suiTrain)
summary(best_uxa)
best_uxm <- lm(SuiRate~Unemp + Vet + Arrest + Mental + Disable + Uninsured + uxm, data = suiTrain)
summary(best_uxm)
best_uxd <- lm(SuiRate~Unemp + Vet + Arrest + Mental + Disable + Uninsured + uxd, data = suiTrain)
summary(best_uxd)
best_uxv <- lm(SuiRate~Unemp + Vet + Arrest + Mental + Disable + Uninsured + uxv, data = suiTrain)
summary(best_uxv)
best_uxu <- lm(SuiRate~Unemp + Vet + Arrest + Mental + Disable + Uninsured + uxu, data = suiTrain)
summary(best_uxu)
best_vxa <- lm(SuiRate~Unemp + Vet + Arrest + Mental + Disable + Uninsured + vxa, data = suiTrain)
summary(best_vxa)
best_vxm <- lm(SuiRate~Unemp + Vet + Arrest + Mental + Disable + Uninsured + vxm, data = suiTrain)
summary(best_vxm)
best_vxd <- lm(SuiRate~Unemp + Vet + Arrest + Mental + Disable + Uninsured + vxd, data = suiTrain)
summary(best_vxd)
best_vxu <- lm(SuiRate~Unemp + Vet + Arrest + Mental + Disable + Uninsured + vxu, data = suiTrain)
summary(best_vxu)
best_axm <- lm(SuiRate~Unemp + Vet + Arrest + Mental + Disable + Uninsured + axm, data = suiTrain)
summary(best_axm)
best_axd <- lm(SuiRate~Unemp + Vet + Arrest + Mental + Disable + Uninsured + axd, data = suiTrain)
summary(best_axd)
best_axu <- lm(SuiRate~Unemp + Vet + Arrest + Mental + Disable + Uninsured + axu, data = suiTrain)
summary(best_axu)
best_mxd <- lm(SuiRate~Unemp + Vet + Arrest + Mental + Disable + Uninsured + mxd, data = suiTrain)
summary(best_mxd)
best_mxu <- lm(SuiRate~Unemp + Vet + Arrest + Mental + Disable + Uninsured + mxu, data = suiTrain)
summary(best_mxu)
best_dxu <- lm(SuiRate~Unemp + Vet + Arrest + Mental + Disable + Uninsured + dxu, data = suiTrain)
summary(dxu)
# The interaction term between the disable and uninsured variable increased the adjusted R square while keeping all variables statistically significant

best_model <- lm(SuiRate~Unemp + Vet + Arrest + Mental + Disable + Uninsured + dxu, data = suiTrain)
summary(best_model)

#Error term analysis
####################
error_terms <- best_model$residuals # from the fit
#All the error terms
suiTrain$SuiRate - predict(best_model, suiTrain) 

### Errors independent and and variance about the same
plot(error_terms)
lmtest::bptest(best_model)

###Approximately Normal?
hist(error_terms)
qqnorm(error_terms)
qqline(error_terms)

###############
# Prediction in R
###########
predict(best_model, suiTest)
suiTest$SuiRate - predict(best_model, suiTest)

# Calculate mean and standard deviation for test and training residuals
mean(suiTest$SuiRate - predict(best_model, suiTest))
sd(suiTest$SuiRate - predict(best_model, suiTest))

mean(suiTrain$SuiRate - predict(best_model, suiTrain))
sd(suiTrain$SuiRate - predict(best_model, suiTrain))

rmse(suiTest$SuiRate, predict(best_model, suiTest))
sse(suiTest$SuiRate, predict(best_model, suiTest))
mae(suiTest$SuiRate, predict(best_model, suiTest))

# Determine observation range between training and test set
min(suiTrain$SuiRate)
max(suiTrain$SuiRate)

min(suiTest$SuiRate)
max(suiTest$SuiRate)

