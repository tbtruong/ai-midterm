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

# All the models - To determine best model 
#####################
# Varaibles: SuiRate, Unemp, Vet, Arrest, Mental, Disable
# 0 variable
SuiRate0 <- lm(SuiRate~SuiRate, data = suiTrain)
# 1 variable
Unemp1 <- lm(SuiRate~Unemp, data = suiTrain)
Vet1 <- lm(SuiRate~Vet, data = suiTrain)
Arrest1 <- lm(SuiRate~Arrest, data = suiTrain)
Mental1 <- lm(SuiRate~Mental, data = suiTrain)
Disable1 <- lm(SuiRate~Disable, data = suiTrain)
# 2 variables
Unemp_Vet <- lm(SuiRate~Unemp + Vet, data = suiTrain)
Unemp_Arrest <- lm(SuiRate~Unemp + Arrest, data = suiTrain)
Unemp_Mental <- lm(SuiRate~Unemp + Mental, data = suiTrain)
Unemp_Disable <- lm(SuiRate~Unemp + Disable, data = suiTrain)
Vet_Arrest <- lm(SuiRate~Vet + Arrest, data = suiTrain)
Vet_Mental <- lm(SuiRate~Vet + Mental, data = suiTrain)
Vet_Disable <- lm(SuiRate~Vet + Disable, data = suiTrain)
Arrest_Mental <- lm(SuiRate~Arrest + Mental, data = suiTrain)
Arrest_Disable <- lm(SuiRate~Arrest + Disable, data = suiTrain)
Mental_Disable <- lm(SuiRate~Mental + Disable, data = suiTrain)
# 3 variables
Unemp_Arrest_Vet <- lm(SuiRate~Unemp + Vet + Arrest, data = suiTrain)
Unemp_Vet_Mental <- lm(SuiRate~Unemp + Vet + Mental, data = suiTrain)
Unemp_Vet_Disable <- lm(SuiRate~Unemp + Vet + Disable, data = suiTrain)
Unemp_Arrest_Mental <- lm(SuiRate~Unemp + Arrest + Mental, data = suiTrain)
Unemp_Arrest_Disable <- lm(SuiRate~Unemp + Arrest + Disable, data = suiTrain)
Unemp_Mental_Disable <- lm(SuiRate~Unemp + Mental + Disable, data = suiTrain)
Vet_Arrest_Mental <- lm(SuiRate~Vet + Arrest + Mental, data = suiTrain)
Vet_Arrest_Disable <- lm(SuiRate~Vet + Arrest + Disable, data = suiTrain)
Vet_Mental_Disable <- lm(SuiRate~Vet + Mental + Disable, data = suiTrain)
Arrest_Mental_Disable <- lm(SuiRate~Arrest + Mental + Disable, data = suiTrain)
# 4 variables
Unemp_Arrest_Vet_Mental <- lm(SuiRate~Unemp + Vet + Arrest + Mental, data = suiTrain)
Unemp_Vet_Disable_Arrest <- lm(SuiRate~Unemp + Vet + Arrest + Disable, data = suiTrain)
Unemp_Vet_Disable_Mental <- lm(SuiRate~Unemp + Vet + Mental + Disable, data = suiTrain)
Unemp_Arrest_Mental_Disable <- lm(SuiRate~Unemp + Arrest + Mental + Disable, data = suiTrain)
Vet_Arrest_Disable_Mental <- lm(SuiRate~Vet + Arrest + Mental + Disable, data = suiTrain)
# 5 variables 
Unemp_Arrest_Mental_Disable_Vet <- lm(SuiRate~Unemp + Vet + Arrest + Mental + Disable, data = suiTrain)

# Very big loop
models <- list(SuiRate0,Unemp1,Vet1,Arrest1,Mental1,Disable1,Unemp_Vet,Unemp_Arrest,Unemp_Mental,Unemp_Disable,Vet_Arrest,Vet_Mental,
Vet_Disable,Arrest_Mental,Arrest_Disable,Mental_Disable,Unemp_Arrest_Vet,Unemp_Vet_Mental,Unemp_Vet_Disable,Unemp_Arrest_Mental,
Unemp_Arrest_Disable,Unemp_Mental_Disable ,Vet_Arrest_Mental,Vet_Arrest_Disable,Arrest_Mental_Disable,
Unemp_Arrest_Vet_Mental,Unemp_Vet_Disable_Arrest,Unemp_Vet_Disable_Mental,Unemp_Arrest_Mental_Disable,Vet_Arrest_Disable_Mental,
Unemp_Arrest_Mental_Disable_Vet)

modelname <- list("SuiRate0","Unemp1","Vet1","Arrest1","Mental1","Disable1","Unemp_Vet","Unemp_Arrest","Unemp_Mental","Unemp_Disable","Vet_Arrest","Vet_Mental",
"Vet_Disable","Arrest_Mental","Arrest_Disable","Mental_Disable","Unemp_Arrest_Vet","Unemp_Vet_Mental","Unemp_Vet_Disable","Unemp_Arrest_Mental",
"Unemp_Arrest_Disable","Unemp_Mental_Disable" ,"Vet_Arrest_Mental","Vet_Arrest_Disable","Arrest_Mental_Disable",
"Unemp_Arrest_Vet_Mental","Unemp_Vet_Disable_Arrest","Unemp_Vet_Disable_Mental","Unemp_Arrest_Mental_Disable","Vet_Arrest_Disable_Mental",
"Unemp_Arrest_Mental_Disable_Vet")

# initialize empty list to compare models
rank <- list()

for (i in 1:31) {
  # R outputs to .txt file instead of consolde
  #sink(paste(modelname[i],".txt",sep="")) 
  #print(lapply(models[i],summary))
  summ <- lapply(models[i],summary)
  rsquared <- summ[[1]][9]
  #rank <- c(rank,list(rsquared,modelname[i]))
  rank[[i]] <- c(rsquared,modelname[i])
  # Revert R outputs to console
  #sink()
}

# Arrange list of lists by first element of sublist (adjusted r-squared)
rank <- rank[order(sapply(rank,'[[',1))]
# Print sublist with highest adjusted R squared value
print(rank[[31]])
best_model <- Unemp_Arrest_Mental_Disable_Vet

# Quicker way to do best subset - lecture 6 code:
best_subsets <- regsubsets(SuiRate~Unemp + Vet + Arrest + Mental + Disable, nbest = 1, nvmax= 5, data=suiTrain,method="exhaustive")
summary_best<- summary(best_subsets)
which.max(summary_best$adjr2)



#Error term analysis
####################
error_terms <- best_model$residuals # from the fit
#All the error terms
suiTrain$SuiRate - predict(best_model, suiTrain) 

###Linear Response
plot(suiTrain$Unemp,suiTrain$SuiRate)
lines(suiTrain$Unemp, fitted(Unemp1), col="blue")
plot(suiTrain$Vet,suiTrain$SuiRate)
lines(suiTrain$Vet, fitted(Vet1), col="red")
plot(suiTrain$Arrest,suiTrain$SuiRate)
lines(suiTrain$Arrest, fitted(Arrest1), col="green")
plot(suiTrain$Mental,suiTrain$SuiRate)
lines(suiTrain$Mental, fitted(Mental1), col="yellow")
plot(suiTrain$Disable,suiTrain$SuiRate)

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

############################################################
# Cross - Validated Error
############################################################

lmFit<-train(SuiRate~Unemp + Vet + Arrest + Mental + Disable, data = suidat, method = "lm")

ctrl<-trainControl(method = "cv",number = 10)
lmCVFit<-train(SuiRate~Unemp + Vet + Arrest + Mental + Disable, data = suidat, method = "lm", trControl = ctrl, metric="RMSE")
summary(lmCVFit)

bestest_model <- lm(SuiRate~Unemp + Vet + Arrest + Mental, data = suidat)
summary(bestest_model)

