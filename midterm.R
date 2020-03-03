############################################
# CS440 Midterm Project: Tuan Anh Vu, Tung Truong
##########################################
library(lmtest)
library(caret)
setwd("/Users/BenMeow/Desktop/BU\ 20\ Spring/CS440/Midterm/")
# Read file
suidat <- read.csv("/")
#Suicide Rate Data + Variables:
#LINK#

# Split data randomly into training set and test set, each with 75% and 25% of the data respectively
set.seed(3456)
trainData <- createDataPartition(suidat$Residual, p=.75,list=FALSE,times=1)
head(suidat)

suiTrain <- suidat[trainData,]
suiTest <- suidat[-trainData,]
head(suiTrain)
head(suiTest)

# All the models
#####################
# Varaibles: SuiRate, Unemp, Vet, Arrest, Mental, Disable
# 0 variable
SuiRate <- lm(SuiRate~SuiRate, data = suiTrain)
# 1 variable
Unemp <- lm(SuiRate~Unemp, data = suiTrain)
Vet <- lm(SuiRate~Vet, data = suiTrain)
Arrest <- lm(SuiRate~Arrest, data = suiTrain)
Mental <- lm(SuiRate~Mental, data = suiTrain)
Disable <- lm(SuiRate~Disable, data = suiTrain)
# 2 variables
Unemp_Vet <- lm(SuiRate~Unemp + Vet, data = suiTrain)
Unemp_Arrest <- lm(SuiRate~Unemp + Arrest, data = suiTrain)
Unemp_Mental <- lm(SuiRate~Unemp + Mental, data = suiTrain)
Unemp_Disable <- lm(SuiRate~Unemp = Disable, data = suiTrain)
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
models <- list(SuiRate,Unemp,Vet,Arrest,Mental,Disable,Unemp_Vet,Unemp_Arrest,Unemp_Mental,Unemp_Disable,Vet_Arrest,Vet_Mental,\
Vet_Disable,Arrest_Mental,Arrest_Disable,Mental_Disable,Unemp_Arrest_Vet,Unemp_Vet_Mental,Unemp_Vet_Disable,Unemp_Arrest_Mental,\
Unemp_Arrest_Disable,Unemp_Mental_Disable ,Vet_Arrest_Mental,Vet_Arrest_Disable,Arrest_Mental_Disable,\
Unemp_Arrest_Vet_Mental,Unemp_Vet_Disable_Arrest,Unemp_Vet_Disable_Mental,Unemp_Arrest_Mental_Disable,Vet_Arrest_Disable_Mental,\
Unemp_Arrest_Mental_Disable_Vet)

git 


for (i in models) {
  # Convert model name into text
  name <- deparse(substitute(i))
  # R outputs to .txt file instead of consolde
  sink(name + ".txt")
  print(summary(i))
  # Revert R outputs to console
  sink()
  # name to list
  # if (____ in list)
  # plot
  
}

#Error term analysis
####################
error_terms <- multiple_regression_model$residuals # from the fit
yhTrain$Residual  - predict(multiple_regression_model, yhTrain) 

###Linear Response
plot(suiTrain$Unemp,yhTrain$Residual)
plot(suiTrain$Vet,yhTrain$Residual)
plot(suiTrain$Arrest,yhTrain$Residual)
plot(suiTrain$Mental,yhTrain$Residual)
plot(suiTrain$Disable,yhTrain$Residual)
# Exponential relationship ?!??!?!?!?
plot(yhTrain$Froude,yhTrain$Residual)

### Errors independent and and variance about the same
plot(error_terms)
lmtest::bptest(multiple_regression_model)

###Approximately Normal?
hist(error_terms)
qqnorm(error_terms)
qqline(error_terms)

###############
# Prediction in R
###########
predict(multiple_regression_model, yhTest)
yhTest$Residual - predict(multiple_regression_model, yhTest)

# Calculate mean and standard deviation for test and training residuals
mean(yhTest$Residual - predict(multiple_regression_model, yhTest))
sd(yhTest$Residual - predict(multiple_regression_model, yhTest))

mean(yhTrain$Residual - predict(multiple_regression_model, yhTrain))
sd(yhTrain$Residual - predict(multiple_regression_model, yhTrain))

# Determine observation range between training and test set
min(yhTrain$Froude)
max(yhTrain$Froude)

min(yhTest$Froude)
max(yhTest$Froude)
