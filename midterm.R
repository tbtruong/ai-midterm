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
regression_model0 <- lm(SuiRate~SuiRate, data = suiTrain)
# 1 variable
regression_model1 <- lm(SuiRate~Unemp, data = suiTrain)
regression_model2 <- lm(SuiRate~Vet, data = suiTrain)
regression_model3 <- lm(SuiRate~Arrest, data = suiTrain)
regression_model4 <- lm(SuiRate~Mental, data = suiTrain)
regression_model5 <- lm(SuiRate~Disable, data = suiTrain)
# 2 variables
regression_model6 <- lm(SuiRate~Unemp + Vet, data = suiTrain)
regression_model7 <- lm(SuiRate~Unemp + Arrest, data = suiTrain)
regression_model8 <- lm(SuiRate~Unemp + Mental, data = suiTrain)
regression_model9 <- lm(SuiRate~Unemp = Disable, data = suiTrain)
regression_model10 <- lm(SuiRate~Vet + Arrest, data = suiTrain)
regression_model11 <- lm(SuiRate~Vet + Mental, data = suiTrain)
regression_model12 <- lm(SuiRate~Vet + Disable, data = suiTrain)
regression_model13 <- lm(SuiRate~Arrest + Mental, data = suiTrain)
regression_model14 <- lm(SuiRate~Arrest + Disable, data = suiTrain)
regression_model15 <- lm(SuiRate~Mental + Disable, data = suiTrain)
# 3 variables
regression_model16 <- lm(SuiRate~Unempt + Vet + Arrest, data = suiTrain)
regression_model17 <- lm(SuiRate~Unemp + Vet + Mental, data = suiTrain)
regression_model18 <- lm(SuiRate~Unemp + Vet + Disable, data = suiTrain)
regression_model19 <- lm(SuiRate~Unemp + Arrest + Mental, data = suiTrain)
regression_model20 <- lm(SuiRate~Unemp + Arrest + Disable, data = suiTrain)
regression_model21 <- lm(SuiRate~Unemp + Mental + Disable, data = suiTrain)
regression_model22 <- lm(SuiRate~Vet + Arrest + Mental, data = suiTrain)
regression_model23 <- lm(SuiRate~Vet + Arrest + Disable, data = suiTrain)
regression_model24 <- lm(SuiRate~Vet + Mental + Disable, data = suiTrain)
regression_model25 <- lm(SuiRate~Arrest + Mental + Disable, data = suiTrain)
# 4 variables
regression_model26 <- lm(SuiRate~Unemp + Vet + Arrest + Mental, data = suiTrain)
regression_model27 <- lm(SuiRate~Unemp + Vet + Arrest + Disable, data = suiTrain)
regression_model28 <- lm(SuiRate~Unemp + Vet + Mental + Disable, data = suiTrain)
regression_model29 <- lm(SuiRate~Unemp + Arrest + Mental + Disable, data = suiTrain)
regression_model30 <- lm(SuiRate~Vet + Arrest + Mental + Disable, data = suiTrain)
# 5 variables 
regression_model31 <- lm(SuiRate~Unemp + Vet + Arrest + Mental + Disable, data = suiTrain)

models <- list(regression_model0, regression_model1, regression_model2, regression_model3, regression_model4, regression_model5, regression_model6, regression_model7, regression_model8, regression_model9, regression_model10, regression_model11, regression_model12, regression_model13, regression_model14, regression_model15, regression_model16, regression_model17, regression_model18, regression_model19, regression_model20, regression_model21, regression_model22, regression_model23, regression_model24, regression_model25, regression_model26, regression_model27, regression_model28, regression_model29, regression_model30, regression_model31)


# Very big loop

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
