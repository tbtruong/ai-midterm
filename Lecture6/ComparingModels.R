library(leaps)
library(caret)

set.seed(314)

#########################################################
#Create data
#########################################################
n <- 30
temp = as.data.frame(matrix(runif(300), ncol=10))
y <- 10 + 5*temp$V1 + 3*temp$V2 + 4*temp$V4 + rnorm(n, sd=.1)
#y = 10 + 5 * X1 + 3 *X2 + 4*X4 + e_i 
df <- data.frame(temp, y)


#########################################################
#The F-statistic
#########################################################

lm_full <- lm(formula = y ~ ., data=df)
lm_reduced <- lm(formula = y ~ V1 + V2 + V4, data=df)
lm_drop_useful_variable <- lm(formula = y~V1 +V2, data=df)

anova(lm_full, lm_reduced) #comparison of two models where second model only has insignificant variables
anova(lm_full, lm_drop_useful_variable) #comparison of two models where second model drops significant variable


#Check of the formula by hand
# Fit full model
rss_full <- sum(lm_full$residuals**2)
ndf_full <- lm_full$df.residual
cat("\nRSS on full model: ", rss_full, ", degrees of freedom: ", ndf_full)
# Fit submodel
rss_reduced <- sum(lm_reduced$residuals**2)
ndf_reduced <- lm_reduced$df.residual
cat("\nRSS on submodel: ", rss_reduced, ", degrees of freedom: ", ndf_reduced)
#Compute F
Fstat <- ((rss_reduced-rss_full)/(ndf_reduced-ndf_full))/(rss_full/ndf_full)
pval <- pf(Fstat, ndf_reduced-ndf_full, ndf_full, lower.tail=FALSE)
cat("\nF-statistic = ", Fstat, ", p-value = ", pval)


#########################################################
#R and R^2
#########################################################
summary(lm_full) #R^2 = 0.9973 and #R^2_adj = 0.9959
summary(lm_reduced) #R^2 = 0.9967 R^2_adj = 0.9963
summary(lm_drop_useful_variable) #R2 = 0.4979 R^2_adj = 0.4608


#########################################################
# Best Subsets, Forward, and Backward Regression
#########################################################
summary(lm_full)

best_subsets <- regsubsets(y ~ ., nbest = 1, nvmax= NULL, data=df,method="exhaustive")
summary_best<- summary(best_subsets)
which.max(summary_best$adjr2)

foward_regression <-  regsubsets(y ~ ., nbest = 1, nvmax= NULL, data=df,method="forward")
summary_forward <- summary(foward_regression)
which.max(summary_forward$adjr2)

backward_regression <-  regsubsets(y ~ ., nbest = 1, nvmax= NULL, data=df,method="backward")
summary_backward <- summary(backward_regression)
which.max(summary_backward$adjr2)


############################################################
# Cross - Validated Error
############################################################

lmFit<-train(y~., data = df, method = "lm")

ctrl<-trainControl(method = "cv",number = 10)
lmCVFit<-train(y ~ ., data = df, method = "lm", trControl = ctrl, metric="RMSE")
summary(lmCVFit)

summary(lm(y~V1+V3+V4))
