library(caret)


######################################################
# Cross-validation
####################################################

### Data and model fitting
set.seed(314)
n <- 50
x <- sort(runif(n, -2, 2))
y <- 3*x^3 + 5*x^2 + 0.5*x + 20 +rnorm(n, sd =3) # a 3 polynomial model
df <- data.frame(x,y)
nterm <- c(1,2,3,5,7,15)

plot(y~x, df, ylab="y")
PAL <- colorRampPalette(c("red", "blue", "cyan","green"))
COLS <- PAL(length(nterm))
for(i in seq(nterm)){
  fit <- lm(y ~ poly(x, degree=nterm[i]), data=df)
  newdat <- data.frame(x=seq(min(df$x), max(df$x),length.out =100))
  lines(newdat$x, predict(fit, newdat), col=COLS[i])
}
legend(x=-2, y= 55, legend=paste0(nterm, c("", "", "*", "", "")), title="Poly Deg", bty="n", col=COLS, lty=1)


RMSE_results <- c() 
for(i in 1:12){
  print(paste("Processing degree:", i))
  ctrl<-trainControl(method = "cv",number = 10)
  f = bquote(y~ poly(x, .(i)))
  lmCVFit<-train(as.formula(f), data = df, method = "lm", trControl = ctrl, metric="Rsquared")
  RMSE_results <- c(RMSE_results,lmCVFit$results$RMSE)
}

overfit_errors <- c()
for(i in 1:12){
  fit <- lm(y ~ poly(x, degree=i))
  overfit_errors <- c(overfit_errors, sqrt(mean(fit$residuals^2)))
}

ylim <- range(min(RMSE_results)*.8, max(RMSE_results)*1.2)
plot(1:12, RMSE_results, log="y", col=1, t="o", ylim=ylim, xlab="Complexity (poly degree)", ylab="RMSE")
lines(1:12, overfit_errors, col=2, t="o")
abline(v=3, lty=2, col=8)
abline(h=min(RMSE_results), lty=2, col=8)
legend("top", legend=c("No CV RMSE", "Cross-validation RMSE"), bty="n", col=2:1, lty=1, pch=1)
