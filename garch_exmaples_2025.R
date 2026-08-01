library(rugarch)
library(quantmod)
library(vars)
library(rugarch)
library(rmgarch)
library(tidyverse)

# GOOGLE, IBM & BP
getSymbols('GOOGL', from = '2015-01-01', to = '2025-01-01')
getSymbols('IBM', from = '2015-01-01', to = '2025-01-01')
getSymbols('BP', from = '2015-01-01', to = '2025-01-01')

# get closing prices
closeGOOGL <- GOOGL[,4]
closeIBM   <- IBM[,4]
closeBP    <- BP[,4]

# returns
rGOOGL <- dailyReturn(closeGOOGL)
rIBM   <- dailyReturn(closeIBM)
rBP    <- dailyReturn(closeBP)

# plots
par(mfrow=c(2,3))
plot(closeGOOGL, main="Google Closing")
plot(closeIBM, main="IBM closing")
plot(closeBP, main="BP closing")
plot(rGOOGL, main="First difference Google")
plot(rIBM, main="First difference IBM")
plot(rBP, main="First difference BP")

# Create dataframe with all 
rX <- data.frame(closeGOOGL, closeIBM, closeBP)
head(rX)

par(mfrow=c(3,1))
plot(rGOOGL)
plot(rIBM)
plot(rBP)

par(mfrow=c(2,3))
plot(rGOOGL)
plot(rIBM)
plot(rBP)
plot(GOOGL)
plot(IBM)
plot(BP)

# multivariate GARCH df
rX <- data.frame(rGOOGL, rIBM, rBP)
names(rX)[1] <- "rGOOGL"
names(rX)[2] <- "rIBM"
names(rX)[3] <- "rBP"

# GARCH specs
garchSPEC <- ugarchspec(variance.model = list(model ="sGARCH", garchOrder=c(1,1)),
                        mean.model = list(armaOrder=c(1,0)),
                        distribution.model = "norm")

garchSPECE <- ugarchspec(variance.model = list(model ="eGARCH", garchOrder=c(1,1)),
                         mean.model = list(armaOrder=c(1,0)),
                         distribution.model = "std")

garchSPECN <- ugarchspec(variance.model = list(model="eGARCH", garchOrder = c(1,1)),
                                mean.model = list(armaOrder=c(1,0)),
                                distribution.model = "norm")

# plot
par(mfrow=c(1,1))

ggplot(rGOOGL, aes(daily.returns)) +
  geom_histogram(binwidth = 0.005, col="black", fill ="lightgrey") +
  theme_minimal() +
  labs(title = "Histogram of daily Google returns (adjusted)",
       x = "Daily returns", y = "count") +
  theme(plot.title = element_text(face="bold"),
        axis.title.x = element_text(face="bold"))

# fit model for GOOGL - no time left for BP and IBM :( 
garchSPECgoogl <- ugarchspec(variance.model = list(model="eGARCH", garchOrder =c(1,1)),
                             mean.model = list(armaOrder=c(1,0)),
                             distribution.model = "std")

ugfitNORM <- ugarchfit(spec=garchSPEC, data = rGOOGL)
ugfitE <- ugarchfit(spec=garchSPECE, data = rGOOGL)
ugfitEN <- ugarchfit(spec=garchSPECN, data = rGOOGL)

# extract variance and residuals
ugvar <- ugfitE@fit$var
ugres <- (ugfitE@fit$residuals)^2

par(mfrow=c(1,1))
plot(ugres, type = "l", main = "GARCH for IBM returns", ylab="residuals", lwd = 1)
lines(ugvar, col = "green", lwd = 2)
legend("topright", legend = c("Squared residuals", "Conditional variance"),
       col = c("black", "green"), lty = 1, lwd = 2)

par(mfrow=c(3,1))
plot(ugfitNORM, which = 8)
plot(ugfitE, which = 8)
plot(ugfitEN, which = 8)

# std distribution is preferable

# multivariate
ug_spec_n <- multispec(replicate(3, garchSPEC))
multf <- multifit(ug_spec_n, rX)

spec1 <- dccspec(uspec = ug_spec_n, dccOrder = c(1,1), distribution = "mvnorm")
fit1 <- dccfit(spec1, data = rX, fit.control = list(eval.se = TRUE), fit = multf)
fit1

cov1 <- rcov(fit1)
cor1 <- rcor(fit1)

dim(cor1)

cor1[,,dim(cor1)[3]]

cor_IMPGOOGL <- cor1[2,1,]
cor_IMPGOOGL <- as.xts(cor_IMPGOOGL)

par(mfrow=c(1,1))
plot(cor_IMPGOOGL)

par(mfrow=c(3,1))
plot(as.xts(cor1[1,2,]), main = "Google and IBM")
plot(as.xts(cor1[2,3,]), main = "IBM and BP")
plot(as.xts(cor1[3,1,]), main = "BP and Google")

# granger causality
sqresIBM <- ugarchfit(spec = garchSPEC, data = rIBM)
sqresGOOGL <- ugarchfit(spec = garchSPEC, data = rGOOGL)
sqresBP <- ugarchfit(spec = garchSPEC, data = rBP)

res_IBM <- (sqresIBM@fit$residuals)^2
res_GOOGL <- (sqresGOOGL@fit$residuals)^2
res_BP <- (sqresBP@fit$residuals)^2

squared_residuals <- data.frame(res_IBM=res_IBM, res_GOOGL=res_GOOGL, res_BP=res_BP)

squared_residuals_ts <- ts(squared_residuals)
lag_selection <- VARselect(squared_residuals_ts, lag.max = 10, type = "const")
# VAR select is primarily for vector autoregressive models, but soaking up serial correlation is the still purpose
# sc(n) = Bayesian Information Criterion stricter penalty term (schwartz something.. )
print(lag_selection$selection)

grangertest(res_IBM ~ res_GOOGL, order = 8, data = squared_residuals)
# reject that Google volatility does not Granger-cause IBM volatility

grangertest(res_BP ~ res_IBM, order = 8, data = squared_residuals)
# reject that IBM volatility does not Granger-cause BP volatility

# this provides strong evidence of volatility spillover from IBM to BP and GOOGL to IBM
