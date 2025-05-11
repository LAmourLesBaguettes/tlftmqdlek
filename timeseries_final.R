if(!require(dplyr)){ install.packages("dplyr") }
if(!require(astsa)){ install.packages("astsa") }
library(dplyr)
library(astsa)

###########################################################
# Problem 1 - Classical Regression Model
# ---------------------------------------------------------
load("Final_Pr1.RData")
x1 <- ts(xt)

plot(x1, type='b')
# upward trend가 있고 Seasonality가 있는 것 같기도 하지만
# Seasonality는 불규칙해보여서 확실하지는 않다.
# constant variance 인 것 처럼 보인다.
# linear 혹은 quadratic trend일지도 모른다.
# 그래서 일단은 seasonality는 고려하지 않고 linear trend와 
# quadratic trend 만 고려하려고 한다.
acf2(x1)

t <- seq(x1) # linear trend
t2 <- t^2  # quadratic trend

lmfit <- lm(x1 ~ t + t2) # zero means no intercept
summary(lmfit)
# 모델 적합 결과, linear trend에 대한 계수의 p값이 유의하지 않다
# quadratic trend에 대한 계수의 값은 0.05보다 많이 작으므로 유의하다.
# 해당 시계열 데이터를 분석하기 위해 적절한 모델을 적합한 것으로 보인다.
# x_t = 0.154 + 0.0049 t + 0.00121 t^2 + e_t
# where e_t ~ i.i.d. N(0, sigma^2)

# residual analysis
plot(lmfit$fit, lmfit$residuals) 
# 특별한 패턴이 없이 잔차가 랜덤하게 퍼져있다.-> constant variance 이다.
plot(lmfit)

# ---------------------------------------------------------
#
###########################################################

###########################################################
# Problem 2 - ARIMA Model
# ---------------------------------------------------------
load("Final_Pr2.RData")
x2 <- ts(xt)

plot(x2, type='b')
# monotonic trend는 없는 것 같다.
# seasonality도 찾아보기 어렵다.
# E(x_t) != 0 -> non-stationary한 형태를 보이고 있다.

acf2(x2)
# ACF가 slow decay 하기 때문에 differencing이 필요해보인다.

x2diff <- diff(x2, 1)
plot(x2diff, type='b')
# differencing 후에는 E(x_t)=0인 것 같고,
# constant variance라고 말하고 싶지만 조금 크게 튀는 
# outlier도 있는 것 같다.

acf2(x2diff)
# ACF는 2개의 유의미한 spike가 보이고,
# tails-off 하는 것 같기도 하다.
# PACF는 1개의 유의미한 spike가 보이고 
# 마찬가지로 tails-off 하는 것 같기도 하다.
# 하나의 모델을 뽑기가 어려우니
# 후보모델군을 정하고 
# 그 중에서 가장 좋은 모델을 선정해보고자 한다.

# 1. ARIMA(1,1,0)
# ACF : tails-off, PACF : nonzero at lag 1
m1 <- sarima(x2, 1, 1, 0)
# 잔차는 0을 중심으로 랜덤하게 있으므로 괜찮다.
# ACF of residuals는 파란 선을 넘는게 2개는 보이지만 이정도는 괜찮다.
# Q-Q plot은 파란선에 대부분 모여있으므로 정규성도 괜찮다.
# p values for Ljung-Box statistic에 대해서는 
# -> H0: x_t s are independet
# -> H1: there is autocorrelation between x_t
# 이기 때문에 0.05인 파란선 보다는 커야 x_t들이 독립이라는 뜻이다.
# 파란선 밑으로 내려간 것이 조금 있지만 이정도는 괜찮다.
# 해당 모델에 대한 coefficient도 p-values < 0.05이므로 유의하다.


# 2. ARIMA(0,1,2)
# ACF : nonzero at lag 1 and 2, PACF : tails-off
m2 <- sarima(x2, 0, 1, 2)
# 잔차는 0을 중심으로 랜덤하게 있으므로 괜찮다.
# ACF of residuals는 파란 선을 넘는게 1개는 보이지만 이정도는 괜찮다.
# Q-Q plot은 파란선에 대부분 모여있으므로 정규성도 괜찮다.
# Ljung-Box statistic에 대해서는 
# 파란선 밑으로 내려간 것이 조금 있지만 이정도는 괜찮다.
# ma2 에 대한 coefficient도 p-values < 0.05이므로 유의하다.

# 3. ARIMA(1,1,1)
# ACF : tails-off, PACF : tails-off
m3 <- sarima(x2, 1, 1, 1)
# 잔차는 0을 중심으로 랜덤하게 있으므로 괜찮다.
# ACF of residuals는 파란 선을 넘는게 2개는 보이지만 이정도는 괜찮다.
# Q-Q plot은 파란선에 대부분 모여있으므로 정규성도 괜찮다.
# Ljung-Box statistic에 대해서는 
# 파란선 모든 p값이 0.05 보다 크므로 문제 없다.
# ar1과 ma1 에 대한 각각의 coefficient도 p-values < 0.05이므로 유의하다.

# 세 모델에 대한 diagnotics는 다 괜찮기 때문에
# 다른 모델 선택 기준으로 모델을 선정하고자 한다.
# AIC, AICc, BIC
m1$ICs
m2$ICs
m3$ICs
# ARIMA(1,1,1)이 AIC, AICc 측면에서 다른 모델들보다 가장 작은 값을 
# 가지므로 
# ARIMA(1,1,1) 모델을 최종 모델로 선정한다.

# residual plot
res <- m3$fit$residuals
fitted_values <- x2 - res
plot(fitted_values, res) # constant variance
# 잔차가 0중심으로 랜덤하게 잘 퍼져있으므로
# 모델이 잘 적합되었다고 볼 수 있다.
# x_t = 0.0013 - 0.593 x_{t-1} + w_t + 0.3291 w_{t-1}

# ---------------------------------------------------------
#
###########################################################


###########################################################
# Problem 3 
# ---------------------------------------------------------

load("Final_Pr3.RData")
x3 <- ts(xts)

plot(x3)
# looks zero mean, constant variance.
# no trend, no seasonality.
acf2(x3)

m <- sarima(x3, 1,0,1)
sig <- m$fit$sigma2 # 1.022439

# ARMA(1,1) : x_t = 0.1366 x_{t-1} + w_t - 0.11  w_{t-1}
psi <- ARMAtoMA(ar=c(0.1366), ma=c(-0.11), 10) # 일반적, Psi_0 = 1 이기 때문에 생략

# 1. var()
sig * psi[1]^2 # = 0.00072

# 2. var()
sig *(psi[1]^2 + psi[2]^2) # 0.0007369

# 3. var()
sig * (psi[1]^2 + psi[2]^2 + psi[3]^2) # .0007371882

# ---------------------------------------------------------
#
###########################################################

###########################################################
# Problem 4 - ARIMA Model
# ---------------------------------------------------------

load("Final_Pr4.RData")
x4 <- ts(xt)

plot(x4, type='b')
# 150t 시점 전까지는 특별한 trend나 seasonality 없이 진행되다가
# 150t 이후에는 upward trend가 있다가 잠시 안정되었다가
# 마지막에는 갑자기 치솟는다.
# abrupt change가 있다.
acf2(x4)
# ACF가 slow decay 하므로 differencing이 필요해보인다.

x4diff <- diff(x4, 1)
plot(x4diff, type='b')
# differencing 후에 time series plot을 보니 
# increasing variance의 형태가 보인다.
# 이러한 경우에는 log변환이나 sqrt 변환을 고려할 수 있다.
# log 변환을 하고자 한다.
x4logdiff <- diff(log(x4),1)
plot(x4logdiff, type='b') # log 변환 후 differencing
# 이제는 trend는 없어보인다. 
# constant variance인 것으로 보인다.
# seasonality는 잘 모르겠다.

acf2(x4logdiff)
# ACF는 2개의 유의미한 spike가 있거나,
# tails-off 하는 형태로 보이고,
# PACF는 tails-off하는 형태로 보인다.
# 그래서 후보 모델군을 정하고 그중에서 가장 좋은 모델을 
# 최종 모델로 선정하고자 한다.

# 1. ARIMA(0,1,2)
# ACF : nonzero at lag 1 and 2, PACF : tails-off
m11 <- sarima(log(x4), 0,1,2)
# 잔차는 0을 중심으로 랜덤하게 있으므로 괜찮다.
# ACF of residuals는 파란 선을 넘는게 없어서 바람직하다.
# Q-Q plot은 파란선에 대부분 모여있으므로 정규성도 괜찮다.
# Ljung-Box statistic에 대해서는 
# 모든 p-value 값이 0.05보다 크므로 유의하지 않다.
# ma2 에 대한 coefficient도 p-values < 0.05이므로 유의하다.


# 2. ARIMA(1,1,1)
# ACF : tails-off, PACF : tails-off
m12 <- sarima(log(x4), 1,1,1)
# 잔차는 0을 중심으로 랜덤하게 있으므로 괜찮다.
# ACF of residuals는 파란 선을 넘는게 없어서 바람직하다.
# Q-Q plot은 파란선에 대부분 모여있으므로 정규성도 괜찮다.
# Ljung-Box statistic에 대해서는 
# 파란선 밑으로 내려간 것이 하나 있지만 이정도는 괜찮다.
# ar1과 ma1 에 대한 각각의 coefficient도 p-values < 0.05이므로 유의하다.

# 3. ARIMA(2,1,1)
# ACF : tails-off, PACF : tails-off
m13 <- sarima(log(x4), 2,1,1)
# diagnotics는 앞전 모델과 마찬가지로 괜찮지만
# ar2에 대한 coefficient가 0.05보다 크므로 유의하지 않으므로
# 이 모델을 후보에서 제외한다.

# 4. ARIMA(1,1,2)
# ACF : tails-off, PACF : tails-off
m14 <- sarima(log(x4), 1,1,2)
# diagnotics는 앞전 모델과 마찬가지로 괜찮지만
# ar1에 대한 coefficient가 0.05보다 크므로 유의하지 않으므로
# 이 모델 역시 최종 후보에서 제외한다.

# 살아남은 ARIMA(0,1,2)와 ARIMA(1,1,1) 모두 괜찮은 모델이기에
# 둘 중 하나를 선택하기 위해서
# AIC, AICc, BIC 기준으로 살펴보고자 한다.
m11$ICs
m12$ICs
# ARIMA(0,1,2) 모형이 ARIMA(1,1,1)에 비해서
# 세가지 기준 값이 모두 작으므로
# ARIMA(0,1,2)를 최종 모형으로 선정한다. 
# residual analysis
res <- m11$fit$residuals
fitted_values <- x4 - res
plot(fitted_values, res) # constant variance
# 예상대로 잔차가 0중심으로 랜덤하게 잘 퍼져있으므로
# 모델이 잘 적합되었다고 볼 수 있다.
# x_t = 0.0081 + w_t + 0.411 w_{t-1} -0.420 w_{t-2}

# ---------------------------------------------------------
#
###########################################################
