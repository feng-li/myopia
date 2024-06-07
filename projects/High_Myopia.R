library(dplyr)
library("fields")
high_myopia = read.csv("data/High_Myopia.csv", header = TRUE)


## Calculate the raw rates
high_myopia["rate_overall"] = high_myopia["高度近视人数"] / high_myopia["总人数"]
high_myopia["rate_10_12_urban"] = high_myopia["X10_12年级城市高度近视人数"] / high_myopia["X10_12年级城市人数"]


hdi = read.csv("data/kahdi.csv", header = TRUE)
high_hdi = hdi[hdi$hdistate == "high", ]

high_hdi_cn = c("北京", "浙江", "内蒙", "江苏")

## Overall High Myopia in High HDI

year = high_myopia[high_myopia$省份 %in% high_hdi_cn,'年份']
data = high_myopia[high_myopia$省份 %in% high_hdi_cn, "rate_overall"]

data1_logit = log(data/(1-data))

x = cbind(year)
reg1 = Tps(x, data1_logit)
fitted_ = fitted(reg1)[,1]

fitted_orig1 = 1 / (1 + exp(-fitted_))

sd1 = sd(data - fitted_orig1)
upper = fitted_orig1 + 1.96 * sd1
lower = fitted_orig1 - 1.96 * sd1

plot(year, data, ylim = c(0, 0.25))
points(year, fitted_orig1, type="l", col = "blue")
points(year, upper, type="l", col = "red")
points(year, lower, type="l", col = "red")

## Overall high myopia in other regions

year = high_myopia[!(high_myopia$省份 %in% high_hdi_cn),'年份']
data = high_myopia[!(high_myopia$省份 %in% high_hdi_cn), "rate_overall"]

data_logit = log(data/(1-data))

x = cbind(year)
reg2 = Tps(x, data_logit)
fitted_ = fitted(reg2)[,1]

fitted_orig2 = 1 / (1 + exp(-fitted_))

sd2 = sd(data - fitted_orig2)
upper = fitted_orig2 + 1.96 * sd2
lower = fitted_orig2 - 1.96 * sd2

plot(year, data, ylim = c(-1, 0.25))
points(year, fitted_orig2, type="l", col = "blue")
points(year, upper, type="l", col = "red")
points(year, lower, type="l", col = "red")

## Group 10-12
## We first use a year based smoothing to estimate the mean time-varying rate. Then an auto.arima model is used to produce 50 year forecasting. ARIMA model is a stationary model. The trend estimated in the spline is added to the forecast.

data_10_12 = high_myopia[, "rate_10_12_urban"]
year = high_myopia[,'年份']

# data_10_12 = high_myopia[high_myopia$省份 %in% high_hdi_cn, "rate_10_12_urban"]
# year = high_myopia[high_myopia$省份 %in% high_hdi_cn,'年份']

data = data_10_12[!is.na(data_10_12)]
year = year[!is.na(data_10_12)]

data_logit = log(data/(1-data))

x = cbind(year, year^2)
reg12 = Tps(x, data_logit, lambda = 0.5)
fitted_ = fitted(reg12)[,1]

# Estimating the linear trend
trend = lm(fitted_~year)$coef[2] * 0.5


fitted_orig12 = 1 / (1 + exp(-fitted_))
mean12 = fitted_orig12

resid12 = residuals(reg12)
sd12 = sd(data - fitted_orig12)
# sd12 = sd(fitted_)/4

upper = fitted_orig12 + 1.96 * sd12
lower = fitted_orig12 - 1.96 * sd12

# tweak to avoid negative intervals
delta = abs(min(lower))
lower = lower + delta
upper = upper - delta

plot(year, data, ylim = c(0, 0.3), ylab = "High Myopia")
points(year, fitted_orig12, type="l", col = "blue")
points(year, upper, type="l", col = "red")
points(year, lower, type="l", col = "red")

library("forecast")

resid = data_logit - fitted_

arima12 = auto.arima(data_logit)

fore = forecast(arima12, 28)
fore_mean = fore$mean  + (1:28)* trend

fore12_ = 1 / (1 + exp(-fore$mean))

se_logit = (fore$upper[, 2] - fore$mean) / 1.96

se12 = fore12_ * (1 - fore12_) * se_logit
upper12  = fore12_ + 1.96 * se12

## lower = mean12 - 1.96 * se12
## the lower bound is below zero, using transformed version.
lower12 = 1/(1+exp(-fore$lower))[, 2]
# autoplot(fore)

fore12 = 1 / (1 + exp(-fore_mean))

out = data.frame(year = c(year, 2023:2050),
                 mean = c(mean12, fore12),
                 lower = c(lower, lower12),
                 upper = c(upper, upper12)
                 )
out = out %>% distinct()


plot(out$year, out$upper, type="l", col = "red", ylim = c(0, 1), xlab = "Year", ylab = "High Myopia")
points(out$year, out$lower, type="l", col = "red")
points(out$year, out$mean, type="l", col = "blue")
points(year, data)


write.table(out, file = "~/high_myopia_est.csv", row.names = FALSE, sep = ",")
