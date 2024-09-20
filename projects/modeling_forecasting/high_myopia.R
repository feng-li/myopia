library("dplyr")
library("fields")

high_myopia = read.csv("../data/High_Myopia.csv", header = TRUE)

## Calculate the raw rates
high_myopia["rate_overall"] = high_myopia["高度近视人数"] / high_myopia["总人数"]
high_myopia["rate_10_12_urban"] = high_myopia["X10_12年级城市高度近视人数"] / high_myopia["X10_12年级城市人数"]



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

## Estimating the linear trend
window = 25:30
lm(fitted_[window]~year[window])$coef[2] * 0.5

trend = lm(fitted_[window]~year[window])$coef[2] * 0.5


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

# resid = data_logit - fitted_
fore12 = NULL
lower12 = NULL
upper12 = NULL
for(h in 1)
{
    arima12 = auto.arima(data_logit)

    fore = forecast(arima12, h = 28)
    fore_mean = fore$mean  # + (1:28)* trend

    fore12_ = 1 / (1 + exp(-fore$mean))

    se_logit = (fore$upper[, 2] - fore$mean) / 1.96

    ## Calibrating forecasting intervals
    scaling_factor = 0.2
    se12 = fore12_ * (1 - fore12_) * se_logit * scaling_factor

    upper12  = c(upper12, fore12_ + 1.96 * se12)
    # upper12 = c(upper12, 1/(1+exp(-fore$upper))[, 2])

    lower12  = c(lower12, fore12_ - 1.96 * se12)

    ## the lower bound is below zero, using transformed version.
    ## lower12 = c(lower12, 1/(1+exp(-fore$lower))[, 2])
    ## autoplot(fore)

    fore12 = c(fore12, 1 / (1 + exp(-fore_mean)))

    data_logit = c(data_logit, fore_mean)
}

out = data.frame(year = c(year, 2023:2050),
                 mean = c(mean12, fore12),
                 lower = c(lower, lower12),
                 upper = c(upper, upper12)
                 )
out = out %>% distinct()

## Smoothing
out$upper = lowess(out$upper)$y
out$lower = lowess(out$lower)$y
out$mean = lowess(out$mean)$y



plot(out$year, out$upper, type="l", col = "red", ylim = c(0, 1), xlab = "Year", ylab = "High Myopia")
points(out$year, out$lower, type="l", col = "red")
points(out$year, out$mean, type="l", col = "blue")
points(year, data)

dev.copy2pdf(file = "high_myopia_forecast.pdf")

write.table(out, file = "../high_myopia_forecast.csv", row.names = FALSE, sep = ",")
