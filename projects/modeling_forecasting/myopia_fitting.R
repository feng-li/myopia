library("fields")

## RATE COUNTRY
rm(list = ls())

file = "../rates_estimation/rate_country.csv"
data_ts = read.csv(file, header = TRUE)

year = data_ts[,'year']
rate = data_ts[, "rate"]
x = cbind(year, year^2)
reg1 = Tps(x, rate, lambda = 0.01)
fitted = fitted(reg1)[,1]

resid1 = residuals(reg1)
upper = fitted + 1.96 * sd(resid1)
lower = fitted - 1.96 * sd(resid1)
reg_sd = sd(resid1)

plot(year, rate)
points(year, fitted, type="l", col = "blue")
points(year, upper, type="l", col = "red")
points(year, lower, type="l", col = "red")

out = data.frame(cbind(year, rate, fitted, upper, lower))
write.csv(out, file = "../rate_country_with_95CI.csv")

## Forecast
library("forecast")

year_forec = 2023:2050

mean_g = fitted[length(fitted)]
mean1 = fitted

sd_inflation_factor = 0.015


for (h in 0:27){

    arima1 = auto.arima(resid1)
    fore1 = forecast(arima1, h = 1)
    resid1 = rbind(resid1, fore1$mean)
    mean1 = c(mean1, fore1$mean + mean_g)

    fore_mean = (mean_g + fore1$mean)
    lower_new = (fore_mean - 1.96 * ((fore1$mean - fore1$lower[2]) / 1.96 + reg_sd) *
                 (1 + sd_inflation_factor * h))
    upper_new = (fore_mean + 1.96 * ((fore1$upper[2] - fore1$mean) / 1.96 + reg_sd) *
                 (1 + sd_inflation_factor * h))

    upper = c(upper, upper_new)
    lower = c(lower, lower_new)
}

year1 = 1998:2050
plot(year1, upper, type="l", col = "red")
points(year1, lower, type="l", col = "red")
points(year1, mean1, type="l", col = "blue")
# plot(year, rate)

out1 = data.frame(cbind(year1, mean1, upper, lower))
write.csv(out1, file = "../rate_country_forec_with_95CI.csv")



## RATE COUNTRY URBAN and RURAL
rm(list = ls())

file = "../rates_estimation/rate_country_urban_and_rural.csv"
data_ts = read.csv(file, header = TRUE)

year = data_ts[,'year']

##-- URBAN
rate_urban = data_ts[, "myopia_rate_urban"]
x = cbind(year)
reg1 = Tps(x, rate_urban, lambda = 0.06)
fitted_urban = fitted(reg1)[,1]

resid1 = residuals(reg1)
upper_urban = fitted_urban + 1.96 * sd(resid1)
lower_urban = fitted_urban - 1.96 * sd(resid1)


plot(year, rate_urban, ylim = c(0.2, 0.8))
points(year, fitted_urban, type="l", col = "blue")
points(year, upper_urban, type="l", col = "red")
points(year, lower_urban, type="l", col = "red")

##-- RURAL
rate_rural = data_ts[, "myopia_rate_rural"]
x = cbind(year, year ^ 2)
reg2 = Tps(x, rate_rural, lambda = 0.02)
fitted_rural = fitted(reg2)[,1]

resid2 = residuals(reg2)
upper_rural = fitted_rural + 1.96 * sd(resid2)
lower_rural = fitted_rural - 1.96 * sd(resid2)


plot(year, rate_rural, ylim = c(0.2, 0.8))
points(year, fitted_rural, type="l", col = "blue")
points(year, upper_rural, type="l", col = "red")
points(year, lower_rural, type="l", col = "red")

out = data.frame(cbind(year, rate_urban, fitted_urban, upper_urban, lower_urban, rate_rural, fitted_rural, upper_rural, lower_rural))
write.csv(out, file = "../rate_country_urban_and_rural_with_95CI.csv")


## RATE HDI
rm(list = ls())

file = "../rates_estimation/rate_hdi.csv"
data_ts = read.csv(file, header = TRUE)

##-- High HDI
rate_h = data_ts[data_ts$hdistate == "high", c("year", "rate")]
year = rate_h[,'year']
rate_h = rate_h[, 'rate']

x = cbind(year)
reg1 = Tps(x, rate_h, lambda = 0.01)
fitted_h = fitted(reg1)[,1]

resid1 = residuals(reg1)
upper_h = fitted_h + 1.96 * sd(resid1)
lower_h = fitted_h - 1.96 * sd(resid1)

## Medium
rate_m = data_ts[data_ts$hdistate == "medium", c("year", "rate")]
year = rate_m[,'year']
rate_m = rate_m[, 'rate']

x = cbind(year)
reg1 = Tps(x, rate_m, lambda = 0.01)
fitted_m = fitted(reg1)[,1]

resid1 = residuals(reg1)
upper_m = fitted_m + 1.96 * sd(resid1)
lower_m = fitted_m - 1.96 * sd(resid1)


# rate_l = data_ts[data_ts$hdistate == "low and medium", c("year", "rate")]
rate_l = data_ts[data_ts$hdistate == "low", c("year", "rate")]
year = rate_l[,'year']
rate_l = rate_l[, 'rate']

x = cbind(year)
reg1 = Tps(x, rate_l, lambda = 0.01)
fitted_l = fitted(reg1)[,1]

resid1 = residuals(reg1)
upper_l = fitted_l + 1.96 * sd(resid1)
lower_l = fitted_l - 1.96 * sd(resid1)


par(mfrow = c(3, 1))
plot(year, rate_h, ylim = c(0, 1))
points(year, fitted_h, type="l", col = "blue")
points(year, upper_h, type="l", col = "red")
points(year, lower_h, type="l", col = "red")

plot(year, rate_m, ylim = c(0, 1))
points(year, fitted_m, type="l", col = "blue")
points(year, upper_m, type="l", col = "red")
points(year, lower_m, type="l", col = "red")

plot(year, rate_l, ylim = c(0, 1))
points(year, fitted_l, type="l", col = "blue")
points(year, upper_l, type="l", col = "red")
points(year, lower_l, type="l", col = "red")

out = data.frame(cbind(year, rate_h, fitted_h, upper_h, lower_h, rate_m, fitted_m, upper_m, lower_m, rate_l, fitted_l, upper_l, lower_l))
write.csv(out, file = "../rate_hdi_with_95CI.csv")
