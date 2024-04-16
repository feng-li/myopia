library("fields")

## RATE COUNTRY
rm(list = ls())

file = "test/data_interpolation/summary sheet/rate_country.csv"
data_ts = read.csv(file, header = TRUE)

year = data_ts[,'year']
rate = data_ts[, "rate"]
x = cbind(year, year^2)
reg1 = Tps(x, rate, lambda = 0.01)
fitted = fitted(reg1)[,1]

resid1 = residuals(reg1)
upper = fitted + 1.96 * sd(resid1)
lower = fitted - 1.96 * sd(resid1)


plot(year, rate)
points(year, fitted, type="l", col = "blue")
points(year, upper, type="l", col = "red")
points(year, lower, type="l", col = "red")

out = data.frame(cbind(year, rate, fitted, upper, lower))
write.csv(out, file = "test/data_interpolation/summary sheet/rate_country_with_95CI.csv")


## RATE COUNTRY URBAN and RURAL
rm(list = ls())

file = "test/data_interpolation/summary sheet/rate_country_urban_and_rural.csv"
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
write.csv(out, file = "test/data_interpolation/summary sheet/rate_country_urban_and_rural_with_95CI.csv")


## RATE HDI
rm(list = ls())

file = "test/data_interpolation/summary sheet/rate_hdi.csv"
data_ts = read.csv(file, header = TRUE)

##-- High HDI
rate_vh = data_ts[data_ts$HDI_state == "very high", c("year", "rate")]
year = rate_vh[,'year']
rate_vh = rate_vh[, 'rate']

x = cbind(year)
reg1 = Tps(x, rate_vh, lambda = 0.01)
fitted_vh = fitted(reg1)[,1]

resid1 = residuals(reg1)
upper_vh = fitted_vh + 1.96 * sd(resid1)
lower_vh = fitted_vh - 1.96 * sd(resid1)


rate_h = data_ts[data_ts$HDI_state == "high", c("year", "rate")]
year = rate_h[,'year']
rate_h = rate_h[, 'rate']

x = cbind(year)
reg1 = Tps(x, rate_h, lambda = 0.01)
fitted_h = fitted(reg1)[,1]

resid1 = residuals(reg1)
upper_h = fitted_h + 1.96 * sd(resid1)
lower_h = fitted_h - 1.96 * sd(resid1)


rate_ml = data_ts[data_ts$HDI_state == "low and medium", c("year", "rate")]
year = rate_ml[,'year']
rate_ml = rate_ml[, 'rate']

x = cbind(year)
reg1 = Tps(x, rate_ml, lambda = 0.01)
fitted_ml = fitted(reg1)[,1]

resid1 = residuals(reg1)
upper_ml = fitted_ml + 1.96 * sd(resid1)
lower_ml = fitted_ml - 1.96 * sd(resid1)


par(mfrow = c(3, 1))
plot(year, rate_vh, ylim = c(0, 1))
points(year, fitted_vh, type="l", col = "blue")
points(year, upper_vh, type="l", col = "red")
points(year, lower_vh, type="l", col = "red")

plot(year, rate_h, ylim = c(0, 1))
points(year, fitted_h, type="l", col = "blue")
points(year, upper_h, type="l", col = "red")
points(year, lower_h, type="l", col = "red")

plot(year, rate_ml, ylim = c(0, 1))
points(year, fitted_ml, type="l", col = "blue")
points(year, upper_ml, type="l", col = "red")
points(year, lower_ml, type="l", col = "red")

out = data.frame(cbind(year, rate_vh, fitted_vh, upper_vh, lower_vh, rate_h, fitted_h, upper_h, lower_h, rate_ml, fitted_ml, upper_ml, lower_ml))
write.csv(out, file = "test/data_interpolation/summary sheet/rate_hdi_with_95CI.csv")
