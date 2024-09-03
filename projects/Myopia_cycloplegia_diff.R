library(dplyr)
library("fields")
library("forecast")

myopia = read.csv("data_processing/filtered_data_cycloplegia.csv", header = TRUE)

## URBAN CYCLOPLEGIA
par(mfrow = c(2, 2))
lambda = c(0.1, 0.1, 0.5, 0.05)
g = 0
for (col in c(4, 10, 16, 22)) # for groups
{
    g = g + 1
    g1 = myopia[c(2, col, col + 2)]
    names(g1) = c("year", "size", "rate")
    g1 = g1[complete.cases(g1), ] # remove NA
    g1 = sort_by(g1, g1[[1]]) # sort
    # Weighted mean by group
    wg1 <- g1 %>% group_by(year) %>% summarise(rate = sum(rate * size) / sum(size)) %>% as.data.frame
    print(wg1)


    year = wg1[, 1]
    data = wg1[, 2]

    if(g == 3)
    {
        # remove outlier
        # year = year[-1]
        # data = data[-1]
        x = year
        # x = cbind(year, year^2)
    }else
    {
    x = cbind(year, year^2)
    }

    # x = cbind(year)
    reg12 = Tps(x, data, lambda = lambda[g])
    fitted_ = fitted(reg12)[,1]

    resid12 = residuals(reg12)

    arima12 = auto.arima(resid12)
    fore = forecast(arima12, h = 2050 - year[length(year)])

    error_confidence_prior = 0.64
    sd12 = sd(data - fitted_) * error_confidence_prior
    ## sd12 = sd(fitted_)/4
    upper = fitted_ + 1.96 * sd12
    lower = fitted_ - 1.96 * sd12

    ## tweak to avoid negative intervals

    mean1 = fore$mean + fitted_[length(fitted_)]
    mean = c(fitted_, mean1)
    sd1 = seq(sd12, sd(fore$residuals), length.out = length(mean1))
    upper = c(upper, mean1 + 1.96 * sd1)
    lower = c(lower, mean1 - 1.96 * sd1)

    if(any(lower <= 0)){
    delta = abs(min(lower)) + 0.05
    lower = lower + delta
    }

    if(any(upper >= 1)){
    delta =  0.01
    upper[upper >= 1] = upper[upper >= 1] - delta
    }

    year_all = c(year, (year[length(year)] + 1):2050)
    plot(x = 1998:2050, y = rep(0, length(1998:2050)), ylim = c(0, 1), col = "white", ylab = "Prevalence")
    points(year, data, col = "blue")
    points(year_all, mean, col = "blue", type = "l")
    points(year_all, lower, col = "red", type = "l")
    points(year_all, upper, col = "red", type = "l")

    out = data.frame(cbind(year_all, mean, lower, upper))
    out_cycloplegia_urban = out
    write.table(out, file = paste0("cycloplegia_forecast_urban_group",
                                   g, ".csv"), row.names = FALSE, sep = ",")
}
dev.copy2pdf(file = "cycloplegia_forecast_urban_4groups.pdf")

## RURAL CYCLOPLEGIA
par(mfrow = c(2, 2))
lambda = c(0.1, 0.1, 0.5, 0.05)
error_confidence_prior = c(0.64, 0.48, 0.4, 0.36)
error_forc_confidence_prior = c(1, 0.9, 0.7, 0.6)

g = 0
for (col in c(7, 13, 19, 25)) # for groups
{
    g = g + 1
    g1 = myopia[c(2, col, col + 2)]
    names(g1) = c("year", "size", "rate")
    g1 = g1[complete.cases(g1), ] # remove NA
    g1 = sort_by(g1, g1[[1]]) # sort
    # Weighted mean by group
    wg1 <- g1 %>% group_by(year) %>% summarise(rate = sum(rate * size) / sum(size)) %>% as.data.frame
    print(wg1)


    year = wg1[, 1]
    data = wg1[, 2]

    if(g == 3)
    {
        # remove outlier
        # year = year[-1]
        # data = data[-1]
        x = year
        # x = cbind(year, year^2)
    }else
    {
    x = cbind(year, year^2)
    }

    # x = cbind(year)
    reg12 = Tps(x, data, lambda = lambda[g])
    fitted_ = fitted(reg12)[,1]

    resid12 = residuals(reg12)

    arima12 = auto.arima(resid12)
    fore = forecast(arima12, h = 2050 - year[length(year)])

    sd12 = sd(data - fitted_) * error_confidence_prior[g]
    ## sd12 = sd(fitted_)/4
    upper = fitted_ + 1.96 * sd12
    lower = fitted_ - 1.96 * sd12

    ## tweak to avoid negative intervals

    mean1 = fore$mean + fitted_[length(fitted_)]
    mean = c(fitted_, mean1)
    sd1 = seq(sd12, sd(fore$residuals) * error_forc_confidence_prior[g],
              length.out = length(mean1))
    upper = c(upper, mean1 + 1.96 * sd1)
    lower = c(lower, mean1 - 1.96 * sd1)

    if(any(lower <= 0)){
    delta = 0.01
    lower[lower <= 0] = lower[lower <= 0] + delta
    }

    if(any(upper >= 1)){
    delta =  0.01
    upper[upper >= 1] = upper[upper >= 1] - delta
    }

    year_all = c(year, (year[length(year)] + 1):2050)
    plot(x = 1998:2050, y = rep(0, length(1998:2050)), ylim = c(0, 1), col = "white", ylab = "Prevalence")
    points(year, data, col = "blue")
    points(year_all, mean, col = "blue", type = "l")
    points(year_all, lower, col = "red", type = "l")
    points(year_all, upper, col = "red", type = "l")

    out = data.frame(cbind(year_all, mean, lower, upper))
    out_cycloplegia_rural = out
    write.table(out, file = paste0("cycloplegia_forecast_rural_group",
                                   g, ".csv"), row.names = FALSE, sep = ",")
}
dev.copy2pdf(file = "cycloplegia_forecast_rural_4groups.pdf")

## URBAN NO CYCLOPLEGIA
rm(list = ls())
myopia = read.csv("data_processing/filtered_data_nocycloplegia.csv", header = TRUE)

error_confidence_prior


par(mfrow = c(2, 2))
lambda = c(0.1, 0.1, 0.5, 0.05)
error_confidence_prior = c(0.64, 0.36, 0.4, 0.36)
error_forc_confidence_prior = c(1, 0.8, 1, 1)

g = 0
for (col in c(4, 10, 16, 22)) # for groups
{
    g = g + 1
    g1 = myopia[c(2, col, col + 2)]
    names(g1) = c("year", "size", "rate")
    g1 = g1[complete.cases(g1), ] # remove NA
    g1 = sort_by(g1, g1[[1]]) # sort
    # Weighted mean by group
    wg1 <- g1 %>% group_by(year) %>% summarise(rate = sum(rate * size) / sum(size)) %>% as.data.frame
    print(wg1)


    year = wg1[, 1]
    data = wg1[, 2]

    if(g == 1)
    {
        # remove outlier
        # year = year[-1]
        # data = data[-1]
        x = year
        # x = cbind(year, year^2)
    }else
    {
    x = cbind(year, year^2)
    }

    # x = cbind(year)
    reg12 = Tps(x, data, lambda = lambda[g])
    fitted_ = fitted(reg12)[,1]

    resid12 = residuals(reg12)

    arima12 = auto.arima(resid12)
    fore = forecast(arima12, h = 2050 - year[length(year)])

    sd12 = sd(data - fitted_) * error_confidence_prior[g]
    ## sd12 = sd(fitted_)/4
    upper = fitted_ + 1.96 * sd12
    lower = fitted_ - 1.96 * sd12

    ## tweak to avoid negative intervals

    mean1 = fore$mean + fitted_[length(fitted_)]
    mean = c(fitted_, mean1)
    sd1 = seq(sd12, sd(fore$residuals) * error_forc_confidence_prior[g],
              length.out = length(mean1))
    upper = c(upper, mean1 + 1.96 * sd1)
    lower = c(lower, mean1 - 1.96 * sd1)

    if(any(lower <= 0)){
    delta = 0.01
    lower[lower <= 0] = lower[lower <= 0] + delta
    }

    if(any(upper >= 1)){
    delta =  0.01
    upper[upper >= 1] = upper[upper >= 1] - delta
    }

    year_all = c(year, (year[length(year)] + 1):2050)
    plot(x = 1998:2050, y = rep(0, length(1998:2050)), ylim = c(0, 1), col = "white", ylab = "Prevalence")
    points(year, data, col = "blue")
    points(year_all, mean, col = "blue", type = "l")
    points(year_all, lower, col = "red", type = "l")
    points(year_all, upper, col = "red", type = "l")

    out = data.frame(cbind(year_all, mean, lower, upper))
    out_nocycloplegia_urban = out
    write.table(out, file = paste0("nocycloplegia_forecast_urban_group",
                                   g, ".csv"), row.names = FALSE, sep = ",")
}
dev.copy2pdf(file = "nocycloplegia_forecast_urban_4groups.pdf")


## RURAL NO CYCLOPLEGIA
rm(list = ls())
myopia = read.csv("data_processing/filtered_data_nocycloplegia.csv", header = TRUE)

par(mfrow = c(2, 2))
lambda = c(0.1, 0.1, 0.5, 0.05)
error_confidence_prior = c(0.36, 0.36, 0.36, 0.36)
error_forc_confidence_prior = c(0.7, 0.6, 0.45, 0.45)

g = 0
for (col in c(7, 13, 19, 25)) # for groups
{
    g = g + 1
    g1 = myopia[c(2, col, col + 2)]
    names(g1) = c("year", "size", "rate")
    g1 = g1[complete.cases(g1), ] # remove NA
    g1 = sort_by(g1, g1[[1]]) # sort
    # Weighted mean by group
    wg1 <- g1 %>% group_by(year) %>% summarise(rate = sum(rate * size) / sum(size)) %>% as.data.frame
    print(wg1)


    year = wg1[, 1]
    data = wg1[, 2]

    ## if(g == 1)
    ## {
    ##     # remove outlier
    ##     # year = year[-1]
    ##     # data = data[-1]
    ##     x = year
    ##     # x = cbind(year, year^2)
    ## }else
    ## {
    ## x = cbind(year, year^2)
    ## }

    x = cbind(year)

    reg12 = Tps(x, data, lambda = lambda[g])
    fitted_ = fitted(reg12)[,1]

    resid12 = residuals(reg12)

    arima12 = auto.arima(resid12)
    fore = forecast(arima12, h = 2050 - year[length(year)])

    sd12 = sd(data - fitted_) * error_confidence_prior[g]
    ## sd12 = sd(fitted_)/4
    upper = fitted_ + 1.96 * sd12
    lower = fitted_ - 1.96 * sd12

    ## tweak to avoid negative intervals

    mean1 = fore$mean + fitted_[length(fitted_)]
    mean = c(fitted_, mean1)
    sd1 = seq(sd12, sd(fore$residuals) * error_forc_confidence_prior[g],
              length.out = length(mean1))
    upper = c(upper, mean1 + 1.96 * sd1)
    lower = c(lower, mean1 - 1.96 * sd1)



    if(any(lower <= 0)){
    delta = 0.01
    lower[lower <= 0] = lower[lower <= 0] + delta
    }

    if(any(upper >= 1)){
    delta =  0.01
    upper[upper >= 1] = upper[upper >= 1] - delta
    }

    year_all = c(year, (year[length(year)] + 1):2050)
    plot(x = 1998:2050, y = rep(0, length(1998:2050)), ylim = c(0, 1), col = "white", ylab = "Prevalence")
    points(year, data, col = "blue")
    points(year_all, mean, col = "blue", type = "l")
    points(year_all, lower, col = "red", type = "l")
    points(year_all, upper, col = "red", type = "l")

    out = data.frame(cbind(year_all, mean, lower, upper))
    out_nocycloplegia_rural = out
    write.table(out, file = paste0("nocycloplegia_forecast_rural_group",
                                   g, ".csv"), row.names = FALSE, sep = ",")
}
dev.copy2pdf(file = "nocycloplegia_forecast_rural_4groups.pdf")

######################################################################
## Calculate difference between cycloplegia and noncycloplegia effects.

## Estimate the urban mean difference over time
diff_group = data.frame(year = 1998:2023)
for(g in 1:4){
    data_cy = cycloplegia_urban[[g]]
    data_nocy = nocycloplegia_urban[[g]]

    df = full_join(data_cy,data_nocy,by = "year_all")

    df[, "meandiff"] = df$mean.x - df$mean.y

    diff = arrange(df, year_all) %>% na.omit

    idx = 1:(nrow(diff)-27)
    smg = diff[idx, ] # only select data before 2023

    lmdata = data.frame(x = cbind(smg$year_all),
                        data = smg$meandiff)
    reg = lm(data ~ x, data = lmdata)
    diff_group[, g + 1] = predict(reg, newdata = data.frame(x = 1998:2023))
}
colnames(diff_group) = c("year", "g1", "g2", "g3", "g4")
write.table(diff_group, file = "meandiff_cycloplegia_urban_4groups.csv", row.names = FALSE, sep = ",")


## Estimate the rural  mean difference over time
diff_group = data.frame(year = 1998:2023)
for(g in 1:4){
    data_cy = cycloplegia_rural[[g]]
    data_nocy = nocycloplegia_rural[[g]]

    df = full_join(data_cy,data_nocy,by = "year_all")

    df[, "meandiff"] = df$mean.x - df$mean.y

    diff = arrange(df, year_all) %>% na.omit

    idx = 1:(nrow(diff)-27)
    smg = diff[idx, ] # only select data before 2023

    lmdata = data.frame(x = cbind(smg$year_all),
                        data = smg$meandiff)
    reg = lm(data ~ x, data = lmdata)
    diff_group[, g + 1] = predict(reg, newdata = data.frame(x = 1998:2023))
}
colnames(diff_group) = c("year", "g1", "g2", "g3", "g4")
write.table(diff_group, file = "meandiff_cycloplegia_rural_4groups.csv", row.names = FALSE, sep = ",")


######################################################################
## Compare difference between cycloplegia and noncycloplegia effects.

## Load the filtered data
cycloplegia_urban = list()
cycloplegia_rural = list()
nocycloplegia_urban = list()
nocycloplegia_rural = list()
adjusted_urban = list()
adjusted_rural = list()

for(g in 1:4){
    cycloplegia_urban[[g]] = read.csv(paste0("cycloplegia_forecast_urban_group", g, ".csv"), header = TRUE)
    cycloplegia_rural[[g]] = read.csv(paste0("cycloplegia_forecast_rural_group", g, ".csv"), header = TRUE)
    nocycloplegia_urban[[g]] = read.csv(paste0("nocycloplegia_forecast_urban_group", g, ".csv"), header = TRUE)
    nocycloplegia_rural[[g]] = read.csv(paste0("nocycloplegia_forecast_rural_group", g, ".csv"), header = TRUE)

    adjusted_urban[[g]] = read.csv("forc_urban_with_interval_4groups.csv", header = TRUE)[,c(1, (2 +  (g - 1) * 3):(4 +  (g - 1) * 3))]

    colnames(adjusted_urban[[g]]) = c("year_all", "lower", "mean", "upper")

    adjusted_rural[[g]] = read.csv("forc_rural_with_interval_4groups.csv", header = TRUE)[,c(1, (2 +  (g - 1) * 3):(4 +  (g - 1) * 3))]

    colnames(adjusted_rural[[g]]) = c("year_all", "lower", "mean", "upper")

}


## Plot
## Urban difference

par(mfrow = c(1, 2))
lty = c("dotted", "dashed", "solid")
d = 0
for(name in c("cycloplegia_urban", "nocycloplegia_urban", "adjusted_urban")){
    d = d + 1
    for(g in 1:4){
        data = get(name)[[g]]
        idx = 1:(nrow(data)-27)
        smg = data[idx, ] # only select data before 2023

        year = smg[, 'year_all']
        myopia = smg[, 'mean']

        if(g == 1 & d == 1){
            plot(year, myopia, xlim = c(1998, 2023), ylim = c(0, 1),
                 type = "l", col = g, lty = lty[d], lwd = 4,
                 main = "cycloplegia (dotted), nocycloplegia (dashed) and adjusted (solid) difference for 4 groups",
                 ylab = "Myopia (Urban)")
        } else
        {
            points(year, myopia, xlim = c(1998, 2023), ylim = c(0, 1),
                   type = "l", col = g, lty = lty[d], lwd = 4)
        }
    }
}


## Rural difference
lty = c("dotted", "dashed", "solid")
d = 0
for(name in c("cycloplegia_rural", "nocycloplegia_rural", "adjusted_rural")){
    d = d + 1
    for(g in 1:4){
        data = get(name)[[g]]
        idx = 1:(nrow(data)-27)
        smg = data[idx, ] # only select data before 2023

        year = smg[, 'year_all']
        myopia = smg[, 'mean']

        if(g == 1 & d == 1){
            plot(year, myopia, xlim = c(1998, 2023), ylim = c(0, 1),
                 type = "l", col = g, lty = lty[d], lwd = 4,
                 main = "cycloplegia (dotted), nocycloplegia (dashed) and adjusted (solid) difference for 4 groups",
                 ylab = "Myopia (Rural)")
        } else
        {
            points(year, myopia, xlim = c(1998, 2023), ylim = c(0, 1),
                   type = "l", col = g, lty = lty[d], lwd = 4)
        }
    }
}
