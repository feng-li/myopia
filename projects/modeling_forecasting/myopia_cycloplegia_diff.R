library("dplyr")
library("fields")
library("forecast")

myopia = read.csv("../data_processing/filtered_data_cycloplegia.csv", header = TRUE)

## URBAN CYCLOPLEGIA
par(mfrow = c(2, 2))
lambda = c(0.1, 0.1, 0.5, 0.05)
g = 0


cycloplegia_predict_urban_insample = data.frame(year = 1998:2023)
cycloplegia_size_urban = c()
for (col in c(4, 10, 16, 22)) # four groups
{
    g = g + 1
    g1 = myopia[c(2, col, col + 2)]
    names(g1) = c("year", "size", "rate")
    g1 = g1[complete.cases(g1), ] # remove NA
    g1 = g1 %>% arrange(year) # sort
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
        x1 = 1998:2023
        x_new = cbind(x1)
        # x = cbind(year, year^2)
    }else
    {
        x = cbind(year, year^2)
        x1 = 1998:2023
        x_new = cbind(x1, x1 ^ 2)
    }

    reg12 = Tps(x, data, lambda = lambda[g])
    fitted_ = fitted(reg12)[,1]

    cycloplegia_size_urban = c(cycloplegia_size_urban, length(data))

    # in sample prediction
    y1 = predict(reg12, x = x_new)
    cycloplegia_predict_urban_insample[, paste0("g", g)] = y1

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
    write.table(out, file = paste0("../cycloplegia_forecast_urban_group",
                                   g, ".csv"), row.names = FALSE, sep = ",")
}

write.table(cycloplegia_predict_urban_insample, file = "../cycloplegia_predict_urban_insample.csv",
            row.names = FALSE, sep = ",")


dev.copy2pdf(file = "cycloplegia_forecast_urban_4groups.pdf")

## RURAL CYCLOPLEGIA
rm(list = ls())
myopia = read.csv("../data_processing/filtered_data_cycloplegia.csv", header = TRUE)
par(mfrow = c(2, 2))
lambda = c(0.1, 0.1, 0.5, 0.05)
error_confidence_prior = c(0.64, 0.48, 0.4, 0.36)
error_forc_confidence_prior = c(1, 0.9, 0.7, 0.6)

cycloplegia_predict_rural_insample = data.frame(year = 1998:2023)
cycloplegia_size_rural = c()

g = 0
for (col in c(7, 13, 19, 25)) # for groups
{
    g = g + 1
    g1 = myopia[c(2, col, col + 2)]
    names(g1) = c("year", "size", "rate")
    g1 = g1[complete.cases(g1), ] # remove NA
    g1 = g1 %>% arrange(year) # sort
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
        x1 = 1998:2023
        x_new = cbind(x1)
        # x = cbind(year, year^2)
    }else
    {
        x = cbind(year, year^2)
        x1 = 1998:2023
        x_new = cbind(x1, x1 ^ 2)
    }

    # x = cbind(year)
    reg12 = Tps(x, data, lambda = lambda[g])
    fitted_ = fitted(reg12)[,1]

    cycloplegia_size_rural = c(cycloplegia_size_rural, length(data))

    # in sample prediction
    y1 = predict(reg12, x = x_new)
    cycloplegia_predict_rural_insample[, paste0("g", g)] = y1

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
    write.table(out, file = paste0("../cycloplegia_forecast_rural_group",
                                   g, ".csv"), row.names = FALSE, sep = ",")
}
dev.copy2pdf(file = "cycloplegia_forecast_rural_4groups.pdf")

write.table(cycloplegia_predict_rural_insample, file = "../cycloplegia_predict_rural_insample.csv",
            row.names = FALSE, sep = ",")

## URBAN NO CYCLOPLEGIA
rm(list = ls())
myopia = read.csv("../data_processing/filtered_data_nocycloplegia.csv", header = TRUE)
nocycloplegia_predict_urban_insample = data.frame(year = 1998:2023)


par(mfrow = c(2, 2))
lambda = c(0.1, 0.1, 0.5, 0.05)
error_confidence_prior = c(0.64, 0.36, 0.4, 0.36)
error_forc_confidence_prior = c(1, 0.8, 1, 1)
nocycloplegia_size_urban = c()

g = 0
for (col in c(4, 10, 16, 22)) # for groups
{
    g = g + 1
    g1 = myopia[c(2, col, col + 2)]
    names(g1) = c("year", "size", "rate")
    g1 = g1[complete.cases(g1), ] # remove NA
    g1 = g1 %>% arrange(year) # sort
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
        x1 = 1998:2023
        x_new = cbind(x1)
        # x = cbind(year, year^2)
    }else
    {
        x = cbind(year, year^2)
        x1 = 1998:2023
        x_new = cbind(x1, x1 ^ 2)
    }

    # x = cbind(year)
    reg12 = Tps(x, data, lambda = lambda[g])
    fitted_ = fitted(reg12)[,1]

    nocycloplegia_size_urban = c(nocycloplegia_size_urban, length(data))

    # in sample prediction
    y1 = predict(reg12, x = x_new)
    nocycloplegia_predict_urban_insample[, paste0("g", g)] = y1

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
    write.table(out, file = paste0("../nocycloplegia_forecast_urban_group",
                                   g, ".csv"), row.names = FALSE, sep = ",")
}
dev.copy2pdf(file = "nocycloplegia_forecast_urban_4groups.pdf")

write.table(nocycloplegia_predict_urban_insample, file = "../nocycloplegia_predict_urban_insample.csv",
            row.names = FALSE, sep = ",")


## RURAL NO CYCLOPLEGIA
rm(list = ls())
myopia = read.csv("../data_processing/filtered_data_nocycloplegia.csv", header = TRUE)

par(mfrow = c(2, 2))
lambda = c(0.1, 0.1, 0.5, 0.05)
error_confidence_prior = c(0.36, 0.36, 0.36, 0.36)
error_forc_confidence_prior = c(0.7, 0.6, 0.45, 0.45)
nocycloplegia_predict_rural_insample = data.frame(year = 1998:2023)
nocycloplegia_size_rural = c()

g = 0
for (col in c(7, 13, 19, 25)) # for groups
{
    g = g + 1
    g1 = myopia[c(2, col, col + 2)]
    names(g1) = c("year", "size", "rate")
    g1 = g1[complete.cases(g1), ] # remove NA
    g1 = g1 %>% arrange(year) # sort
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

    x1 = 1998:2023
    x_new = cbind(x1)

    reg12 = Tps(x, data, lambda = lambda[g])
    fitted_ = fitted(reg12)[,1]

    nocycloplegia_size_rural = c(nocycloplegia_size_rural, length(data))

    # in sample prediction
    y1 = predict(reg12, x = x_new)
    nocycloplegia_predict_rural_insample[, paste0("g", g)] = y1

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
    write.table(out, file = paste0("../nocycloplegia_forecast_rural_group",
                                   g, ".csv"), row.names = FALSE, sep = ",")
}
dev.copy2pdf(file = "nocycloplegia_forecast_rural_4groups.pdf")

write.table(nocycloplegia_predict_rural_insample, file = "../nocycloplegia_predict_rural_insample.csv",
            row.names = FALSE, sep = ",")

######################################################################
## Calculate difference between cycloplegia and noncycloplegia effects.

## Load the filtered data
cycloplegia_urban = list()
cycloplegia_rural = list()
nocycloplegia_urban = list()
nocycloplegia_rural = list()

for(g in 1:4){
    cycloplegia_urban[[g]] = read.csv(paste0("../cycloplegia_forecast_urban_group", g, ".csv"), header = TRUE)
    cycloplegia_rural[[g]] = read.csv(paste0("../cycloplegia_forecast_rural_group", g, ".csv"), header = TRUE)
    nocycloplegia_urban[[g]] = read.csv(paste0("../nocycloplegia_forecast_urban_group", g, ".csv"), header = TRUE)
    nocycloplegia_rural[[g]] = read.csv(paste0("../nocycloplegia_forecast_rural_group", g, ".csv"), header = TRUE)
}


## Estimate the urban mean difference over time
diff_group_urban = data.frame(year = 1998:2023)
for(g in 1:4){
    data_cy = cycloplegia_urban[[g]]
    data_nocy = nocycloplegia_urban[[g]]

    df = full_join(data_cy,data_nocy,by = "year_all")

    ## cy_mean = df[df$year_all <= 2023, c('year_all', 'mean.x')] %>% na.omit
    ## interpolation = lm(mean.x~year_all,data=cy_mean)
    ## predict(interpolation, newdata = data.frame(year_all = 1998:2023))

    ## nocy_mean = df$mean.y

    df[, "meandiff"] = df$mean.x - df$mean.y

    diff = arrange(df, year_all) %>% na.omit

    idx = 1:(nrow(diff)-27)
    smg = diff[idx, ] # only select data before 2023

    lmdata = data.frame(x = cbind(smg$year_all),
                        data = smg$meandiff)
    reg = lm(data ~ x, data = lmdata)
    diff_group_urban[, g + 1] = predict(reg, newdata = data.frame(x = 1998:2023))
}
colnames(diff_group_urban) = c("year", "g1", "g2", "g3", "g4")
write.table(diff_group_urban, file = "../meandiff_cycloplegia_urban_4groups.csv", row.names = FALSE, sep = ",")


## Estimate the rural  mean difference over time
diff_group_rural = data.frame(year = 1998:2023)
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
    diff_group_rural[, g + 1] = predict(reg, newdata = data.frame(x = 1998:2023))
}
colnames(diff_group_rural) = c("year", "g1", "g2", "g3", "g4")
write.table(diff_group_rural, file = "../meandiff_cycloplegia_rural_4groups.csv", row.names = FALSE, sep = ",")


## Estimate the urban mean sum over time
sum_group_urban = data.frame(year = 1998:2023)
for(g in 1:4){
    data_cy = cycloplegia_urban[[g]]
    data_nocy = nocycloplegia_urban[[g]]

    df = full_join(data_cy,data_nocy,by = "year_all")

    df[, "meansum"] = df$mean.x + df$mean.y

    diff = arrange(df, year_all) %>% na.omit

    idx = 1:(nrow(diff)-27)
    smg = diff[idx, ] # only select data before 2023

    lmdata = data.frame(x = cbind(smg$year_all),
                        data = smg$meansum)
    reg = lm(data ~ x, data = lmdata)
    sum_group_urban[, g + 1] = predict(reg, newdata = data.frame(x = 1998:2023))
}
colnames(sum_group_urban) = c("year", "g1", "g2", "g3", "g4")
write.table(sum_group_urban, file = "../meansum_cycloplegia_urban_4groups.csv", row.names = FALSE, sep = ",")


## Estimate the rural mean sum over time
sum_group_rural = data.frame(year = 1998:2023)
for(g in 1:4){
    data_cy = cycloplegia_rural[[g]]
    data_nocy = nocycloplegia_rural[[g]]

    df = full_join(data_cy,data_nocy,by = "year_all")

    df[, "meansum"] = df$mean.x + df$mean.y

    diff = arrange(df, year_all) %>% na.omit

    idx = 1:(nrow(diff)-27)
    smg = diff[idx, ] # only select data before 2023

    lmdata = data.frame(x = cbind(smg$year_all),
                        data = smg$meansum)
    reg = lm(data ~ x, data = lmdata)
    sum_group_rural[, g + 1] = predict(reg, newdata = data.frame(x = 1998:2023))
}
colnames(sum_group_rural) = c("year", "g1", "g2", "g3", "g4")
write.table(sum_group_rural, file = "../meansum_cycloplegia_rural_4groups.csv", row.names = FALSE, sep = ",")


######################################################################
## Compare difference between cycloplegia and noncycloplegia effects.
weight_cycloplegia_urban = colMeans(2 * abs(diff_group_urban)/sum_group_urban)
weight_cycloplegia_rural = colMeans(2 * abs(diff_group_rural)/sum_group_rural)
# weight_cycloplegia_rural = c(2, 0.95, 0.5, 0.95, 0.5)

cycloplegia_predict_urban_insample = read.csv("../cycloplegia_predict_urban_insample.csv", header = TRUE)
cycloplegia_predict_rural_insample = read.csv("../cycloplegia_predict_rural_insample.csv", header = TRUE)
nocycloplegia_predict_urban_insample = read.csv("../nocycloplegia_predict_urban_insample.csv", header = TRUE)
nocycloplegia_predict_rural_insample = read.csv("../nocycloplegia_predict_rural_insample.csv", header = TRUE)


myopia_urban_adj = (cycloplegia_predict_urban_insample[,2:5]*matrix(weight_cycloplegia_urban[2:5],26,4,byrow = TRUE) +
                    nocycloplegia_predict_urban_insample[,2:5]*matrix(1 - weight_cycloplegia_urban[2:5],26,4,byrow = TRUE))
myopia_urban_adj['year'] = 1998:2023

myopia_rural_adj = (cycloplegia_predict_rural_insample[,2:5]*matrix(weight_cycloplegia_rural[2:5],26,4,byrow = TRUE) +
                    nocycloplegia_predict_rural_insample[,2:5]*matrix(1 - weight_cycloplegia_rural[2:5],26,4,byrow = TRUE))
myopia_rural_adj['year'] = 1998:2023

write.table(myopia_urban_adj, file = "../myopia_urban_adjust_singlecase_4groups.csv", row.names = FALSE, sep = ",")
write.table(myopia_rural_adj, file = "../myopia_rural_adjust_singlecase_4groups.csv", row.names = FALSE, sep = ",")
