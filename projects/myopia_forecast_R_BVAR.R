# Urban forecasting

rm(list = ls())
data_ts = read.csv("test/data_interpolation/myopia_rate_urban.csv",
                   header = TRUE)
# select year range
year = data_ts[1:22, 1]
data = data_ts[1:22,2:5]
year_forec = 2020:2050

## logit transform
data_logit = log(data/(1-data))

# Using a spline regression to filter the main effects
#install.packages("fields")
library("fields")

resid = data
forc_reg = matrix(NA, 31, 4)
par(mfrow = c(2, 2))
x = cbind(year, year^2)
reg1_fitted_orig_mat = data
for(g in 1:ncol(data)){
    reg1 = Tps(year, data_logit[, g], lambda = 0.1) # change lambda to achieve better smoothness
    reg1_fitted = fitted(reg1)[,1]
    reg1_fitted_orig = 1 / (1 + exp(-reg1_fitted))

    plot(year, reg1_fitted_orig, type = "l", ylim = c(0, 1), lwd = 2, col = "red", xlab = "Year", ylab = paste("Group", g))
    points(year, data[, g], col = "blue")

    resid1 = residuals(reg1)
    reg1_fitted_orig_mat[, g] = reg1_fitted_orig

    resid[, g] = resid1

    # For thin plate spline, naive forecast used: the last value is used as forecast for the regression component
    forc_reg[, g] = reg1_fitted[length(reg1_fitted)]

}
dev.copy2pdf(file = "Fitted_by_year.pdf")

## Groupped plot
time_group = list (
    1998:2003,
    2004:2007,
    2008:2011,
    2012:2015,
    2016:2019
)

par(mfrow = c(2, 2))
for(g in 1:ncol(data)){
    x = 1:length(time_group)
    y = lapply(time_group, function(x) mean(reg1_fitted_orig_mat[year%in%x, g]))

    plot(x, y, type = "b", ylim = c(0, 1), axes = FALSE, lwd = 2, col = "blue", xlab = "Year", ylab = paste("Group", g))
    axis(1, at = x,
         labels = c("1998-2003", "2004-2007", "2008-2011", "2012-2015", "2016-2019"))
    axis(2)
}
dev.copy2pdf(file = "Fitted_by_groupped_year.pdf")


## Load the package
# install.packages("BVAR")
library("BVAR")

# Estimate using default priors and MH step
x <- bvar(resid, lags = 3, n_draw=50000)

# Check convergence via trace and density plots
plot(x)

# predict(x) <- predict(x, horizon = 29)
# Plot forecasts and impulse responses

forc = predict(x, horizon = 31)
plot(forc)
plot(irf(x))

# Obtain the year base probabilistic forecasts
forc_sample_logit = forc$fcast

pred_sample_prob = 1/(1+exp(-forc_sample_logit))

# Now I am only interested in the mean value of five year forecast
years = c(2020, seq(2025, 2050, 5))

# mean for five-year forecast
mean5 = matrix(NA, 7, 5)
mean5[, 1] = years

mean1 = apply(pred_sample_prob, c(2, 3), mean)

idx = 1
mean5[idx, 2:5] = mean1[1, ] # Using mean instead of median
for (y in seq(2, 31, 5))
{
    idx = idx + 1
    mean5[idx, 2:5] = apply(mean1[y:(y + 4), ], 2, mean)
}

# The long forecasting error is not stable, we use the first 7 to approximate the five-year HPD
pred_quants = apply(pred_sample_prob, c(2, 3),
                          quantile, BVAR:::quantile_check(0.84))


lower_diff = pred_quants[1,1:7, ] - pred_quants[2,1:7, ]
upper_diff = pred_quants[3,1:7, ] - pred_quants[2,1:7, ]

quants5_lower = mean5[, 2:5] + lower_diff
quants5_upper = mean5[, 2:5] + upper_diff

# Save the output
colnames(mean5) = c("year", "group 1", "group 2", "group 3", "group 4")
write.csv(mean5, "forecast_mean_2020-2050_4groups.csv", row.names=FALSE)

quants5_mat = cbind(quants5_lower, quants5_upper)
colnames(quants5_mat) = c("group 1 lower", "group 2 lower",
                                "group 3 lower", "group 4 lower",
                                "group 1 upper", "group 2 upper",
                                "group 3 upper", "group 4 upper")
write.csv(quants5_mat, "forecast_interval_2020-2050_4groups.csv", row.names=FALSE)

# install.packages("ggplot2")
library("ggplot2")

plotlist = list()
# pdf(file = "Prevalence_forecasting.pdf")
for (i in 1:4) {

    g_curr=as.data.frame(cbind(mean5[,c(1, i + 1)],
                               quants5_lower[, i],
                               quants5_upper[, i]))
    names(g_curr)=c("year", "mean","lower","upper")

    p <- ggplot(g_curr, aes(x=year, y=mean)) +
        geom_line(linewidth=2, color="red") +
        ylim(0, 1)+
        xlab("Year") + ylab("Prevalence")+
        geom_point(shape=16, fill="purple", size=3) +
        geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5, linewidth=1,
                      position=position_dodge(width = 0.5), color="purple")
                                        # print(p)
    plotlist[[i]] = p
}

library("ggpubr")
figure <- ggarrange(plotlist = plotlist,
                    labels = c("Group 1", "Group 2", "Group3", "Group 4"),
                    ncol = 2, nrow = 2)
print(figure)

## dev.off()

# Rural forecasting

data_ts_rural = read.csv("myopia_rate_rural_revised.csv",
                         header = TRUE)
data_ts_rural

# Load the package
# install.packages("BVAR")
library("BVAR")

data_rural = data_ts_rural[1:22,2:5]

data_logit_rural = log(data_rural/(1-data_rural))

# Estimate using default priors and MH step
x_rural <- bvar(data_logit_rural, lags = 1, n_draw=50000)

# Check convergence via trace and density plots
plot(x_rural)

# predict(x) <- predict(x, horizon = 29)
# Plot forecasts and impulse responses

options(repr.plot.width=10, repr.plot.height=10)
forc_rural = predict(x_rural, horizon = 31)
plot(forc_rural)
plot(irf(x_rural))

# Obtain the year base probabilistic forecasts
forc_sample_logit_rural = forc_rural$fcast
pred_sample_prob_rural = 1/(1+exp(-forc_sample_logit_rural))

# Now I am only interested in the mean value of five year forecast
years = c(2020, seq(2025, 2050, 5))

# mean for five-year forecast
mean5_rural = matrix(NA, 7, 5)
mean5_rural[, 1] = years

mean1_rural = apply(pred_sample_prob_rural, c(2, 3), mean)

idx = 1
mean5_rural[idx, 2:5] = mean1_rural[1, ] # Using mean instead of median
for (y in seq(2, 31, 5))
{
    idx = idx + 1
    mean5_rural[idx, 2:5] = apply(mean1_rural[y:(y + 4), ], 2, mean)
}

# The long forecasting error is not stable, we use the first 7 to approximate the five-year HPD
pred_quants_rural = apply(pred_sample_prob_rural, c(2, 3),
                          quantile, BVAR:::quantile_check(0.84))


lower_diff = pred_quants_rural[1,1:7, ] - pred_quants_rural[2,1:7, ]
upper_diff = pred_quants_rural[3,1:7, ] - pred_quants_rural[2,1:7, ]

quants5_rural_lower = mean5_rural[, 2:5] + lower_diff
quants5_rural_upper = mean5_rural[, 2:5] + upper_diff

# Save the output
colnames(mean5_rural) = c("year", "group 1", "group 2", "group 3", "group 4")
write.csv(mean5_rural, "forecast_mean_rural_2020-2050_4groups.csv", row.names=FALSE)

quants5_rural_mat = cbind(quants5_rural_lower, quants5_rural_upper)
colnames(quants5_rural_mat) = c("group 1 lower", "group 2 lower",
                                "group 3 lower", "group 4 lower",
                                "group 1 upper", "group 2 upper",
                                "group 3 upper", "group 4 upper")
write.csv(quants5_rural_mat, "forecast_interval_rural_2020-2050_4groups.csv", row.names=FALSE)

# install.packages("ggplot2")
library("ggplot2")

for (i in 1:4) {
    pdf(paste("Prevalence_forecasting_rural_group_",i,".pdf",sep=""))

    g_curr=as.data.frame(cbind(mean5_rural[,c(1, i + 1)],
                               quants5_rural_lower[, i],
                               quants5_rural_upper[, i]))
    names(g_curr)=c("year", "mean","lower","upper")

    p<- ggplot(g_curr, aes(x=year, y=mean)) +
      geom_line(linewidth=2, color="red") +
      ylim(0, 1)+
      xlab("Year") + ylab("Prevalence")+
      geom_point(shape=16, fill="purple", size=3) +
      geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5, linewidth=1,
                    position=position_dodge(width = 0.5), color="purple")
    print(p)

    dev.off()
    }
