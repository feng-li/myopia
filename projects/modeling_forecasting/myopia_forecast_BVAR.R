rm(list = ls())
data_ts = read.csv("../rates_estimation/rate_country_age_urban_rural_all.csv",
                   header = TRUE)

## select year range
year = data_ts[data_ts$age_group==1,'year']

data_urban = c()
data_rural = c()
data_overall = c()

for (g in 1:4){
    data_urban = cbind(data_urban, data_ts[data_ts$age_group==g,'myopia_rate_urban'])
    data_rural = cbind(data_rural, data_ts[data_ts$age_group==g,'myopia_rate_rural'])
    data_overall = cbind(data_overall, data_ts[data_ts$age_group==g,'myopia_rate'])
}


year_forec = 2023:2050

######################################################################
## Change here to switch among urban, rural, overall

## Modeling urban data
# Urban forecasting
## data = data_urban
## lambda = 0.1
## sd_inflation_factor = 0.02 # Urban, Overall, the factors are manually adjusted utilizing bootstrapping methods to empirically estimate the confidence interval
## sd_shrink_factor = 1
## outfile = "../forc_urban_with_interval_4groups.csv"

## Rural forecasting
data = data_rural
lambda = 0.05
sd_inflation_factor = 0.005 # Rural
sd_shrink_factor = 0.7 # Rural
outfile = "../forc_rural_with_interval_4groups.csv"

## Overall forecasting
## data = data_overall
## lambda = 0.1
## sd_inflation_factor = 0.02 # Urban, Overall
## sd_shrink_factor = 1
## outfile = "../forc_overall_with_interval_4groups.csv"

######################################################################

rolling_window = 1
horizon = round(length(year_forec) / rolling_window)

data_new = NULL
forc_array = array(NA, c(horizon, 3, 4))
# install.packages("fields")
library("fields")
# install.packages("BVAR")
library("BVAR")

## logit transform
data_logit = log(data/(1-data))
for (nstep in 0:horizon){


    ## Using a spline regression to filter the main effects
    forc_reg = matrix(NA, rolling_window, 4)

    if(nstep >= 1){
        year = c(year, (year[length(year)] + 1):(year[length(year)] + rolling_window))
    }

    x = cbind(year, year^2)

    reg_fitted = data_logit
    reg_resid = data_logit
    reg_resid_orig = data
    for(g in 1:ncol(data)){

        reg1 = Tps(x, data_logit[, g], lambda = lambda)
        reg1_fitted = fitted(reg1)[,1]

        reg_fitted[, g] = reg1_fitted

        resid1 = residuals(reg1)
        reg_resid[, g] = resid1

        # Forecasting. For spline regression, the naive forecasting is used.
        forc_reg[, g] = reg1_fitted[length(reg1_fitted)]
    }

    ## Estimate using default priors and MH step
    nlags = 1
    mh_opts =  bv_mh(adjust_acc = TRUE, acc_lower = 0.2, acc_upper = 0.7, scale_hess = 0.2)
    resid_model <- bvar(reg_resid, lags = nlags, n_draw=20000,
                        mh = mh_opts, verbose = TRUE)

    # browser()
    fitted_resid_model = matrix(fitted(resid_model),,4)

    zeros = matrix(0, nlags, 4)
    fitted_resid_model = rbind(zeros, fitted_resid_model)

    fitted_ = reg_fitted + fitted_resid_model
    fitted_orig = 1 / (1 + exp(-fitted_))

    ##
    forc = predict(resid_model, horizon = rolling_window)
    ## plot(forc)
    ## plot(irf(x))


    ## Obtain the year base probabilistic forecasts
    forc_bvar_sample_logit = forc$fcast

    ## Add regression part
    reg_forc_mean = apply(fitted_[nrow(data),, drop = FALSE], 2, mean)

    for(g in 1:4){
        forc_bvar_sample_logit[,, g] = forc_bvar_sample_logit[,, g] + reg_forc_mean[g]
    }

    forc_bvar_sample_prob = 1/(1+exp(-forc_bvar_sample_logit))

    ## Final forecast and 95% HPD
    forc_quants = apply(forc_bvar_sample_prob,c(2,3), quantile, c(0.025,0.5,0.975))
    for(g in 1:4){
        forc_array[nstep, , g] = forc_quants[, , g]
    }


    forc_mean_logit = apply(forc_bvar_sample_logit, c(2, 3), mean)
    data_logit = rbind(data_logit, forc_mean_logit)

}

## HPD calibration with original fitted model
reg_sd0 = apply(reg_resid[1:nrow(data_urban),],2,sd)
reg_sd = reg_sd0 / 4 # Delta methods

forc_array_new = forc_array
fitted_mat = NULL
for(g in 1:4){

    f_mean = fitted_orig[1:nrow(data_urban), g]
    f_upper = f_mean + 1.96 * reg_sd[g] * sd_shrink_factor
    f_lower = f_mean - 1.96 * reg_sd[g] * sd_shrink_factor

    fitted_mat = cbind(fitted_mat, f_lower, f_mean, f_upper)

    ## Forecast mean and CI
    mean_g = forc_array[ ,2, g]
    lower_g = forc_array[ ,1, g]
    upper_g = forc_array[, 3, g]
    reg_sd_g = reg_sd[g]

    lower_g_new = mean_g - 1.96 * ((mean_g - lower_g) / 1.96 + reg_sd_g) * (1 + sd_inflation_factor * seq(1, length(year_forec))) * sd_shrink_factor
    upper_g_new = mean_g + 1.96 * ((upper_g - mean_g) / 1.96 + reg_sd_g)* (1 + sd_inflation_factor * seq(1, length(year_forec))) * sd_shrink_factor

    forc_array_new[, 1, g] = lower_g_new
    forc_array_new[, 3, g] = upper_g_new
}


forc_mat = matrix(forc_array_new, dim(forc_array_new)[1],
                  dim(forc_array_new)[2]*dim(forc_array_new)[3])


year_all = 1998:2050
out_mat = rbind(fitted_mat, forc_mat)
out_mat = cbind(year_all, out_mat)

write.table(out_mat, file = outfile, sep = ",", row.names = FALSE)

par(mfrow = c(2, 2))
for(g in 1:4){
    plot(year_all, out_mat[,1 + (g - 1) * 3 + 1],type="l",ylim=c(0,1), col = "blue")
    lines(year_all, out_mat[,2 + (g - 1) * 3 + 1],type="l",ylim=c(0,1), col = "red")
    lines(year_all, out_mat[,3 + (g - 1) * 3 + 1],type="l",ylim=c(0,1), col = "blue")
}
