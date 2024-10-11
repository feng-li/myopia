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
    cycloplegia_urban[[g]] = read.csv(paste0("../cycloplegia_forecast_urban_group", g, ".csv"), header = TRUE)
    cycloplegia_rural[[g]] = read.csv(paste0("../cycloplegia_forecast_rural_group", g, ".csv"), header = TRUE)
    nocycloplegia_urban[[g]] = read.csv(paste0("../nocycloplegia_forecast_urban_group", g, ".csv"), header = TRUE)
    nocycloplegia_rural[[g]] = read.csv(paste0("../nocycloplegia_forecast_rural_group", g, ".csv"), header = TRUE)
}

for(g in 1:4){

    adjusted_urban[[g]] = read.csv("../forc_urban_with_interval_4groups.csv", header = TRUE)[,c(1, (2 +  (g - 1) * 3):(4 +  (g - 1) * 3))]
    colnames(adjusted_urban[[g]]) = c("year_all", "lower", "mean", "upper")

    adjusted_rural[[g]] = read.csv("../forc_rural_with_interval_4groups.csv", header = TRUE)[,c(1, (2 +  (g - 1) * 3):(4 +  (g - 1) * 3))]
    colnames(adjusted_rural[[g]]) = c("year_all", "lower", "mean", "upper")

}
myopia_urban_adj = read.csv(file = "../myopia_urban_adjust_singlecase_4groups.csv", header = TRUE)
myopia_rural_adj = read.csv(file = "../myopia_rural_adjust_singlecase_4groups.csv", header = TRUE)

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
                 main = "Single cycloplegia (dotted), nocycloplegia (dashed), \n adjusted (circles) and globally aggregated (solid) for 4 groups",
                 ylab = "Myopia (Urban)")
        } else
        {
            points(year, myopia, xlim = c(1998, 2023), ylim = c(0, 1),
                   type = "l", col = g, lty = lty[d], lwd = 4)

            points(myopia_urban_adj$year, myopia_urban_adj[, g], xlim = c(1998, 2023), ylim = c(0, 1),
                   type = "p", col = g, lty = 6, lwd = 4)
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
                 main = "Single cycloplegia (dotted), nocycloplegia (dashed), \n adjusted (circles) and globally aggregated (solid) for 4 groups",
                 ylab = "Myopia (Rural)")
        } else
        {
            points(year, myopia, xlim = c(1998, 2023), ylim = c(0, 1),
                   type = "l", col = g, lty = lty[d], lwd = 4)

            points(myopia_rural_adj$year, myopia_rural_adj[, g], xlim = c(1998, 2023), ylim = c(0, 1),
                   type = "p", col = g, lty = 6, lwd = 4)

        }
    }
}

dev.copy2pdf(file="adjusted_urban_rural_weights.pdf")

## Plot Adjusted only
par(mfrow = c(1, 2))
for(name in "adjusted_urban"){
    for(g in 1:4){
        data = get(name)[[g]]
        idx = 1:(nrow(data)-27)
        smg = data[idx, ] # only select data before 2023

        year = smg[, 'year_all']
        myopia = smg[, 'mean']

        if(g == 1 ){
            plot(year, myopia, xlim = c(1998, 2023), ylim = c(0, 1),
                 type = "l", col = g, lty = "solid", lwd = 4,
                 main = "Adjusted (solid) difference for 4 groups",
                 ylab = "Myopia (Urban)")
        } else
        {
            points(year, myopia, xlim = c(1998, 2023), ylim = c(0, 1),
                   type = "l", col = g, lty = "solid", lwd = 4)
        }
    }
}

## Rural difference
for(name in "adjusted_rural"){
    for(g in 1:4){
        data = get(name)[[g]]
        idx = 1:(nrow(data)-27)
        smg = data[idx, ] # only select data before 2023

        year = smg[, 'year_all']
        myopia = smg[, 'mean']

        if(g == 1){
            plot(year, myopia, xlim = c(1998, 2023), ylim = c(0, 1),
                 type = "l", col = g, lty = "solid", lwd = 4,
                 main = "Adjusted (solid) difference for 4 groups",
                 ylab = "Myopia (Rural)")
        } else
        {
            points(year, myopia, xlim = c(1998, 2023), ylim = c(0, 1),
                   type = "l", col = g, lty = "solid", lwd = 4)
        }
    }
}

dev.copy2pdf(file="adjusted_urban_rural_different_weights_clean.pdf")
