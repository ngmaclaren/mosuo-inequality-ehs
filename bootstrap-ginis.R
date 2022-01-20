library(ineq)
library(boot)

### For plotting
if(!(dir.exists("img"))) dir.create("img")
matcol <- "red3"
patcol <- "darkslategray"
refcol <- "dodgerblue"
###

houses <- read.csv("./data/houses-assets-ngm.csv")
villages <- read.csv("./data/villages-ngm.csv")

## here, aggregate the three worth variables by village and merge to the village data set
worthcols <- c("NetWorth", "ModernWorth", "FarmWorth")
for(col in worthcols) {
    agg <- aggregate(houses[, col], by = list(Village = houses[, "Village"]),
                     function(x) mean(x, na.rm = TRUE))
    colnames(agg)[2] <- col#paste0("mean_", col)
    villages <- merge(villages, agg, by = "Village")
}
colnames(villages)[which(colnames(villages) == "mean_HHIncome")] <- "HHIncomeTotalLastYear"

### Bootstrap Ginis
nboots <- 5000

ginicols <- c("HHIncomeTotalLastYear", "NetWorth", "ModernWorth", "FarmWorth")
popginis <- data.frame(# no grouping by village: the whole sample
    var = ginicols, est = NA, ll = NA, ul = NA)

for(i in 1:nrow(popginis)) {
    v <- popginis$var[i]
    boot_ <- boot(houses[, v], function(x, i) Gini(x[i], corr = FALSE, na.rm = TRUE), nboots)
    CI_ <- boot.ci(boot_, type = "basic")
    popginis[i, "est"] <- CI_$t0
    popginis[i, "ll"] <- CI_$basic[4]
    popginis[i, "ul"] <- CI_$basic[5]
}

vils <- unique(houses$Village)
vilginis <- vector("list", length(vils))
names(vilginis) <- vils

for(i in 1:length(vils)) {
    df <- houses[houses$Village == vils[i], ]
    boots <- data.frame(vil = vils[i], var = ginicols, est = NA, ll = NA, ul = NA)

    for(j in 1:nrow(popginis)) {
        v <- popginis$var[j]
        boot_ <- boot(df[, v], function(x, i) Gini(x[i], corr = FALSE, na.rm = TRUE), nboots)
        CI_ <- boot.ci(boot_, type = "basic")
        boots[j, "est"] <- CI_$t0
        boots[j, "ll"] <- CI_$basic[4]
        boots[j, "ul"] <- CI_$basic[5]
    }

    vilginis[[i]] <- boots

}

vilginis <- do.call(rbind, c(vilginis, make.row.names = FALSE))

### Make Plots
plotvars <- unique(vilginis$var)
labs <- c("Household Income", "Total Value of Inventoried Assets", "Value of Modern Assets", "Value of Farm Animal Assets")
ylabs <- paste0("Gini: ", labs)
xlabs <- paste0("Mean ", labs)
pchs <- 15:18

##png("./img/ginis-with-CIs-and-HH-nolog.png", width = 12, height = 12, units = "in", res = 180, type = "cairo-png")
par(mfrow = c(2, 2))
for(i in 1:length(plotvars)) {
    village_order <- villages[order(villages[, ginicols[i]]), c("Village", "Lineality", ginicols[i])]
    village_order$sequence <- 1:nrow(village_order)
    
    plotginis <- vilginis[vilginis$var == plotvars[i], ]
    plotginis <- merge(plotginis, village_order, by.x = "vil", by.y = "Village")
    plotginis <- plotginis[order(plotginis$sequence), ]
    plotginis$color <- ifelse(plotginis$Lineality == "Matrilineal", matcol, patcol)

    ## this is the main plot
    par(mar = c(5, 4, 4, 4) + 0.3)
    altpchs <- c(0, 1, 2, 5)
    hhdf <- houses[, c("Village", "Lineality", plotvars[i])]
    ##hhdf[, plotvars[i]] <- hhdf[, plotvars[i]] + 1
    hhdf$sequence <- sapply(hhdf[, "Village"], function(x) plotginis$sequence[plotginis$vil == x])
    hhdf$color <- ifelse(hhdf$Lineality == "Matrilineal", matcol, patcol)
    plot(jitter(hhdf[, "sequence"]), hhdf[, plotvars[i]], col = hhdf$color,
         cex = 1, pch = altpchs[i],
         xlab = xlabs[i], ylab = labs[i], xaxt = "n", yaxt = "n")#, log = "y")
    axis(1, at = plotginis$sequence, labels = round(plotginis[, ginicols[i]]))
    axis(2, at = axTicks(2), labels = format(axTicks(2), scientific = TRUE))
    mtext(LETTERS[i], side = 3, line = .5, adj = -.15, font = 2, cex = 1.25)
    if(i == 1) legend("topleft", pch = pchs[i], col = c(matcol, patcol),
                      legend = c("Matrilineal", "Patrilineal"), bty = "n")
    
    par(new = TRUE)
    plot(est ~ sequence, data = plotginis, pch = pchs[i], cex = 3, col = plotginis$color,
         axes = FALSE, xlab = "", ylab = "",
         ylim = c(min(plotginis$ll)*.9, max(plotginis$ul)*1.1),
         xlim = c(min(plotginis$sequence), max(plotginis$sequence)))
    segments(x0 = plotginis$sequence, y0 = plotginis$ll, y1 = plotginis$ul,
             col = plotginis$color, lwd = 3, lty = 1)
    axis(4, at = pretty(range(plotginis$est)))
    mtext(ylabs[i], side = 4, line = 2.75, cex = .8)
}

##dev.off() 
