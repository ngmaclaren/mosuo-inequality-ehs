library(ineq)
library(ggplot2)
library(grid)
library(gridExtra)
library(cowplot)

### For plotting
if(!(dir.exists("img"))) dir.create("img")
matcol <- "red3"
patcol <- "darkslategray"
refcol <- "dodgerblue"
###

houses <- read.csv("./data/houses-assets-ngm.csv")
villages <- read.csv("./data/villages-ngm.csv")

worthcols <- c("NetWorth", "ModernWorth", "FarmWorth")
for(col in worthcols) {
    agg <- aggregate(houses[, col], by = list(Village = houses[, "Village"]),
                     function(x) mean(x, na.rm = TRUE))
    colnames(agg)[2] <- paste0("mean_", col)
    villages <- merge(villages, agg, by = "Village")

    agg <- aggregate(houses[, col], by = list(Village = houses[, "Village"]), FUN = ineq::Gini)
    colnames(agg)[2] <- paste0("Gini_", col)
    villages <- merge(villages, agg, by = "Village")
}
colnames(villages)[which(colnames(villages) == "HHGini")] <- "Gini_Income"
colnames(houses)[which(colnames(houses) == "HHIncomeTotalLastYear")] <- "HHIncome"

## Adjusting data frames to prepare for ggploting
houses <- merge(houses, villages, by = c("Village", "Lineality"))

xlim <- c(0, 70)
plotcolors <- c(matcol, patcol)
ginicolors <- c("#0000cd", "#a3915f")
hhvars <- c("HHIncome", "NetWorth", "ModernWorth", "FarmWorth")
ginivars <- c("Gini_Income", "Gini_NetWorth", "Gini_ModernWorth", "Gini_FarmWorth")
ylabs <- c("Household Income (CNY)", "Total Worth (CNY)",
           "Modern Worth (CNY)", "Farm Animal Worth (CNY)")
plotdfs <- list(
    subset(houses, Lineality == "Matrilineal"),
    subset(houses, Lineality == "Patrilineal")
)
plotlist <- vector("list", length(hhvars)*length(plotdfs))

plotnames <- LETTERS[1:(length(hhvars)*length(plotdfs))]
pos <- 1
for(i in 1:length(hhvars)) {
    for(j in 1:length(plotdfs)) {
        df <- plotdfs[[j]]
        ##df$KmToTourism <- factor(df$KmToTourism) # to view violinplots arranged by distance to local tourism center instead of the main tourism center
        df$KmToLake <- factor(df$KmToLake)
        
        yvar <- hhvars[i]
        ginivar <- ginivars[i]
        df <- na.omit(df[, c("KmToLake", "Lineality", yvar, ginivar)]) # Tourism

        ylim <- c(1, max(df[, yvar], na.rm = TRUE)*1.01)
        giniscale <- max(df[, yvar], na.rm = TRUE)#*1.01
        
        p <- ggplot(data = df, xlim = xlim, ylim = ylim) +
            ##xlab("Distance to Local Tourism Center (km)") +
            xlab("Distance to Lugu Lake (km)") +
            geom_violin(aes(x = as.factor(KmToLake), y = .data[[yvar]], fill = Lineality),
                        col = "black", draw_quantiles = 0.5) +
            scale_y_continuous(
                name = ylabs[i], labels = function(x) format(x, scientific = TRUE, digits = 2),
                sec.axis = sec_axis(~ ./giniscale, name = "Gini")) +
            geom_point(
                aes(x = as.factor(KmToLake), y = .data[[ginivar]]*giniscale),
                color = ginicolors[j], size = 5) +
            scale_fill_manual(values = plotcolors[j]) +
            theme_classic() +
            theme(text = element_text(size = 12), legend.position = "none")

        plotlist[[pos]] <- as_grob(p)
        pos <- pos + 1
    }
}

##png("./img/violinplots-ngm.png", width = 11, height = 12, units = "in", res = 180, type = "cairo-png")
plot_grid(plotlist = plotlist, labels = c(LETTERS[1:pos]), vjust = 1, ncol = 2)
##dev.off()
