indiv <- read.csv("./data/individuals-ngm.csv")
houses <- read.csv("./data/houses-assets-ngm.csv")
villages <- read.csv("./data/villages-ngm.csv")

colnames(villages)[which(colnames(villages) == "HHGini")] <- "Gini_HHIncome"

## aggregate the three worth variables by village and merge to the village data set
## make Ginis at the same time
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

sink("./r-out/descriptives.txt", append = FALSE, type = "output", split = TRUE)

## Individual: number, age, gender
                                        # n
nrow(indiv[indiv$HomeID %in% houses$HomeID, ])
                                        # age
mean(indiv$Age, na.rm = TRUE)
sd(indiv$Age, na.rm = TRUE)
                                        # gender
gendertable <- table(indiv$Gender)
gendertable
proportions(gendertable)

## Households: number, income, assets (total, modern, farm)
                                        # n
nrow(houses)
                                        # income
quantile(houses$HHIncomeTotalLastYear, probs = c(.5, .25, .75), na.rm = TRUE)
                                        # assets
quantile(houses$NetWorth, probs = c(.5, .25, .75), na.rm = TRUE)
quantile(houses$ModernWorth, probs = c(.5, .25, .75), na.rm = TRUE)
quantile(houses$FarmWorth, probs = c(.5, .25, .75), na.rm = TRUE)

## Village: number, mean/sd distance to lake, mean/sd each gini
                                        # n
nrow(villages)
                                        # distance to lake
mean(villages$KmToLake[villages$Lineality == "Matrilineal"])
sd(villages$KmToLake[villages$Lineality == "Matrilineal"])
mean(villages$KmToLake[villages$Lineality == "Patrilineal"])
sd(villages$KmToLake[villages$Lineality == "Patrilineal"])
                                        # mean/sd each gini
ginis <- c("Gini_HHIncome", "Gini_NetWorth", "Gini_ModernWorth", "Gini_FarmWorth")
for(col in ginis) {
    print(col)
    print(mean(villages[, col]))
    print(sd(villages[, col]))
}

sink()
