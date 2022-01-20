if(!(dir.exists("r-out"))) dir.create("r-out")

sink("./r-out/gini-models.txt", append = FALSE, type = "output", split = TRUE)

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

### Question 2, without lineality
income <- glm(Gini_HHIncome ~ mean_HHIncome + KmToLake, data = villages)
modernworth <- glm(Gini_ModernWorth ~ mean_ModernWorth + KmToLake, data = villages)
farmworth <- glm(Gini_FarmWorth ~ mean_FarmWorth + KmToLake, data = villages)
networth <- glm(Gini_NetWorth ~ mean_NetWorth + KmToLake, data = villages)
models <- list(income, networth, modernworth, farmworth)
for(m in models) {
    print(summary(m))
    print(confint(m))
}

for(m in models) {
    m. <- update(m, . ~ . - KmToLake)
    print(summary(m.))
    print(confint(m.))
}

### Question 3, with lineality

## income, modern worth, farm worth
income <- glm(Gini_HHIncome ~ mean_HHIncome + Lineality, data = villages) # y distrib?
income_i <- update(income, . ~ . + mean_HHIncome:Lineality)

modernworth <- glm(Gini_ModernWorth ~ mean_ModernWorth + Lineality, data = villages)
modernworth_i <- update(modernworth, . ~ . + mean_ModernWorth:Lineality)

farmworth <-  glm(Gini_FarmWorth ~ mean_FarmWorth + Lineality, data = villages)
farmworth_i <- update(farmworth, . ~ . + mean_FarmWorth:Lineality)

networth <- glm(Gini_NetWorth ~ mean_NetWorth + Lineality, data = villages)
networth_i <- update(networth, . ~ . + mean_NetWorth:Lineality)

models <- list(income, income_i,
               networth, networth_i,
               modernworth, modernworth_i,
               farmworth, farmworth_i)

for(m in models) {
    print(summary(m))
    print(confint(m))
}

sink()
