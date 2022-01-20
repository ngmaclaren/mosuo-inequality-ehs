if(!(dir.exists("r-out"))) dir.create("r-out")

sink("./r-out/participation-models.txt", append = FALSE, type = "output", split = TRUE)

houses <- read.csv("./data/houses-assets-ngm.csv")
villages <- read.csv("./data/villages-ngm.csv")
houses <- merge(houses, villages, by = c("Village", "Lineality"))

## All together
income <- glm(HHIncomeTotalLastYear ~ KmToLake + PrimarySourceHHIncome,
              data = houses, family = quasipoisson)
networth <- glm(NetWorth ~ KmToLake + PrimarySourceHHIncome + logHHIncome,
                   data = houses, family = quasipoisson)
modernworth <- glm(ModernWorth ~ KmToLake + PrimarySourceHHIncome + logHHIncome,
                   data = houses, family = quasipoisson)
farmworth <- glm(FarmWorth ~ KmToLake + PrimarySourceHHIncome + logHHIncome,
                   data = houses, family = quasipoisson)

models <- list(income, networth, modernworth, farmworth)#, modernworth_income)

for(m in models) {
    print(summary(m))
    print(confint(m))
}

## Matrilineal
for(m in models) {
    m. <- update(m, data = houses[houses$Lineality == "Matrilineal", ])
    print(summary(m.))
    print(confint(m.))
}

## Patrilinea
for(m in models) {
    m. <- update(m, data = houses[houses$Lineality == "Patrilineal", ])
    print(summary(m.))
    print(confint(m.))
}

sink()
