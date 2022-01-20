indiv <- read.csv("./data/individuals-ngm.csv")
house <- read.csv("./data/houses-ngm.csv")
vil <- read.csv("./data/villages-ngm.csv")

## Contingency table of counts of Lineality vs. Poor2
ct1 <- table(indiv$Lineality, indiv$Poor2)
round(proportions(ct1, margin = 1), 2)

x <- house$HHPerCapitaIncome[house$Lineality == "Matrilineal"]
y <- house$HHPerCapitaIncome[house$Lineality == "Patrilineal"]
wilcox.test(x, y, alternative = "greater")
t.test(x, y, alternative = "greater")

x <- vil$HHGini[vil$Lineality == "Matrilineal"]
y <- vil$HHGini[vil$Lineality == "Patrilineal"]
wilcox.test(x, y, alternative = "greater")
t.test(x, y, alternative = "greater")
boxplot(HHGini ~ Lineality, data = vil)

x <- indiv$YearsOfSchooling[indiv$Lineality == "Matrilineal"]
y <- indiv$YearsOfSchooling[indiv$Lineality == "Patrilineal"]
wilcox.test(x, y, alternative = "greater")
t.test(x, y, alternative = "greater")

round(proportions(table(house$Lineality, house$OwnFarm, house$OwnGuesthouse), margin = 1), 2)
