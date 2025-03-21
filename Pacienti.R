#Načtení, který nejede

library(readxl)
library(dplyr)
library(tidyr)
D <- read_excel("ELOVLs(1).xlsx", sheet = "data_all")

#Načtení/import, který snad jede

library(readxl)
D <- read_excel("ELOVLs (1).xlsx")
View(ELOVLs_1_)

#Statistika

#1. zda se liší exprese v tumoru a zdravé tkáni? Párový T-test/neparametrická varianta
#2. zda koreluje exprese v tumoru a zdravé tkáni? Korelace
#3. zda se liší exprese v tumoru u mužů a žen? Nepárový t-test/neparametrická var.
#4. zda se liší exprese v tumoru u různých genů? ANOVA
#5. zda poměr exprese v tumor/non-tumor tkáni vykazuje nějaký významný posun? Jednovýběrový
#6. zda zvýšení exprese (ano/ne) souvisí s pohlavím? Chí-kvadrát/Fischerův test


#Existuje vztah mezi expresí v tumor a non-tumor vzorcích? 

#korelace: cor.test()
#lineární regrese: lm()

#cor.test(ELOVLs_1_$tumor, ELOVLs_1_$non.tumor, method = "pearson")

cor.test(D$tumor, D$non.tumor, method = "pearson")   

#Proč vyšla jiná hodnota než v prezentaci?

#Vyplotuj to!

plot(tumor ~ non.tumor, data = D)

#Testování předpokladů - histogram

hist(D$tumor)
hist(D$non.tumor)

#Transformace - normální rozložení je naprd

hist(log10(D$tumor))
hist(log10(D$non.tumor)) 

#Transformace taky nepomohla = neparametrický test

lillie.test(log10(D$tumor))
lillie.test(log10(D$non.tumor))

cor.test(D$tumor, D$non.tumor, method = "pearson")
cor.test(D$tumor, D$non.tumor, method = "spearman")

#Vizualizace - histogram/ratio

hist(D$ratio)
hist(log10(D$ratio))

D <- D %>% mutate(ratio_log = log10(ratio))

t.test(D$ratio_log, mu = 0)

# srovnání s hodnotou 0, protože 1 je po log transformaci 0! 

#Vykreslení grafu

boxplot(D$ratio_log)
plot(D$ratio_log)
abline(0, 0, col="red")

#Wilcoxonův neparametrická metoda - sedí to jako v prezentaci

wilcox.test(D$ratio_log, mu = 0)
