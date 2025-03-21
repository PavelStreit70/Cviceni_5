#Načtení, který nejede

#library(readxl)
#library(dplyr)
#library(tidyr)
#D <- read_excel("ELOVLs(1).xlsx", sheet = "data_all")

#Načtení/import, který snad jede

library(readxl)
library(dplyr)
library(tidyr)
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


#Párový dvouvýběrový t-test

t.test(D$tumor, D$non.tumor, paired = TRUE)

#Vykreslení histogramu

hist(D$tumor - D$non.tumor)
# nejede - lillie.test(D$tumor - D$non.tumor)

#Boxplot - tumor/netumor

boxplot(D$tumor, D$non.tumor)

#Jeden - dohromady

boxplot(D$tumor - D$non.tumor); abline(0, 0, col="red")

#Wilcox

wilcox.test(D$tumor, D$non.tumor, paired = TRUE)

# Built-in independent t-test on wide data

t.test(D$tumor, D$non.tumor)

# Built-in independent t-test on long data

t.test(expression ~ tissue, data = D)

t.test(ratio_log ~ sex, data = D)
# Welch’s t-test (default)

t.test(ratio_log ~ sex, data = D, var.equal = TRUE)
# Student’s t-test assumes identical variances

#Mann-Whitney je tady obsažen ve Wilcoxovi

wilcox.test(ratio_log ~ sex, data = D)

#ANOVA

a <- aov(ratio_log ~ gene, D)

# post-hoc testování dvojic

TukeyHSD(a) 

# ověření předpokladu homogenity rozptylů

#leveneTest(ratio_log ~ factor(gene), D) 

kruskal.test(ratio_log ~ gene, D)

# post-hoc testování dvojic

pairwise.wilcox.test(D$ratio_log, D$gene,p.adjust.method = "BH")

D %>% 
  group_by(gene, sex) %>%
  summarise(mean = mean(ratio_log, na.rm = TRUE))

boxplot(ratio_log ~ sex + gene, D, las = 2)

#Dvoucestná ANOVA

a <- aov(ratio_log ~ sex + gene, D)
summary(a)

# model with interaction!
b <- aov(ratio_log ~ sex * gene, D)
summary(b)

# post-hoc tests

TukeyHSD(b)
TukeyHSD(b, "sex")
TukeyHSD(b, "gene")

#Binární proměnná ve dvou skupinách - tenhle kós mi z nějakého důvodu nejede

D <- D %>% mutate(ratio_increase_bin = factor(case_when(ratio <= 1 ~ "No",ratio > 1 ~ "Yes")))

#T_bin <- as.data.frame(table(D$gene,
#                             D$ratio_increase_bin))
#barplot(Freq ~ Var2 + Var1, data = T_bin)

D <- D %>% mutate(ratio_increase =
                    case_when(ratio<=1 ~ "No",
                              ratio <=5 ~ "Weak",
                              ratio>5 ~ "Strong")) %>%
  mutate(ratio_increase = factor(ratio_increase,
                                 levels = c("No", "Weak", "Strong")))

#S tabulkou, ale je tu nějaký problém, nejede to...

table(D$ratio_increase)
T <- D %>%
  group_by(ratio_increase) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  # count(ratio_increase) %>% # nebo takto
  filter(!is.na(ratio_increase))


