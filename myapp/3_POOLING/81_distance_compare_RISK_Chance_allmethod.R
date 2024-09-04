library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)

d_risk <- read.csv(file = 'myapp/files/80_distances/distances_risk.csv', header = TRUE)
d_chance <- read.csv(file = 'myapp/files/80_distances/distances_chance.csv', header = TRUE)

#all.answers <- answers %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)
#number.scenarios <- nrow(as.data.frame(table(all.answers$QUES_ID)))
#scenarios <- as.data.frame(table(all.answers$QUES_ID))

I_risk_classic_area <- d_risk %>% filter(method1 == "classic" & method2 == "area" & type =="IMPACT")
O_risk_classic_area <- d_risk %>% filter(method1 == "classic" & method2 == "area" & type =="OCCURRENCE")
I_chance_classic_area <- d_chance %>% filter(method1 == "classic" & method2 == "area" & type =="IMPACT")
O_chance_classic_area <- d_chance %>% filter(method1 == "classic" & method2 == "area" & type =="OCCURRENCE")

I_risk_classic_center <- d_risk %>% filter(method1 == "classic" & method2 == "center" & type =="IMPACT")
O_risk_classic_center <- d_risk %>% filter(method1 == "classic" & method2 == "center" & type =="OCCURRENCE")
I_chance_classic_center <- d_chance %>% filter(method1 == "classic" & method2 == "center" & type =="IMPACT")
O_chance_classic_center <- d_chance %>% filter(method1 == "classic" & method2 == "center" & type =="OCCURRENCE")

I_risk_classic_centertogrid <- d_risk %>% filter(method1 == "classic" & method2 == "centertogrid" & type =="IMPACT")
O_risk_classic_centertogrid <- d_risk %>% filter(method1 == "classic" & method2 == "centertogrid" & type =="OCCURRENCE")
I_chance_classic_centertogrid <- d_chance %>% filter(method1 == "classic" & method2 == "centertogrid" & type =="IMPACT")
O_chance_classic_centertogrid <- d_chance %>% filter(method1 == "classic" & method2 == "centertogrid" & type =="OCCURRENCE")

I_risk_classic_reachedgrid <- d_risk %>% filter(method1 == "classic" & method2 == "reachedgrid" & type =="IMPACT")
O_risk_classic_reachedgrid <- d_risk %>% filter(method1 == "classic" & method2 == "reachedgrid" & type =="OCCURRENCE")
I_chance_classic_reachedgrid <- d_chance %>% filter(method1 == "classic" & method2 == "reachedgrid" & type =="IMPACT")
O_chance_classic_reachedgrid <- d_chance %>% filter(method1 == "classic" & method2 == "reachedgrid" & type =="OCCURRENCE")

I_risk_classic_weighted <- d_risk %>% filter(method1 == "classic" & method2 == "weighted" & type =="IMPACT")
O_risk_classic_weighted <- d_risk %>% filter(method1 == "classic" & method2 == "weighted" & type =="OCCURRENCE")
I_chance_classic_weighted <- d_chance %>% filter(method1 == "classic" & method2 == "weighted" & type =="IMPACT")
O_chance_classic_weighted <- d_chance %>% filter(method1 == "classic" & method2 == "weighted" & type =="OCCURRENCE")

I_risk_classic_pooling <- d_risk %>% filter(method1 == "classic" & method2 == "pooling" & type =="IMPACT")
O_risk_classic_pooling <- d_risk %>% filter(method1 == "classic" & method2 == "pooling" & type =="OCCURRENCE")
I_chance_classic_pooling <- d_chance %>% filter(method1 == "classic" & method2 == "pooling" & type =="IMPACT")
O_chance_classic_pooling <- d_chance %>% filter(method1 == "classic" & method2 == "pooling" & type =="OCCURRENCE")

I_risk_area_center <- d_risk %>% filter(method1 == "area" & method2 == "center" & type =="IMPACT")
O_risk_area_center <- d_risk %>% filter(method1 == "area" & method2 == "center" & type =="OCCURRENCE")
I_chance_area_center <- d_chance %>% filter(method1 == "area" & method2 == "center" & type =="IMPACT")
O_chance_area_center <- d_chance %>% filter(method1 == "area" & method2 == "center" & type =="OCCURRENCE")

I_risk_area_centertogrid <- d_risk %>% filter(method1 == "area" & method2 == "centertogrid" & type =="IMPACT")
O_risk_area_centertogrid <- d_risk %>% filter(method1 == "area" & method2 == "centertogrid" & type =="OCCURRENCE")
I_chance_area_centertogrid <- d_chance %>% filter(method1 == "area" & method2 == "centertogrid" & type =="IMPACT")
O_chance_area_centertogrid <- d_chance %>% filter(method1 == "area" & method2 == "centertogrid" & type =="OCCURRENCE")

I_risk_area_reachedgrid <- d_risk %>% filter(method1 == "area" & method2 == "reachedgrid" & type =="IMPACT")
O_risk_area_reachedgrid <- d_risk %>% filter(method1 == "area" & method2 == "reachedgrid" & type =="OCCURRENCE")
I_chance_area_reachedgrid <- d_chance %>% filter(method1 == "area" & method2 == "reachedgrid" & type =="IMPACT")
O_chance_area_reachedgrid <- d_chance %>% filter(method1 == "area" & method2 == "reachedgrid" & type =="OCCURRENCE")

I_risk_area_weighted <- d_risk %>% filter(method1 == "area" & method2 == "weighted" & type =="IMPACT")
O_risk_area_weighted <- d_risk %>% filter(method1 == "area" & method2 == "weighted" & type =="OCCURRENCE")
I_chance_area_weighted <- d_chance %>% filter(method1 == "area" & method2 == "weighted" & type =="IMPACT")
O_chance_area_weighted <- d_chance %>% filter(method1 == "area" & method2 == "weighted" & type =="OCCURRENCE")

I_risk_area_pooling <- d_risk %>% filter(method1 == "area" & method2 == "pooling" & type =="IMPACT")
O_risk_area_pooling <- d_risk %>% filter(method1 == "area" & method2 == "pooling" & type =="OCCURRENCE")
I_chance_area_pooling <- d_chance %>% filter(method1 == "area" & method2 == "pooling" & type =="IMPACT")
O_chance_area_pooling <- d_chance %>% filter(method1 == "area" & method2 == "pooling" & type =="OCCURRENCE")

I_risk_center_centertogrid <- d_risk %>% filter(method1 == "center" & method2 == "centertogrid" & type =="IMPACT")
O_risk_center_centertogrid <- d_risk %>% filter(method1 == "center" & method2 == "centertogrid" & type =="OCCURRENCE")
I_chance_center_centertogrid <- d_chance %>% filter(method1 == "center" & method2 == "centertogrid" & type =="IMPACT")
O_chance_center_centertogrid <- d_chance %>% filter(method1 == "center" & method2 == "centertogrid" & type =="OCCURRENCE")

I_risk_center_reachedgrid <- d_risk %>% filter(method1 == "center" & method2 == "reachedgrid" & type =="IMPACT")
O_risk_center_reachedgrid <- d_risk %>% filter(method1 == "center" & method2 == "reachedgrid" & type =="OCCURRENCE")
I_chance_center_reachedgrid <- d_chance %>% filter(method1 == "center" & method2 == "reachedgrid" & type =="IMPACT")
O_chance_center_reachedgrid <- d_chance %>% filter(method1 == "center" & method2 == "reachedgrid" & type =="OCCURRENCE")

I_risk_center_weighted <- d_risk %>% filter(method1 == "center" & method2 == "weighted" & type =="IMPACT")
O_risk_center_weighted <- d_risk %>% filter(method1 == "center" & method2 == "weighted" & type =="OCCURRENCE")
I_chance_center_weighted <- d_chance %>% filter(method1 == "center" & method2 == "weighted" & type =="IMPACT")
O_chance_center_weighted <- d_chance %>% filter(method1 == "center" & method2 == "weighted" & type =="OCCURRENCE")

I_risk_center_pooling <- d_risk %>% filter(method1 == "center" & method2 == "pooling" & type =="IMPACT")
O_risk_center_pooling <- d_risk %>% filter(method1 == "center" & method2 == "pooling" & type =="OCCURRENCE")
I_chance_center_pooling <- d_chance %>% filter(method1 == "center" & method2 == "pooling" & type =="IMPACT")
O_chance_center_pooling <- d_chance %>% filter(method1 == "center" & method2 == "pooling" & type =="OCCURRENCE")

I_risk_centertogrid_reachedgrid <- d_risk %>% filter(method1 == "centertogrid" & method2 == "reachedgrid" & type =="IMPACT")
O_risk_centertogrid_reachedgrid <- d_risk %>% filter(method1 == "centertogrid" & method2 == "reachedgrid" & type =="OCCURRENCE")
I_chance_centertogrid_reachedgrid <- d_chance %>% filter(method1 == "centertogrid" & method2 == "reachedgrid" & type =="IMPACT")
O_chance_centertogrid_reachedgrid <- d_chance %>% filter(method1 == "centertogrid" & method2 == "reachedgrid" & type =="OCCURRENCE")

I_risk_centertogrid_weighted <- d_risk %>% filter(method1 == "centertogrid" & method2 == "weighted" & type =="IMPACT")
O_risk_centertogrid_weighted <- d_risk %>% filter(method1 == "centertogrid" & method2 == "weighted" & type =="OCCURRENCE")
I_chance_centertogrid_weighted <- d_chance %>% filter(method1 == "centertogrid" & method2 == "weighted" & type =="IMPACT")
O_chance_centertogrid_weighted <- d_chance %>% filter(method1 == "centertogrid" & method2 == "weighted" & type =="OCCURRENCE")

I_risk_centertogrid_pooling <- d_risk %>% filter(method1 == "centertogrid" & method2 == "pooling" & type =="IMPACT")
O_risk_centertogrid_pooling <- d_risk %>% filter(method1 == "centertogrid" & method2 == "pooling" & type =="OCCURRENCE")
I_chance_centertogrid_pooling <- d_chance %>% filter(method1 == "centertogrid" & method2 == "pooling" & type =="IMPACT")
O_chance_centertogrid_pooling <- d_chance %>% filter(method1 == "centertogrid" & method2 == "pooling" & type =="OCCURRENCE")

I_risk_reachedgrid_weighted <- d_risk %>% filter(method1 == "reachedgrid" & method2 == "weighted" & type =="IMPACT")
O_risk_reachedgrid_weighted <- d_risk %>% filter(method1 == "reachedgrid" & method2 == "weighted" & type =="OCCURRENCE")
I_chance_reachedgrid_weighted <- d_chance %>% filter(method1 == "reachedgrid" & method2 == "weighted" & type =="IMPACT")
O_chance_reachedgrid_weighted <- d_chance %>% filter(method1 == "reachedgrid" & method2 == "weighted" & type =="OCCURRENCE")

I_risk_reachedgrid_pooling <- d_risk %>% filter(method1 == "reachedgrid" & method2 == "pooling" & type =="IMPACT")
O_risk_reachedgrid_pooling <- d_risk %>% filter(method1 == "reachedgrid" & method2 == "pooling" & type =="OCCURRENCE")
I_chance_reachedgrid_pooling <- d_chance %>% filter(method1 == "reachedgrid" & method2 == "pooling" & type =="IMPACT")
O_chance_reachedgrid_pooling <- d_chance %>% filter(method1 == "reachedgrid" & method2 == "pooling" & type =="OCCURRENCE")

I_risk_weighted_pooling <- d_risk %>% filter(method1 == "weighted" & method2 == "pooling" & type =="IMPACT")
O_risk_weighted_pooling <- d_risk %>% filter(method1 == "weighted" & method2 == "pooling" & type =="OCCURRENCE")
I_chance_weighted_pooling <- d_chance %>% filter(method1 == "weighted" & method2 == "pooling" & type =="IMPACT")
O_chance_weighted_pooling <- d_chance %>% filter(method1 == "weighted" & method2 == "pooling" & type =="OCCURRENCE")


# Extrahieren der Daten für den Test
I_risk_classic_area_value <- I_risk_classic_area$distance
I_chance_classic_area_value <- I_chance_classic_area$distance
O_risk_classic_area_value <- O_risk_classic_area$distance
O_chance_classic_area_value <- O_chance_classic_area$distance

I_risk_classic_center_value <- I_risk_classic_center$distance
I_chance_classic_center_value <- I_chance_classic_center$distance
O_risk_classic_center_value <- O_risk_classic_center$distance
O_chance_classic_center_value <- O_chance_classic_center$distance

I_risk_classic_centertogrid_value <- I_risk_classic_centertogrid$distance
I_chance_classic_centertogrid_value <- I_chance_classic_centertogrid$distance
O_risk_classic_centertogrid_value <- O_risk_classic_centertogrid$distance
O_chance_classic_centertogrid_value <- O_chance_classic_centertogrid$distance

I_risk_classic_reachedgrid_value <- I_risk_classic_reachedgrid$distance
I_chance_classic_reachedgrid_value <- I_chance_classic_reachedgrid$distance
O_risk_classic_reachedgrid_value <- O_risk_classic_reachedgrid$distance
O_chance_classic_reachedgrid_value <- O_chance_classic_reachedgrid$distance

I_risk_classic_weighted_value <- I_risk_classic_weighted$distance
I_chance_classic_weighted_value <- I_chance_classic_weighted$distance
O_risk_classic_weighted_value <- O_risk_classic_weighted$distance
O_chance_classic_weighted_value <- O_chance_classic_weighted$distance

I_risk_classic_pooling_value <- I_risk_classic_pooling$distance
I_chance_classic_pooling_value <- I_chance_classic_pooling$distance
O_risk_classic_pooling_value <- O_risk_classic_pooling$distance
O_chance_classic_pooling_value <- O_chance_classic_pooling$distance

I_risk_area_center_value <- I_risk_area_center$distance
I_chance_area_center_value <- I_chance_area_center$distance
O_risk_area_center_value <- O_risk_area_center$distance
O_chance_area_center_value <- O_chance_area_center$distance

I_risk_area_centertogrid_value <- I_risk_area_centertogrid$distance
I_chance_area_centertogrid_value <- I_chance_area_centertogrid$distance
O_risk_area_centertogrid_value <- O_risk_area_centertogrid$distance
O_chance_area_centertogrid_value <- O_chance_area_centertogrid$distance

I_risk_area_reachedgrid_value <- I_risk_area_reachedgrid$distance
I_chance_area_reachedgrid_value <- I_chance_area_reachedgrid$distance
O_risk_area_reachedgrid_value <- O_risk_area_reachedgrid$distance
O_chance_area_reachedgrid_value <- O_chance_area_reachedgrid$distance

I_risk_area_weighted_value <- I_risk_area_weighted$distance
I_chance_area_weighted_value <- I_chance_area_weighted$distance
O_risk_area_weighted_value <- O_risk_area_weighted$distance
O_chance_area_weighted_value <- O_chance_area_weighted$distance

I_risk_area_pooling_value <- I_risk_area_pooling$distance
I_chance_area_pooling_value <- I_chance_area_pooling$distance
O_risk_area_pooling_value <- O_risk_area_pooling$distance
O_chance_area_pooling_value <- O_chance_area_pooling$distance

I_risk_center_centertogrid_value <- I_risk_center_centertogrid$distance
I_chance_center_centertogrid_value <- I_chance_center_centertogrid$distance
O_risk_center_centertogrid_value <- O_risk_center_centertogrid$distance
O_chance_center_centertogrid_value <- O_chance_center_centertogrid$distance

I_risk_center_reachedgrid_value <- I_risk_center_reachedgrid$distance
I_chance_center_reachedgrid_value <- I_chance_center_reachedgrid$distance
O_risk_center_reachedgrid_value <- O_risk_center_reachedgrid$distance
O_chance_center_reachedgrid_value <- O_chance_center_reachedgrid$distance

I_risk_center_weighted_value <- I_risk_center_weighted$distance
I_chance_center_weighted_value <- I_chance_center_weighted$distance
O_risk_center_weighted_value <- O_risk_center_weighted$distance
O_chance_center_weighted_value <- O_chance_center_weighted$distance

I_risk_center_pooling_value <- I_risk_center_pooling$distance
I_chance_center_pooling_value <- I_chance_center_pooling$distance
O_risk_center_pooling_value <- O_risk_center_pooling$distance
O_chance_center_pooling_value <- O_chance_center_pooling$distance

I_risk_centertogrid_reachedgrid_value <- I_risk_centertogrid_reachedgrid$distance
I_chance_centertogrid_reachedgrid_value <- I_chance_centertogrid_reachedgrid$distance
O_risk_centertogrid_reachedgrid_value <- O_risk_centertogrid_reachedgrid$distance
O_chance_centertogrid_reachedgrid_value <- O_chance_centertogrid_reachedgrid$distance

I_risk_centertogrid_weighted_value <- I_risk_centertogrid_weighted$distance
I_chance_centertogrid_weighted_value <- I_chance_centertogrid_weighted$distance
O_risk_centertogrid_weighted_value <- O_risk_centertogrid_weighted$distance
O_chance_centertogrid_weighted_value <- O_chance_centertogrid_weighted$distance

I_risk_centertogrid_pooling_value <- I_risk_centertogrid_pooling$distance
I_chance_centertogrid_pooling_value <- I_chance_centertogrid_pooling$distance
O_risk_centertogrid_pooling_value <- O_risk_centertogrid_pooling$distance
O_chance_centertogrid_pooling_value <- O_chance_centertogrid_pooling$distance

I_risk_reachedgrid_weighted_value <- I_risk_reachedgrid_weighted$distance
I_chance_reachedgrid_weighted_value <- I_chance_reachedgrid_weighted$distance
O_risk_reachedgrid_weighted_value <- O_risk_reachedgrid_weighted$distance
O_chance_reachedgrid_weighted_value <- O_chance_reachedgrid_weighted$distance

I_risk_reachedgrid_pooling_value <- I_risk_reachedgrid_pooling$distance
I_chance_reachedgrid_pooling_value <- I_chance_reachedgrid_pooling$distance
O_risk_reachedgrid_pooling_value <- O_risk_reachedgrid_pooling$distance
O_chance_reachedgrid_pooling_value <- O_chance_reachedgrid_pooling$distance

I_risk_weighted_pooling_value <- I_risk_weighted_pooling$distance
I_chance_weighted_pooling_value <- I_chance_weighted_pooling$distance
O_risk_weighted_pooling_value <- O_risk_weighted_pooling$distance
O_chance_weighted_pooling_value <- O_chance_weighted_pooling$distance


# Durchführen des Mann-Whitney-U-Tests
I_classic_area_result <- wilcox.test(I_risk_classic_area_value, I_chance_classic_area_value, alternative = "two.sided", exact = FALSE)
O_classic_area_result <- wilcox.test(O_risk_classic_area_value, O_chance_classic_area_value, alternative = "two.sided", exact = FALSE)

I_classic_center_result <- wilcox.test(I_risk_classic_center_value, I_chance_classic_center_value, alternative = "two.sided", exact = FALSE)
O_classic_center_result <- wilcox.test(O_risk_classic_center_value, O_chance_classic_center_value, alternative = "two.sided", exact = FALSE)

I_classic_centertogrid_result <- wilcox.test(I_risk_classic_centertogrid_value, I_chance_classic_centertogrid_value, alternative = "two.sided", exact = FALSE)
O_classic_centertogrid_result <- wilcox.test(O_risk_classic_centertogrid_value, O_chance_classic_centertogrid_value, alternative = "two.sided", exact = FALSE)

I_classic_reachedgrid_result <- wilcox.test(I_risk_classic_reachedgrid_value, I_chance_classic_reachedgrid_value, alternative = "two.sided", exact = FALSE)
O_classic_reachedgrid_result <- wilcox.test(O_risk_classic_reachedgrid_value, O_chance_classic_reachedgrid_value, alternative = "two.sided", exact = FALSE)

I_classic_weighted_result <- wilcox.test(I_risk_classic_weighted_value, I_chance_classic_weighted_value, alternative = "two.sided", exact = FALSE)
O_classic_weighted_result <- wilcox.test(O_risk_classic_weighted_value, O_chance_classic_weighted_value, alternative = "two.sided", exact = FALSE)

I_classic_pooling_result <- wilcox.test(I_risk_classic_pooling_value, I_chance_classic_pooling_value, alternative = "two.sided", exact = FALSE)
O_classic_pooling_result <- wilcox.test(O_risk_classic_pooling_value, O_chance_classic_pooling_value, alternative = "two.sided", exact = FALSE)

I_area_center_result <- wilcox.test(I_risk_area_center_value, I_chance_area_center_value, alternative = "two.sided", exact = FALSE)
O_area_center_result <- wilcox.test(O_risk_area_center_value, O_chance_area_center_value, alternative = "two.sided", exact = FALSE)

I_area_centertogrid_result <- wilcox.test(I_risk_area_centertogrid_value, I_chance_area_centertogrid_value, alternative = "two.sided", exact = FALSE)
O_area_centertogrid_result <- wilcox.test(O_risk_area_centertogrid_value, O_chance_area_centertogrid_value, alternative = "two.sided", exact = FALSE)

I_area_reachedgrid_result <- wilcox.test(I_risk_area_reachedgrid_value, I_chance_area_reachedgrid_value, alternative = "two.sided", exact = FALSE)
O_area_reachedgrid_result <- wilcox.test(O_risk_area_reachedgrid_value, O_chance_area_reachedgrid_value, alternative = "two.sided", exact = FALSE)

I_area_weighted_result <- wilcox.test(I_risk_area_weighted_value, I_chance_area_weighted_value, alternative = "two.sided", exact = FALSE)
O_area_weighted_result <- wilcox.test(O_risk_area_weighted_value, O_chance_area_weighted_value, alternative = "two.sided", exact = FALSE)

I_area_pooling_result <- wilcox.test(I_risk_area_pooling_value, I_chance_area_pooling_value, alternative = "two.sided", exact = FALSE)
O_area_pooling_result <- wilcox.test(O_risk_area_pooling_value, O_chance_area_pooling_value, alternative = "two.sided", exact = FALSE)

I_center_centertogrid_result <- wilcox.test(I_risk_center_centertogrid_value, I_chance_center_centertogrid_value, alternative = "two.sided", exact = FALSE)
O_center_centertogrid_result <- wilcox.test(O_risk_center_centertogrid_value, O_chance_center_centertogrid_value, alternative = "two.sided", exact = FALSE)

I_center_reachedgrid_result <- wilcox.test(I_risk_center_reachedgrid_value, I_chance_center_reachedgrid_value, alternative = "two.sided", exact = FALSE)
O_center_reachedgrid_result <- wilcox.test(O_risk_center_reachedgrid_value, O_chance_center_reachedgrid_value, alternative = "two.sided", exact = FALSE)

I_center_weighted_result <- wilcox.test(I_risk_center_weighted_value, I_chance_center_weighted_value, alternative = "two.sided", exact = FALSE)
O_center_weighted_result <- wilcox.test(O_risk_center_weighted_value, O_chance_center_weighted_value, alternative = "two.sided", exact = FALSE)

I_center_pooling_result <- wilcox.test(I_risk_center_pooling_value, I_chance_center_pooling_value, alternative = "two.sided", exact = FALSE)
O_center_pooling_result <- wilcox.test(O_risk_center_pooling_value, O_chance_center_pooling_value, alternative = "two.sided", exact = FALSE)

I_centertogrid_reachedgrid_result <- wilcox.test(I_risk_centertogrid_reachedgrid_value, I_chance_centertogrid_reachedgrid_value, alternative = "two.sided", exact = FALSE)
O_centertogrid_reachedgrid_result <- wilcox.test(O_risk_centertogrid_reachedgrid_value, O_chance_centertogrid_reachedgrid_value, alternative = "two.sided", exact = FALSE)

I_centertogrid_weighted_result <- wilcox.test(I_risk_centertogrid_weighted_value, I_chance_centertogrid_weighted_value, alternative = "two.sided", exact = FALSE)
O_centertogrid_weighted_result <- wilcox.test(O_risk_centertogrid_weighted_value, O_chance_centertogrid_weighted_value, alternative = "two.sided", exact = FALSE)

I_centertogrid_pooling_result <- wilcox.test(I_risk_centertogrid_pooling_value, I_chance_centertogrid_pooling_value, alternative = "two.sided", exact = FALSE)
O_centertogrid_pooling_result <- wilcox.test(O_risk_centertogrid_pooling_value, O_chance_centertogrid_pooling_value, alternative = "two.sided", exact = FALSE)

I_reachedgrid_weighted_result <- wilcox.test(I_risk_reachedgrid_weighted_value, I_chance_reachedgrid_weighted_value, alternative = "two.sided", exact = FALSE)
O_reachedgrid_weighted_result <- wilcox.test(O_risk_reachedgrid_weighted_value, O_chance_reachedgrid_weighted_value, alternative = "two.sided", exact = FALSE)

I_reachedgrid_pooling_result <- wilcox.test(I_risk_reachedgrid_pooling_value, I_chance_reachedgrid_pooling_value, alternative = "two.sided", exact = FALSE)
O_reachedgrid_pooling_result <- wilcox.test(O_risk_reachedgrid_pooling_value, O_chance_reachedgrid_pooling_value, alternative = "two.sided", exact = FALSE)

I_weighted_pooling_result <- wilcox.test(I_risk_weighted_pooling_value, I_chance_weighted_pooling_value, alternative = "two.sided", exact = FALSE)
O_weighted_pooling_result <- wilcox.test(O_risk_weighted_pooling_value, O_chance_weighted_pooling_value, alternative = "two.sided", exact = FALSE)


# Ausgabe des Testergebnisses
print(I_classic_area_result$p.value)
print(O_classic_area_result$p.value)
print(I_classic_center_result$p.value)
print(O_classic_center_result$p.value)
print(I_classic_centertogrid_result$p.value)
print(O_classic_centertogrid_result$p.value)
print(I_classic_reachedgrid_result$p.value)
print(O_classic_reachedgrid_result$p.value)
print(I_classic_weighted_result$p.value)
print(O_classic_weighted_result$p.value)
print(I_classic_pooling_result$p.value)
print(O_classic_pooling_result$p.value)
print(I_area_center_result$p.value)
print(O_area_center_result$p.value)
print(I_area_centertogrid_result$p.value)
print(O_area_centertogrid_result$p.value)
print(I_area_reachedgrid_result$p.value)
print(O_area_reachedgrid_result$p.value)
print(I_area_weighted_result$p.value)
print(O_area_weighted_result$p.value)
print(I_area_pooling_result$p.value)
print(O_area_pooling_result$p.value)
print(I_center_centertogrid_result$p.value)
print(O_center_centertogrid_result$p.value)
print(I_center_reachedgrid_result$p.value)
print(O_center_reachedgrid_result$p.value)
print(I_center_weighted_result$p.value)
print(O_center_weighted_result$p.value)
print(I_center_pooling_result$p.value)
print(O_center_pooling_result$p.value)
print(I_centertogrid_reachedgrid_result$p.value)
print(O_centertogrid_reachedgrid_result$p.value)
print(I_centertogrid_weighted_result$p.value)
print(O_centertogrid_weighted_result$p.value)
print(I_centertogrid_pooling_result$p.value)
print(O_centertogrid_pooling_result$p.value)
print(I_reachedgrid_weighted_result$p.value)
print(O_reachedgrid_weighted_result$p.value)
print(I_reachedgrid_pooling_result$p.value)
print(O_reachedgrid_pooling_result$p.value)
print(I_weighted_pooling_result$p.value)
print(O_weighted_pooling_result$p.value)


labels <- c("I_classic_area", "O_classic_area", "I_classic_center", "O_classic_center",
            "I_classic_centertogrid", "O_classic_centertogrid", "I_classic_reachedgrid", "O_classic_reachedgrid",
            "I_classic_weighted", "O_classic_weighted", "I_classic_pooling", "O_classic_pooling",
            "I_area_center", "O_area_center", "I_area_centertogrid", "O_area_centertogrid",
            "I_area_reachedgrid", "O_area_reachedgrid", "I_area_weighted", "O_area_weighted",
            "I_area_pooling", "O_area_pooling", "I_center_centertogrid", "O_center_centertogrid",
            "I_center_reachedgrid", "O_center_reachedgrid", "I_center_weighted", "O_center_weighted",
            "I_center_pooling", "O_center_pooling", "I_centertogrid_reachedgrid", "O_centertogrid_reachedgrid",
            "I_centertogrid_weighted", "O_centertogrid_weighted", "I_centertogrid_pooling", "O_centertogrid_pooling",
            "I_reachedgrid_weighted", "O_reachedgrid_weighted", "I_reachedgrid_pooling", "O_reachedgrid_pooling",
            "I_weighted_pooling", "O_weighted_pooling")

p_values <- c(I_classic_area_result$p.value, O_classic_area_result$p.value, I_classic_center_result$p.value,
              O_classic_center_result$p.value, I_classic_centertogrid_result$p.value, O_classic_centertogrid_result$p.value,
              I_classic_reachedgrid_result$p.value, O_classic_reachedgrid_result$p.value, I_classic_weighted_result$p.value,
              O_classic_weighted_result$p.value, I_classic_pooling_result$p.value, O_classic_pooling_result$p.value,
              I_area_center_result$p.value, O_area_center_result$p.value, I_area_centertogrid_result$p.value,
              O_area_centertogrid_result$p.value, I_area_reachedgrid_result$p.value, O_area_reachedgrid_result$p.value,
              I_area_weighted_result$p.value, O_area_weighted_result$p.value, I_area_pooling_result$p.value,
              O_area_pooling_result$p.value, I_center_centertogrid_result$p.value, O_center_centertogrid_result$p.value,
              I_center_reachedgrid_result$p.value, O_center_reachedgrid_result$p.value, I_center_weighted_result$p.value,
              O_center_weighted_result$p.value, I_center_pooling_result$p.value, O_center_pooling_result$p.value,
              I_centertogrid_reachedgrid_result$p.value, O_centertogrid_reachedgrid_result$p.value,
              I_centertogrid_weighted_result$p.value, O_centertogrid_weighted_result$p.value, I_centertogrid_pooling_result$p.value,
              O_centertogrid_pooling_result$p.value, I_reachedgrid_weighted_result$p.value, O_reachedgrid_weighted_result$p.value,
              I_reachedgrid_pooling_result$p.value, O_reachedgrid_pooling_result$p.value, I_weighted_pooling_result$p.value,
              O_weighted_pooling_result$p.value)

# Split the labels into components based on underscores
split_labels <- strsplit(labels, "_")

# Create a data frame with the split components
results_df <- data.frame(
  P_Value = p_values,
  Type = sapply(split_labels, function(x) x[1]),
  Method1 = sapply(split_labels, function(x) x[2]),
  Method2 = sapply(split_labels, function(x) x[3]),
  stringsAsFactors = FALSE
)

# Optional: Add the full label as a column if needed
results_df$Full_Label <- labels

# Ausgabe des Datenrahmens
print(results_df)


scenfile <- paste0("myapp/files/81_distances/distances_method_risk_chance.xlsx")

write.csv(results_df, file = paste0("myapp/files/81_distances/distances_method_risk_chance.csv"), row.names = TRUE)

# Erstellt einen neuen Workbook und füge den transponierten Dataframe ein
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", results_df)

# Speichern des Workbooks als XLSX-Datei
saveWorkbook(wb, file = scenfile, overwrite = TRUE)



#print (I_risk_classic_area)
#print (O_risk_classic_area)
#print (I_chance_classic_area)
#print (O_chance_classic_area)