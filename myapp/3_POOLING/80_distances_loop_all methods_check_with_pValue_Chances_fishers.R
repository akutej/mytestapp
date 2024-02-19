library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)
library(tibble)
dataframe <- read.csv("myapp/files/80_distances/distances_pValue_Chances.csv")
print (dataframe)
# Anzeigen der ersten paar Zeilen des DataFrames
dataframe.IMP.classic.area <- dataframe %>% filter(method1 == "classic" & method2 == "area" & type == "IMPACT")
dataframe.OCC.classic.area <- dataframe %>% filter(method1 == "classic" & method2 == "area" & type == "OCCURRENCE")

dataframe.IMP.classic.weighted <- dataframe %>% filter(method1 == "classic" & method2 == "weighted" & type == "IMPACT")
dataframe.OCC.classic.weighted <- dataframe %>% filter(method1 == "classic" & method2 == "weighted" & type == "OCCURRENCE")

dataframe.IMP.classic.center <- dataframe %>% filter(method1 == "classic" & method2 == "center" & type == "IMPACT")
dataframe.OCC.classic.center <- dataframe %>% filter(method1 == "classic" & method2 == "center" & type == "OCCURRENCE")

dataframe.IMP.classic.centertogrid <- dataframe %>% filter(method1 == "classic" & method2 == "centertogrid" & type == "IMPACT")
dataframe.OCC.classic.centertogrid <- dataframe %>% filter(method1 == "classic" & method2 == "centertogrid" & type == "OCCURRENCE")

dataframe.IMP.classic.reachedgrid <- dataframe %>% filter(method1 == "classic" & method2 == "reachedgrid" & type == "IMPACT")
dataframe.OCC.classic.reachedgrid <- dataframe %>% filter(method1 == "classic" & method2 == "reachedgrid" & type == "OCCURRENCE")

dataframe.IMP.classic.pooling <- dataframe %>% filter(method1 == "classic" & method2 == "pooling" & type == "IMPACT")
dataframe.OCC.classic.pooling <- dataframe %>% filter(method1 == "classic" & method2 == "pooling" & type == "OCCURRENCE")

dataframe.IMP.area.center <- dataframe %>% filter(method1 == "area" & method2 == "center" & type == "IMPACT")
dataframe.OCC.area.center <- dataframe %>% filter(method1 == "area" & method2 == "center" & type == "OCCURRENCE")

dataframe.IMP.area.centertogrid <- dataframe %>% filter(method1 == "area" & method2 == "centertogrid" & type == "IMPACT")
dataframe.OCC.area.centertogrid <- dataframe %>% filter(method1 == "area" & method2 == "centertogrid" & type == "OCCURRENCE")

dataframe.IMP.area.reachedgrid <- dataframe %>% filter(method1 == "area" & method2 == "reachedgrid" & type == "IMPACT")
dataframe.OCC.area.reachedgrid <- dataframe %>% filter(method1 == "area" & method2 == "reachedgrid" & type == "OCCURRENCE")

dataframe.IMP.area.weighted <- dataframe %>% filter(method1 == "area" & method2 == "weighted" & type == "IMPACT")
dataframe.OCC.area.weighted <- dataframe %>% filter(method1 == "area" & method2 == "weighted" & type == "OCCURRENCE")

dataframe.IMP.area.pooling <- dataframe %>% filter(method1 == "area" & method2 == "pooling" & type == "IMPACT")
dataframe.OCC.area.pooling <- dataframe %>% filter(method1 == "area" & method2 == "pooling" & type == "OCCURRENCE")

dataframe.IMP.center.centertogrid <- dataframe %>% filter(method1 == "center" & method2 == "centertogrid" & type == "IMPACT")
dataframe.OCC.center.centertogrid <- dataframe %>% filter(method1 == "center" & method2 == "centertogrid" & type == "OCCURRENCE")

dataframe.IMP.center.reachedgrid <- dataframe %>% filter(method1 == "center" & method2 == "reachedgrid" & type == "IMPACT")
dataframe.OCC.center.reachedgrid <- dataframe %>% filter(method1 == "center" & method2 == "reachedgrid" & type == "OCCURRENCE")

dataframe.IMP.center.weighted <- dataframe %>% filter(method1 == "center" & method2 == "weighted" & type == "IMPACT")
dataframe.OCC.center.weighted <- dataframe %>% filter(method1 == "center" & method2 == "weighted" & type == "OCCURRENCE")

dataframe.IMP.center.pooling <- dataframe %>% filter(method1 == "center" & method2 == "pooling" & type == "IMPACT")
dataframe.OCC.center.pooling <- dataframe %>% filter(method1 == "center" & method2 == "pooling" & type == "OCCURRENCE")

dataframe.IMP.centertogrid.reachedgrid <- dataframe %>% filter(method1 == "centertogrid" & method2 == "reachedgrid" & type == "IMPACT")
dataframe.OCC.centertogrid.reachedgrid <- dataframe %>% filter(method1 == "centertogrid" & method2 == "reachedgrid" & type == "OCCURRENCE")

dataframe.IMP.centertogrid.weighted <- dataframe %>% filter(method1 == "centertogrid" & method2 == "weighted" & type == "IMPACT")
dataframe.OCC.centertogrid.weighted <- dataframe %>% filter(method1 == "centertogrid" & method2 == "weighted" & type == "OCCURRENCE")

dataframe.IMP.centertogrid.pooling <- dataframe %>% filter(method1 == "centertogrid" & method2 == "pooling" & type == "IMPACT")
dataframe.OCC.centertogrid.pooling <- dataframe %>% filter(method1 == "centertogrid" & method2 == "pooling" & type == "OCCURRENCE")

dataframe.IMP.reachedgrid.weighted <- dataframe %>% filter(method1 == "reachedgrid" & method2 == "weighted" & type == "IMPACT")
dataframe.OCC.reachedgrid.weighted <- dataframe %>% filter(method1 == "reachedgrid" & method2 == "weighted" & type == "OCCURRENCE")

dataframe.IMP.reachedgrid.pooling <- dataframe %>% filter(method1 == "reachedgrid" & method2 == "pooling" & type == "IMPACT")
dataframe.OCC.reachedgrid.pooling <- dataframe %>% filter(method1 == "reachedgrid" & method2 == "pooling" & type == "OCCURRENCE")

dataframe.IMP.weighted.pooling <- dataframe %>% filter(method1 == "weighted" & method2 == "pooling" & type == "IMPACT")
dataframe.OCC.weighted.pooling <- dataframe %>% filter(method1 == "weighted" & method2 == "pooling" & type == "OCCURRENCE")


#print (dataframe.IMP.classic.area)
p_values.IMP.classic.area <- dataframe.IMP.classic.area$pValue 
p_values.OCC.classic.area <- dataframe.OCC.classic.area$pValue 
p_values.IMP.classic.weighted <- dataframe.IMP.classic.weighted
p_values.OCC.classic.weighted <- dataframe.OCC.classic.weighted
p_values.IMP.classic.center <- dataframe.IMP.classic.center
p_values.OCC.classic.center <- dataframe.OCC.classic.center
p_values.IMP.classic.centertogrid <- dataframe.IMP.classic.centertogrid
p_values.OCC.classic.centertogrid <- dataframe.OCC.classic.centertogrid
p_values.IMP.classic.reachedgrid <- dataframe.IMP.classic.reachedgrid
p_values.OCC.classic.reachedgrid <- dataframe.OCC.classic.reachedgrid
p_values.IMP.classic.pooling <- dataframe.IMP.classic.pooling
p_values.OCC.classic.pooling <- dataframe.OCC.classic.pooling
p_values.IMP.area.center <- dataframe.IMP.area.center
p_values.OCC.area.center <- dataframe.OCC.area.center
p_values.IMP.area.centertogrid <- dataframe.IMP.area.centertogrid
p_values.OCC.area.centertogrid <- dataframe.OCC.area.centertogrid
p_values.IMP.area.reachedgrid <- dataframe.IMP.area.reachedgrid
p_values.OCC.area.reachedgrid <- dataframe.OCC.area.reachedgrid
p_values.IMP.area.weighted <- dataframe.IMP.area.weighted
p_values.OCC.area.weighted <- dataframe.OCC.area.weighted
p_values.IMP.area.pooling <- dataframe.IMP.area.pooling
p_values.OCC.area.pooling <- dataframe.OCC.area.pooling
p_values.IMP.center.centertogrid <- dataframe.IMP.center.centertogrid
p_values.OCC.center.centertogrid <- dataframe.OCC.center.centertogrid
p_values.IMP.center.reachedgrid <- dataframe.IMP.center.reachedgrid
p_values.OCC.center.reachedgrid <- dataframe.OCC.center.reachedgrid
p_values.IMP.center.weighted <- dataframe.IMP.center.weighted
p_values.OCC.center.weighted <- dataframe.OCC.center.weighted
p_values.IMP.center.pooling <- dataframe.IMP.center.pooling
p_values.OCC.center.pooling <- dataframe.OCC.center.pooling
p_values.IMP.centertogrid.reachedgrid <- dataframe.IMP.centertogrid.reachedgrid
p_values.OCC.centertogrid.reachedgrid <- dataframe.OCC.centertogrid.reachedgrid
p_values.IMP.centertogrid.weighted <- dataframe.IMP.centertogrid.weighted
p_values.OCC.centertogrid.weighted <- dataframe.OCC.centertogrid.weighted
p_values.IMP.centertogrid.pooling <- dataframe.IMP.centertogrid.pooling
p_values.OCC.centertogrid.pooling <- dataframe.OCC.centertogrid.pooling
p_values.IMP.reachedgrid.weighted <- dataframe.IMP.reachedgrid.weighted
p_values.OCC.reachedgrid.weighted <- dataframe.OCC.reachedgrid.weighted
p_values.IMP.reachedgrid.pooling <- dataframe.IMP.reachedgrid.pooling
p_values.OCC.reachedgrid.pooling <- dataframe.OCC.reachedgrid.pooling
p_values.IMP.weighted.pooling <- dataframe.IMP.weighted.pooling
p_values.OCC.weighted.pooling <- dataframe.OCC.weighted.pooling

combined_p_values_df <- data.frame(
  category = character(),
  combined_p_value = numeric(),
  stringsAsFactors = FALSE
)

# Funktion zur Berechnung der kombinierten p-Werte nach Fishers Methode
calculate_combined_p_value <- function(p_values) {
  chi_sq_statistic <- -2 * sum(log(p_values))
  combined_p_value <- pchisq(chi_sq_statistic, df = 2 * length(p_values), lower.tail = FALSE)
  return(combined_p_value)
}



# Funktion zur Berechnung der kombinierten p-Werte nach Fishers Methode
calculate_combined_p_value <- function(p_values) {
  chi_sq_statistic <- -2 * sum(log(p_values))
  combined_p_value <- pchisq(chi_sq_statistic, df = 2 * length(p_values), lower.tail = FALSE)
  return(combined_p_value)
}

# Erstellen eines leeren DataFrames zur Speicherung der Ergebnisse
combined_p_values_df <- data.frame(
  category = character(),
  combined_p_value = numeric(),
  stringsAsFactors = FALSE
)

# Liste aller p-Werte Gruppen (ersetzen Sie dies durch Ihre tatsächlichen p-Werte)
p_values_list <- list(
  IMP.classic.area = dataframe.IMP.classic.area$pValue,
  OCC.classic.area = dataframe.OCC.classic.area$pValue,
  IMP.classic.weighted = dataframe.IMP.classic.weighted$pValue,
  OCC.classic.weighted = dataframe.OCC.classic.weighted$pValue,
  IMP.classic.center = dataframe.IMP.classic.center$pValue,
  OCC.classic.center = dataframe.OCC.classic.center$pValue,
  IMP.classic.centertogrid = dataframe.IMP.classic.centertogrid$pValue,
  OCC.classic.centertogrid = dataframe.OCC.classic.centertogrid$pValue,
  IMP.classic.reachedgrid = dataframe.IMP.classic.reachedgrid$pValue,
  OCC.classic.reachedgrid = dataframe.OCC.classic.reachedgrid$pValue,
  IMP.classic.pooling = dataframe.IMP.classic.pooling$pValue,
  OCC.classic.pooling = dataframe.OCC.classic.pooling$pValue,
  IMP.area.center = dataframe.IMP.area.center$pValue,
  OCC.area.center = dataframe.OCC.area.center$pValue,
  IMP.area.centertogrid = dataframe.IMP.area.centertogrid$pValue,
  OCC.area.centertogrid = dataframe.OCC.area.centertogrid$pValue,
  IMP.area.reachedgrid = dataframe.IMP.area.reachedgrid$pValue,
  OCC.area.reachedgrid = dataframe.OCC.area.reachedgrid$pValue,
  IMP.area.weighted = dataframe.IMP.area.weighted$pValue,
  OCC.area.weighted = dataframe.OCC.area.weighted$pValue,
  IMP.area.pooling = dataframe.IMP.area.pooling$pValue,
  OCC.area.pooling = dataframe.OCC.area.pooling$pValue,
  IMP.center.centertogrid = dataframe.IMP.center.centertogrid$pValue,
  OCC.center.centertogrid = dataframe.OCC.center.centertogrid$pValue,
  IMP.center.reachedgrid = dataframe.IMP.center.reachedgrid$pValue,
  OCC.center.reachedgrid = dataframe.OCC.center.reachedgrid$pValue,
  IMP.center.weighted = dataframe.IMP.center.weighted$pValue,
  OCC.center.weighted = dataframe.OCC.center.weighted$pValue,
  IMP.center.pooling = dataframe.IMP.center.pooling$pValue,
  OCC.center.pooling = dataframe.OCC.center.pooling$pValue,
  IMP.centertogrid.reachedgrid = dataframe.IMP.centertogrid.reachedgrid$pValue,
  OCC.centertogrid.reachedgrid = dataframe.OCC.centertogrid.reachedgrid$pValue,
  IMP.centertogrid.weighted = dataframe.IMP.centertogrid.weighted$pValue,
  OCC.centertogrid.weighted = dataframe.OCC.centertogrid.weighted$pValue,
  IMP.centertogrid.pooling = dataframe.IMP.centertogrid.pooling$pValue,
  OCC.centertogrid.pooling = dataframe.OCC.centertogrid.pooling$pValue,  
  IMP.reachedgrid.weighted = dataframe.IMP.reachedgrid.weighted$pValue,
  OCC.reachedgrid.weighted = dataframe.OCC.reachedgrid.weighted$pValue,
  IMP.reachedgrid.pooling = dataframe.IMP.reachedgrid.pooling$pValue,
  OCC.reachedgrid.pooling = dataframe.OCC.reachedgrid.pooling$pValue,
  IMP.weighted.pooling = dataframe.IMP.weighted.pooling$pValue,
  OCC.weighted.pooling = dataframe.OCC.weighted.pooling$pValue
  
)

# Schleife über alle p-Werte Gruppen, Berechnung und Speicherung der Ergebnisse
for (name in names(p_values_list)) {
  combined_p_value <- calculate_combined_p_value(p_values_list[[name]])
  combined_p_values_df <- rbind(combined_p_values_df, data.frame(category = name, combined_p_value = combined_p_value))
}

# Anzeigen des DataFrames mit den kombinierten p-Werten
print(combined_p_values_df)




