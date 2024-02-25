library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)
library(tibble)
weighteddataframe <- read.csv("myapp/files/80_distances/weighted_pValue_Chances.csv")
dataframe <- read.csv("myapp/files/80_distances/distances_pValue_Chances.csv")
#print (dataframe)
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



dataframe2.IMP.classic.weighted <- weighteddataframe %>% filter(method1 == "classic" & method2 == "weighted" & type == "IMPACT")
dataframe2.OCC.classic.weighted <- weighteddataframe %>% filter(method1 == "classic" & method2 == "weighted" & type == "OCCURRENCE")

dataframe2.IMP.area.weighted <- weighteddataframe %>% filter(method1 == "area" & method2 == "weighted" & type == "IMPACT")
dataframe2.OCC.area.weighted <- weighteddataframe %>% filter(method1 == "area" & method2 == "weighted" & type == "OCCURRENCE")

dataframe2.IMP.center.weighted <- weighteddataframe %>% filter(method1 == "center" & method2 == "weighted" & type == "IMPACT")
dataframe2.OCC.center.weighted <- weighteddataframe %>% filter(method1 == "center" & method2 == "weighted" & type == "OCCURRENCE")

dataframe2.IMP.centertogrid.weighted <- weighteddataframe %>% filter(method1 == "centertogrid" & method2 == "weighted" & type == "IMPACT")
dataframe2.OCC.centertogrid.weighted <- weighteddataframe %>% filter(method1 == "centertogrid" & method2 == "weighted" & type == "OCCURRENCE")

dataframe2.IMP.reachedgrid.weighted <- weighteddataframe %>% filter(method1 == "reachedgrid" & method2 == "weighted" & type == "IMPACT")
dataframe2.OCC.reachedgrid.weighted <- weighteddataframe %>% filter(method1 == "reachedgrid" & method2 == "weighted" & type == "OCCURRENCE")

dataframe2.IMP.pooling.weighted <- weighteddataframe %>% filter(method1 == "pooling" & method2 == "weighted" & type == "IMPACT")
dataframe2.OCC.pooling.weighted <- weighteddataframe %>% filter(method1 == "pooling" & method2 == "weighted" & type == "OCCURRENCE")

#print (dataframe.IMP.classic.area)


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

scentext <- paste0("fisher")
scenfile <- paste0("myapp/files/80_distances/pValue_fisher_CHANCES.xlsx")

write.csv(combined_p_values_df, file = paste0("myapp/files/80_distances/pValue_fisher_CHANCES.csv"), row.names = TRUE)

# Erstellt einen neuen Workbook und füge den transponierten Dataframe ein
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", combined_p_values_df)

# Speichern des Workbooks als XLSX-Datei
saveWorkbook(wb, file = scenfile, overwrite = TRUE)

# Erstellen eines leeren DataFrames zur Speicherung der Ergebnisse
w.combined_p_values_df <- data.frame(
  category = character(),
  combined_p_value = numeric(),
  stringsAsFactors = FALSE
)

wp_values_list <- list(
IMP.classic.weighted = dataframe2.IMP.classic.weighted$pValue,
OCC.classic.weighted = dataframe2.OCC.classic.weighted$pValue, 
IMP.area.weighted = dataframe2.IMP.area.weighted$pValue, 
OCC.area.weighted = dataframe2.OCC.area.weighted$pValue,
IMP.center.weighted = dataframe2.IMP.center.weighted$pValue, 
OCC.center.weighted = dataframe2.OCC.center.weighted$pValue,
IMP.centertogrid.weighted = dataframe2.IMP.centertogrid.weighted$pValue, 
OCC.centertogrid.weighted = dataframe2.OCC.centertogrid.weighted$pValue,
IMP.reachedgrid.weighted = dataframe2.IMP.reachedgrid.weighted$pValue, 
OCC.reachedgrid.weighted = dataframe2.OCC.reachedgrid.weighted$pValue,
IMP.pooling.weighted = dataframe2.IMP.pooling.weighted$pValue, 
OCC.pooling.weighted = dataframe2.OCC.pooling.weighted$pValue
)

#print (wp_values_list)

# Schleife über alle p-Werte Gruppen, Berechnung und Speicherung der Ergebnisse
for (name in names(wp_values_list)) {
  #print (wp_values_list[[name]])
  w.combined_p_value <- calculate_combined_p_value(wp_values_list[[name]])
  w.combined_p_values_df <- rbind(w.combined_p_values_df, data.frame(category = name, combined_p_value = w.combined_p_value))
}

# Anzeigen des DataFrames mit den kombinierten p-Werten
print(w.combined_p_values_df)


scentext <- paste0("weighted.fisher")
scenfile <- paste0("myapp/files/80_distances/weighted_pValue_fisher_CHANCES.xlsx")

write.csv(w.combined_p_values_df, file = paste0("myapp/files/80_distances/weighted_pValue_fisher_CHANCES.csv"), row.names = TRUE)

# Erstellt einen neuen Workbook und füge den transponierten Dataframe ein
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", w.combined_p_values_df)

# Speichern des Workbooks als XLSX-Datei
saveWorkbook(wb, file = scenfile, overwrite = TRUE)



