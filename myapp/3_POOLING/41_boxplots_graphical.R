library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)

answers <- read.csv(file = 'myapp/data/RQ1_corrected_scaled.csv', header=TRUE) #importiere das answers file
all.answers <- answers %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)
number.scenarios <- nrow(as.data.frame(table(all.answers$QUES_ID)))
scenarios <- as.data.frame(table(all.answers$QUES_ID))
for (anz in 1:number.scenarios) {
  actualscenario =as.vector(scenarios[anz,1])
  print (actualscenario)
  #scentext <- (paste0("Scenario ", actualscenario))
  actual.df <- answers %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")
  numberofanswers <- nrow(actual.df)
  #print (numberofanswers)
  
  # Leeren Dataframe erstellen
  x.daten <- data.frame(ACCID = character(),
                      Wert = numeric(),
                      stringsAsFactors = FALSE)
  
  y.daten <- data.frame(ACCID = character(),
                        Wert = numeric(),
                        stringsAsFactors = FALSE)
  
  
  for (i in 1:numberofanswers){
    ACCID <- actual.df[i,"ACC2SURV_ACCID"]
    
    #print (paste0("Account: ", ACCID))
    x.min <- actual.df[i,"scaled_X1"]
    x.max <- actual.df[i,"scaled_X2"]
    y.min <- actual.df[i,"scaled_Y1"]
    y.max <- actual.df[i,"scaled_Y2"]
    
    #x.schritte <- (x.max - x.min) / 0.25 + 1 # Anzahl der x Schritte
    #y.schritte <- (y.max - y.min) / 0.25 + 1 # Anzahl der y Schritte
    
    #print (x.schritte)
    #print (y.schritte)
    
    x_user_werte <- seq(x.min, x.max, by = 0.25)
    y_user_werte <- seq(y.min, y.max, by = 0.25)
  
    x_daten_user <- data.frame(ACCID = rep(ACCID, length(x_user_werte)),
                                   Wert = x_user_werte)
    x.daten <- rbind(x.daten, x_daten_user)
    
    y_daten_user <- data.frame(ACCID = rep(ACCID, length(y_user_werte)),
                               Wert = y_user_werte)
    y.daten <- rbind(y.daten, y_daten_user)
  }
  
  # Ergebnis anzeigen
  #print(x.daten)
  #print(y.daten)
  
  p_IMPACT_graphic_scaled <- ggplot(x.daten, aes(x = "B",y = Wert)) +
    geom_boxplot(width = 0.4) +  # Setze die Breite der Boxen auf 20% der Gesamtbreite
    stat_summary(fun = mean, geom = "crossbar", width = 0.4, color = "red") +
    labs(title = "IMPACT"  , y = "value") +
    theme_light(base_size = 14)  +
    theme(axis.title.x = element_blank(), # Beschriftung der X-Achse ausblenden
          axis.text.x = element_blank(),   # Text der X-Achse ausblenden
          axis.ticks.x = element_blank(),
          plot.title = element_text(hjust = 0.5))  # Skalierungsstriche der X-Achse ausblenden
  
  
  
  p_OCCURRENCE_graphic_scaled <- ggplot(y.daten, aes(x = "B",y = Wert)) +
    geom_boxplot(width = 0.4) +  # Setze die Breite der Boxen auf 20% der Gesamtbreite
    stat_summary(fun = mean, geom = "crossbar", width = 0.4, color = "red") +
    labs(title = "OCCURRENCE"  , y = "value") +
    theme_light(base_size = 14)  +
    theme(axis.title.x = element_blank(), # Beschriftung der X-Achse ausblenden
          axis.text.x = element_blank(),   # Text der X-Achse ausblenden
          axis.ticks.x = element_blank(),
          plot.title = element_text(hjust = 0.5))  # Skalierungsstriche der X-Achse ausblenden
  
  

  # Funktion zum Extrahieren der Ergebnisse und Ausgabe als Text
  ausgabe_ggboxplot <- function(data) {
    # Gruppieren und Zusammenfassen der Daten
    summarised_data <- data %>%
                summarize(min = min(Wert),
                q1 = quantile(Wert, 0.25),
                median = median(Wert),
                mean = mean(Wert),
                q3 = quantile(Wert, 0.75),
                max = max(Wert),
                iqr = (quantile(Wert, 0.75) - quantile(Wert, 0.25))
            )
    
    # Erstellen des Texts für die Ausgabe
    text_ausgabe <- with(summarised_data, paste0("\nMinimum:", min,
                                                "\nUnteres Quartil:", q1,
                                                "\nMedian:", median,
                                                "\nMittelwert:", mean,
                                                "\nOberes Quartil:", q3,
                                                "\nMaximum:", max,
                                                "\nInterquartilsabstand:", iqr,"\n\n")                                               )
    
    # Ausgabe des Texts
    invisible(cat(text_ausgabe))
    return(summarised_data)
    
  }
  
  invisible(cat ("IMPACT DATEN:"))
  ergebnis.x <- ausgabe_ggboxplot(x.daten)
  
  # Boxplot anzeigen
  invisible(cat ("OCCURRENCE DATEN:"))
  ergebnis.y <- ausgabe_ggboxplot(y.daten)
  
  #print(p_IMPACT_graphic_scaled)
  #print(p_OCCURRENCE_graphic_scaled)
  
  x_measures <- (t(ergebnis.x))
  y_measures <- (t(ergebnis.y))
  colnames(x_measures) <- c("IMPACT")
  colnames(y_measures) <- c("OCCURRENCE")
  combined_data <- cbind(x_measures, y_measures)
  print (combined_data)
  

  scentext <- paste0("Scenario", actualscenario)
  scenfile <- paste0("myapp/files/40_Streum/", scentext, ".xlsx")
  
  write.csv(combined_data, file = paste0("myapp/files/40_Streum/",scentext,".csv"), row.names = TRUE)
  
  # Erstelle einen neuen Workbook und füge den transponierten Dataframe ein
  wb <- createWorkbook()
  addWorksheet(wb, "Sheet1")
  writeData(wb, "Sheet1", combined_data)
  
  # Speichern des Workbooks als XLSX-Datei
  saveWorkbook(wb, file = scenfile, overwrite = TRUE)
  
  
  
  
}