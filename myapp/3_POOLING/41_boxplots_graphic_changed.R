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
  print (numberofanswers)
  
  x.daten <- data.frame(ACCID = character(),
                        Wert = numeric(),
                        stringsAsFactors = FALSE)
  
  y.daten <- data.frame(ACCID = character(),
                        Wert = numeric(),
                        stringsAsFactors = FALSE)
  
  
  for (i in 1:numberofanswers){
    ACCID <- actual.df[i,"ACC2SURV_ACCID"]
    
    x.min <- actual.df[i,"scaled_X1"]
    x.max <- actual.df[i,"scaled_X2"]
    y.min <- actual.df[i,"scaled_Y1"]
    y.max <- actual.df[i,"scaled_Y2"]

    x_user_werte <- seq(x.min, x.max, by = 0.25)
    y_user_werte <- seq(y.min, y.max, by = 0.25)
    
    x_daten_user <- data.frame(ACCID = rep(ACCID, length(x_user_werte)),
                               Wert = x_user_werte)
    x.daten <- rbind(x.daten, x_daten_user)
    
    y_daten_user <- data.frame(ACCID = rep(ACCID, length(y_user_werte)),
                               Wert = y_user_werte)
    y.daten <- rbind(y.daten, y_daten_user)
  }
  
    directory <- "myapp/pictures/41_boxplots"
  filename <- paste("boxplot_OCCURRENCE_",actualscenario,".png", sep = "")
  
  # Kompletten Dateipfad erstellen
  filepath <- file.path(directory, filename)
  
  # Öffne die PDF-Datei zum Speichern des Boxplots
  png(filepath, width = 200, height = 500)

boxplot(y.daten$Wert, main = "OCCURRENCE", ylab = "value", ylim = c(0, 100))
mean_value <- mean(y.daten$Wert)
points(mean_value, col = "red", pch = 19)


dev.off()  

directory <- "myapp/pictures/41_boxplots"
filename <- paste("boxplot_IMPACT_",actualscenario,".png", sep = "")


# Kompletten Dateipfad erstellen
filepath <- file.path(directory, filename)

# Öffne die PDF-Datei zum Speichern des Boxplots
png(filepath, width = 200, height = 500)

boxplot(x.daten$Wert, main = "IMPACT", ylab = "value", ylim = c(0, 100))
mean_value <- mean(x.daten$Wert)
points(mean_value, col = "red", pch = 19)


dev.off()  


  
  # Spezifiziere den Pfad, den Dateinamen und die Größe des zu speichernden Plots
  #ordner <- "myapp/pictures/40_boxplots" # Setze hier deinen gewünschten Ordnerpfad
  #dateiname <- "mein_boxplot.png" # Setze hier deinen gewünschten Dateinamen
  #volle_pfad <- file.path(pfad, dateiname) # Kombiniere Ordnerpfad und Dateinamen
  
  
  
  
}