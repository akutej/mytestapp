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
  
  directory <- "myapp/pictures/40_boxplots"
  filename <- paste("boxplot_OCCURRENCE_",actualscenario,".png", sep = "")
  
  # Kompletten Dateipfad erstellen
  filepath <- file.path(directory, filename)
  
  # Öffne die PDF-Datei zum Speichern des Boxplots
  png(filepath, width = 200, height = 500)

boxplot(actual.df$scaled_OCCURRENCE, main = "OCCURRENCE", ylab = "value", ylim = c(0, 100))
mean_value <- mean(actual.df$scaled_OCCURRENCE)
points(mean_value, col = "red", pch = 19)


dev.off()  

directory <- "myapp/pictures/40_boxplots"
filename <- paste("boxplot_IMPACT_",actualscenario,".png", sep = "")


# Kompletten Dateipfad erstellen
filepath <- file.path(directory, filename)

# Öffne die PDF-Datei zum Speichern des Boxplots
png(filepath, width = 200, height = 500)

boxplot(actual.df$scaled_IMPACT, main = "IMPACT", ylab = "value", ylim = c(0, 100))
mean_value <- mean(actual.df$scaled_IMPACT)
points(mean_value, col = "red", pch = 19)


dev.off()  


  
  # Spezifiziere den Pfad, den Dateinamen und die Größe des zu speichernden Plots
  #ordner <- "myapp/pictures/40_boxplots" # Setze hier deinen gewünschten Ordnerpfad
  #dateiname <- "mein_boxplot.png" # Setze hier deinen gewünschten Dateinamen
  #volle_pfad <- file.path(pfad, dateiname) # Kombiniere Ordnerpfad und Dateinamen
  
  
  
  
}