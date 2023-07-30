library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)
# Laden des Pakets "bnlearn" für Bayes'sche Netzwerke
library(bnlearn)


answerstable <- read.csv(file = 'myapp/Data/RQ1_corrected_scaled.csv', header=TRUE) #importiere das answers file
dfall <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)
scenarios <- as.data.frame(table(dfall$QUES_ID))
numberscenarios  <- nrow(scenarios)
for (anz in 1:1){ #numberscenarios) {
  actualscenario =as.vector(scenarios[anz,1])
  #print (actualscenario)
  scentext <- (paste0("Scenario ", actualscenario))
  #print (actualscenario) 
  df <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")
  #print (df)
  numberofanswers <- nrow(df)
  
  
  uberschneidung_xmin <- max(df$scaled_X1)
  uberschneidung_xmax <- min(df$scaled_X2)
  uberschneidung_ymin <- max(df$scaled_Y1)
  uberschneidung_ymax <- min(df$scaled_Y2)
  
  
  cat("Überschneidungsbereich für X: [",
      uberschneidung_xmin, ", ", uberschneidung_xmax, "]\n", sep = "")
  cat("Überschneidungsbereich für Y: [",
      uberschneidung_ymin, ", ", uberschneidung_ymax, "]\n", sep = "")
  
  risikowert_x <- mean(c(uberschneidung_xmin, uberschneidung_xmax))
  risikowert_y <- mean(c(uberschneidung_ymin, uberschneidung_ymax))
  
  # Ausgabe der Risikowerte
  cat("Aggregierter Risikowert für X: ", risikowert_x, "\n", sep = "")
  cat("Aggregierter Risikowert für Y: ", risikowert_y, "\n", sep = "")
 
  unsicherheit_x <- uberschneidung_xmax - uberschneidung_xmin
  unsicherheit_y <- uberschneidung_ymax - uberschneidung_ymin
  
  # Ausgabe der Unsicherheitswerte
  cat("Unsicherheit für X: ", unsicherheit_x, "\n", sep = "")
  cat("Unsicherheit für Y: ", unsicherheit_y, "\n", sep = "")
   
  
  ######ANDERE METHODE 
  
  # Berechnen Sie den Durchschnitt der Minimal- und Maximalwerte
  zentral_x <- mean(c(min(df$scaled_X1), max(df$scaled_X2)))
  zentral_y <- mean(c(min(df$scaled_Y1), max(df$scaled_Y2)))
  
  # Berechnen Sie den Bereich
  reichweite_x <- max(df$scaled_X2) - min(df$scaled_X1)
  reichweite_y <- max(df$scaled_Y2) - min(df$scaled_Y1)
  
  # Ausgabe der aggregierten Werte und Unsicherheit
  cat("Aggregierter zentraler Punkt für X: ", zentral_x, "\n", sep = "")
  cat("Aggregierter zentraler Punkt für Y: ", zentral_y, "\n", sep = "")
  cat("Reichweite für X: ", reichweite_x, "\n", sep = "")
  cat("Reichweite für Y: ", reichweite_y, "\n", sep = "")
  
  
  
  # Erstellen eines leeren Netzwerkobjekts
  network <- empty.graph(nodes = c("xmin", "xmax", "ymin", "ymax"))
  
  dfnet <- data.frame(xmin = df$scaled_X1,
                      xmax = df$scaled_X2,
                      ymin = df$scaled_Y1,
                      ymax = df$scaled_Y2)
  
  # Hinzufügen der Kanten zum Netzwerk
  network <- set.arc(network, from = "xmin", to = "xmax")
  network <- set.arc(network, from = "ymin", to = "ymax")
  
  # Hinzufügen der Kanten zum Netzwerk
  network <- bn.fit(network, data = dfnet)
  # Ausgabe des Netzwerks
  print(network)


}
  
  
  #png(file=filetitleImpact,width=1500, height=1000, res=150)
  #hist(IMPACT,breaks = bin_grenzen, main=headtitleImpact, xlab = "Werte", ylab = "Häufigkeit", xlim = c(0, 100), col = "lightblue", border = "black")
  #dev.off()

#  png(file=filetitleOcc,width=1500, height=1000, res=150)
#  hist(LIKELIHOOD,breaks = bin_grenzen, main = headtitleOcc, xlab = "Werte", ylab = "Häufigkeit", xlim = c(0, 100), col = "lightblue", border = "black")
#  dev.off()
  
    
  #scenfile <- (paste0("myapp/files/4_heatmap/",scentext,"_transformed_new.xlsx"))
  #scenpic <- (paste0("myapp/pictures/17_heatmap_pixel_graphic/",scentext,"_heatmap.bmp"))
  
  #mat1 <- matrix(D$count,ncol=400,nrow=400,byrow=TRUE)
  #datahm <- as.matrix(mat1)  
  
  #bmp(file=scenpic, width = 1000, height = 1000, units = 'px', res = 100)
  #heatmap(datahm, Colv = NA, Rowv = NA, scale="none")
  #dev.off()
  
  
  #print (D)  
  
  #print (scenfile)
  
  #write.csv(D, paste0("myapp/files/4_heatmap/", scentext,"_transformed_new.csv"), row.names=TRUE)
  #write.xlsx(D,file = scenfile, rowNames=TRUE)
  
