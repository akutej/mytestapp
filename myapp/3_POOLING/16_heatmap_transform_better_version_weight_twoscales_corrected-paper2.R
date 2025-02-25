library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)

answerstable <- read.csv(file = 'myapp/data/RQ1_corrected.csv', header=TRUE) #importiere das answers file
dfall <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == 281)
dfall <- dfall %>% filter(QUES_ID == "359")
scenarios <- as.data.frame(table(dfall$QUES_ID))
numberscenarios  <- nrow(scenarios)
for (anz in 1:numberscenarios) {
  actualscenario =as.vector(scenarios[anz,1])
  print (actualscenario)
  scentext <- (paste0("Scenario ", actualscenario))
  #print (actualscenario) 
  df <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")
  #print (df)
  numberofanswers <- nrow(df)
  D <- as.data.frame(expand.grid(1:400,1:400))
  D <- cbind(D,0)
  names(D) <- c("x-Achse","y-Achse","Zähler")
  Overall.Scenario.Impact.Uncertainty <- sum(df$uncertaintyIPixel)
  Overall.Scenario.Occurrence.Uncertainty <- sum(df$uncertaintyOPixel)
  max.Scenario.Impact.Uncertainty <- max(df$uncertaintyIPixel)
  min.Scenario.Impact.Uncertainty <- min(df$uncertaintyIPixel)
  max.Scenario.Occurrence.Uncertainty <- max(df$uncertaintyOPixel)
  min.Scenario.Occurrence.Uncertainty <- min(df$uncertaintyOPixel)
  diffmaxmin.Scenario.Impact.Uncertainty <- max.Scenario.Impact.Uncertainty - min.Scenario.Impact.Uncertainty
  diffmaxmin.Scenario.Occurrence.Uncertainty <- max.Scenario.Occurrence.Uncertainty - min.Scenario.Occurrence.Uncertainty
  
  Overall.Scenario.Area.Uncertainty <- sum(df$uncertaintyAreaPixel)
  max.Scenario.Area.Uncertainty <- max(df$uncertaintyAreaPixel)
  min.Scenario.Area.Uncertainty <- min(df$uncertaintyAreaPixel)
  diffmaxmin.Scenario.Area.Uncertainty <- max.Scenario.Area.Uncertainty - min.Scenario.Area.Uncertainty
  #print (max.Scenario.Impact.Uncertainty)
  #print (min.Scenario.Impact.Uncertainty)
  #print (diffmaxmin.Scenario.Impact.Uncertainty)
  #print (max.Scenario.Occurrence.Uncertainty)
  #print (min.Scenario.Occurrence.Uncertainty)
  #print (diffmaxmin.Scenario.Occurrence.Uncertainty)
  #print (Overall.Scenario.Impact.Uncertainty)
  #print (Overall.Scenario.Occurrence.Uncertainty)
  #print(max.Scenario.Area.Uncertainty)
  #print(min.Scenario.Area.Uncertainty)
  #print (diffmaxmin.Scenario.Area.Uncertainty)
  sum.Occ <- 0
  sum.Imp <- 0
  sum.Scale <- 0
  
  for (m in 1:numberofanswers){
    Uncertainty.I <- df[m,"uncertaintyIPixel"]
    Uncertainty.O <- df[m,"uncertaintyOPixel"]
    #Uncertainty.Area <- df[m,"uncertaintyAreaPixel"]
    Uncertainty.IO <- Uncertainty.I + Uncertainty.O
    Uncertainty.ScaleSwitched <- (800 - Uncertainty.IO)  
    #print (Uncertainty.Area)
    #Uncertainty.Impact <- df[m,"uncertaintyIPixel"]
    #Uncertainty.Occurrence <- df[m,"uncertaintyOPixel"]
    #distance.Occ <- (400-Uncertainty.Occurrence)  #Abstand zu maxWert Occourrence
    #distance.Imp <- (400-Uncertainty.Impact)  #Abstand zu maxWert Impact
    #distance.Area <- (max.Scenario.Area.Uncertainty-Uncertainty.Area)  #Abstand zu maxWert Impact
    #sum.Occ <- sum.Occ + distance.Occ
    #sum.Imp <- sum.Imp + distance.Imp
    sum.Scale <- sum.Scale + Uncertainty.ScaleSwitched
  }  
  #print (sum.Imp)
  #print (sum.Occ)
  print (sum.Scale)
  Uncertaintyweightall <- 0
for (i in 1:numberofanswers){
  #print (i)
  AccId <- df[i,"ACC2SURV_ACCID"]
  QuesId <- df[i,"QUES_ID"]
  UncertaintyI <- df[i,"uncertaintyIPixel"]
  UncertaintyO <- df[i,"uncertaintyOPixel"]
  UncertaintyIO <- UncertaintyI + UncertaintyO
  UncertaintyAreaUser <- df[i,"uncertaintyAreaPixel"]
  Role <- df[i,"ACC2SURV_ROLE"]
  GroupId <- df[i,"ACC2SURV_GROUPID"]
  x.min <- df[i,"X1Pixel"]
  x.max <- df[i,"X2Pixel"]
  y.min <- df[i,"Y1Pixel"]
  y.max <- df[i,"Y2Pixel"]
  Uncertaintyweight <- ((100/sum.Scale)* (800 - UncertaintyIO))
  #print (UncertaintyAreaUser)
  #print (Uncertaintyweight)
  Uncertaintyweightall <- Uncertaintyweightall + Uncertaintyweight
  index <- which( D[,"x-Achse"] >= x.min & D[,"x-Achse"] <= x.max &
                    D[,"y-Achse"] >= y.min & D[,"y-Achse"] <= y.max)
  
  D[index,"Zähler"] <- D[index,"Zähler"] + (1 * Uncertaintyweight)
  #print (AccId)
  print (Uncertaintyweight)
  
}
  print (Uncertaintyweightall)
  #print (D)
  scenfile <- (paste0("myapp/files/4_heatmap/",scentext,"_transformed_new_weight_scales.xlsx"))
  scenpic <- (paste0("myapp/pictures/paper2/",scentext,"_heatmap_weight_scales.png"))
  
  mat1 <- matrix(D$Zähler,ncol=400,nrow=400,byrow=TRUE)
  datahm <- as.matrix(mat1)  
  
  
  #png(file=scenpic, width = 1000, height = 1000, units = 'px', res = 100)
  #heatmap(datahm, Colv = NA, Rowv = NA, scale="none",labRow = NA, labCol = NA) # Achsenbeschriftungen entfernen
  
  #dev.off()
  
  # Erstelle die x- und y-Werte für die Matrix-Koordinaten
  x_vals <- seq(1, ncol(datahm))  # Spalten (x-Achse)
  y_vals <- seq(1, nrow(datahm))  # Zeilen (y-Achse)
  
  # 🔹 Farbpalette für die Heatmap
  heatmap_colors <- colorRampPalette(c("#FFF5E1", "#F5DEB3", "#E69F00", "#D55E00", "#8B0000"))(256)  
  
  # 🔹 Heatmap-Daten bereinigen
  D_long$count <- as.numeric(D_long$count)  # Falls `count` als Faktor gespeichert ist
  D_long$count[is.na(D_long$count)] <- 0  # NA-Werte auf 0 setzen
  D_long$count[D_long$count < 0] <- 0  # Negative Werte auf 0 setzen
  
  # 🔹 Heatmap-Plot mit Achsenbeschriftungen
  heatmap_plot <- ggplot() +  
    # 🔹 Heatmap zeichnen (exakt auf 0-400 begrenzt!)
    geom_tile(data = D_long, aes(x = y, y = x, fill = count), width = 1, height = 1) +  
    scale_fill_gradientn(colors = heatmap_colors) +  # Heatmap-Farben
    
    # 🔹 Verhindere, dass ggplot unnötigen Rand hinzufügt
    coord_fixed(xlim = c(min(D_long$y), max(D_long$y) + 1), 
                ylim = c(min(D_long$x), max(D_long$x) + 1), 
                expand = FALSE) +  
    
    #ggtitle(scentext)+
    theme(#legend.position = "none",
      plot.title = element_blank(),
      axis.line  = element_blank(),
      axis.ticks = element_blank(),
      axis.text  = element_blank(),
      axis.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      #panel.background = element_rect(fill = "white", color = NA),
      panel.background = element_blank(),
      plot.margin = margin(0, 0, 0, 0)) +  
    coord_fixed(xlim = c(-100, 400), ylim = c(0, 500), expand = FALSE) +  
    # 🔹 JETZT den 400x400 Rahmen OBEN drauf zeichnen, damit er sichtbar bleibt!
    geom_rect(aes(xmin = 0, xmax = 400, ymin = 0, ymax = 400), fill = NA, color = "black", size = 0.5) +
    # 🔹 Hintergrundbalken für Achsenbeschriftung
    geom_rect(data = data.frame(), aes(xmin = 0, xmax = 400, ymin = 400, ymax = 450), fill = "lightgrey", color = "black", alpha = 1) +
    geom_rect(data = data.frame(), aes(xmin = 0, xmax = 400, ymin = 450, ymax = 500), fill = "grey", color = "black", alpha = 1) +
    geom_rect(data = data.frame(), aes(xmin = -50, xmax = 0, ymin = 0, ymax = 400), fill = "lightgrey", color = "black", alpha = 1) +
    geom_rect(data = data.frame(), aes(xmin = -100, xmax = -50, ymin = 0, ymax = 400), fill = "grey", color = "black", alpha = 1) +
    
    # 🔹 Textbeschriftungen (Achsenlabels)
    geom_text(data = data.frame(), aes(x = -25, y = 40, label = "low"), size = 7, angle = 90) +
    geom_text(data = data.frame(), aes(x = -25, y = 200, label = "medium"), size = 7, angle = 90) +
    geom_text(data = data.frame(), aes(x = -25, y = 360, label = "high"), size = 7, angle = 90) +
    geom_text(data = data.frame(), aes(x = 40, y = 425, label = "low"), size = 7) +
    geom_text(data = data.frame(), aes(x = 200, y = 425, label = "medium"), size = 7) +
    geom_text(data = data.frame(), aes(x = 360, y = 425, label = "high"), size = 7) +
    geom_text(data = data.frame(), aes(x = 200, y = 475, label = "IMPACT"), size = 10) +
    geom_text(data = data.frame(), aes(x = -75, y = 200, label = "PROBABILITY OF OCCURRENCE"), size = 10, angle = 90)
  
  
  
  
  # 🔹 Speichern mit exakten Abmessungen
  ggsave(filename = scenpic, plot = heatmap_plot, device = "png",  width = 23, height = 23, units = "cm", dpi = 600,limitsize = FALSE)
  
  
  
}