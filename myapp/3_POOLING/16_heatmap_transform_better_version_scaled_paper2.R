library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)

answerstable <- read.csv(file = 'myapp/data/RQ1_corrected_scaled.csv', header=TRUE) #importiere das answers file
dfall <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)
dfall <- dfall %>% filter(QUES_ID == "359")
scenarios <- as.data.frame(table(dfall$QUES_ID))
numberscenarios  <- nrow(scenarios)
for (anz in 1:numberscenarios) {
  actualscenario =as.vector(scenarios[anz,1])
  print (actualscenario)
  scentext <- (paste0("Scenario ", actualscenario))
  print (actualscenario) 
  df <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")
  #print (df)
  numberofanswers <- nrow(df)
  vec1 <- seq(0.25, 100, by = 0.25)
  vec2 <- seq(0.25, 100, by = 0.25)
  D <- as.data.frame(expand.grid(vec1,vec2))
  D <- cbind(D,0)
  names(D) <- c("x","y","count")

for (i in 1:numberofanswers){
  print (i)
  AccId <- df[i,"ACC2SURV_ACCID"]
  QuesId <- df[i,"QUES_ID"]
  UncertaintyI <- df[i,"scaled_uncertainty_X"]
  UncertaintyO <- df[i,"scaled_uncertainty_Y"]
  Role <- df[i,"ACC2SURV_ROLE"]
  GroupId <- df[i,"ACC2SURV_GROUPID"]
  x.min <- df[i,"scaled_X1"]
  print (x.min)
  x.max <- df[i,"scaled_X2"]
  y.min <- df[i,"scaled_Y1"]
  y.max <- df[i,"scaled_Y2"]
  
  
  index <- which( D[,"x"] >= x.min & D[,"x"] <= x.max &
                    D[,"y"] >= y.min & D[,"y"] <= y.max)
  
  D[index,"count"] <- D[index,"count"] + 1 
 
  
}

  
  scenfile <- (paste0("myapp/files/4_heatmap/",scentext,"_transformed_new_scaled.xlsx"))
  scenpic <- (paste0("myapp/pictures/paper2/",scentext,"_heatmap_scaled.png"))
  
  mat1 <- matrix(D$count,ncol=400,nrow=400,byrow=TRUE)
  datahm <- as.matrix(mat1)  
  #png(file=scenpic, width = 1000, height = 1000, units = 'px', res = 100)
  #heatmap(datahm, Colv = NA, Rowv = NA, scale="none",labRow = NA, labCol = NA) # Achsenbeschriftungen entfernen
  
  #dev.off()
  
  # Erstelle die x- und y-Werte für die Matrix-Koordinaten
  x_vals <- seq(1, ncol(datahm))  # Spalten (x-Achse)
  y_vals <- seq(1, nrow(datahm))  # Zeilen (y-Achse)
  
  # Erstelle ein DataFrame mit den Koordinaten
  D_long <- expand.grid(x = x_vals, y = y_vals)
  
  # Füge die Werte aus der Matrix hinzu
  D_long$count <- as.vector(datahm)  # Die Matrix wird in einen Vektor umgewandelt
  
  library(ggplot2)
  
  # Farbpalette aus base R übernehmen
  heatmap_colors <- colorRampPalette(c("#FFF5E1", "#F5DEB3", "#E69F00", "#D55E00", "#8B0000"))(256)  
  #heatmap_colors <- rev(heat.colors(256))
  #heat_colors <- colorRampPalette(c("blue", "yellow", "red"))(256) 
  
  heatmap_plot <- ggplot(D_long, aes(x = y, y = x, fill = count)) +
    
    # Hintergrundbalken zuerst zeichnen, damit sie nicht über der Heatmap liegen
    #geom_rect(xmin = 0, xmax = 400, ymin = 400, ymax = 450, fill = "lightgrey", color = "black", alpha = 1) +
    #geom_rect(xmin = 0, xmax = 400, ymin = 450, ymax = 500, fill = "grey", color = "black", alpha = 1) +
    #geom_rect(xmin = -50, xmax = 0, ymin = 0, ymax = 400, fill = "lightgrey", color = "black", alpha = 1) +
    #geom_rect(xmin = -100, xmax = -50, ymin = 0, ymax = 400, fill = "grey", color = "black", alpha = 1) +
    
    # Heatmap über den Rechtecken
    geom_tile() +  
    scale_fill_gradientn(colors = heatmap_colors) +  
    
    # Theme und Achsenfixes
    theme_void() +  
    theme(legend.position = "none",  
          plot.margin = margin(0, 0, 0, 0)) +   # HIER MUSS DAS `+` SEIN!
    coord_fixed(xlim = c(0, 400), ylim = c(0, 400), expand = FALSE)   
  
  # Beschriftungen für die Skalen
  #geom_text(aes(x = -25, y = 40, label = "low"), size = 7, angle=90) +
  #geom_text(aes(x = -25, y = 200, label = "medium"), size = 7, angle=90) +
  #geom_text(aes(x = -25, y = 360, label = "high"), size = 7, angle=90) +
  #geom_text(aes(x = 40, y = 425, label = "low"), size = 7) +
  #geom_text(aes(x = 200, y = 425, label = "medium"), size = 7) +
  #geom_text(aes(x = 360, y = 425, label = "high"), size = 7) +
  #geom_text(aes(x = 200, y = 475, label = "IMPACT"), size = 10) +
  #geom_text(aes(x = -75, y = 200, label = "PROBABILITY OF OCCURRENCE"), size = 10, angle=90)
  
  # Speichern mit exakten Abmessungen
  ggsave(filename = scenpic, plot = heatmap_plot, width = 2, height = 2, dpi = 600)
  
  
}