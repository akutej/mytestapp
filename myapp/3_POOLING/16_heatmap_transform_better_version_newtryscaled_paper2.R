library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)
library(reshape2) 

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
  values <- seq(0, 100, by = 0.25)
  D <- expand.grid(values, values)
  D <- cbind(D, count = 0)
  names(D) <- c("x", "y", "count")
  print (D)

for (i in 1:numberofanswers){
  print (i)
  AccId <- df[i,"ACC2SURV_ACCID"]
  QuesId <- df[i,"QUES_ID"]
  UncertaintyI <- df[i,"uncertaintyIPixel"]
  UncertaintyO <- df[i,"uncertaintyOPixel"]
  Role <- df[i,"ACC2SURV_ROLE"]
  GroupId <- df[i,"ACC2SURV_GROUPID"]
  x.min <- df[i,"scaled_X1"]
  x.max <- df[i,"scaled_X2"]
  y.min <- df[i,"scaled_Y1"]
  y.max <- df[i,"scaled_Y2"]

  
  
  index <- which( D[,"x"] >= x.min & D[,"x"] <= x.max &
                    D[,"y"] >= y.min & D[,"y"] <= y.max)
  
  D[index,"count"] <- D[index,"count"] + 1 
 
  
}
  print(D)

  
  scenfile <- (paste0("myapp/files/4_heatmap/",scentext,"_transformed_new.xlsx"))
  scenpic <- (paste0("myapp/pictures/paper2/",scentext,"_heatmap.png"))
  
  mat1 <- matrix(D$count,ncol=401,nrow=401,byrow=TRUE)
  
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
    geom_tile() +  # Heatmap-Zellen
    scale_fill_gradientn(colors = heatmap_colors) +  # Nutze die exakte heatmap()-Farbpalette
    theme_void() +  # ❌ Entfernt Achsen, Titel und Hintergrund komplett
    theme(legend.position = "none",  # ❌ Entfernt Legende (falls nicht gewünscht)
          plot.margin = margin(0, 0, 0, 0))+ # ❌ Entfernt äußere Ränder
    coord_fixed(xlim = c(0, 400), ylim = c(0, 400), expand = FALSE) +  # 📏 FIX: Heatmap 0-400
    
    geom_rect(aes(xmin=0, xmax=400, ymin=400, ymax=450),fill="lightgrey", color="black", alpha=1)+
    geom_rect(aes(xmin=0, xmax=400, ymin=450, ymax=500),fill="grey", color="black", alpha=1)+
    geom_rect(aes(xmin=-50, xmax=0, ymin=0, ymax=400),fill="lightgrey", color="black", alpha=1)+
    geom_rect(aes(xmin=-100, xmax=-50, ymin=0, ymax=400),fill="grey", color="black", alpha=1)+
    geom_text(aes(x = -25, y = 40, label = "low"),size = 7,angle=90)+
    geom_text(aes(x = -25, y = 200, label = "medium"),size = 7,angle=90)+
    geom_text(aes(x = -25, y = 360, label = "high"),size = 7,angle=90)+
    geom_text(aes(x = 40, y = 425, label = "low"),size = 7)+
    geom_text(aes(x = 200, y = 425, label = "medium"),size = 7)+
    geom_text(aes(x = 360, y = 425, label = "high"),size = 7)+
    geom_text(aes(x = 200, y = 475, label = "IMPACT"),size = 10)+
    geom_text(aes(x = -75, y = 200, label = "PROBABILITY OF OCCURRENCE"),size = 10,angle=90)
  # Speichern mit originalgetreuer Farbgebung
  ggsave(filename = scenpic, plot = heatmap_plot, width = 23, height = 23, dpi = 600)
  
#print (D)  

#print (scenfile)

#write.csv(D, paste0("myapp/files/4_heatmap/", scentext,"_transformed_new.csv"), row.names=TRUE)
#write.xlsx(D,file = scenfile, rowNames=TRUE)
  
}