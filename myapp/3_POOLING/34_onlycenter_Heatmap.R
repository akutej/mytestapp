#Aufbau und Speicherung des Histogramms für Eintritt und Auswirkung jedes Szenarios auf Basis der graphischen Methode - nur center


library(plotrix)
library(dplyr)
library(ggplot2)
library(grid)
library(RColorBrewer)

answerstable <- read.csv(file = 'myapp/Data/RQ1_corrected_scaled.csv', header=TRUE) #importiere das answers file

dfall <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)
scenarios <- as.data.frame(table(dfall$QUES_ID))
numberscenarios  <- nrow(scenarios)

for (anz in 1:numberscenarios) {
  actualscenario =as.vector(scenarios[anz,1])
  scentext <- (paste0("", actualscenario))
  print (scentext)
  df <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == actualscenario)
  actualtype <- (df$QUES_TYP[1])
  numberofanswers <- nrow(df)
  print (numberofanswers)
  IMPACT <- df[,'scaled_uncertainty_middle_X']
  OCCURRENCE <- df[,'scaled_uncertainty_middle_Y']
  headtitleImpact <- (paste0(scentext,"- Impact of classical method"))
  headtitleOcc <- (paste0(scentext,"- Probability of occurrence of the classical method"))
  filetitleImpact <- (paste0("myapp/pictures/34_hist_graphic_center/",scentext,"- Impact.png"))
  filetitleOcc <- (paste0("myapp/pictures/34_hist_graphic_center/",scentext,"- Occurrence.png"))
  #histImpact <- replace(histImpactclassic,,histImpactclassic-0.5)
  #histImpact <- sapply(histImpactclassic,quantile)
  
  merged_df <- data.frame(x = numeric(0), y = numeric(0))
  
  # Annahme: df ist dein ursprünglicher DataFrame mit den Werten
  
  for (m in 1:numberofanswers){
    my.x <- df[m,'scaled_uncertainty_middle_X']
    my.y <- df[m,'scaled_uncertainty_middle_Y']
  
    merged_df <- rbind(merged_df, data.frame(x = my.x, y = my.y))
    }
  
  print (merged_df)
 
  heatmap_plot <- ggplot(merged_df, aes(x = x, y = y)) +
    #geom_tile() +
    geom_point(size = 1, color = "orange") +
    #labs(title = "Heatmap meiner Punkte", x = "X-Werte", y = "Y-Werte") +
    theme(panel.grid.major = element_line(colour = "gray80"), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          axis.text = element_blank(),
          axis.title = element_blank(), 
          axis.ticks = element_blank())+  # Achsenticks ausgeschaltet+
    scale_x_continuous(breaks = seq(0, 100, length.out = 6), expand = c(0, 0), limits = c(0, 100)) +
    scale_y_continuous(breaks = seq(0, 100, length.out = 6), expand = c(0, 0), limits = c(0, 100))
  

  filename <- (paste0("myapp/pictures/34_onlycenter_heatmap/",scentext,".png"))
  
  ggsave(filename = filename, plot = heatmap_plot, width = 1000, height = 1000, units = "px", dpi = 300)
  
  #png (file=filetitleImpact, width = 1000, height = 1000, units = 'px', res = 100)
  #heatmap(kde$z, Rowv = NA, Colv = NA, scale = "none",labRow = NA, labCol = NA) 
  #dev.off()
    
  
  #p <- ggplot(df, aes(x = x, y = y)) + 
  #  geom_density2d_filled() + 
  #  theme_minimal() +
  #  labs(title = "Heatmap von zwei Verteilungen", x = "X Verteilung", y = "Y Verteilung")
  
  #print (p)
  
  
  #kde <- kde2d(simulations.y, simulations.x, n = 400) 
  
  #png (file=filetitleImpact, width = 1000, height = 1000, units = 'px', res = 100)
  #heatmap(kde$z, Rowv = NA, Colv = NA, scale = "none",labRow = NA, labCol = NA) 
  #dev.off()
  
  
  #punkte <- merge(df[,'scaled_uncertainty_middle_X'],df[,'scaled_uncertainty_middle_Y'])
  #data_matrix <- as.matrix(df[,'scaled_uncertainty_middle_X'],df[,'scaled_uncertainty_middle_Y'])
  #print (punkte)

  #min_x <- min(IMPACT)
  #max_x <- max(IMPACT)
  #bin_grenzen <- seq(min_x, max_x, by = 0.25)
  
  
  # Erstelle die Heatmap mit der heatmap() Funktion
  #heatmap(data_matrix, Rowv = NA, Colv = NA, scale = "none")
  
  ############NEU 
  #heatmap_plot <- ggplot(punkte, aes(x = x, y = y)) +
  #  geom_tile() +
  #      labs(title = "Heatmap meiner Punkte", x = "X-Werte", y = "Y-Werte")
  #print(heatmap_plot)
  #####NEU ENDE 
  
  #filetitleImpact1 <- (paste0("myapp/pictures/34_hist_graphic_center/",scentext,"_steps_Impact.png"))
  #filetitleOcc1 <- (paste0("myapp/pictures/34_hist_graphic_center/",scentext,"_steps_Occurrence.png"))
  
  #png(file=filetitleOcc1,width=1500, height=1000, res=150)
  #hist(OCCURRENCE,breaks = bin_grenzen, main = "", xlab = "Occurrence value", ylab = "Frequency", xlim = c(0, 100), col = "lightblue", border = "black")
  #dev.off()
  
  
}