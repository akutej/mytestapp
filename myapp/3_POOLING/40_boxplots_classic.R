library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)


dfkst <- data.frame(D_value_I = numeric(),D_value_O = numeric(), Label = character())

answers <- read.csv(file = 'myapp/data/RQ1_corrected_scaled.csv', header=TRUE) #importiere das answers file
all.answers <- answers %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)
number.scenarios <- nrow(as.data.frame(table(all.answers$QUES_ID)))
scenarios <- as.data.frame(table(all.answers$QUES_ID))
for (anz in 1:1){#number.scenarios) {
  actualscenario =as.vector(scenarios[anz,1])
  print (actualscenario)
  #scentext <- (paste0("Scenario ", actualscenario))
  actual.df <- answers %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")
  
  data_IMPACT_original <- actual.df$IMPACT
  data_OCCURRENCE_original <- actual.df$OCCURRENCE
  data_IMPACT <- actual.df$scaled_IMPACT
  data_OCCURRENCE <- actual.df$scaled_OCCURRENCE
  ordinal_df_IMPACT <- data.frame(value = data_IMPACT, freq = 1)
  ordinal_df_OCCURRENCE <- data.frame(value = data_OCCURRENCE, freq = 1)
  ordinal_freq_IMPACT <- aggregate(freq ~ value, data = ordinal_df_IMPACT, sum)
  ordinal_freq_OCCURRENCE <- aggregate(freq ~ value, data = ordinal_df_OCCURRENCE, sum)
  
  x.q1 <- quantile(actual.df$IMPACT, 0.25)
  x.q3 <- quantile(actual.df$IMPACT, 0.75)
  x.iqr <- x.q3 - x.q1
  x.lower_whisker <- max(min(actual.df$IMPACT), x.q1 - 1.5 * x.iqr)
  x.upper_whisker <- min(max(actual.df$IMPACT), x.q3 + 1.5 * x.iqr)
  
  x.q1.scaled <- quantile(actual.df$scaled_IMPACT, 0.25)
  x.q3.scaled <- quantile(actual.df$scaled_IMPACT, 0.75)
  x.iqr.scaled <- x.q3.scaled - x.q1.scaled
  x.lower_whisker.scaled <- max(min(actual.df$scaled_IMPACT), x.q1.scaled + 1.5 * x.iqr.scaled)
  x.upper_whisker.scaled <- min(max(actual.df$scaled_IMPACT), x.q3.scaled - 1.5 * x.iqr.scaled)
  
  y.q1 <- quantile(actual.df$OCCURRENCE, 0.25)
  y.q3 <- quantile(actual.df$OCCURRENCE, 0.75)
  y.iqr <- y.q3 - y.q1
  y.lower_whisker <- max(min(actual.df$OCCURRENCE), y.q1 - 1.5 * y.iqr)
  y.upper_whisker <- min(max(actual.df$OCCURRENCE), y.q3 + 1.5 * y.iqr)
  
  y.q1.scaled <- quantile(actual.df$scaled_OCCURRENCE, 0.25)
  y.q3.scaled <- quantile(actual.df$scaled_OCCURRENCE, 0.75)
  y.iqr.scaled <- y.q3.scaled - y.q1.scaled
  y.lower_whisker.scaled <- max(min(actual.df$scaled_OCCURRENCE), y.q1.scaled - 1.5 * y.iqr.scaled)
  y.upper_whisker.scaled <- min(max(actual.df$scaled_OCCURRENCE), y.q3.scaled + 1.5 * y.iqr.scaled)
  
  numberofanswers <- nrow(actual.df)
  print (numberofanswers)
  
  boxplot(actual.df$scaled_IMPACT, main = "Boxplot", ylab = "value")
  
  p_IMPACT_classic_original <- ggplot(actual.df, aes(x = "A",y = IMPACT)) +
    geom_boxplot(width = 0.4) +  # Setze die Breite der Boxen auf 20% der Gesamtbreite
    stat_summary(fun = mean, geom = "crossbar", width = 0.4, color = "red") +
    labs(title = "IMPACT", y = "value") +
    theme_light(base_size = 14) +
    theme(axis.title.x = element_blank(), # Beschriftung der X-Achse ausblenden
          axis.text.x = element_blank(),   # Text der X-Achse ausblenden
          axis.ticks.x = element_blank(),
          plot.title = element_text(hjust = 0.5))  # Skalierungsstriche der X-Achse ausblenden
    
  
  p_IMPACT_classic_scaled <- ggplot(actual.df, aes(x = "B",y = scaled_IMPACT)) +
    geom_boxplot(width = 0.4) +  # Setze die Breite der Boxen auf 20% der Gesamtbreite
    stat_summary(fun = mean, geom = "crossbar", width = 0.4, color = "red") +
    geom_segment(aes(x = 0.85, xend = 1.15, y = x.lower_whisker.scaled, yend = x.lower_whisker.scaled),
                 color = "black", size = 1) +
    geom_segment(aes(x = 0.7, xend = 1.3, y = x.upper_whisker.scaled, yend = x.upper_whisker.scaled),
                 color = "black", size = 1) +
    labs(title =  "IMPACT", y = "value") +
    theme_light(base_size = 14)  +
    theme(axis.title.x = element_blank(), # Beschriftung der X-Achse ausblenden
          axis.text.x = element_blank(),   # Text der X-Achse ausblenden
          axis.ticks.x = element_blank(),
          plot.title = element_text(hjust = 0.5)+  # Skalierungsstriche der X-Achse ausblenden
          ylim(0, 100))  # Skalierung der y-Achse auf den Bereich von 0 bis 100 festlegen
    
  p_OCCURRENCE_classic_original <- ggplot(actual.df, aes(x = "A",y = OCCURRENCE)) +
    geom_boxplot(width = 0.4) +  # Setze die Breite der Boxen auf 20% der Gesamtbreite
    stat_summary(fun = mean, geom = "crossbar", width = 0.4, color = "red") +
    geom_hline(yintercept = y.q1, linetype="dashed", color = "black", linewidth = 1) + # Linie für Q1
    geom_hline(yintercept = y.q3, linetype="dashed", color = "black", linewidth = 1) + # Linie für Q3
    labs(title = "OCCURRENCE", y = "value") +
    theme_light(base_size = 14) +
    theme(axis.title.x = element_blank(), # Beschriftung der X-Achse ausblenden
          axis.text.x = element_blank(),   # Text der X-Achse ausblenden
          axis.ticks.x = element_blank(),
          plot.title = element_text(hjust = 0.5)+  # Skalierungsstriche der X-Achse ausblenden
          ylim(0, 100))  # Skalierung der y-Achse auf den Bereich von 0 bis 100 festlegen
  
  
  p_OCCURRENCE_classic_scaled <- ggplot(actual.df, aes(x = "B",y = scaled_OCCURRENCE)) +
    geom_boxplot(width = 0.4) +  # Setze die Breite der Boxen auf 20% der Gesamtbreite
    stat_summary(fun = mean, geom = "crossbar", width = 0.4, color = "red") +
    geom_hline(yintercept = y.q1.scaled, linetype="dashed", color = "black", linewidth = 1) + # Linie für Q1
    geom_hline(yintercept = y.q3.scaled, linetype="dashed", color = "black", linewidth = 1) + # Linie für Q3
    labs(title = "OCCURRENCE"  , y = "value") +
    theme_light(base_size = 14)  +
    theme(axis.title.x = element_blank(), # Beschriftung der X-Achse ausblenden
          axis.text.x = element_blank(),   # Text der X-Achse ausblenden
          axis.ticks.x = element_blank(),
          plot.title = element_text(hjust = 0.5)+  # Skalierungsstriche der X-Achse ausblenden
          ylim(0, 100))  # Skalierung der y-Achse auf den Bereich von 0 bis 100 festlegen

  
  # Boxplot anzeigen
  print(p_IMPACT_classic_original)
  print(p_IMPACT_classic_scaled)
  print(p_OCCURRENCE_classic_original)
  print(p_OCCURRENCE_classic_scaled)
  
  # Spezifiziere den Pfad, den Dateinamen und die Größe des zu speichernden Plots
  ordner <- "myapp/pictures/40_boxplots" # Setze hier deinen gewünschten Ordnerpfad
  #dateiname <- "mein_boxplot.png" # Setze hier deinen gewünschten Dateinamen
  #volle_pfad <- file.path(pfad, dateiname) # Kombiniere Ordnerpfad und Dateinamen
  
  # Plot speichern
  ggsave(filename = paste0(actualscenario,"_IMPACT_classic_original.png"), path = ordner, plot = p_IMPACT_classic_original, width = 3, height = 5, dpi = 300)
  ggsave(filename = paste0(actualscenario,"_OCCURRENCE_classic_original.png"), path = ordner, plot = p_OCCURRENCE_classic_original, width = 3, height = 5, dpi = 300)
    ggsave(filename = paste0(actualscenario,"_IMPACT_classic_scaled.png"), path = ordner, plot = p_IMPACT_classic_scaled, width = 3, height = 5, dpi = 300)
  ggsave(filename = paste0(actualscenario,"_OCCURRENCE_classic_scaled.png"), path = ordner, plot = p_OCCURRENCE_classic_scaled, width = 3, height = 5, dpi = 300)
  
  
  
}