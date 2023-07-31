library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)
library(tidyr)


  # Gewichteter Median
  gewichteter_median <- function(x, w) {
    # Sortieren der Daten
    o <- order(x)
    sx <- x[o]
    sw <- w[o]
    
    # Berechnen der kumulativen Summe der Gewichte und finden des Indexes, der 50% überschreitet
    cumsum_w <- cumsum(sw)
    idx <- which.max(cumsum_w >= sum(sw)/2)
    
    # Rückgabe des gewichteten Medians
    return(sx[idx])
  }

# Funktion um die Werte in einer .tex Datei zu speichern
save_to_latex <- function(actualscenario,method,group, filename,median_x, sd_x,mean_x, q1_x, q3_x, iqr_x,min_x,max_x,range_x, median_y, sd_y,mean_y, q1_y, q3_y,iqr_y,min_y,max_y,range_y) {
  sink(filename)
  actualscenario <- gsub(" ", "", actualscenario)
  
  cat(paste("\\expandafter\\newcommand\\csname ",method,"_",group,"_",actualscenario,"_medianX","\\endcsname{",round(median_x, digits = 2), "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",method,"_",group,"_",actualscenario,"_sdX","\\endcsname{",round(sd_x, digits = 2), "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",method,"_",group,"_",actualscenario,"_meanX","\\endcsname{",round(mean_x, digits = 2), "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",method,"_",group,"_",actualscenario,"_qoneX","\\endcsname{",round(q1_x, digits = 2), "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",method,"_",group,"_",actualscenario,"_qthreeX","\\endcsname{",round(q3_x, digits = 2), "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",method,"_",group,"_",actualscenario,"_iqrX","\\endcsname{",round(iqr_x, digits = 2), "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",method,"_",group,"_",actualscenario,"_minX","\\endcsname{",round(min_x, digits = 2), "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",method,"_",group,"_",actualscenario,"_maxX","\\endcsname{",round(max_x, digits = 2), "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",method,"_",group,"_",actualscenario,"_rangeX","\\endcsname{",round(range_x, digits = 2), "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",method,"_",group,"_",actualscenario,"_medianY","\\endcsname{",round(median_y, digits = 2), "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",method,"_",group,"_",actualscenario,"_sdY","\\endcsname{",round(sd_y, digits = 2), "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",method,"_",group,"_",actualscenario,"_meanY","\\endcsname{",round(mean_y, digits = 2), "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",method,"_",group,"_",actualscenario,"_qoneY","\\endcsname{",round(q1_y, digits = 2), "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",method,"_",group,"_",actualscenario,"_qthreeY","\\endcsname{",round(q3_y, digits = 2), "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",method,"_",group,"_",actualscenario,"_iqrY","\\endcsname{",round(iqr_y, digits = 2), "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",method,"_",group,"_",actualscenario,"_minY","\\endcsname{",round(min_y, digits = 2), "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",method,"_",group,"_",actualscenario,"_maxY","\\endcsname{",round(max_y, digits = 2), "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",method,"_",group,"_",actualscenario,"_rangeY","\\endcsname{",round(range_y, digits = 2), "}\n", sep=""))
  
  sink()
}

# Definition der Funktion zur Berechnung der Kapazität eines Rechtecks
capacity <- function(rectangle) {
  # Berechnet die Breite und Höhe des Rechtecks.
  width <- abs(rectangle$x2 - rectangle$x1)
  height <- abs(rectangle$y2 - rectangle$y1)
  
  # Berechnet die Fläche.
  area <- width * height
  
  # Das Gewicht ist der Kehrwert der Fläche.
  weight <- 1 / area
  
  return(weight)
}

# Definition der Funktion für das Choquet-Integral
choquet_integral <- function(values, weights) {
  # Sortiert die Werte in abnehmender Reihenfolge.
  order <- order(values, decreasing = TRUE)
  values <- values[order]
  
  # Behält die zugehörigen Gewichte und berechnet die kumulativen Gewichte.
  weights <- weights[order]
  cumulative_weights <- cumsum(weights)
  
  # Fügt eine Null am Anfang der kumulativen Gewichte hinzu.
  cumulative_weights <- c(0, cumulative_weights)
  
  # Berechnet das Choquet-Integral.
  integral <- sum(diff(cumulative_weights) * values)
  
  return(integral)
}

answerstable <- read.csv(file = 'myapp/Data/RQ1_corrected_scaled.csv', header=TRUE) #importiere das answers file
dfall <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)
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
  
  rectangles <- list()
  
  for (i in 1:numberofanswers){
    AccId <- df[i,"ACC2SURV_ACCID"]
    #QuesId <- df[i,"QUES_ID"]
    #UncertaintyI <- df[i,"scaled_uncertainty_X"]
    #UncertaintyO <- df[i,"scaled_uncertainty_Y"]
    #Role <- df[i,"ACC2SURV_ROLE"]
    #GroupId <- df[i,"ACC2SURV_GROUPID"]
    x1 <- df[i,"scaled_X1"]
    x2 <- df[i,"scaled_X2"]
    y1 <- df[i,"scaled_Y1"]
    y2 <- df[i,"scaled_Y2"]
    
    
    rectangles[[i]] <- list(x1 = x1, y1 = y1, x2 = x2, y2 = y2)
    
  }
  
  
  # Berechnet die Gewichte für jedes Rechteck.
  weights <- sapply(rectangles, capacity)
  
  # Normalisiert die Gewichte, so dass sie alle zusammen eins ergeben.
  weights <- weights / sum(weights)
  
  # Berechnet das Choquet-Integral für alle x1, x2, y1 und y2 Werte.
  x1_integral <- choquet_integral(sapply(rectangles, function(rectangle) rectangle$x1), weights)
  x2_integral <- choquet_integral(sapply(rectangles, function(rectangle) rectangle$x2), weights)
  y1_integral <- choquet_integral(sapply(rectangles, function(rectangle) rectangle$y1), weights)
  y2_integral <- choquet_integral(sapply(rectangles, function(rectangle) rectangle$y2), weights)
  
  # Das "konsolidierte" Rechteck ist nun definiert durch die gewichteten x1, x2, y1 und y2 Werte.
  rechteck <- list(x1 = x1_integral, y1 = y1_integral, x2 = x2_integral, y2 = y2_integral)
  
  print(rechteck)
  
  # Berechnet das Choquet-Integral für alle x1 Werte.
  x1_werte <- sapply(rectangles, function(rectangle) rectangle$x1)
  x1_integral <- choquet_integral(x1_werte, weights)
  
  # Berechnet die Varianz und den Standardabweichung.
  x1_median <- gewichteter_median(x1_werte, weights)
  x1_varianz <- sum(weights * (x1_werte - x1_median)^2)
  x1_standardabweichung <- sqrt(x1_varianz)
  
  #print(x1_varianz)
  #print(x1_standardabweichung)
  
  
  ######IMPACT und OCCURRENCE ZUSAMMENGEFASST
  # Extrahiert alle X- und Y-Koordinaten
  x_werte <- c(sapply(rectangles, function(rectangle) c(rectangle$x1, rectangle$x2)))
  y_werte <- c(sapply(rectangles, function(rectangle) c(rectangle$y1, rectangle$y2)))
  
  x_werte <- c(sapply(rectangles, function(rectangle) c(rectangle$x1, rectangle$x2)))
  y_werte <- c(sapply(rectangles, function(rectangle) c(rectangle$y1, rectangle$y2)))
  
  # Berechnung der Gewichte für die x-Werte
  x_weights <- rep(weights, each = 2)
  y_weights <- rep(weights, each = 2)
  
  # Normalisierung der Gewichte, so dass sie zusammen eins ergeben
  x_weights <- x_weights / sum(x_weights)
  y_weights <- y_weights / sum(y_weights)
  
  # Berechnung des gewichteten Durchschnitts
  x_average <- sum(x_werte * x_weights)
  y_average <- sum(y_werte * y_weights)
  
  
  # Berechnung der gewichteten Varianz
  x_varianz <- sum(x_weights * (x_werte - x_average)^2)
  y_varianz <- sum(y_weights * (y_werte - y_average)^2)
  
  # Berechnung der Standardabweichung
  x_standardabweichung <- sqrt(x_varianz)
  y_standardabweichung <- sqrt(y_varianz)
  
  
  
  
  
  # Berechnung des gewichteten Mittelwerts
  x_mittelwert <- sum(x_werte * x_weights)
  y_mittelwert <- sum(y_werte * y_weights)
  
  
  
  
  
  x_median <- gewichteter_median(x_werte, x_weights)
  y_median <- gewichteter_median(y_werte, y_weights)
  
  
  
  # Erstellen Sie einen Dataframe aus den X-Werten und Gewichten
  df_x_histo <- data.frame(x = x_werte, weights = x_weights)
  df_y_histo <- data.frame(y = y_werte, weights = y_weights)
  
  #print (df_x_histo)
  #print (sum(df_x_histo$weights))
  
  #df_x_histo$x <- replace_na(df_x_histo$x, 0)
  #df_x_histo <- tidyr::replace_na(df_x_histo, list(x = 0))
  #df_y_histo$y <- replace_na(df_y_histo$y, 0)
  #df_y_histo <- tidyr::replace_na(df_y_histo, list(y = 0))
  
  # Erstellen Sie ein gewichtetes Histogramm
  plot_x <-ggplot(df_x_histo, aes(x = x)) + geom_histogram(aes(y =((..density..)*numberofanswers), weight = weights), bins = 50, fill = "lightblue", color = "black", na.rm = FALSE)+
    theme_minimal() +
    scale_x_continuous(limits = c(0, 100))+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())+
    labs(x = "", y = "Frequency", title = "")
  
  filehist_x <- (paste0("myapp/pictures/200_choquet_hist/",scentext,"_IMPACT.png"))
  ggsave(filehist_x, plot = plot_x, width = 8.3, height = 8.3)
  
  
  
    plot_y <-ggplot(df_y_histo, aes(x = y)) + geom_histogram(aes(y =((..density..)*numberofanswers), weight = weights), bins = 50, fill = "lightblue", color = "black", na.rm = FALSE)+
    theme_minimal() +
    scale_x_continuous(limits = c(0, 100))+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())+
    labs(x = "", y = "Frequency", title = "")
  
  filehist_y <- (paste0("myapp/pictures/200_choquet_hist/",scentext,"_OCCURRENCE.png"))
  ggsave(filehist_y, plot = plot_y, width = 8.3, height = 8.3)
  
  
  #plot (plot_x)
  #plot (plot_y)
  
  # Gewichtete Quantilfunktion
  gewichtete_quantile <- function(x, w, probs){
    # Sortieren der Daten
    o <- order(x)
    sx <- x[o]
    sw <- w[o]
    
    # Berechnung der kumulativen Summe der Gewichte
    cumsum_w <- cumsum(sw)
    
    # Suchen der Indizes, die die gewünschten Quantile erreichen
    idx <- sapply(probs, function(p) which.min(abs(cumsum_w - p * sum(sw))))
    
    # Rückgabe der gewichteten Quantile
    return(sx[idx])
  }
  
  # Berechnung der gewichteten Quantile
  q1_x <- gewichtete_quantile(x_werte, x_weights, 0.25)
  q3_x <- gewichtete_quantile(x_werte, x_weights, 0.75)
  min_x <- gewichtete_quantile(x_werte, x_weights, 0)
  max_x <- gewichtete_quantile(x_werte, x_weights, 1)
  q1_y <- gewichtete_quantile(y_werte, y_weights, 0.25)
  q3_y <- gewichtete_quantile(y_werte, y_weights, 0.75)
  min_y <- gewichtete_quantile(y_werte, y_weights, 0)
  max_y <- gewichtete_quantile(y_werte, y_weights, 1)
  # Berechnung des Interquartilsrange und des Bereichs
  iqr_x <- q3_x - q1_x
  range_x <- max_x - min_x
  iqr_y <- q3_y - q1_y
  range_y <- max_y - min_y
  
  
  
  print(paste0("Mittelwert_x: ",x_mittelwert))
  print(paste0("Mittelwert_y: ",y_mittelwert))
  print(paste0("Standardabweichung_x: ",x_standardabweichung))
  print(paste0("Standardabweichung_y: ",y_standardabweichung))
  print(paste0("Median_x: ", x_median))
  print(paste0("Median_y: ", y_median))
  print(paste0("Q1_x: ", q1_x))
  print(paste0("Q1_y: ", q1_y))
  print(paste0("Q3_x: ", q3_x))
  print(paste0("Q3_y: ", q3_y))
  print(paste0("Min_x: ", min_x))
  print(paste0("Min_y: ", min_y))
  print(paste0("Max_x: ", max_x))
  print(paste0("Max_y: ", max_y))
  print(paste0("IQR_x: ", iqr_x))
  print(paste0("IQR_y: ", iqr_y))
  print(paste0("Range_x: ", range_x))
  print(paste0("Range_y: ", range_y))
  
  
  method <- "choquet"
  group <- "all"
  scentext <- (paste0("", actualscenario))
  scenfile <- (paste0("myapp/tex/50_scattering/",scentext,"_all_",method,".tex")) 
  
  save_to_latex(actualscenario,
                method,
                group,
                scenfile,
                x_median,
                x_standardabweichung,
                x_mittelwert,
                q1_x,
                q3_x,
                iqr_x,
                min_x,
                max_x,
                range_x,
                y_median,
                y_standardabweichung,
                y_mittelwert,
                q1_y,
                q3_y,
                iqr_y,
                min_y,
                max_y,
                range_y
  )
  
  
  
  
  # Rechteck mit den gegebenen Koordinaten
  #rect(xleft = dfcalc[anz,4], ybottom = dfcalc[anz,8], xright = dfcalc[anz,5], ytop = dfcalc[anz,9], col = "blue")
  #x_label <- (paste0("x = ",round(dfcalc[anz,2], digits = 2), " ± ", round(dfcalc[anz,3], digits = 2)))
  #y_label <- (paste0("y = ",round(dfcalc[anz,6], digits = 2), " ± ", round(dfcalc[anz,7], digits = 2)))
  #  text(x = 10, y = 15, labels = x_label, col = "red", cex = 1, font = 1, pos = 4)
  #text(x = 10, y = 10, labels = y_label, col = "red", cex = 1, font = 1, pos = 4)
  
  
  boxplot_stats_x <- list(stats = matrix(c(min_x, q1_x, median_x, q3_x, max_x), nrow=5), n=1)
  boxplot_stats_y <- list(stats = matrix(c(min_y, q1_y, median_y, q3_y, max_y), nrow=5), n=1)
  
  directory <- "myapp/pictures/200_choquet_boxplots"
  filename <- paste("boxplot_IMPACT_",actualscenario,".png", sep = "")
  # Kompletten Dateipfad erstellen
  filepath_boxplot <- file.path(directory, filename)
  
  png(filepath_boxplot, width = 200, height = 500)
  bxp(boxplot_stats_x, main = "IMPACT", ylab = "value", ylim = c(0, 100))
  points(mean_x, col = "red", pch = 19)
  dev.off() 
  
  filename <- paste("boxplot_OCCURRENCE_",actualscenario,".png", sep = "")
  filepath_boxplot <- file.path(directory, filename)
  
  png(filepath_boxplot, width = 200, height = 500)
  bxp(boxplot_stats_y, main = "OCCURRENCE", ylab = "value", ylim = c(0, 100))
  points(mean_x, col = "red", pch = 19)
  dev.off() 
  
  
  
  
  
  filetitle <- (paste0("myapp/pictures/200_choquet/",scentext,".png"))
  
  png(file=filetitle,width=1000, height=1000, res=150)
  plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(0, 100), xlab = "", ylab = "", xaxs = "i", yaxs = "i")
  rect(0.2, 0.05, 100, 99.8, col = "lightyellow", border = NA)
  #axis(1)
  #axis(2)
  #box()
  
  #plot(1, 1, type = "n", xlim = c(0, 100), ylim = c(0, 100), xlab = "", ylab = "", asp = 1,panel.first = rect(0, 0, 100, 100, col = "lightyellow"), xaxs = "i", yaxs = "i")
  
  
  # Zeichnet ein Rechteck mit den gegebenen Koordinaten
  rect(xleft = rechteck$x1, ybottom = rechteck$y1, xright = rechteck$x2, ytop = rechteck$y2, col = "blue")
  #x_label <- (paste0("x = ",round(x_mittelwert, digits = 2), " ± ", round(x_standardabweichung, digits = 2)))
  #y_label <- (paste0("y = ",round(y_mittelwert, digits = 2), " ± ", round(y_standardabweichung, digits = 2)))
  
  #text(x = 10, y = 15, labels = x_label, col = "red", cex = 1, font = 1, pos = 4)
  #text(x = 10, y = 10, labels = y_label, col = "red", cex = 1, font = 1, pos = 4)
  dev.off()
  
  
  
  
  
}