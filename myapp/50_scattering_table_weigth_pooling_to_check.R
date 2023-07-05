library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)
library(tibble)

# Funktion zur Berechnung von statistischen Metriken
calculate_metrics <- function(x) {
  list(
    Median = median(x),
    StandardDeviation = sd(x),
    Mean = mean(x),
    FirstQuartile = quantile(x, probs = 0.25, names = FALSE),
    ThirdQuartile = quantile(x, probs = 0.75, names = FALSE),
    InterquartileRange = IQR(x),
    Minimum = min(x),
    Maximum = max(x),
    Range = max(x) - min(x)
  )
}

# Funktion zur Ausgabe der Reachgrid Variante
calculate_midpoints <- function(min_value, max_value) {
  step_size <- 20
  steps <- seq(0, 100, by = step_size)
  midpoints <- numeric()
  for (i in seq_along(steps)[-length(steps)]) {
    lower_bound <- steps[i]
    upper_bound <- steps[i + 1]
    if (max_value >= lower_bound && min_value <= upper_bound) {
      midpoint <- (lower_bound + upper_bound) / 2
      midpoints <- c(midpoints, midpoint)
    }
  }
   return(midpoints)
}

df <- data.frame(
  scenario = character(0),
  type = character(0),
  group = character(0),
  method = character(0),
  impact_Median = numeric(0),
  impact_StandardDeviation = numeric(0),
  impact_Mean = numeric(0),
  impact_FirstQuartile = numeric(0),
  impact_ThirdQuartile = numeric(0),
  impact_InterquartileRange = numeric(0),
  impact_Minimum = numeric(0),
  impact_Maximum = numeric(0),
  impact_Range = numeric(0),
  occurrence_Median = numeric(0),
  occurrence_StandardDeviation = numeric(0),
  occurrence_Mean = numeric(0),
  occurrence_FirstQuartile = numeric(0),
  occurrence_ThirdQuartile = numeric(0),
  occurrence_InterquartileRange = numeric(0),
  occurrence_Minimum = numeric(0),
  occurrence_Maximum = numeric(0),
  occurrence_Range = numeric(0)
)


answers <- read.csv(file = 'myapp/data/RQ1_corrected_scaled.csv', header=TRUE) #importiere das answers file
all.answers <- answers %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)
number.scenarios <- nrow(as.data.frame(table(all.answers$QUES_ID)))
scenarios <- as.data.frame(table(all.answers$QUES_ID))

for (anz in 1:1){#number.scenarios) {
  actualscenario =as.vector(scenarios[anz,1])
  print (actualscenario)
  actual.df <- answers %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")
  actualtype <- actual.df[1,"QUES_TYP"]
  print (actualtype)
  
  
  numberofanswers <- nrow(actual.df)
  #print (numberofanswers)
  
  #Übernahme der Daten für die Pooling Methode
  Daten <- actual.df
  Daten[,"x_mü"] <- (Daten[,"scaled_X1"]+Daten[,"scaled_X2"])/2
  Daten[,"x_sigma"] <- (Daten[,"scaled_X2"]-Daten[,"scaled_X1"])/6   # korrigierte Formel
  Daten[,"y_mü"] <- (Daten[,"scaled_Y1"]+Daten[,"scaled_Y2"])/2
  Daten[,"y_sigma"] <- (Daten[,"scaled_Y2"]-Daten[,"scaled_Y1"])/6   # korrigierte Formel
  # Anzahl an Probanden: 
  N <- length(Daten[,1])
  
  # Anzahl an Pooling-Iterationen:
  p.max <- 10000
  
  # Wert für delta-Abbruchkriterium und epsilon
  delta <- 0.0001
  epsilon <- 1 # (vgl. Seite 96, Abschnitt B)
  
  # Vorinitialisierung
  M <- Daten[,"x_mü"]
  S <- Daten[,"x_sigma"]
  p.Abbruch <- NA
  Weights <- diag(rep(1,N))
  # Start der for-Schleife
  S.0 <- S
  for(p in 1:p.max){
    if( max(abs(M - M[1]), na.rm = TRUE) > delta ){
      #if( max(abs(M-M[1])) > delta ){  # falls das Abbruchkriterium noch nicht erfüllt ist
      
      c.L <- list()
      S.alt <- S
      for(j in 1:N){
        
        c.j <- 1/(epsilon+abs(M-M[j]))
        c.j <- c.j/sum(c.j)    # Normalisierung, dass die Summe der c.j-Gewichte gleich 1 ist (Korrektur eines weiteren Fehlers im Artikel)
        
        S[j] <- sqrt(1/(N*sum(c.j/S.alt^2)))      # Update von Sigma für den Probanden j
        M[j] <- S[j]^2 * N * sum(M*c.j/S.alt^2)   # Update von mü für den Probanden j
        
        c.L[[j]] <- c.j    # wird unten gebraucht, daher Abspeichern als Liste
        
      }
      
      Weights.Matrix <- matrix(NA,ncol=N,nrow=N)
      for(j in 1:N){
        c.j <- c.L[[j]]   # aus der Liste der c.j-Koeffizienten
        for(k in 1:N){
          Weights.Matrix[j,k] <- S[j]^2 * N * c.j[k]/S.alt[k]^2
        }
      }
      Weights <- Weights.Matrix%*%Weights  
      S <- sqrt(S^2/sum(S^2))
      
      p.Abbruch <- p
      
    }
  }
  p_x <- p.Abbruch
  M_x <- M
  s1_x <-  sqrt(sum(S.0^2*Weights[1,]^2))
  sigma3_x <-  3*(sqrt(sum(S.0^2*Weights[1,]^2)))
  Weights_x <- Weights
  
  rm(Weights)
  
  p.Abbruch <- NA
  Weights <- diag(rep(1,N))
  M <- Daten[,"y_mü"]
  S <- Daten[,"y_sigma"]
  S.0 <- S
  for(p in 1:p.max){
    if( max(abs(M - M[1]), na.rm = TRUE) > delta ){
      #if( max(abs(M-M[1])) > delta ){  # falls das Abbruchkriterium noch nicht erfüllt ist
      
      c.L <- list()
      S.alt <- S
      for(j in 1:N){
        
        #if(is.na(S[j]) || is.na(S.alt[k]) || is.na(c.j[k]) || S[j] == 0 || S.alt[k] == 0 || c.j[k] == 0){
        #  next  # überspringt den aktuellen Durchlauf der Schleife
        #}
        
        
        c.j <- 1/(epsilon+abs(M-M[j]))
        c.j <- c.j/sum(c.j)    # Normalisierung, dass die Summe der c.j-Gewichte gleich 1 ist (Korrektur eines weiteren Fehlers im Artikel)
        
        S[j] <- sqrt(1/(N*sum(c.j/S.alt^2)))      # Update von Sigma für den Probanden j
        M[j] <- S[j]^2 * N * sum(M*c.j/S.alt^2)   # Update von mü für den Probanden j
        
        c.L[[j]] <- c.j    # wird unten gebraucht, daher Abspeichern als Liste
        
      }
      
      Weights.Matrix <- matrix(NA,ncol=N,nrow=N)
      for(j in 1:N){
        c.j <- c.L[[j]]   # aus der Liste der c.j-Koeffizienten
        for(k in 1:N){
          #if(j > length(c.L)){
          #  next  # überspringt den aktuellen Durchlauf der Schleife
          #}
          Weights.Matrix[j,k] <- S[j]^2 * N * c.j[k]/S.alt[k]^2
        }
      }
      Weights <- Weights.Matrix%*%Weights  
      S <- sqrt(S^2/sum(S^2))
      
      p.Abbruch <- p
      
    }
  }
  p_y <- p.Abbruch
  M_y <- M
  s1_y <-  sqrt(sum(S.0^2*Weights[1,]^2))
  sigma3_y <-  3*(sqrt(sum(S.0^2*Weights[1,]^2)))
  Weights_y <- Weights
  rm(Weights)
  
  pooling.x <- M_x[1]
  pooling.x_3xsigma <- sigma3_x
  pooling.x_min <- (M_x[1]-sigma3_x)
  pooling.x_max <- (M_x[1]+sigma3_x)
  pooling.y <- M_y[1]
  pooling.y_3xsigma <- sigma3_y
  pooling.y_min <- (M_y[1]-sigma3_y)
  pooling.y_max <- (M_y[1]+sigma3_y)
 
  pooling.mean_x <- pooling.x
  pooling.sd_x <-   pooling.x_3xsigma / 3
  pooling.median_x <- pooling.mean_x
  pooling.q1_x <- qnorm(0.25,pooling.mean_x,pooling.sd_x)
  pooling.q3_x <- qnorm(0.75,pooling.mean_x,pooling.sd_x)
  pooling.iqr_x <- pooling.q3_x - pooling.q1_x
  pooling.minimum_x <- pooling.mean_x - 3 * pooling.sd_x
  pooling.maximum_x <- pooling.mean_x + 3 * pooling.sd_x
  
  pooling.mean_y <- pooling.y
  pooling.sd_y <- pooling.y_3xsigma / 3
  pooling.median_y <- pooling.mean_y
  pooling.q1_y <- qnorm(0.25,pooling.mean_y,pooling.sd_y)
  pooling.q3_y <- qnorm(0.75,pooling.mean_y,pooling.sd_y)
  pooling.iqr_y <- pooling.q3_y - pooling.q1_y
  pooling.minimum_y <- pooling.mean_y - 3 * pooling.sd_y
  pooling.maximum_y <- pooling.mean_y + 3 * pooling.sd_y
  
  
  
  #build the pooling df
  newdfpooling <- data.frame(
    scenario = actualscenario,
    group = "all",
    type = actualtype,
    method = "pooling ",
    impact_Median = pooling.median_x,
    impact_StandardDeviation = pooling.sd_x,
    impact_Mean = pooling.mean_x,
    impact_FirstQuartile = pooling.q1_x,
    impact_ThirdQuartile = pooling.q3_x,
    impact_InterquartileRange = pooling.iqr_x,
    impact_Minimum = pooling.minimum_x,
    impact_Maximum = pooling.maximum_x,
    impact_Range = pooling.maximum_x - pooling.minimum_x,
    occurrence_Median = pooling.median_y,
    occurrence_StandardDeviation = pooling.sd_y,
    occurrence_Mean = pooling.mean_y,
    occurrence_FirstQuartile = pooling.q1_y,
    occurrence_ThirdQuartile = pooling.q3_y,
    occurrence_InterquartileRange = pooling.iqr_y,
    occurrence_Minimum = pooling.minimum_y,
    occurrence_Maximum = pooling.maximum_y,
    occurrence_Range = pooling.maximum_y - pooling.minimum_y
  )
  
  
  #Erzeugt datenframes die später benötigt werden
  x.daten <- data.frame(ACCID = integer(), Wert = numeric())
  y.daten <- data.frame(ACCID = integer(), Wert = numeric())
  
    #Leere datenframes reached grid
    x_df <- data.frame()
    y_df <- data.frame()
  

    
    
   #Klassischer Wert
   classicX_metrics <- calculate_metrics(actual.df$scaled_IMPACT)
   classicY_metrics <- calculate_metrics(actual.df$scaled_OCCURRENCE)
 
    # Datenframe erstellen
      newdfclassic <- data.frame(
      scenario = actualscenario,
      type = actualtype,
      group = "all",
      method = "classic",
      impact_Median = classicX_metrics$Median,
      impact_StandardDeviation = classicX_metrics$StandardDeviation,
      impact_Mean = classicX_metrics$Mean,
      impact_FirstQuartile = classicX_metrics$FirstQuartile,
      impact_ThirdQuartile = classicX_metrics$ThirdQuartile,
      impact_InterquartileRange = classicX_metrics$InterquartileRange,
      impact_Minimum = classicX_metrics$Minimum,
      impact_Maximum = classicX_metrics$Maximum,
      impact_Range = classicX_metrics$Range,
      occurrence_Median = classicY_metrics$Median,
      occurrence_StandardDeviation = classicY_metrics$StandardDeviation,
      occurrence_Mean = classicY_metrics$Mean,
      occurrence_FirstQuartile = classicY_metrics$FirstQuartile,
      occurrence_ThirdQuartile = classicY_metrics$ThirdQuartile,
      occurrence_InterquartileRange = classicY_metrics$InterquartileRange,
      occurrence_Minimum = classicY_metrics$Minimum,
      occurrence_Maximum = classicY_metrics$Maximum,
      occurrence_Range = classicY_metrics$Range
    )
    
    df <- rbind(df, newdfclassic)
    
    #graphische Center Werte auf grid basis
    
    
      gctcX_scaled_result <- numeric()
      gctcY_scaled_result <- numeric()
      gctcX_scaled <- actual.df[,"middleIGRID"]
      gctcY_scaled <- actual.df[,"middleOGRID"]
      
      # Iteration über die Werte in gctcX_scaled
      for (value in gctcX_scaled) {
        # Überprüfen des Wertes und Speichern des entsprechenden Wertes
        gctcX_scaled_result <- c(gctcX_scaled_result, switch(value,
                                   "1" = 10,
                                   "2" = 30,
                                   "3" = 50,
                                   "4" = 70,
                                   "5" = 90))
      }
      
      gctcX_scaled_values <- data.frame(gctcX_scaled_result = gctcX_scaled_result)
      
      for (value in gctcY_scaled) {
        # Überprüfen des Wertes und Speichern des entsprechenden Wertes
        gctcY_scaled_result <- c(gctcY_scaled_result, switch(value,
                                                             "1" = 10,
                                                             "2" = 30,
                                                             "3" = 50,
                                                             "4" = 70,
                                                             "5" = 90))
      }
      
      gctcY_scaled_values <- data.frame(gctcY_scaled_result = gctcY_scaled_result)
     
    graphiccentertoclassicX_metrics <- calculate_metrics(gctcX_scaled_values$gctcX_scaled_result)
    
    graphiccentertoclassicY_metrics <- calculate_metrics(gctcY_scaled_values$gctcY_scaled_result)
    
    newdfg5 <- data.frame(
      scenario = actualscenario,
      type = actualtype,
      group = "all",
      method = "graphic center to grid",
      impact_Median = graphiccentertoclassicX_metrics$Median,
      impact_StandardDeviation = graphiccentertoclassicX_metrics$StandardDeviation,
      impact_Mean = graphiccentertoclassicX_metrics$Mean,
      impact_FirstQuartile = graphiccentertoclassicX_metrics$FirstQuartile,
      impact_ThirdQuartile = graphiccentertoclassicX_metrics$ThirdQuartile,
      impact_InterquartileRange = graphiccentertoclassicX_metrics$InterquartileRange,
      impact_Minimum = graphiccentertoclassicX_metrics$Minimum,
      impact_Maximum = graphiccentertoclassicX_metrics$Maximum,
      impact_Range = graphiccentertoclassicX_metrics$Range,
      occurrence_Median = graphiccentertoclassicY_metrics$Median,
      occurrence_StandardDeviation = graphiccentertoclassicY_metrics$StandardDeviation,
      occurrence_Mean = graphiccentertoclassicY_metrics$Mean,
      occurrence_FirstQuartile = graphiccentertoclassicY_metrics$FirstQuartile,
      occurrence_ThirdQuartile = graphiccentertoclassicY_metrics$ThirdQuartile,
      occurrence_InterquartileRange = graphiccentertoclassicY_metrics$InterquartileRange,
      occurrence_Minimum = graphiccentertoclassicY_metrics$Minimum,
      occurrence_Maximum = graphiccentertoclassicY_metrics$Maximum,
      occurrence_Range = graphiccentertoclassicY_metrics$Range
    )
    
    df <- rbind(df, newdfg5)
    
    
    
    
    
    graphic_center_X_metrics <- calculate_metrics(actual.df$scaled_uncertainty_middle_X)
    graphic_center_Y_metrics <- calculate_metrics(actual.df$scaled_uncertainty_middle_Y)
    
    newdfg2 <- data.frame(
      scenario = actualscenario,
      type = actualtype,
      group = "all",
      method = "graphic center",
      impact_Median = graphic_center_X_metrics$Median,
      impact_StandardDeviation = graphic_center_X_metrics$StandardDeviation,
      impact_Mean = graphic_center_X_metrics$Mean,
      impact_FirstQuartile = graphic_center_X_metrics$FirstQuartile,
      impact_ThirdQuartile = graphic_center_X_metrics$ThirdQuartile,
      impact_InterquartileRange = graphic_center_X_metrics$InterquartileRange,
      impact_Minimum = graphic_center_X_metrics$Minimum,
      impact_Maximum = graphic_center_X_metrics$Maximum,
      impact_Range = graphic_center_X_metrics$Range,
      occurrence_Median = graphic_center_Y_metrics$Median,
      occurrence_StandardDeviation = graphic_center_Y_metrics$StandardDeviation,
      occurrence_Mean = graphic_center_Y_metrics$Mean,
      occurrence_FirstQuartile = graphic_center_Y_metrics$FirstQuartile,
      occurrence_ThirdQuartile = graphic_center_Y_metrics$ThirdQuartile,
      occurrence_InterquartileRange = graphic_center_Y_metrics$InterquartileRange,
      occurrence_Minimum = graphic_center_Y_metrics$Minimum,
      occurrence_Maximum = graphic_center_Y_metrics$Maximum,
      occurrence_Range = graphic_center_Y_metrics$Range
    )
    
    df <- rbind(df, newdfg2)
    
    #Graphischer Wert über die erreichten Grids
    
    result_g3_x <- data.frame(ACCID = integer(), Stage = integer(), Midpoint = numeric())
    result_g3_y <- data.frame(ACCID = integer(), Stage = integer(), Midpoint = numeric())
    
    
    for (anzsur in 1:numberofanswers){
      g3.ACCID <- actual.df[anzsur,"ACC2SURV_ACCID"]
      g3_x_min <- actual.df[anzsur,"scaled_X1"]
      g3_x_max <- actual.df[anzsur,"scaled_X2"]
      g3_y_min <- actual.df[anzsur,"scaled_Y1"]
      g3_y_max <- actual.df[anzsur,"scaled_Y2"]
      
      x_midpoints <- calculate_midpoints(g3_x_min,g3_x_max)
      y_midpoints <- calculate_midpoints(g3_y_min, g3_y_max)
      
      for (j in seq_along(x_midpoints)) {
        result_g3_x <- rbind(result_g3_x, data.frame(ACCID = g3.ACCID, Grid = j, Midpoint = x_midpoints[j]))
      }
      for (j in seq_along(y_midpoints)) {
        result_g3_y <- rbind(result_g3_y, data.frame(ACCID = g3.ACCID, Grid = j, Midpoint = y_midpoints[j]))
      }
    }
    
    graphic_gridreach_X_metrics <- calculate_metrics(result_g3_x$Midpoint)
    graphic_gridreach_Y_metrics <- calculate_metrics(result_g3_y$Midpoint)
    
    newdfg3 <- data.frame(
      scenario = actualscenario,
      type = actualtype,
      group = "all",
      method = "graphic grid reached",
      impact_Median = graphic_gridreach_X_metrics$Median,
      impact_StandardDeviation = graphic_gridreach_X_metrics$StandardDeviation,
      impact_Mean = graphic_gridreach_X_metrics$Mean,
      impact_FirstQuartile = graphic_gridreach_X_metrics$FirstQuartile,
      impact_ThirdQuartile = graphic_gridreach_X_metrics$ThirdQuartile,
      impact_InterquartileRange = graphic_gridreach_X_metrics$InterquartileRange,
      impact_Minimum = graphic_gridreach_X_metrics$Minimum,
      impact_Maximum = graphic_gridreach_X_metrics$Maximum,
      impact_Range = graphic_gridreach_X_metrics$Range,
      occurrence_Median = graphic_gridreach_Y_metrics$Median,
      occurrence_StandardDeviation = graphic_gridreach_Y_metrics$StandardDeviation,
      occurrence_Mean = graphic_gridreach_Y_metrics$Mean,
      occurrence_FirstQuartile = graphic_gridreach_Y_metrics$FirstQuartile,
      occurrence_ThirdQuartile = graphic_gridreach_Y_metrics$ThirdQuartile,
      occurrence_InterquartileRange = graphic_gridreach_Y_metrics$InterquartileRange,
      occurrence_Minimum = graphic_gridreach_Y_metrics$Minimum,
      occurrence_Maximum = graphic_gridreach_Y_metrics$Maximum,
      occurrence_Range = graphic_gridreach_Y_metrics$Range
    )
    
    df <- rbind(df, newdfg3)
    

    
    
    
#Graphischer Wert über Flächen & REACHED GRID TEIL & WEIGHT VARIANTE
  
    #Dataframe für weight variante
    weight.user_data <- data.frame(
      user_id = actual.df[,"ACC2SURV_ACCID"],
      x.min_range = actual.df[,"scaled_X1"],
      x.max_range = actual.df[,"scaled_X2"],
      x.distance = actual.df[,"scaled_uncertainty_X"],
      y.min_range = actual.df[,"scaled_Y1"],
      y.max_range = actual.df[,"scaled_Y2"],
      y.distance = actual.df[,"scaled_uncertainty_Y"]
    )
    #Berechnungen für weight variante
    weight.x.df_all <- data.frame(weight.x.scale = numeric(), weight.x.value = numeric())
    weight.y.df_all <- data.frame(weight.y.scale = numeric(), weight.y.value = numeric())
    weight.user_data <- weight.user_data %>%  mutate(weight.x.normalized_range = 1 - (x.max_range - x.min_range) / 100)
    weight.user_data <- weight.user_data %>%  mutate(weight.y.normalized_range = 1 - (y.max_range - y.min_range) / 100)
    weight.x.total_normalized_range <- sum(weight.user_data$weight.x.normalized_range)
    weight.y.total_normalized_range <- sum(weight.user_data$weight.y.normalized_range)
    weight.user_data <- weight.user_data %>%  mutate(x.weight = weight.x.normalized_range / weight.x.total_normalized_range)
    weight.user_data <- weight.user_data %>%  mutate(y.weight = weight.y.normalized_range / weight.y.total_normalized_range)
    
    
    
    #schleife für die drei Varianten
      
  for (i in 1:numberofanswers){
    ACCID <- actual.df[i,"ACC2SURV_ACCID"]
    weight.x.normalized_range_user <- (1 - (actual.df[i,"scaled_X2"] - actual.df[i,"scaled_X1"]) / 100)
    weight.y.normalized_range_user <- (1 - (actual.df[i,"scaled_Y2"] - actual.df[i,"scaled_Y1"]) / 100)
    x.min <- actual.df[i,"scaled_X1"]
    x.max <- actual.df[i,"scaled_X2"]
    y.min <- actual.df[i,"scaled_Y1"]
    y.max <- actual.df[i,"scaled_Y2"]
    
    weight.actualvaluex <- x.min
    weight.actualvaluey <- y.min
    weight.x.values <- seq(from = 0, to = 100, by = 0.25)
    weight.y.values <- seq(from = 0, to = 100, by = 0.25)
    weight.x.df_prod <- data.frame(weight.x.scale = weight.x.values, weight.x.value = 0)
    weight.y.df_prod <- data.frame(weight.y.scale = weight.y.values, weight.y.value = 0)
    weight.x.df_prod <- weight.x.df_prod[order(weight.x.df_prod$weight.x.scale), , drop = FALSE]
    weight.y.df_prod <- weight.y.df_prod[order(weight.y.df_prod$weight.y.scale), , drop = FALSE]
    
    while (weight.actualvaluex <= x.max) {
      index <- which(weight.x.df_prod$weight.x.scale == weight.actualvaluex)
      weight.x.df_prod$weight.x.value[index] <- weight.user_data$x.weight[i]
      weight.actualvaluex <- weight.actualvaluex + 0.25
    }
      weight.x.df_all <- rbind(weight.x.df_all, weight.x.df_prod)
    
    while (weight.actualvaluey <= y.max) {
      index <- which(weight.y.df_prod$weight.y.scale == weight.actualvaluey)
      weight.y.df_prod$weight.y.value[index] <- weight.user_data$y.weight[i]
      weight.actualvaluey <- weight.actualvaluey + 0.25
    }
    
      weight.y.df_all <- rbind(weight.y.df_all, weight.y.df_prod)
    
    
    #Datenframes für die anderen Teie (nicht weight)
    x_user_werte <- seq(x.min, x.max, by = 0.25)
    y_user_werte <- seq(y.min, y.max, by = 0.25)
  
    x_daten_user <- data.frame(ACCID = rep(ACCID, length(x_user_werte)), Wert = x_user_werte)
    x.daten <- rbind(x.daten, x_daten_user)
    y_daten_user <- data.frame(ACCID = rep(ACCID, length(y_user_werte)), Wert = y_user_werte)
    y.daten <- rbind(y.daten, y_daten_user)
    
    #Reached grid NICHT EINGEBLENDET
 
    x_entry_values <- numeric()
    y_entry_values <- numeric()
    
    if (x.min <= 20 && x.max >= 0) {
      x_entry_values <- c(x_entry_values, 10)
    }
    if (x.min <= 40 && x.max >= 20) {
      x_entry_values <- c(x_entry_values, 30)
    }
    if (x.min <= 60 && x.max >= 40) {
      x_entry_values <- c(x_entry_values, 50)
    }
    if (x.min <= 80 && x.max >= 60) {
      x_entry_values <- c(x_entry_values, 70)
    }
    if (x.min <= 100 && x.max >= 80) {
      x_entry_values <- c(x_entry_values, 90)
    }
    
    if (y.min <= 20 && y.max >= 0) {
      y_entry_values <- c(y_entry_values, 10)
    }
    if (y.min <= 40 && y.max >= 20.25) {
      y_entry_values <- c(y_entry_values, 30)
    }
    if (y.min <= 60 && y.max >= 40.25) {
      y_entry_values <- c(y_entry_values, 50)
    }
    if (y.min <= 80 && y.max >= 60.25) {
      y_entry_values <- c(y_entry_values, 70)
    }
    if (y.min <= 100 && y.max >= 80.25) {
      y_entry_values <- c(y_entry_values, 90)
    }
    
    x_df <- bind_rows(x_df, data.frame(Grid = x_entry_values, User = ACCID))
    y_df <- bind_rows(y_df, data.frame(Grid = y_entry_values, User = ACCID))
    
  }
    
    #Weight Teil
    
    
    weight.grouped_x.daten <- weight.x.df_all %>% group_by(weight.x.scale)
    weight.x.summary_df <- weight.grouped_x.daten %>% summarise(weight.x.Sum = sum(weight.x.value))
    weight.x.summary_df <- weight.x.summary_df %>% mutate(weight.x.Wahrs = weight.x.Sum / sum(weight.x.Sum))
    weight.x.summary_df <- weight.x.summary_df %>% mutate(weight.x.Histo = weight.x.Wahrs * numberofanswers)
    
    weight.grouped_y.daten <- weight.y.df_all %>% group_by(weight.y.scale)
    weight.y.summary_df <- weight.grouped_y.daten %>% summarise(weight.y.Sum = sum(weight.y.value))
    weight.y.summary_df <- weight.y.summary_df %>% mutate(weight.y.Wahrs = weight.y.Sum / sum(weight.y.Sum))
    weight.y.summary_df <- weight.y.summary_df %>% mutate(weight.y.Histo = weight.y.Wahrs * numberofanswers)
    
    weight.x.simulations <- sample(weight.x.summary_df$weight.x.scale, size = 100000000, replace = TRUE, prob = weight.x.summary_df$weight.x.Wahrs)
    weight.y.simulations <- sample(weight.y.summary_df$weight.y.scale, size = 100000000, replace = TRUE, prob = weight.y.summary_df$weight.y.Wahrs)#500000000
    
    # Mittelwert berechnen
    weight.x.mean <- mean(weight.x.simulations)
    weight.y.mean <- mean(weight.y.simulations)
    # Median berechnen
    weight.x.median <- median(weight.x.simulations)
    weight.y.median <- median(weight.y.simulations)
    # Erstes Quartil (Q1) berechnen
    weight.x.q1 <- quantile(weight.x.simulations, 0.25)
    weight.y.q1 <- quantile(weight.y.simulations, 0.25)
    # Drittes Quartil (Q3) berechnen
    weight.x.q3 <- quantile(weight.x.simulations, 0.75)
    weight.y.q3 <- quantile(weight.y.simulations, 0.75)
    # Interquartilbereich (IQR) berechnen
    weight.x.iqr <- IQR(weight.x.simulations)
    weight.y.iqr <- IQR(weight.y.simulations)
    # Standardabweichung berechnen
    weight.x.standard_deviation <- sd(weight.x.simulations)
    weight.y.standard_deviation <- sd(weight.y.simulations)
    # Min
    weight.x.min <- min(weight.x.simulations)
    weight.y.min <- min(weight.y.simulations)
    #weight.x.min <- min(weight.user_data$x.min_range)
    #weight.y.min <- min(weight.user_data$y.min_range)
    # Max
    weight.x.max <- max(weight.x.simulations)
    weight.y.max <- max(weight.y.simulations)
    #weight.x.max <- max(weight.user_data$x.max_range)
    #weight.y.max <- max(weight.user_data$y.max_range)
    # Range
    weight.x.range <- weight.x.max - weight.x.min
    weight.y.range <- weight.y.max - weight.y.min
    
    newdfweight <- data.frame(
      scenario = actualscenario,
      type = actualtype,
      group = "all",
      method = "weighted",
      impact_Median = weight.x.median,
      impact_StandardDeviation = weight.x.standard_deviation,
      impact_Mean = weight.x.mean,
      impact_FirstQuartile = weight.x.q1,
      impact_ThirdQuartile = weight.x.q3,
      impact_InterquartileRange = weight.x.iqr,
      impact_Minimum = weight.x.min,
      impact_Maximum = weight.x.max,
      impact_Range = weight.x.range,
      occurrence_Median = weight.y.median,
      occurrence_StandardDeviation = weight.y.standard_deviation,
      occurrence_Mean = weight.y.mean,
      occurrence_FirstQuartile = weight.y.q1,
      occurrence_ThirdQuartile = weight.y.q3,
      occurrence_InterquartileRange = weight.y.iqr,
      occurrence_Minimum = weight.y.min,
      occurrence_Maximum = weight.y.max,
      occurrence_Range =  weight.y.range
    )
    rownames(newdfweight) <- NULL
    
    
    #Reachgrid Teil
  
    ImpactReachedGrid <- x_df[,'Grid']
    OccurrenceReachedGrid <- y_df[,'Grid']
    
    RGRIDX_metrics <- calculate_metrics(ImpactReachedGrid)
    RGRIDY_metrics <- calculate_metrics(OccurrenceReachedGrid)
    
    newdfrgrid <- data.frame(
      scenario = actualscenario,
      type = actualtype,
      group = "all",
      method = "reachedgrid",
      impact_Median = RGRIDX_metrics$Median,
      impact_StandardDeviation = RGRIDX_metrics$StandardDeviation,
      impact_Mean = RGRIDX_metrics$Mean,
      impact_FirstQuartile = RGRIDX_metrics$FirstQuartile,
      impact_ThirdQuartile = RGRIDX_metrics$ThirdQuartile,
      impact_InterquartileRange = RGRIDX_metrics$InterquartileRange,
      impact_Minimum = RGRIDX_metrics$Minimum,
      impact_Maximum = RGRIDX_metrics$Maximum,
      impact_Range = RGRIDX_metrics$Range,
      occurrence_Median = RGRIDY_metrics$Median,
      occurrence_StandardDeviation = RGRIDY_metrics$StandardDeviation,
      occurrence_Mean = RGRIDY_metrics$Mean,
      occurrence_FirstQuartile = RGRIDY_metrics$FirstQuartile,
      occurrence_ThirdQuartile = RGRIDY_metrics$ThirdQuartile,
      occurrence_InterquartileRange = RGRIDY_metrics$InterquartileRange,
      occurrence_Minimum = RGRIDY_metrics$Minimum,
      occurrence_Maximum = RGRIDY_metrics$Maximum,
      occurrence_Range = RGRIDY_metrics$Range
    )
    
  # Berechnung der statistischen Werte der Graphischen Area-Methode
    
    
  g1_i_median_value <- median(x.daten$Wert)
  g1_i_sd_value <- sd(x.daten$Wert)
  g1_i_mean_value <- mean(x.daten$Wert)
  g1_i_first_quartile <- quantile(x.daten$Wert, probs = 0.25, names = FALSE)
  g1_i_third_quartile <- quantile(x.daten$Wert, probs = 0.75, names = FALSE)
  g1_i_interquartile_range <- IQR(x.daten$Wert)
  g1_i_min_value <- min(x.daten$Wert)
  g1_i_max_value <- max(x.daten$Wert)
  g1_i_range_value <- max(x.daten$Wert) - min(x.daten$Wert)
  
  g1_o_median_value <- median(y.daten$Wert)
  g1_o_sd_value <- sd(y.daten$Wert)
  g1_o_mean_value <- mean(y.daten$Wert)
  g1_o_first_quartile <- quantile(y.daten$Wert, probs = 0.25, names = FALSE)
  g1_o_third_quartile <- quantile(y.daten$Wert, probs = 0.75, names = FALSE)
  g1_o_interquartile_range <- IQR(y.daten$Wert)
  g1_o_min_value <- min(y.daten$Wert)
  g1_o_max_value <- max(y.daten$Wert)
  g1_o_range_value <- max(y.daten$Wert) - min(y.daten$Wert)
  
  # Datenframe erstellen
  newdfg1 <- data.frame(
    scenario = actualscenario,
    type = actualtype,
    group = "all",
    method = "graphic areas",
    impact_Median = g1_i_median_value,
    impact_StandardDeviation = g1_i_sd_value,
    impact_Mean = g1_i_mean_value,
    impact_FirstQuartile = g1_i_first_quartile,
    impact_ThirdQuartile = g1_i_third_quartile,
    impact_InterquartileRange = g1_i_interquartile_range,
    impact_Minimum = g1_i_min_value,
    impact_Maximum = g1_i_max_value,
    impact_Range = g1_i_range_value,
    occurrence_Median = g1_o_median_value,
    occurrence_StandardDeviation = g1_o_sd_value,
    occurrence_Mean = g1_o_mean_value,
    occurrence_FirstQuartile = g1_o_first_quartile,
    occurrence_ThirdQuartile = g1_o_third_quartile,
    occurrence_InterquartileRange = g1_o_interquartile_range,
    occurrence_Minimum = g1_o_min_value,
    occurrence_Maximum = g1_o_max_value,
    occurrence_Range = g1_o_range_value
  )
  

  
  # Zeilen zusammenführen
  df <- rbind(df, newdfg1)
  df <- rbind(df, newdfweight)
  #df <- rbind(df, newdfrgrid)
  df <- rbind(df, newdfpooling)
  
  # Datenframe anzeigen
  print("TABELLE")
  print(df)
  

}


scentext <- paste0("Streumaße")
scenfile <- paste0("myapp/files/50_scattering/Streumaße.xlsx")

write.csv(df, file = paste0("myapp/files/50_scattering/",scentext,".csv"), row.names = TRUE)

# Erstelle einen neuen Workbook und füge den transponierten Dataframe ein
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", df)

# Speichern des Workbooks als XLSX-Datei
saveWorkbook(wb, file = scenfile, overwrite = TRUE)
