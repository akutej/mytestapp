library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)
library(tibble)


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

distance.df <- data.frame(
  scenario = character(0),
  type = character(0),
  method1 = character(0),
  method2 = character(0),
  distance = numeric(0))

distance.df_all <- data.frame(
  scenario = character(0),
  type = character(0),
  method1 = character(0),
  method2 = character(0),
  distance = numeric(0))



answers <- read.csv(file = 'myapp/data/RQ1_corrected_scaled.csv', header=TRUE) #importiere das answers file
all.answers <- answers %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID != "401" & QUES_ID != "402" & QUES_ID != "403")
all.answers <- all.answers %>% filter(QUES_ID == "281" )
number.scenarios <- nrow(as.data.frame(table(all.answers$QUES_ID)))
scenarios <- as.data.frame(table(all.answers$QUES_ID))

for (anz in 1:number.scenarios) {
  actualscenario =as.vector(scenarios[anz,1])
  print (actualscenario)
  actual.df <- answers %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")
  actualtype <- actual.df[1,"QUES_TYP"]
  print (actualtype)
  
  
  numberofanswers <- nrow(actual.df)
  print (numberofanswers)
  
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
  
  

  
  #Erzeugt datenframes die später benötigt werden
  x.daten <- data.frame(ACCID = integer(), Wert = numeric())
  y.daten <- data.frame(ACCID = integer(), Wert = numeric())
  
    #Leere datenframes reached grid
    x_df <- data.frame()
    y_df <- data.frame()
  

    
    
   #Klassischer Wert
   classicX_metrics <- actual.df$scaled_IMPACT
   classicY_metrics <- actual.df$scaled_OCCURRENCE
 


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
     
    graphiccentertoclassicX_metrics <- gctcX_scaled_values$gctcX_scaled_result
    
    graphiccentertoclassicY_metrics <- gctcY_scaled_values$gctcY_scaled_result
    
    
    graphic_center_X_metrics <- actual.df$scaled_uncertainty_middle_X
    graphic_center_Y_metrics <- actual.df$scaled_uncertainty_middle_Y

    
    
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
    
    graphic_gridreach_X_metrics <- result_g3_x$Midpoint
    graphic_gridreach_Y_metrics <- result_g3_y$Midpoint
    

    
    
    
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
    
    
    #Datenframes für die anderen Teile (nicht weight)
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
    
    weight.x.simulations <- sample(weight.x.summary_df$weight.x.scale, size = 10000000, replace = TRUE, prob = weight.x.summary_df$weight.x.Wahrs)
    weight.y.simulations <- sample(weight.y.summary_df$weight.y.scale, size = 10000000, replace = TRUE, prob = weight.y.summary_df$weight.y.Wahrs)#500000000
    
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
    

    
    
    #Reachgrid Teil
  
    ImpactReachedGrid <- x_df[,'Grid']
    OccurrenceReachedGrid <- y_df[,'Grid']
    
    RGRIDX_metrics <- ImpactReachedGrid
    RGRIDY_metrics <- OccurrenceReachedGrid
    



  

  
  

  # Datenframe anzeigen
  #print("TABELLE")
  #print(df)
  
  
  
  ######VERTEILUNGEN
      #KLASSICH
        # classicX_metrics
        # classicY_metrics
      #AREA
        # x.daten$Wert
        # y.daten$Wert
      #CENTER
        # actual.df$scaled_uncertainty_middle_X
        # actual.df$scaled_uncertainty_middle_Y
      #CENTERTOGRID
        #graphiccentertoclassicX_metrics
        #graphiccentertoclassicY_metrics
      #REACHEDGRID
        #graphic_gridreach_X_metrics
        #graphic_gridreach_Y_metrics
      #GRAPHICWEIGHT
        # weight.x.simulations
        # weight.y.simulations
      #POOLING
        # pooling.x
        # pooling.y
    
      type.method1 <- "classic"    
      type.method2 <- "area"
      type.method3 <- "center"
      type.method4 <- "centertogrid"
      type.method5 <- "reachedgrid"
      type.method6 <- "weighted"
      type.method7 <- "pooled"
  
      areax <- x.daten$Wert
      areay <- y.daten$Wert
      middlegridx <- actual.df$scaled_uncertainty_middle_X
      middlegridy <- actual.df$scaled_uncertainty_middle_Y
    
  
  
      #KLASSISCH <-> AREA
      x.test <- (ks.test(classicX_metrics, x.daten$Wert, alternative = "two.sided", exact = NULL)) 
      y.test <- (ks.test(classicY_metrics, y.daten$Wert, alternative = "two.sided", exact = NULL)) 
      D.value.x <- x.test$statistic
      value <- (D.value.x[[1]])
      #distance.df <- bind_rows(distance.df, data.frame(scenario = actualscenario, type = "IMPACT" , method1 = type.method1, method2 = type.method2 , distance = value))  
      D.value.y <- y.test$statistic
      value <- (D.value.y[[1]])
      #distance.df <- bind_rows(distance.df, data.frame(scenario = actualscenario, type = "OCCURRENCE" , method1 = type.method1, method2 = type.method2 , distance = value))  
      
      #KLASSISCH <-> CENTER
      x.center <- actual.df$scaled_uncertainty_middle_X
      y.center <- actual.df$scaled_uncertainty_middle_Y
      x1.test <- (ks.test(classicX_metrics, x.center, alternative = "two.sided", exact = NULL)) 
      y1.test <- (ks.test(classicY_metrics, y.center, alternative = "two.sided", exact = NULL)) 
      D.value.x1 <- x1.test$statistic
      value <- D.value.x1[[1]]
      #distance.df <- bind_rows(distance.df, data.frame(scenario = actualscenario, type = "IMPACT" , method1 = type.method1, method2 = type.method3 , distance = value))  
      D.value.y1 <- y1.test$statistic
      value <- D.value.y1[[1]]
      #distance.df <- bind_rows(distance.df, data.frame(scenario = actualscenario, type = "OCCURRENCE" , method1 = type.method1, method2 = type.method3 , distance = value))  
      
      #KLASSISCH <-> CENTERTOGRID
      x.test <- (ks.test(classicX_metrics, graphiccentertoclassicX_metrics, alternative = "two.sided", exact = NULL)) 
      y.test <- (ks.test(classicY_metrics, graphiccentertoclassicY_metrics, alternative = "two.sided", exact = NULL)) 
      D.value.x <- x.test$statistic
      value <- (D.value.x[[1]])
      #distance.df <- bind_rows(distance.df, data.frame(scenario = actualscenario, type = "IMPACT" , method1 = type.method1, method2 = type.method4 , distance = value))  
      D.value.y <- y.test$statistic
      value <- (D.value.y[[1]])
      #distance.df <- bind_rows(distance.df, data.frame(scenario = actualscenario, type = "OCCURRENCE" , method1 = type.method1, method2 = type.method4 , distance = value))  
      
      #KLASSISCH <-> REACHEDGRID
      x.test <- (ks.test(classicX_metrics, graphic_gridreach_X_metrics, alternative = "two.sided", exact = NULL)) 
      y.test <- (ks.test(classicY_metrics, graphic_gridreach_Y_metrics, alternative = "two.sided", exact = NULL)) 
      D.value.x <- x.test$statistic
      value <- (D.value.x[[1]])
      #distance.df <- bind_rows(distance.df, data.frame(scenario = actualscenario, type = "IMPACT" , method1 = type.method1, method2 = type.method5 , distance = value))  
      D.value.y <- y.test$statistic
      value <- (D.value.y[[1]])
      #distance.df <- bind_rows(distance.df, data.frame(scenario = actualscenario, type = "OCCURRENCE" , method1 = type.method1, method2 = type.method5 , distance = value))  
      
      #KLASSISCH <-> WEIGHTED
      x.test <- (ks.test(classicX_metrics, weight.x.simulations, alternative = "two.sided", exact = NULL)) 
      y.test <- (ks.test(classicY_metrics, weight.y.simulations, alternative = "two.sided", exact = NULL)) 
      D.value.x <- x.test$statistic
      value <- (D.value.x[[1]])
      #distance.df <- bind_rows(distance.df, data.frame(scenario = actualscenario, type = "IMPACT" , method1 = type.method1, method2 = type.method6 , distance = value))  
      D.value.y <- y.test$statistic
      value <- (D.value.y[[1]])
      #distance.df <- bind_rows(distance.df, data.frame(scenario = actualscenario, type = "OCCURRENCE" , method1 = type.method1, method2 = type.method6 , distance = value))  
      
      #KLASSISCH <-> POOLING
      print (pooling.x)
      print (pooling.sd_x)
      
      x.test <- ks.test(classicX_metrics, "pnorm", mean = pooling.x, sd = pooling.sd_x, alternative = "two.sided", exact = NULL)
      y.test <- ks.test(classicY_metrics, "pnorm", mean = pooling.y, sd = pooling.sd_y, alternative = "two.sided", exact = NULL)
      print (x.test)
      D.value.x <- x.test$statistic
      value <- (D.value.x[[1]])
      #distance.df <- bind_rows(distance.df, data.frame(scenario = actualscenario, type = "IMPACT" , method1 = type.method1, method2 = type.method7 , distance = value))  
      D.value.y <- y.test$statistic
      value <- (D.value.y[[1]])
      #distance.df <- bind_rows(distance.df, data.frame(scenario = actualscenario, type = "OCCURRENCE" , method1 = type.method1, method2 = type.method7 , distance = value))  
      
      
      
      
      
      #KLASSICH
      # classicX_metrics
      # classicY_metrics
      #AREA
      # x.daten$Wert
      # y.daten$Wert
      #CENTER
      # actual.df$scaled_uncertainty_middle_X
      # actual.df$scaled_uncertainty_middle_Y
      #CENTERTOGRID
      #graphiccentertoclassicX_metrics
      #graphiccentertoclassicY_metrics
      #REACHEDGRID
      #graphic_gridreach_X_metrics
      #graphic_gridreach_Y_metrics
      #GRAPHICWEIGHT
      # weight.x.simulations
      # weight.y.simulations
      #POOLING
      # pooling.x
      # pooling.y
      
      
      
      #print (distance.df)
      #distance.df_all <- rbind(distance.df_all, distance.df)
  
      # Liste der Methoden
      methods <- c("classic", "area", "center", "centertogrid", "reachedgrid", "weighted", "pooling")
      name_DF_IMP <- c("classicX_metrics", "areax", "middlegridx", "graphiccentertoclassicX_metrics", "graphic_gridreach_X_metrics", "weight.x.simulations", "pooling.x")
      name_DF_IMP2 <- c("-","-","-","-","-","-","pooling.sd_x")
      name_DF_OCC <- c("classicY_metrics", "areay", "middlegridy", "graphiccentertoclassicY_metrics", "graphic_gridreach_Y_metrics", "weight.y.simulations", "pooling.y")
      name_DF_OCC2 <- c("-","-","-","-","-","-","pooling.sd_y")
      
      df_basis <- data.frame(methods = methods, variableIMP = name_DF_IMP, variableIMP2 = name_DF_IMP2, variableOCC = name_DF_OCC, variableOCC2 = name_DF_OCC2 )

      # Schleifenfunktion zum Vergleichen der Methoden
      for (i in 1:(length(df_basis$methods)-1)) {
        for (j in (i+1):length(df_basis$methods)) {
          M1 <- df_basis$methods[i]
          value_M1_IMP <- df_basis$variableIMP[i]
          #value_M1_IMP2 <- df_basis$variableIMP2[i]
          value_M1_OCC <- df_basis$variableOCC[i]
          #value_M1_OCC2 <- df_basis$variableOCC2[i]
          M2 <- df_basis$methods[j]
          value_M2_IMP <- df_basis$variableIMP[j]
          #value_M2_IMP2 <- df_basis$variableIMP2[j]
          value_M2_OCC <- df_basis$variableOCC[j]
          #value_M2_OCC2 <- df_basis$variableOCC2[j]
           
          v_M1_IMP <- get (value_M1_IMP)
          v_M2_IMP <- get (value_M2_IMP)
          v_M1_OCC <- get (value_M1_OCC)
          v_M2_OCC <- get (value_M2_OCC)
          if (M2!="pooling")
          {
            new.x.test <- (ks.test(v_M1_IMP, v_M2_IMP, alternative = "two.sided", exact = NULL)) 
            new.y.test <- (ks.test(v_M1_OCC, v_M2_OCC, alternative = "two.sided", exact = NULL))   
          }  
          else{
            new.x.test <- ks.test(v_M1_IMP, "pnorm", mean = pooling.x, sd = pooling.sd_x, alternative = "two.sided", exact = NULL)
            new.y.test <- ks.test(v_M1_OCC, "pnorm", mean = pooling.y, sd = pooling.sd_y, alternative = "two.sided", exact = NULL)
            
          }
          
          
          new.D.value.x <- new.x.test$statistic
          get.x.value <- (new.D.value.x[[1]])
          distance.df <- bind_rows(distance.df, data.frame(scenario = actualscenario, type = "IMPACT" , method1 = M1, method2 = M2 , distance = get.x.value ))  
          new.D.value.y <- new.y.test$statistic
          get.y.value <- (new.D.value.y[[1]])
          distance.df <- bind_rows(distance.df, data.frame(scenario = actualscenario, type = "OCCURRENCE" , method1 = M1, method2 = M2 , distance = get.y.value ))  
       
        }
      }
      
}
print (distance.df)

runde_wenn_numerisch <- function(x) {
  if (is.numeric(x)) {
    return(round(x, 4))
  } else {
    return(x)
  }
}


gerundeter_df <- as.data.frame(lapply(distance.df, runde_wenn_numerisch))

print("TABELLE")
print(gerundeter_df)

scentext <- paste0("Streumaße")
scenfile <- paste0("myapp/files/80_distances/distances_corrected.xlsx")

write.csv(gerundeter_df, file = paste0("myapp/files/80_distances/distances_corrected.csv"), row.names = TRUE)

# Erstellt einen neuen Workbook und füge den transponierten Dataframe ein
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", gerundeter_df)

# Speichern des Workbooks als XLSX-Datei
saveWorkbook(wb, file = scenfile, overwrite = TRUE)



summary_df <- aggregate(distance ~ type + method1 + method2, data = gerundeter_df, FUN = mean)

# Ausgabe des zusammengefassten Dataframes
print(summary_df)

scentext <- paste0("Streumaße")
scenfile <- paste0("myapp/files/80_distances/distances_aggregate1_corrected.xlsx")

write.csv(summary_df, file = paste0("myapp/files/80_distances/distances_aggregate1_corrected.csv"), row.names = TRUE)

# Erstellt einen neuen Workbook und füge den transponierten Dataframe ein
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", summary_df)

# Speichern des Workbooks als XLSX-Datei
saveWorkbook(wb, file = scenfile, overwrite = TRUE)


new_summary_df <- aggregate(distance ~ type + method1 + method2, data = gerundeter_df, FUN = sum)

# Ausgabe des zusammengefassten Dataframes
print(new_summary_df)
scentext <- paste0("Streumaße")
scenfile <- paste0("myapp/files/80_distances/distances_aggregate2_corrected.xlsx")

write.csv(new_summary_df, file = paste0("myapp/files/80_distances/distances_aggregate2_corrected.csv"), row.names = TRUE)

# Erstellt einen neuen Workbook und füge den transponierten Dataframe ein
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", new_summary_df)

# Speichern des Workbooks als XLSX-Datei
saveWorkbook(wb, file = scenfile, overwrite = TRUE)


