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
  numberofanswers <- nrow(actual.df)
  #print (numberofanswers)
  
  # Leeren Dataframe erstellen
  x.daten <- data.frame(ACCID = character(),
                      Wert = numeric(),
                      stringsAsFactors = FALSE)
  
  y.daten <- data.frame(ACCID = character(),
                        Wert = numeric(),
                        stringsAsFactors = FALSE)
  
  
  weight.x.daten <- data.frame(ACCID = character(),
                        Wert = numeric(),
                        stringsAsFactors = FALSE)
  
  weight.y.daten <- data.frame(ACCID = character(),
                        Wert = numeric(),
                        stringsAsFactors = FALSE)
  
  
   #Klassischer Wert

    classicX_metrics <- calculate_metrics(actual.df$scaled_IMPACT)
    classicY_metrics <- calculate_metrics(actual.df$scaled_OCCURRENCE)
    
    
    #gewichtete WErte
    weight.IMPACT <- c()
    weight.OCCURRENCE <- c()
    
    
    sum_I <- actual.df %>% mutate(scaled_uncertainty_X = 100 - scaled_uncertainty_X) %>% summarize(sum_I = sum(scaled_uncertainty_X)) %>% pull(sum_I)
    sum_O <- actual.df %>% mutate(scaled_uncertainty_Y = 100 - scaled_uncertainty_Y) %>% summarize(sum_O = sum(scaled_uncertainty_Y)) %>% pull(sum_O)
    
    for (i in 1:numberofanswers){

      ACCID <- actual.df[i,"ACC2SURV_ACCID"]
      UncertaintyI <- actual.df[i,"scaled_uncertainty_X"]
      UncertaintyO <- actual.df[i,"scaled_uncertainty_Y"]
      weight.x.min <- actual.df[i,"scaled_X1"]
      weight.x.max <- actual.df[i,"scaled_X2"]
      weight.y.min <- actual.df[i,"scaled_Y1"]
      weight.y.max <- actual.df[i,"scaled_Y2"]
      weight.actualvaluex <- weight.x.min
      weight.actualvaluey <- weight.y.min
      
      
      while (weight.actualvaluex <= weight.x.max){
        weight.IMPACT.value <- (100/sum_I)*(100-UncertaintyI)
        #transformedx <- (((actualvaluex/400)*100))
        #transformedx <- (((actualvaluex/400)*5)) #+0.5 !!!!!!!!!!!OHNE 0.5
        weight.transformedx <- ((weight.actualvaluex)*weight.IMPACT.value)
        #print (transformedx)
        
        weight.IMPACT <- c(weight.IMPACT,weight.transformedx)
        weight.actualvaluex = weight.actualvaluex + 0.25
      }
      
      while (weight.actualvaluey <= weight.y.max){
        weight.OCCURRENCE.value <- (100/sum_O)*(100-UncertaintyO)
        #   print (weight.OCCURRENCE)
        #transformedy <- (((actualvaluey/400)*100))
        #transformedy <- (((actualvaluey/400)*5))
        weight.transformedy <- ((weight.actualvaluey)*weight.OCCURRENCE.value)
        weight.OCCURRENCE <- c(weight.OCCURRENCE,weight.transformedy)
        weight.actualvaluey = weight.actualvaluey + 0.25
      }
      
      
      
      
    }
    
    # Berechnung der statistischen Werte der Graphischen Area-Methode
    
    wg1_i_median_value <- median(weight.IMPACT)
    wg1_i_sd_value <- sd(weight.IMPACT)
    wg1_i_mean_value <- mean(weight.IMPACT)
    wg1_i_first_quartile <- quantile(weight.IMPACT, probs = 0.25, names = FALSE)
    wg1_i_third_quartile <- quantile(weight.IMPACT, probs = 0.75, names = FALSE)
    wg1_i_interquartile_range <- IQR(weight.IMPACT)
    wg1_i_min_value <- min(weight.IMPACT)
    wg1_i_max_value <- max(weight.IMPACT)
    wg1_i_range_value <- max(weight.IMPACT) - min(weight.IMPACT)
    
    wg1_o_median_value <- median(weight.OCCURRENCE)
    wg1_o_sd_value <- sd(weight.OCCURRENCE)
    wg1_o_mean_value <- mean(weight.OCCURRENCE)
    wg1_o_first_quartile <- quantile(weight.OCCURRENCE, probs = 0.25, names = FALSE)
    wg1_o_third_quartile <- quantile(weight.OCCURRENCE, probs = 0.75, names = FALSE)
    wg1_o_interquartile_range <- IQR(weight.OCCURRENCE)
    wg1_o_min_value <- min(weight.OCCURRENCE)
    wg1_o_max_value <- max(weight.OCCURRENCE)
    wg1_o_range_value <- max(weight.OCCURRENCE) - min(weight.OCCURRENCE)
    
    # Datenframe erstellen
    newdfwg1 <- data.frame(
      Scenario = actualscenario,
      method = "weight graphic areas",
      impact_Median = wg1_i_median_value,
      impact_StandardDeviation = wg1_i_sd_value,
      impact_Mean = wg1_i_mean_value,
      impact_FirstQuartile = wg1_i_first_quartile,
      impact_ThirdQuartile = wg1_i_third_quartile,
      impact_InterquartileRange = wg1_i_interquartile_range,
      impact_Minimum = wg1_i_min_value,
      impact_Maximum = wg1_i_max_value,
      impact_Range = wg1_i_range_value,
      occurrence_Median = wg1_o_median_value,
      occurrence_StandardDeviation = wg1_o_sd_value,
      occurrence_Mean = wg1_o_mean_value,
      occurrence_FirstQuartile = wg1_o_first_quartile,
      occurrence_ThirdQuartile = wg1_o_third_quartile,
      occurrence_InterquartileRange = wg1_o_interquartile_range,
      occurrence_Minimum = wg1_o_min_value,
      occurrence_Maximum = wg1_o_max_value,
      occurrence_Range = wg1_o_range_value
    )
    
    
    
    
    
    
    
    
    
       
    # Datenframe erstellen
    newdfclassic <- data.frame(
      Scenario = actualscenario,
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
    
    
    graphic_center_X_metrics <- calculate_metrics(actual.df$scaled_uncertainty_middle_X)
    graphic_center_Y_metrics <- calculate_metrics(actual.df$scaled_uncertainty_middle_Y)
    
    newdfg2 <- data.frame(
      Scenario = actualscenario,
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
      Scenario = actualscenario,
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
    

    
#Graphischer Wert über Flächen
    
  for (i in 1:numberofanswers){
    ACCID <- actual.df[i,"ACC2SURV_ACCID"]
    
    x.min <- actual.df[i,"scaled_X1"]
    x.max <- actual.df[i,"scaled_X2"]
    y.min <- actual.df[i,"scaled_Y1"]
    y.max <- actual.df[i,"scaled_Y2"]
    x_user_werte <- seq(x.min, x.max, by = 0.25)
    y_user_werte <- seq(y.min, y.max, by = 0.25)
  
    x_daten_user <- data.frame(ACCID = rep(ACCID, length(x_user_werte)), Wert = x_user_werte)
    x.daten <- rbind(x.daten, x_daten_user)
    y_daten_user <- data.frame(ACCID = rep(ACCID, length(y_user_werte)), Wert = y_user_werte)
    y.daten <- rbind(y.daten, y_daten_user)
    
  }
  

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
    Scenario = actualscenario,
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
  # Zeilen zusammenführen
  df <- rbind(df, newdfwg1)
  
  
  
  # Datenframe anzeigen
  print("TABELLE")
  print(df)
  

  #scentext <- paste0("Scenario", actualscenario)
  #scenfile <- paste0("myapp/files/50_scattering/", scentext, ".xlsx")
  
  #write.csv(combined_data, file = paste0("myapp/files/40_Streum/",scentext,".csv"), row.names = TRUE)
  
  # Erstelle einen neuen Workbook und füge den transponierten Dataframe ein
  #wb <- createWorkbook()
  #addWorksheet(wb, "Sheet1")
  #writeData(wb, "Sheet1", combined_data)
  
  # Speichern des Workbooks als XLSX-Datei
  #saveWorkbook(wb, file = scenfile, overwrite = TRUE)
  
  
  
  
}