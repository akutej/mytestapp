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
for (anz in 1:3){#number.scenarios) {
  actualscenario =as.vector(scenarios[anz,1])
  print (actualscenario)
  actual.df <- answers %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")
  numberofanswers <- nrow(actual.df)
  #print (numberofanswers)
  
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
    

    
#Graphischer Wert über Flächen & REACHED GRID TEIL
    
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
    
    #Reached grid
 
    x_entry_values <- numeric()
    y_entry_values <- numeric()
    
    if (x.min <= 20 && x.max >= 0) {
      x_entry_values <- c(x_entry_values, 10)
    }
    if (x.min <= 40 && x.max >= 20.25) {
      x_entry_values <- c(x_entry_values, 30)
    }
    if (x.min <= 60 && x.max >= 40.25) {
      x_entry_values <- c(x_entry_values, 50)
    }
    if (x.min <= 80 && x.max >= 60.25) {
      x_entry_values <- c(x_entry_values, 70)
    }
    if (x.min <= 100 && x.max >= 80.25) {
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
  
    
    ImpactReachedGrid <- x_df[,'Grid']
    OccurrenceReachedGrid <- y_df[,'Grid']
    
    RGRIDX_metrics <- calculate_metrics(ImpactReachedGrid)
    RGRIDY_metrics <- calculate_metrics(OccurrenceReachedGrid)
    
    newdfrgrid <- data.frame(
      Scenario = actualscenario,
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
  df <- rbind(df, newdfrgrid)
  
  
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