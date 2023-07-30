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


answers <- read.csv(file = 'myapp/data/RQ1_corrected_scaled.csv', header=TRUE) #importiere das answers file
all.answers <- answers %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)
number.scenarios <- nrow(as.data.frame(table(all.answers$QUES_ID)))
scenarios <- as.data.frame(table(all.answers$QUES_ID))


for (anz in 1:1){#number.scenarios) {
  actualscenario =as.vector(scenarios[anz,1])
  print (actualscenario)
  actual.df <- answers %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")
  numberofanswers <- nrow(actual.df)
  scentext <- (paste0("", actualscenario))
  #print (numberofanswers)
  
  x_df <- data.frame()
  y_df <- data.frame()
    
  for (i in 1:numberofanswers){
    ACCID <- actual.df[i,"ACC2SURV_ACCID"]
    
    xmin <- actual.df[i,"scaled_X1"]
    xmax <- actual.df[i,"scaled_X2"]
    ymin <- actual.df[i,"scaled_Y1"]
    ymax <- actual.df[i,"scaled_Y2"]
    
    x_entry_values <- numeric()
    y_entry_values <- numeric()
    
    if (xmin <= 20 && xmax >= 0) {
      x_entry_values <- c(x_entry_values, 10)
    }
    if (xmin <= 40 && xmax >= 20.25) {
      x_entry_values <- c(x_entry_values, 30)
    }
    if (xmin <= 60 && xmax >= 40.25) {
      x_entry_values <- c(x_entry_values, 50)
    }
    if (xmin <= 80 && xmax >= 60.25) {
      x_entry_values <- c(x_entry_values, 70)
    }
    if (xmin <= 100 && xmax >= 80.25) {
      x_entry_values <- c(x_entry_values, 90)
    }
    
    if (ymin <= 20 && ymax >= 0) {
      y_entry_values <- c(y_entry_values, 10)
    }
    if (ymin <= 40 && ymax >= 20.25) {
      y_entry_values <- c(y_entry_values, 30)
    }
    if (ymin <= 60 && ymax >= 40.25) {
      y_entry_values <- c(y_entry_values, 50)
    }
    if (ymin <= 80 && ymax >= 60.25) {
      y_entry_values <- c(y_entry_values, 70)
    }
    if (ymin <= 100 && ymax >= 80.25) {
      y_entry_values <- c(y_entry_values, 90)
    }
    
    x_df <- bind_rows(x_df, data.frame(Grid = x_entry_values, User = ACCID))
    y_df <- bind_rows(y_df, data.frame(Grid = y_entry_values, User = ACCID))
  }
    
    # Zeige den resultierenden DataFrame
    
  
  print(x_df)
  # Zähle die Anzahl der Einträge pro Stufe
  x.counts <- table(x_df$Grid)
  y.counts <- table(y_df$Grid)
  
  # Zeige das Ergebnis
  print(x.counts)
  print(y.counts)
  
  histImpactclassic <- x_df[,'Grid']
  histOccurrenceclassic <- y_df[,'Grid']
  IMPACT <- (histImpactclassic)
  print (IMPACT)
  OCCURRENCE <- (histOccurrenceclassic)
  filetitleImpact <- (paste0("myapp/pictures/65_hist_reachedgrid/",scentext,"- Impact.png"))
  filetitleOcc <- (paste0("myapp/pictures/65_hist_reachedgrid/",scentext,"- Occurrence.png"))
  #histImpact <- replace(histImpactclassic,,histImpactclassic-0.5)
  #histImpact <- sapply(histImpactclassic,quantile)
  
  png(file=filetitleImpact,width=600, height=600)
  hist(IMPACT,breaks = c(0,20,40,60,80,100), main="",xlim=c(0,100),labels = FALSE,col="lightblue",xlab = "" )
  dev.off()
  
  png(file=filetitleOcc,width=600, height=600)
  hist(OCCURRENCE,breaks = c(0,20,40,60,80,100), main="",xlim=c(0,100),labels = FALSE,col="lightblue",xlab = "" )
  dev.off()
  
  classicX_metrics <- calculate_metrics(histImpactclassic)
  classicY_metrics <- calculate_metrics(histOccurrenceclassic)
  
  
  # Datenframe erstellen
  newdfclassic <- data.frame(
    Scenario = actualscenario,
    method = "reachedgrid",
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
  
  resultdf <- rbind(df, newdfclassic)  
  
  print (resultdf)
  
  
    

  
}