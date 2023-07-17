library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)
library(tibble)


answers <- read.csv(file = 'myapp/data/RQ1_corrected_scaled.csv', header=TRUE) #importiere das answers file
all.answers <- answers %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)
number.scenarios <- nrow(as.data.frame(table(all.answers$QUES_ID)))
scenarios <- as.data.frame(table(all.answers$QUES_ID))


for (anz in 1:number.scenarios) {
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

  
  
  filetitleImpact <- (paste0("myapp/pictures/65_reachedgrid_boxplot/",actualscenario,"_Impact.png"))
  filetitleOcc <- (paste0("myapp/pictures/65_reachedgrid_boxplot/",actualscenario,"_Occurrence.png"))
  
  
  png(filetitleImpact, width = 200, height = 500)
  boxplot(IMPACT, main = "IMPACT", ylab = "value", ylim = c(0, 100))
  mean_value <- mean(IMPACT)
  points(mean_value, col = "red", pch = 19)
  dev.off()  
  
  
  png(filetitleOcc, width = 200, height = 500)
  boxplot(OCCURRENCE, main = "OCCURRENCE", ylab = "value", ylim = c(0, 100))
  mean_value <- mean(OCCURRENCE)
  points(mean_value, col = "red", pch = 19)
  dev.off() 
  
 
  
    

  
}