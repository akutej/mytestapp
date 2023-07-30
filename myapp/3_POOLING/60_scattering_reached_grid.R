library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)
library(tibble)


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
  
  
  # Definiere die Breakpoints und Labels für die Stufen
  breakpoints <- c(0, 20, 40, 60, 80, 100)
  labels <- c("1", "2", "3", "4", "5")
  
  # Erstelle einen leeren DataFrame für das Ergebnis
  x.result <- data.frame(
    userID = integer(),
    Grid = character()
  )
  
  y.result <- data.frame(
    userID = integer(),
    Grid = character()
  )
  
    
  for (i in 1:numberofanswers){
    ACCID <- actual.df[i,"ACC2SURV_ACCID"]
    
    x_min <- actual.df[i,"scaled_X1"]
    x_max <- actual.df[i,"scaled_X2"]
    y_min <- actual.df[i,"scaled_Y1"]
    y_max <- actual.df[i,"scaled_Y2"]
    
    

      
      # Erstellt eine Sequenz von Werten innerhalb des Bereichs mit einem Abstand von 0.25
      x_values <- seq(from = x_min, to = x_max, by = 0.25)
      y_values <- seq(from = y_min, to = y_max, by = 0.25)
      
      # Ermittelt die Stufen für alle Werte in der Sequenz
      x.stages <- cut(x_values, breaks = breakpoints, labels = labels, right = TRUE)
      y.stages <- cut(y_values, breaks = breakpoints, labels = labels, right = TRUE)
      
      # Einzigartige Stufen
      x.unique_stages <- unique(x.stages)
      y.unique_stages <- unique(y.stages)
      
      # Füge für jede einzigartige Stufe eine Zeile in den DataFrame ein
      for (stage in x.unique_stages) {
        x.result <- rbind(x.result, data.frame(userID = ACCID, Grid = as.character(stage)))
      }
      
      for (stage in y.unique_stages) {
        y.result <- rbind(y.result, data.frame(userID = ACCID, Grid = as.character(stage)))
      }
      
    }
    
    # Zeige den resultierenden DataFrame
    
  
  #print(x.result)
  # Zähle die Anzahl der Einträge pro Stufe
  x.counts <- table(x.result$Grid)
  y.counts <- table(y.result$Grid)
  
  # Zeige das Ergebnis
  print(x.counts)
  print(y.counts)
    
    

  
}