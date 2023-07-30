library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)
library(tibble)


answers <- read.csv(file = 'myapp/data/RQ1_corrected_scaled.csv', header=TRUE) #importiere das answers file
all.answers <- answers %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)
number.scenarios <- nrow(as.data.frame(table(all.answers$QUES_ID)))
scenarios <- as.data.frame(table(all.answers$QUES_ID))


for (anz in 1:2){#number.scenarios) {
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
  
  result_df <- data.frame()
  for (i in 1:numberofanswers){
    ACCID <- actual.df[i,"ACC2SURV_ACCID"]
    
    x_min <- actual.df[i,"scaled_X1"]
    x_max <- actual.df[i,"scaled_X2"]
    y_min <- actual.df[i,"scaled_Y1"]
    y_max <- actual.df[i,"scaled_Y2"]
    print (ACCID)
    print(x_min)
    print(x_max)
    print(y_min)
    print(y_max)
    
    
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
 
    # Kombiniere die einzigartigen Stufen von x und y in den DataFrame
    for (x_stage in x.unique_stages) {
      for (y_stage in y.unique_stages) {
        combined_stage <- paste0(as.character(y_stage), as.character(x_stage))
        result_df <- rbind(result_df, data.frame(userID = ACCID, Grid_X = as.character(x_stage), Grid_Y = as.character(y_stage), Grid_YX = combined_stage))
      }
    }
    
  }
  print (x.result)
  print (y.result)
  print (result_df)
  
  # Zeige den resultierenden DataFrame
  
  
  #print(x.result)
  # Zähle die Anzahl der Einträge pro Stufe
  x.counts <- table(x.result$Grid)
  y.counts <- table(y.result$Grid)

  sum_table <- table(result_df$Grid_YX)
  print (sum_table)
  # Extrahiere die Zeilennamen (Kombinationen von Grid_X und Grid_Y) in einen Vektor
  result_names <- rownames(sum_table)
  
  # Extrahiere die Summenwerte in einen Vektor
  result_values <- as.vector(sum_table)
  
  # Ergebnis anzeigen
  print(result_names)
  print(result_values)
  
  # Erstelle eine leere 5x5 Matrix
  matrix_dimension <- 5
  matrix_result <- matrix(result_values, nrow = 5, byrow = TRUE)
  
  # Drehe die Reihenfolge der Zeilen der Matrix um
  #matrix_result <- matrix_result[, 5:1]
  
  print (matrix_result)
  
  # Lade die benötigte Bibliothek für die Farbskala
  library(RColorBrewer)
  
  
  #vektortest <- c(11, 12, 13, 14, 15, 21, 22, 23, 24, 25, 31, 32, 33, 34, 35, 41, 42, 43, 44, 45, 51, 52, 53, 54, 55)
  #matrixtest <- matrix(vektortest, nrow = 5, byrow = TRUE)
  # Erstelle die Heatmap mit 11 in der linken unteren Ecke

  
  scentext_pic <- (paste0("", actualscenario))
  scenfile_picm1 <- (paste0("myapp/pictures/65_reachedgrid_heatmap/",scentext_pic,".png"))

  ####KURZ MAL WEG ANFANG
  png(file=scenfile_picm1, width = 500, height = 500, units = 'px', res = 100)
  heatmap(matrix_result,  Rowv = NA, Colv = NA,scale = "none", labCol = NA,labRow = NA)
    dev.off()
  ####KURZ MAL WEG ENDE
  
    # Zeige das Ergebnis
  #print(x.counts)
  #print(y.counts)
  
  
  
  
}