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
  actualtype <- actual.df[1,"QUES_TYP"]
  print (actualtype)
  
  
  numberofanswers <- nrow(actual.df)
  print (numberofanswers)
  
 
  
     
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
     
      print (IMPACT <- gctcX_scaled_values)
      print (OCCURRENCE <- gctcY_scaled_values)
      
      filetitleImpact <- (paste0("myapp/pictures/110_centertogrid_boxplot/",actualscenario,"_Impact.png"))
      filetitleOcc <- (paste0("myapp/pictures/110_centertogrid_boxplot/",actualscenario,"_Occurrence.png"))

      
      png(filetitleImpact, width = 200, height = 500)
      boxplot(IMPACT$gctcX_scaled_result, main = "IMPACT", ylab = "value", ylim = c(0, 100))
      mean_value <- mean(IMPACT$gctcX_scaled_result)
      points(mean_value, col = "red", pch = 19)
      dev.off()  
      
      
      png(filetitleOcc, width = 200, height = 500)
      boxplot(OCCURRENCE$gctcY_scaled_result, main = "OCCURRENCE", ylab = "value", ylim = c(0, 100))
      mean_value <- mean(OCCURRENCE$gctcY_scaled_result)
      points(mean_value, col = "red", pch = 19)
      dev.off()  
      
      
      
      
    
    
}