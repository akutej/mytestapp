library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)
library(tibble)


answers <- read.csv(file = 'myapp/data/RQ1_corrected_scaled.csv', header=TRUE) #importiere das answers file
answers <- answers %>% filter(QUES_ID == "359")
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
      
      filetitleImpact <- (paste0("myapp/pictures/110_hist_centertogrid/",actualscenario,"_Impact.png"))
      filetitleOcc <- (paste0("myapp/pictures/110_hist_centertogrid/",actualscenario,"_Occurrence.png"))
      
      png(file=filetitleImpact,width=600, height=600)
      #hist(IMPACT,breaks = c(0.5,1.5,2.5,3.5,4.5,5.5), main=headtitleImpact,xlim=c(0.5,5.5),labels = FALSE,col="lightblue",xlab = "" )
      hist(IMPACT$gctcX_scaled_result,breaks = c(0,20,40,60,80,100), main="",xlim=c(0,100),labels = FALSE,col="lightblue",xlab = "" )
      dev.off()
      
      png(file=filetitleOcc,width=600, height=600)
      #hist(OCCURRENCE,breaks = c(0.5,1.5,2.5,3.5,4.5,5.5), main=headtitleOcc,xlim=c(0.5,5.5),labels = FALSE,col="lightblue",xlab = "" )
      hist(OCCURRENCE$gctcY_scaled_result,breaks = c(0,20,40,60,80,100), main="",xlim=c(0,100),labels = FALSE,col="lightblue",xlab = "" )
      dev.off()
    
    
}