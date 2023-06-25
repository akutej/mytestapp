library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)

answerstable <- read.csv(file = 'myapp/data/RQ1_corrected_scaled.csv', header=TRUE) #importiere das answers file
dfall <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == 281)
scenarios <- as.data.frame(table(dfall$QUES_ID))
numberscenarios  <- nrow(scenarios)
for (anz in 1:1) {
  actualscenario =as.vector(scenarios[anz,1])
  print (actualscenario)
  scentext <- (paste0("Scenario ", actualscenario))
  #print (actualscenario) 
  df <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")
  #print (df)
  numberofanswers <- nrow(df)
  
  D <- as.data.frame(expand.grid(seq(0, 100, by = 0.25)))
  D <- cbind(D,0)
  names(D) <- c("x-Achse","Zähler")
  
  sum.Occ <- 0
  sum.Imp <- 0
  sum.Area <- 0
  
  for (m in 1:numberofanswers){
    #print (Uncertainty.Area)
    Uncertainty.Impact <- df[m,"scaled_uncertainty_X"]
    #Uncertainty.Occurrence <- df[m,"uncertaintyOPixel"]
    #distance.Occ <- (400-Uncertainty.Occurrence)  #Abstand zu maxWert Occourrence
    distance.Imp <- (100-Uncertainty.Impact)  #Abstand zu maxWert Impact
    #distance.Area <- (max.Scenario.Area.Uncertainty-Uncertainty.Area)  #Abstand zu maxWert Impact
    #sum.Occ <- sum.Occ + distance.Occ
    sum.Imp <- sum.Imp + distance.Imp
    #sum.Area <- sum.Area + Uncertainty.AreaSwitched
  }  
  print (sum.Imp)
  #print (sum.Occ)
  #rint (sum.Area)
  Uncertaintyweightall <- 0
for (i in 1:numberofanswers){
  #print (i)
  AccId <- df[i,"ACC2SURV_ACCID"]
  QuesId <- df[i,"QUES_ID"]
  UncertaintyI <- df[i,"scaled_uncertainty_X"]
  Role <- df[i,"ACC2SURV_ROLE"]
  GroupId <- df[i,"ACC2SURV_GROUPID"]
  x.min <- df[i,"scaled_X1"]
  x.max <- df[i,"scaled_X2"]
  weightI <- UncertaintyI/sum.Imp
  #distanceAreaUser <- (max.Scenario.Area.Uncertainty-UncertaintyAreaUser)
  Uncertaintyweight <- ((100/sum.Imp)* (100 - UncertaintyI))
  #print (UncertaintyAreaUser)
  #print (Uncertaintyweight)
  Uncertaintyweightall <- Uncertaintyweightall + Uncertaintyweight
  index <- which( D[,"x-Achse"] >= x.min & D[,"x-Achse"] <= x.max)
  
  
  D[index,"Zähler"] <- D[index,"Zähler"] + (1 * Uncertaintyweight)
  #print (AccId)
}
  
  
  
  print (Uncertaintyweight)
  print (D)
  l
  wg1_i_median_value <- median(D$Zähler)
  wg1_i_sd_value <- sd(D$Zähler)
  wg1_i_mean_value <- mean(D$Zähler)
  
  histogram <- hist(D$Zähler, breaks = seq(0, 100, by = 0.25), plot = FALSE)
  
  plot(histogram, main = "Histogramm", xlab = "Werte", ylab = "Häufigkeit", 
       col = "lightblue", border = "black")
  #produkt <- D$'x-Achse' * D$Zähler
  #print (produkt)
  
  # Summe der multiplizierten Werte
  #summe <- sum(produkt)
  
  # Durchschnitt berechnen
#  durchschnitt <- summe / nrow(D)
  
 # print (nrow(D))
  # Ausgabe des Durchschnitts
  #print(durchschnitt)
  

} 