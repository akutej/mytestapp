library(dplyr)
library(openxlsx)
library(MASS)
library(gplots)
library(ggplot2)
library(ellipse)



answerstable <- read.csv(file = 'myapp/Data/RQ1_corrected_scaled.csv', header=TRUE) #importiere das answers file
dfall <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)
scenarios <- as.data.frame(table(dfall$QUES_ID))
numberscenarios  <- nrow(scenarios)
for (anz in 1:1) {
  actualscenario =as.vector(scenarios[anz,1])
  print (actualscenario)
  scentext <- (paste0("Scenario ", actualscenario))
  df <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == 281)# & ACC2SURV_ACCID == "22")
  #print (df)
  numberofanswers <- nrow(df)
  x <- seq(0, 100, by = 0.25)
  y <- seq(0, 100, by = 0.25)
  
  for (i in 1:numberofanswers){
    print (i)
    AccId <- df[i,"ACC2SURV_ACCID"]
    QuesId <- df[i,"QUES_ID"]
    UncertaintyI <- df[i,"scaled_uncertainty_X"]
    UncertaintyO <- df[i,"scaled_uncertainty_Y"]
    Role <- df[i,"ACC2SURV_ROLE"]
    GroupId <- df[i,"ACC2SURV_GROUPID"]
    x.min <- df[i,"scaled_X1"]
    x.max <- df[i,"scaled_X2"]
    y.min <- df[i,"scaled_Y1"]
    y.max <- df[i,"scaled_Y2"]
    
    neue_wertX <- seq (x.min,x.max,0.25)
    neue_wertY <- seq (y.min,y.max,0.25)
    
    #x <- c(x, neue_wertX)
    #y <- c(y, neue_wertY)
    
    
    
  }
  print(x)
}

hist(x, main = "Histogramm für x", xlab = "Werte von x",breaks=400, ylab = "Häufigkeit", col = "lightblue")
#  scenfile <- (paste0("myapp/files/4_heatmap/",scentext,"_transformed_new_scaled_dichte.xlsx"))
#  write.csv(D, paste0("myapp/files/4_heatmap/", scentext,"_transformed_new_scaled.csv"), row.names=TRUE)
#  write.xlsx(D,file = scenfile, rowNames=TRUE)
  

#x <- table$x
#y <- table$y
#count <- table$count
#data <- data.frame(x=x,y=y,count=count)
#total_count <- sum(data$count) #Gesamtzahl der Ergebnisse
#data$density <- data$count / total_count #relative Dichte -> Anzahl der Ergebnisse für jede Kombination aus X und Y durch die Gesamtzahl der Ergebnisse

#myplot <- ggplot(data, aes(x = x, y = y, fill = density)) +
#  geom_tile() +
#  scale_fill_gradientn(colours = colorRampPalette(c("white", "blue", "green", "yellow", "red"))(100)) +
#  labs(x = "X-Werte", y = "Y-Werte", fill = "Dichte", title = "Zweidimensionale Dichtefunktion")

#print (myplot)
#density_est <- density(data, bw = 0.5) 


#xtest <- c(1,3,4,5,5,5,5,5,5,5,6,7,8,9,10)
#ytest <- c(1,3,4,5,5,5,5,5,5,5,6,7,8,9,10)
density_est <- kde2d(x,y) 

# Plotten Sie die Ergebnisse als Heatmap
filled.contour(density_est)
