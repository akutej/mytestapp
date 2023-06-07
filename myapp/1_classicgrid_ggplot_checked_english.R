#Erstellung der Klassischen Risikomatrix für alle Szenarien 
#getrennt nach Risiko und Chance inklusive Verspeicherung der einzelnen Bilder

library(plotrix)
library(dplyr)
library(ggplot2)
library(grid)
library(RColorBrewer)

answerstable <- read.csv(file = 'myapp/Data/RQ1_corrected.csv', header=TRUE) #importiere das answers file

dfall <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)
scenarios <- as.data.frame(table(dfall$QUES_ID))
#print(scenarios)
numberscenarios  <- nrow(scenarios)
#print (numberscenarios)

for (anz in 1:numberscenarios) {
actualscenario =as.vector(scenarios[anz,1])
#actualType =as.vector(scenarios[anz,1])
scentext <- (paste0("Scenario ", actualscenario))
df <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == actualscenario)
actualtype <- (df$QUES_TYP[1])
numberofanswers <- nrow(df)
#print (numberofanswers)
#dfgroup1 <- df %>% filter(ACC2SURV_ROLE == "1")# ACC2SURV_GROUPID== "1"
#numberofanswersg1 <- nrow(dfgroup1)
#dfgroup2 <- df %>% filter(ACC2SURV_ROLE == "2")
#numberofanswersg2 <- nrow(dfgroup2)
scenariogrid <- data.frame(table(df$ClassicGrid))
#print (scenariogrid)
basicgrid <- data.frame(c("Grid11","Grid21","Grid31","Grid41","Grid51","Grid12","Grid22","Grid32","Grid42","Grid52","Grid13","Grid23","Grid33","Grid43","Grid53","Grid14","Grid24","Grid34","Grid44","Grid54","Grid15","Grid25","Grid35","Grid45","Grid55"))

numberbasic  <- nrow(basicgrid)

for (i in 1:numberbasic) {
      #print (basicgrid[i,1])
      actualGRID <- basicgrid[i,1]
      m = which(scenariogrid == actualGRID)
      x <- identical(m, integer(0))
      if(x != TRUE){
        value <- (scenariogrid[m,"Freq"])
        #print (value)
        basicgrid[i,"MatrixSum"] <- value
      } else{
        basicgrid[i,"MatrixSum"] <- "0"
      }
}

classicgrid <- (basicgrid$MatrixSum)
#classicgrid<- c(20,2,3,4,2,20,2,3,4,2,20,2,3,4,2,20,2,3,4,2)

if (actualtype == "Risiko"){

thisis <- ggplot()+ 
  ggtitle(scentext)+ 
  theme(#plot.title = element_text(hjust = 0.5,color="black", size=16),#face="bold"),
        plot.title = element_blank(),
        axis.line  = element_blank(),
        axis.ticks = element_blank(),
        axis.text  = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
        )+
  xlim(-150,400)+
  ylim(0,530)+
  geom_rect(aes(xmin=-150, xmax=0, ymin=0, ymax=80),fill="lightgrey", color="black", alpha=1)+
  geom_rect(aes(xmin=-150, xmax=0, ymin=80, ymax=160),fill="lightgrey", color="black", alpha=1)+
  geom_rect(aes(xmin=-150, xmax=0, ymin=160, ymax=240),fill="lightgrey", color="black", alpha=1)+
  geom_rect(aes(xmin=-150, xmax=0, ymin=240, ymax=320),fill="lightgrey", color="black", alpha=1)+
  geom_rect(aes(xmin=-150, xmax=0, ymin=320, ymax=400),fill="lightgrey", color="black", alpha=1)+
  geom_rect(aes(xmin=-150, xmax=-100, ymin=0, ymax=400),fill="grey", color="black", alpha=1)+
  geom_rect(aes(xmin=0, xmax=80, ymin=400, ymax=480),fill="lightgrey", color="black", alpha=1)+
  geom_rect(aes(xmin=80, xmax=160, ymin=400, ymax=480),fill="lightgrey", color="black", alpha=1)+
  geom_rect(aes(xmin=160, xmax=240, ymin=400, ymax=480),fill="lightgrey", color="black", alpha=1)+
  geom_rect(aes(xmin=240, xmax=320, ymin=400, ymax=480),fill="lightgrey", color="black", alpha=1)+
  geom_rect(aes(xmin=320, xmax=400, ymin=400, ymax=480),fill="lightgrey", color="black", alpha=1)+
  geom_rect(aes(xmin=0, xmax=400, ymin=480, ymax=530),fill="grey", color="black", alpha=1)+
  
  geom_rect(aes(xmin=0, xmax=80, ymin=0, ymax=80),fill="green", color="black", alpha=1)+
  geom_rect(aes(xmin=0, xmax=80, ymin=80, ymax=160),fill="green", color="black", alpha=1)+
  geom_rect(aes(xmin=0, xmax=80, ymin=160, ymax=240),fill="green", color="black", alpha=1)+
  geom_rect(aes(xmin=0, xmax=80, ymin=240, ymax=320),fill="green", color="black", alpha=1)+
  geom_rect(aes(xmin=0, xmax=80, ymin=320, ymax=400),fill="yellow", color="black", alpha=1)+
  geom_rect(aes(xmin=80, xmax=160, ymin=0, ymax=80),fill="green", color="black", alpha=1)+
  geom_rect(aes(xmin=80, xmax=160, ymin=80, ymax=160),fill="green", color="black", alpha=1)+
  geom_rect(aes(xmin=80, xmax=160, ymin=160, ymax=240),fill="yellow", color="black", alpha=1)+
  geom_rect(aes(xmin=80, xmax=160, ymin=240, ymax=320),fill="yellow", color="black", alpha=1)+
  geom_rect(aes(xmin=80, xmax=160, ymin=320, ymax=400),fill="yellow", color="black", alpha=1)+
  geom_rect(aes(xmin=160, xmax=240, ymin=0, ymax=80),fill="green", color="black", alpha=1)+
  geom_rect(aes(xmin=160, xmax=240, ymin=80, ymax=160),fill="yellow", color="black", alpha=1)+
  geom_rect(aes(xmin=160, xmax=240, ymin=160, ymax=240),fill="yellow", color="black", alpha=1)+
  geom_rect(aes(xmin=160, xmax=240, ymin=240, ymax=320),fill="yellow", color="black", alpha=1)+
  geom_rect(aes(xmin=160, xmax=240, ymin=320, ymax=400),fill="red", color="black", alpha=1)+
  geom_rect(aes(xmin=240, xmax=320, ymin=0, ymax=80),fill="yellow", color="black", alpha=1)+
  geom_rect(aes(xmin=240, xmax=320, ymin=80, ymax=160),fill="yellow", color="black", alpha=1)+
  geom_rect(aes(xmin=240, xmax=320, ymin=160, ymax=240),fill="yellow", color="black", alpha=1)+
  geom_rect(aes(xmin=240, xmax=320, ymin=240, ymax=320),fill="red", color="black", alpha=1)+
  geom_rect(aes(xmin=240, xmax=320, ymin=320, ymax=400),fill="red", color="black", alpha=1)+
  geom_rect(aes(xmin=320, xmax=400, ymin=0, ymax=80),fill="yellow", color="black", alpha=1)+
  geom_rect(aes(xmin=320, xmax=400, ymin=80, ymax=160),fill="red", color="black", alpha=1)+
  geom_rect(aes(xmin=320, xmax=400, ymin=160, ymax=240),fill="red", color="black", alpha=1)+
  geom_rect(aes(xmin=320, xmax=400, ymin=240, ymax=320),fill="red", color="black", alpha=1)+
  geom_rect(aes(xmin=320, xmax=400, ymin=320, ymax=400),fill="red", color="black", alpha=1)+
  
  geom_text(aes(x = 40, y = 40, label = classicgrid[1]),size = 10)+
  geom_text(aes(x = 40, y = 120, label = classicgrid[2]),size = 10)+
  geom_text(aes(x = 40, y = 200, label = classicgrid[3]),size = 10)+
  geom_text(aes(x = 40, y = 280, label = classicgrid[4]),size = 10)+
  geom_text(aes(x = 40, y = 360, label = classicgrid[5]),size = 10)+
  
  geom_text(aes(x = 120, y = 40, label = classicgrid[6]),size = 10)+
  geom_text(aes(x = 120, y = 120, label = classicgrid[7]),size = 10)+
  geom_text(aes(x = 120, y = 200, label = classicgrid[8]),size = 10)+
  geom_text(aes(x = 120, y = 280, label = classicgrid[9]),size = 10)+
  geom_text(aes(x = 120, y = 360, label = classicgrid[10]),size = 10)+
  
  geom_text(aes(x = 200, y = 40, label = classicgrid[11]),size = 10)+
  geom_text(aes(x = 200, y = 120, label = classicgrid[12]),size = 10)+
  geom_text(aes(x = 200, y = 200, label = classicgrid[13]),size = 10)+
  geom_text(aes(x = 200, y = 280, label = classicgrid[14]),size = 10)+
  geom_text(aes(x = 200, y = 360, label = classicgrid[15]),size = 10)+
  
  geom_text(aes(x = 280, y = 40, label = classicgrid[16]),size = 10)+
  geom_text(aes(x = 280, y = 120, label = classicgrid[17]),size = 10)+
  geom_text(aes(x = 280, y = 200, label = classicgrid[18]),size = 10)+
  geom_text(aes(x = 280, y = 280, label = classicgrid[19]),size = 10)+
  geom_text(aes(x = 280, y = 360, label = classicgrid[20]),size = 10)+
  
  geom_text(aes(x = 360, y = 40, label = classicgrid[21]),size = 10)+
  geom_text(aes(x = 360, y = 120, label = classicgrid[22]),size = 10)+
  geom_text(aes(x = 360, y = 200, label = classicgrid[23]),size = 10)+
  geom_text(aes(x = 360, y = 280, label = classicgrid[24]),size = 10)+
  geom_text(aes(x = 360, y = 360, label = classicgrid[25]),size = 10)+
  
  geom_text(aes(x = -50, y = 40, label = "unlikely"),size = 5)+
  geom_text(aes(x = -50, y = 120, label = "very low"),size = 5)+
  geom_text(aes(x = -50, y = 200, label = "low"),size = 5)+
  geom_text(aes(x = -50, y = 280, label = "high"),size = 5)+
  geom_text(aes(x = -50, y = 360, label = "very high"),size = 5)+
  
  geom_text(aes(x = 40 , y = 440, label = "insignificant"),size = 5)+
  geom_text(aes(x = 120, y = 440, label = "low"),size = 5)+
  geom_text(aes(x = 200, y = 440, label = "noticeable"),size = 5)+
  geom_text(aes(x = 280, y = 440, label = "critical"),size = 5)+
  geom_text(aes(x = 360, y = 440, label = "catastrophic"),size = 5)+
  
  geom_text(aes(x = 200, y = 505, label = "Impact"),size = 5)+
  geom_text(aes(x = -125, y = 200, label = "Probability of occurrence"),size = 5,angle=90)#+
  
}

else
{
  thisis <- ggplot()+ 
    ggtitle(scentext)+ 
    theme(
          #plot.title = element_text(hjust = 0.5,color="black", size=16),#face="bold"),
          plot.title = element_blank(),    
          axis.line  = element_blank(),
          axis.ticks = element_blank(),
          axis.text  = element_blank(),
          axis.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()
    )+
    xlim(-150,400)+
    ylim(0,530)+
    geom_rect(aes(xmin=-150, xmax=0, ymin=0, ymax=80),fill="lightgrey", color="black", alpha=1)+
    geom_rect(aes(xmin=-150, xmax=0, ymin=80, ymax=160),fill="lightgrey", color="black", alpha=1)+
    geom_rect(aes(xmin=-150, xmax=0, ymin=160, ymax=240),fill="lightgrey", color="black", alpha=1)+
    geom_rect(aes(xmin=-150, xmax=0, ymin=240, ymax=320),fill="lightgrey", color="black", alpha=1)+
    geom_rect(aes(xmin=-150, xmax=0, ymin=320, ymax=400),fill="lightgrey", color="black", alpha=1)+
    geom_rect(aes(xmin=-150, xmax=-100, ymin=0, ymax=400),fill="grey", color="black", alpha=1)+
    geom_rect(aes(xmin=0, xmax=80, ymin=400, ymax=480),fill="lightgrey", color="black", alpha=1)+
    geom_rect(aes(xmin=80, xmax=160, ymin=400, ymax=480),fill="lightgrey", color="black", alpha=1)+
    geom_rect(aes(xmin=160, xmax=240, ymin=400, ymax=480),fill="lightgrey", color="black", alpha=1)+
    geom_rect(aes(xmin=240, xmax=320, ymin=400, ymax=480),fill="lightgrey", color="black", alpha=1)+
    geom_rect(aes(xmin=320, xmax=400, ymin=400, ymax=480),fill="lightgrey", color="black", alpha=1)+
    geom_rect(aes(xmin=0, xmax=400, ymin=480, ymax=530),fill="grey", color="black", alpha=1)+
    
    geom_rect(aes(xmin=0, xmax=80, ymin=0, ymax=80),fill="darkolivegreen1", color="black", alpha=1)+
    geom_rect(aes(xmin=0, xmax=80, ymin=80, ymax=160),fill="darkolivegreen1", color="black", alpha=1)+
    geom_rect(aes(xmin=0, xmax=80, ymin=160, ymax=240),fill="darkolivegreen1", color="black", alpha=1)+
    geom_rect(aes(xmin=0, xmax=80, ymin=240, ymax=320),fill="darkolivegreen1", color="black", alpha=1)+
    geom_rect(aes(xmin=0, xmax=80, ymin=320, ymax=400),fill="chartreuse2", color="black", alpha=1)+
    geom_rect(aes(xmin=80, xmax=160, ymin=0, ymax=80),fill="darkolivegreen1", color="black", alpha=1)+
    geom_rect(aes(xmin=80, xmax=160, ymin=80, ymax=160),fill="darkolivegreen1", color="black", alpha=1)+
    geom_rect(aes(xmin=80, xmax=160, ymin=160, ymax=240),fill="chartreuse2", color="black", alpha=1)+
    geom_rect(aes(xmin=80, xmax=160, ymin=240, ymax=320),fill="chartreuse2", color="black", alpha=1)+
    geom_rect(aes(xmin=80, xmax=160, ymin=320, ymax=400),fill="chartreuse2", color="black", alpha=1)+
    geom_rect(aes(xmin=160, xmax=240, ymin=0, ymax=80),fill="darkolivegreen1", color="black", alpha=1)+
    geom_rect(aes(xmin=160, xmax=240, ymin=80, ymax=160),fill="chartreuse2", color="black", alpha=1)+
    geom_rect(aes(xmin=160, xmax=240, ymin=160, ymax=240),fill="chartreuse2", color="black", alpha=1)+
    geom_rect(aes(xmin=160, xmax=240, ymin=240, ymax=320),fill="chartreuse2", color="black", alpha=1)+
    geom_rect(aes(xmin=160, xmax=240, ymin=320, ymax=400),fill="chartreuse3", color="black", alpha=1)+
    geom_rect(aes(xmin=240, xmax=320, ymin=0, ymax=80),fill="chartreuse2", color="black", alpha=1)+
    geom_rect(aes(xmin=240, xmax=320, ymin=80, ymax=160),fill="chartreuse2", color="black", alpha=1)+
    geom_rect(aes(xmin=240, xmax=320, ymin=160, ymax=240),fill="chartreuse2", color="black", alpha=1)+
    geom_rect(aes(xmin=240, xmax=320, ymin=240, ymax=320),fill="chartreuse3", color="black", alpha=1)+
    geom_rect(aes(xmin=240, xmax=320, ymin=320, ymax=400),fill="chartreuse3", color="black", alpha=1)+
    geom_rect(aes(xmin=320, xmax=400, ymin=0, ymax=80),fill="chartreuse2", color="black", alpha=1)+
    geom_rect(aes(xmin=320, xmax=400, ymin=80, ymax=160),fill="chartreuse3", color="black", alpha=1)+
    geom_rect(aes(xmin=320, xmax=400, ymin=160, ymax=240),fill="chartreuse3", color="black", alpha=1)+
    geom_rect(aes(xmin=320, xmax=400, ymin=240, ymax=320),fill="chartreuse3", color="black", alpha=1)+
    geom_rect(aes(xmin=320, xmax=400, ymin=320, ymax=400),fill="chartreuse3", color="black", alpha=1)+
    
    geom_text(aes(x = 40, y = 40, label = classicgrid[1]),size = 10)+
    geom_text(aes(x = 40, y = 120, label = classicgrid[2]),size = 10)+
    geom_text(aes(x = 40, y = 200, label = classicgrid[3]),size = 10)+
    geom_text(aes(x = 40, y = 280, label = classicgrid[4]),size = 10)+
    geom_text(aes(x = 40, y = 360, label = classicgrid[5]),size = 10)+
    
    geom_text(aes(x = 120, y = 40, label = classicgrid[6]),size = 10)+
    geom_text(aes(x = 120, y = 120, label = classicgrid[7]),size = 10)+
    geom_text(aes(x = 120, y = 200, label = classicgrid[8]),size = 10)+
    geom_text(aes(x = 120, y = 280, label = classicgrid[9]),size = 10)+
    geom_text(aes(x = 120, y = 360, label = classicgrid[10]),size = 10)+
    
    geom_text(aes(x = 200, y = 40, label = classicgrid[11]),size = 10)+
    geom_text(aes(x = 200, y = 120, label = classicgrid[12]),size = 10)+
    geom_text(aes(x = 200, y = 200, label = classicgrid[13]),size = 10)+
    geom_text(aes(x = 200, y = 280, label = classicgrid[14]),size = 10)+
    geom_text(aes(x = 200, y = 360, label = classicgrid[15]),size = 10)+
    
    geom_text(aes(x = 280, y = 40, label = classicgrid[16]),size = 10)+
    geom_text(aes(x = 280, y = 120, label = classicgrid[17]),size = 10)+
    geom_text(aes(x = 280, y = 200, label = classicgrid[18]),size = 10)+
    geom_text(aes(x = 280, y = 280, label = classicgrid[19]),size = 10)+
    geom_text(aes(x = 280, y = 360, label = classicgrid[20]),size = 10)+
    
    geom_text(aes(x = 360, y = 40, label = classicgrid[21]),size = 10)+
    geom_text(aes(x = 360, y = 120, label = classicgrid[22]),size = 10)+
    geom_text(aes(x = 360, y = 200, label = classicgrid[23]),size = 10)+
    geom_text(aes(x = 360, y = 280, label = classicgrid[24]),size = 10)+
    geom_text(aes(x = 360, y = 360, label = classicgrid[25]),size = 10)+
    
    geom_text(aes(x = -50, y = 40, label = "unlikely"),size = 5)+
    geom_text(aes(x = -50, y = 120, label = "very low"),size = 5)+
    geom_text(aes(x = -50, y = 200, label = "low"),size = 5)+
    geom_text(aes(x = -50, y = 280, label = "high"),size = 5)+
    geom_text(aes(x = -50, y = 360, label = "very high"),size = 5)+
    
    geom_text(aes(x = 40 , y = 440, label = "insignificant"),size = 5)+
    geom_text(aes(x = 120, y = 440, label = "low"),size = 5)+
    geom_text(aes(x = 200, y = 440, label = "noticeable"),size = 5)+
    geom_text(aes(x = 280, y = 440, label = "positive"),size = 5)+
    geom_text(aes(x = 360, y = 440, label = "significant"),size = 5)+
    
    geom_text(aes(x = 200, y = 505, label = "Potential"),size = 5)+
    geom_text(aes(x = -125, y = 200, label = "Probability of occurrence"),size = 5,angle=90)#+
  
  
}  
  #heme_classic() +
  #theme(axis.line  = element_blank(),
  #      axis.ticks = element_blank(),
  #      axis.text  = element_blank(),
  #      axis.title = element_blank())


print(thisis)
scenfile <- (paste0("myapp/pictures/1_classicgrid_otitle/", scentext,"_",actualtype,".png"))  

ggsave(filename = scenfile, device = "png", width = 25, height = 25, units = "cm",limitsize = FALSE)
}


## set up the plot region:
#,
     #main = "2 x 2 rectangles; `rect(0,400,0,400)'")
#i <- 4*(0:10)
## draw rectangles with bottom left (100, 300)+i  and top right (150, 380)+i




#rect(0, 0, 300, 300,col= rgb(0,0,1.0,alpha=0.2),border = "blue")
#rect(200, 200, 201, 201,col= rgb(1.0,1.0,0,alpha=0.7))
#draw.circle(x=300, y=80, radius=50, col=heat.colors(9))
#rect(100, 300, 250, 430,col= rgb(1.0,1.0,0,alpha=0.1))
#rect(240-i, 320+i, 250-i, 410+i, col=heat.colors(9))

#Beispiel um einen Text zu platzieren
#center <- c(mean(c(0, 40)), mean(c(0, 35)))
#text(center[1], center[2], labels = 'Hier kann auch dein Text stehen')