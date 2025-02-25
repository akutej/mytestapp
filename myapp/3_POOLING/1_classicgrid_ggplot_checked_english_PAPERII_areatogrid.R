#Erstellung der Klassischen Risikomatrix für alle Szenarien 
#getrennt nach Risiko und Chance inklusive Verspeicherung der einzelnen Bilder

library(plotrix)
library(dplyr)
library(ggplot2)
library(grid)
library(RColorBrewer)

answerstable <- read.csv(file = 'myapp/Data/RQ1_corrected_scaled.csv', header=TRUE) #importiere das answers file
answerstable <- answerstable %>% filter(QUES_ID == "359")
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
  xlim(-100,400)+
  ylim(0,500)+
  geom_rect(aes(xmin=-50, xmax=0, ymin=0, ymax=80),fill="lightgrey", color="black", alpha=1)+
  geom_rect(aes(xmin=-50, xmax=0, ymin=80, ymax=160),fill="lightgrey", color="black", alpha=1)+
  geom_rect(aes(xmin=-50, xmax=0, ymin=160, ymax=240),fill="lightgrey", color="black", alpha=1)+
  geom_rect(aes(xmin=-50, xmax=0, ymin=240, ymax=320),fill="lightgrey", color="black", alpha=1)+
  geom_rect(aes(xmin=-50, xmax=0, ymin=320, ymax=400),fill="lightgrey", color="black", alpha=1)+
  geom_rect(aes(xmin=-100, xmax=-50, ymin=0, ymax=400),fill="grey", color="black", alpha=1)+
  geom_rect(aes(xmin=0, xmax=80, ymin=400, ymax=450),fill="lightgrey", color="black", alpha=1)+
  geom_rect(aes(xmin=80, xmax=160, ymin=400, ymax=450),fill="lightgrey", color="black", alpha=1)+
  geom_rect(aes(xmin=160, xmax=240, ymin=400, ymax=450),fill="lightgrey", color="black", alpha=1)+
  geom_rect(aes(xmin=240, xmax=320, ymin=400, ymax=450),fill="lightgrey", color="black", alpha=1)+
  geom_rect(aes(xmin=320, xmax=400, ymin=400, ymax=450),fill="lightgrey", color="black", alpha=1)+
  geom_rect(aes(xmin=0, xmax=400, ymin=450, ymax=500),fill="grey", color="black", alpha=1)+
  
  geom_rect(aes(xmin=0, xmax=80, ymin=0, ymax=80),fill="#FFFFC8", color="black", alpha=1)+
  geom_rect(aes(xmin=0, xmax=80, ymin=80, ymax=160),fill="#FFFFC8", color="black", alpha=1)+
  geom_rect(aes(xmin=0, xmax=80, ymin=160, ymax=240),fill="#FFFFC8", color="black", alpha=1)+
  geom_rect(aes(xmin=0, xmax=80, ymin=240, ymax=320),fill="#FFFFC8", color="black", alpha=1)+
  geom_rect(aes(xmin=0, xmax=80, ymin=320, ymax=400),fill="#FFFFC8", color="black", alpha=1)+ #1.1
  
  geom_rect(aes(xmin=80, xmax=160, ymin=0, ymax=80),fill="#FFFFC8", color="black", alpha=1)+
  geom_rect(aes(xmin=80, xmax=160, ymin=80, ymax=160),fill="#F8D074", color="black", alpha=1)+
  geom_rect(aes(xmin=80, xmax=160, ymin=160, ymax=240),fill="#F5A100", color="black", alpha=1)+
  geom_rect(aes(xmin=80, xmax=160, ymin=240, ymax=320),fill="#F7BA3C", color="black", alpha=1)+
  geom_rect(aes(xmin=80, xmax=160, ymin=320, ymax=400),fill="#FFFFC8", color="black", alpha=1)+ #1.2
  
  geom_rect(aes(xmin=160, xmax=240, ymin=0, ymax=80),fill="#FFF4B7", color="black", alpha=1)+
  geom_rect(aes(xmin=160, xmax=240, ymin=80, ymax=160),fill="#ED6200", color="black", alpha=1)+
  geom_rect(aes(xmin=160, xmax=240, ymin=160, ymax=240),fill="#7D0025", color="black", alpha=1)+
  geom_rect(aes(xmin=160, xmax=240, ymin=240, ymax=320),fill="#ED6200", color="black", alpha=1)+
  geom_rect(aes(xmin=160, xmax=240, ymin=320, ymax=400),fill="#FBE49A", color="black", alpha=1)+ #1.3
  
  geom_rect(aes(xmin=240, xmax=320, ymin=0, ymax=80),fill="#FBE49A", color="black", alpha=1)+
  geom_rect(aes(xmin=240, xmax=320, ymin=80, ymax=160),fill="#E13C00", color="black", alpha=1)+
  geom_rect(aes(xmin=240, xmax=320, ymin=160, ymax=240),fill="#7D0025", color="black", alpha=1)+
  geom_rect(aes(xmin=240, xmax=320, ymin=240, ymax=320),fill="#7D0025", color="black", alpha=1)+
  geom_rect(aes(xmin=240, xmax=320, ymin=320, ymax=400),fill="#F28400", color="black", alpha=1)+ #1.4
  
  geom_rect(aes(xmin=320, xmax=400, ymin=0, ymax=80),fill="#FFF4B7", color="black", alpha=1)+
  geom_rect(aes(xmin=320, xmax=400, ymin=80, ymax=160),fill="#F5A100", color="black", alpha=1)+
  geom_rect(aes(xmin=320, xmax=400, ymin=160, ymax=240),fill="#C32200", color="black", alpha=1)+
  geom_rect(aes(xmin=320, xmax=400, ymin=240, ymax=320),fill="#A20706", color="black", alpha=1)+
  geom_rect(aes(xmin=320, xmax=400, ymin=320, ymax=400),fill="#F28400", color="black", alpha=1)+ #1.8
  

  
  geom_text(aes(x = -25, y = 40, label = "un-\nlikely"),size = 7)+
  geom_text(aes(x = -25, y = 120, label = "very\nlow"),size = 7)+
  geom_text(aes(x = -25, y = 200, label = "low"),size = 7)+
  geom_text(aes(x = -25, y = 280, label = "high"),size = 7)+
  geom_text(aes(x = -25, y = 360, label = "very\nhigh"),size = 7)+
  
  geom_text(aes(x = 40 , y = 425, label = "insig-\nnificant"),size = 7)+
  geom_text(aes(x = 120, y = 425, label = "low"),size = 7)+
  geom_text(aes(x = 200, y = 425, label = "notice-\nable"),size = 7)+
  geom_text(aes(x = 280, y = 425, label = "critical"),size = 7)+
  geom_text(aes(x = 360, y = 425, label = "catastro-\nphic"),size = 7)+
  
  geom_text(aes(x = 200, y = 475, label = "IMPACT"),size = 10)+
  geom_text(aes(x = -75, y = 200, label = "PROBABILITY OF OCCURRENCE"),size = 10,angle=90)#+
  
}

else
{

  
}  

#print(thisis)
#thisis <- thisis +  theme_void() +  theme(plot.margin = unit(c(-1.35,-1,-0.6,-1), "cm"))+ ggtitle("")
scenfile <- (paste0("myapp/pictures/paper2/", scentext,"_heat_areatogrid",".png"))  

ggsave(filename = scenfile,plot = thisis, device = "png", width = 23, height = 23, units = "cm",dpi = 600, limitsize = FALSE,bg = "transparent")
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