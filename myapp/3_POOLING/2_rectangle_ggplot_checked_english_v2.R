#Erstellung der graphischen Risikomatrix für alle Szenarien getrennt nach Risiko und Chance 
#Mittelpunkte und Rechtecke mit Mittelpunkten inklusive Verspeicherung der einzelnen Bilder

library(plotrix)
library(dplyr)
library(ggplot2)
library(grid)
library(RColorBrewer)




answerstable <- read.csv(file = 'myapp/data/RQ1_corrected_scaled.csv', header=TRUE) #importiere das answers file

dfall <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)
scenarios <- as.data.frame(table(dfall$QUES_ID))
numberscenarios  <- nrow(scenarios)
for (anz in 1:numberscenarios) {
  actualscenario =as.vector(scenarios[anz,1])
  scentext <- (paste0("Scenario ", actualscenario))
  df <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == actualscenario)
  numberofanswers <- nrow(df)
  actualtype <- (df$QUES_TYP[1])
#print (numberofanswers)
dfgroup1 <- df %>% filter(ACC2SURV_ROLE == "1")# ACC2SURV_GROUPID== "1"
numberofanswersg1 <- nrow(dfgroup1)
dfgroup2 <- df %>% filter(ACC2SURV_ROLE == "2")
numberofanswersg2 <- nrow(dfgroup2)


#transformedy <- getswitchedy(0)
#print (transformedy)
#print (dfgroup2)

x1g1 <- c()
x2g1 <- c()
y1g1 <- c()
y2g1 <- c()

x1g1m <- c()
x2g1m <- c()
y1g1m <- c()
y2g1m <- c()

x1g2 <- c()
x2g2 <- c()
y1g2 <- c()
y2g2 <- c()

x1g2m <- c()
x2g2m <- c()
y1g2m <- c()
y2g2m <- c()


rectangleg1 <- data.frame(x1g1,
                          x2g1,                             
                          y1g1,
                          y2g1
                         )

rectangleg1m <- data.frame(x1g1m,
                           x2g1m,                             
                           y1g1m,
                           y2g1m
                          )

rectangleg2 <- data.frame(x1g2,
                          x2g2,                             
                          y1g2,
                          y2g2
)

rectangleg2m <- data.frame(x1g2m,
                           x2g2m,                             
                           y1g2m,
                           y2g2m
)




for (i in 1:numberofanswersg1) {
  actualrowg1 <- nrow(rectangleg1) + 1
  actualrowg1m <- nrow(rectangleg1m) + 1
  
  x1 <- dfgroup1[i,"X1Pixel"]
  x2 <- dfgroup1[i,"X2Pixel"]
  y1 <- dfgroup1[i,"Y1Pixel"]
  y2 <- dfgroup1[i,"Y2Pixel"]
  mx= ((x2-x1)/2)+x1
  my= ((y2-y1)/2)+y1
  mx1=mx-1
  mx2=mx+1
  my1=my-1
  my2=my+1
  rectangleg1[actualrowg1,"x1g1"] <- x1
  rectangleg1[actualrowg1,"x2g1"] <- x2
  rectangleg1[actualrowg1,"y1g1"] <- y1
  rectangleg1[actualrowg1,"y2g1"] <- y2
  
  rectangleg1m[actualrowg1m,"x1g1m"] <- mx1
  rectangleg1m[actualrowg1m,"x2g1m"] <- mx2
  rectangleg1m[actualrowg1m,"y1g1m"] <- my1
  rectangleg1m[actualrowg1m,"y2g1m"] <- my2
  
}

for (i in 1:numberofanswersg2) {
  actualrowg2 <- nrow(rectangleg2) + 1
  actualrowg2m <- nrow(rectangleg2m) + 1
  x1 <- dfgroup2[i,"X1Pixel"]
  x2 <- dfgroup2[i,"X2Pixel"]
  y1 <- dfgroup2[i,"Y1Pixel"]
  y2 <- dfgroup2[i,"Y2Pixel"]
  mx= ((x2-x1)/2)+x1
  my= ((y2-y1)/2)+y1
  mx1=mx-1
  mx2=mx+1
  my1=my-1
  my2=my+1
  rectangleg2[actualrowg2,"x1g2"] <- x1
  rectangleg2[actualrowg2,"x2g2"] <- x2
  rectangleg2[actualrowg2,"y1g2"] <- y1
  rectangleg2[actualrowg2,"y2g2"] <- y2
  
  rectangleg2m[actualrowg2m,"x1g2m"] <- mx1
  rectangleg2m[actualrowg2m,"x2g2m"] <- mx2
  rectangleg2m[actualrowg2m,"y1g2m"] <- my1
  rectangleg2m[actualrowg2m,"y2g2m"] <- my2
  
  
}

#print(rectangleg1m)

if (actualtype == "Risiko"){
make_gradient <- function(deg = 180, n = 100, cols = blues9) {
  cols <- colorRampPalette(cols)(n + 1)
  rad <- deg / (62/pi)
  mat <- matrix(
    data = rep(seq(0, 1, length.out = n) * cos(rad), n),
    byrow = TRUE,
    ncol = n
  ) +
    matrix(
      data = rep(seq(0, 1, length.out = n) * sin(rad), n),
      byrow = FALSE,
      ncol = n
    )
  mat <- mat - min(mat)
  mat <- mat / max(mat)
  mat <- 1 + mat * n
  mat <- matrix(data = cols[round(mat)], ncol = n)
  grid::rasterGrob(
    image = mat,
    width = unit(1, "npc"),
    height = unit(1, "npc"), 
    interpolate = TRUE
  )
}
g <- make_gradient(
  deg = 45, n = 500, cols = brewer.pal(3, "Spectral")
)

thisis <- ggplot() + 
  ggtitle(scentext)+
  theme(plot.title = element_blank(),#element_text(hjust = 0.5,color="black", size=16),#face="bold"),
        axis.line  = element_blank(),
        axis.ticks = element_blank(),
        axis.text  = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  annotation_custom(
    #grob = g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
    grob = g, xmin = 0, xmax = 400, ymin = 0, ymax = 400
  ) +
  xlim(-60,400) +
  ylim(0,460) +
  geom_rect(data=rectangleg1, mapping=aes(xmin=x1g1, xmax=x2g1, ymin=y1g1, ymax=y2g1), color="red", alpha=0)+
  #geom_rect(data=rectangleg1m, mapping=aes(xmin=x1g1m, xmax=x2g1m, ymin=y1g1m, ymax=y2g1m),fill = "red", color="red", alpha=1)+
  geom_rect(data=rectangleg2, mapping=aes(xmin=x1g2, xmax=x2g2, ymin=y1g2, ymax=y2g2), color="blue", alpha=0)+
  #geom_rect(data=rectangleg2m, mapping=aes(xmin=x1g2m, xmax=x2g2m, ymin=y1g2m, ymax=y2g2m),fill = "blue", color="blue", alpha=1)+
  geom_rect(aes(xmin=0, xmax=400, ymin=400, ymax=430),fill="lightgrey", color=NA, alpha=1)+
  geom_rect(aes(xmin=0, xmax=400, ymin=430, ymax=460),fill="grey", color=NA, alpha=1)+
  geom_rect(aes(xmin=-30, xmax=0, ymin=0, ymax=400),fill="lightgrey", color=NA, alpha=1)+
  geom_rect(aes(xmin=-60, xmax=-30, ymin=0, ymax=400),fill="grey", color=NA, alpha=1)+
  geom_text(aes(x = -15, y = 40, label = "low"),size = 5,angle=90)+
  geom_text(aes(x = -15, y = 200, label = "medium"),size = 5,angle=90)+
  geom_text(aes(x = -15, y = 360, label = "high"),size = 5,angle=90)+
  geom_text(aes(x = 40, y = 415, label = "low"),size = 5)+
  geom_text(aes(x = 200, y = 415, label = "medium"),size = 5)+
  geom_text(aes(x = 360, y = 415, label = "high"),size = 5)+
  geom_text(aes(x = 200, y = 445, label = "Impact"),size = 5)+
  geom_text(aes(x = -45, y = 200, label = "Probability of occurrence"),size = 5,angle=90)#+




print (thisis)
scenfile <- (paste0("myapp/pictures/2_graphicrectangle_eot/", scentext,"_rectangle.png"))  
#scenfile <- (paste0("myapp/pictures/2_graphicrectangle_eot/", scentext,"_rectangle_",actualtype,".png"))  
ggsave(filename = scenfile, device = "png", width = 25, height = 25, units = "cm",limitsize = FALSE)

thisis2 <- ggplot() + 
  ggtitle(scentext)+
  theme(plot.title = element_blank(),#element_text(hjust = 0.5,color="black", size=16),#face="bold"),
        axis.line  = element_blank(),
        axis.ticks = element_blank(),
        axis.text  = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  annotation_custom(
    grob = g, xmin = 0, xmax = 400, ymin = 0, ymax = 400
  ) +
  xlim(-60,400) +
  ylim(0,460) +
  #geom_rect(data=rectangleg1, mapping=aes(xmin=x1g1, xmax=x2g1, ymin=y1g1, ymax=y2g1), color="red", alpha=0)+
  geom_rect(data=rectangleg1m, mapping=aes(xmin=x1g1m, xmax=x2g1m, ymin=y1g1m, ymax=y2g1m),fill = "red", color="red", alpha=1)+
  #geom_rect(data=rectangleg2, mapping=aes(xmin=x1g2, xmax=x2g2, ymin=y1g2, ymax=y2g2), color="blue", alpha=0)+
  geom_rect(data=rectangleg2m, mapping=aes(xmin=x1g2m, xmax=x2g2m, ymin=y1g2m, ymax=y2g2m),fill = "blue", color="blue", alpha=1)+
  geom_rect(aes(xmin=0, xmax=400, ymin=400, ymax=430),fill="lightgrey", color=NA, alpha=1)+
  geom_rect(aes(xmin=0, xmax=400, ymin=430, ymax=460),fill="grey", color=NA, alpha=1)+
  geom_rect(aes(xmin=-30, xmax=0, ymin=0, ymax=400),fill="lightgrey", color=NA, alpha=1)+
  geom_rect(aes(xmin=-60, xmax=-30, ymin=0, ymax=400),fill="grey", color=NA, alpha=1)+
  geom_text(aes(x = -15, y = 40, label = "low"),size = 5,angle=90)+
  geom_text(aes(x = -15, y = 200, label = "medium"),size = 5,angle=90)+
  geom_text(aes(x = -15, y = 360, label = "high"),size = 5,angle=90)+
  geom_text(aes(x = 40, y = 415, label = "low"),size = 5)+
  geom_text(aes(x = 200, y = 415, label = "medium"),size = 5)+
  geom_text(aes(x = 360, y = 415, label = "high"),size = 5)+
  geom_text(aes(x = 200, y = 445, label = "Impact"),size = 5)+
  geom_text(aes(x = -45, y = 200, label = "Probability of occurrence"),size = 5,angle=90)#+
  #geom_text(aes(x = 460, y = 215, label = "coreteam-members"),size = 3)+
  #geom_text(aes(x = 460, y = 185, label = "none coreteam-members"),size = 3)


print (thisis2)
#scenfile2 <- (paste0("myapp/pictures/2_graphicrectangle_eot/", scentext,"_onlycenter_",actualtype,".png"))  
scenfile2 <- (paste0("myapp/pictures/2_graphicrectangle_eot/", scentext,"_onlycenter.png")) 
ggsave(filename = scenfile2, device = "png", width = 25, height = 25, units = "cm",limitsize = FALSE)

}
else{

make_gradient <- function(deg = 180, n = 100, cols = blues9) {
  cols <- colorRampPalette(cols)(n + 1)
  rad <- deg / (12/pi)
  mat <- matrix(
    data = rep(seq(0, 1, length.out = n) * cos(rad), n),
    byrow = TRUE,
    ncol = n
  ) +
    matrix(
      data = rep(seq(0, 1, length.out = n) * sin(rad), n),
      byrow = FALSE,
      ncol = n
    )
  mat <- mat - min(mat)
  mat <- mat / max(mat)
  mat <- 1 + mat * n
  mat <- matrix(data = cols[round(mat)], ncol = n)
  grid::rasterGrob(
    image = mat,
    width = unit(1, "npc"),
    height = unit(1, "npc"), 
    interpolate = TRUE
  )
}
g <- make_gradient(
  deg = 45, n = 500, cols = brewer.pal(3, "Greens")
)

thisis <- ggplot() + 
  ggtitle(scentext)+
  theme(plot.title = element_blank(),#element_text(hjust = 0.5,color="black", size=16),#face="bold"),
        axis.line  = element_blank(),
        axis.ticks = element_blank(),
        axis.text  = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  annotation_custom(
    #grob = g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
    grob = g, xmin = 0, xmax = 400, ymin = 0, ymax = 400
  ) +
  xlim(-60,400) +
  ylim(0,460) +
  geom_rect(data=rectangleg1, mapping=aes(xmin=x1g1, xmax=x2g1, ymin=y1g1, ymax=y2g1), color="red", alpha=0)+
  #geom_rect(data=rectangleg1m, mapping=aes(xmin=x1g1m, xmax=x2g1m, ymin=y1g1m, ymax=y2g1m),fill = "red", color="red", alpha=1)+
  geom_rect(data=rectangleg2, mapping=aes(xmin=x1g2, xmax=x2g2, ymin=y1g2, ymax=y2g2), color="blue", alpha=0)+
  #geom_rect(data=rectangleg2m, mapping=aes(xmin=x1g2m, xmax=x2g2m, ymin=y1g2m, ymax=y2g2m),fill = "blue", color="blue", alpha=1)+
  geom_rect(aes(xmin=0, xmax=400, ymin=400, ymax=430),fill="lightgrey", color=NA, alpha=1)+
  geom_rect(aes(xmin=0, xmax=400, ymin=430, ymax=460),fill="grey", color=NA, alpha=1)+
  geom_rect(aes(xmin=-30, xmax=0, ymin=0, ymax=400),fill="lightgrey", color=NA, alpha=1)+
  geom_rect(aes(xmin=-60, xmax=-30, ymin=0, ymax=400),fill="grey", color=NA, alpha=1)+
  geom_text(aes(x = -15, y = 40, label = "low"),size = 5,angle=90)+
  geom_text(aes(x = -15, y = 200, label = "medium"),size = 5,angle=90)+
  geom_text(aes(x = -15, y = 360, label = "high"),size = 5,angle=90)+
  geom_text(aes(x = 40, y = 415, label = "low"),size = 5)+
  geom_text(aes(x = 200, y = 415, label = "medium"),size = 5)+
  geom_text(aes(x = 360, y = 415, label = "high"),size = 5)+
  geom_text(aes(x = 200, y = 445, label = "Potential"),size = 5)+
  geom_text(aes(x = -45, y = 200, label = "Probability of occurrence"),size = 5,angle=90)#+




print (thisis)
#scenfile <- (paste0("myapp/pictures/2_graphicrectangle_eot/", scentext,"_rectangle_",actualtype,".png"))  
scenfile <- (paste0("myapp/pictures/2_graphicrectangle_eot/", scentext,"_rectangle.png"))  

ggsave(filename = scenfile, device = "png", width = 25, height = 25, units = "cm",limitsize = FALSE)

thisis2 <- ggplot() + 
  ggtitle(scentext)+
  theme(plot.title = element_blank(),#element_text(hjust = 0.5,color="black", size=16),#face="bold"),
        axis.line  = element_blank(),
        axis.ticks = element_blank(),
        axis.text  = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )+
  annotation_custom(
    grob = g, xmin = 0, xmax = 400, ymin = 0, ymax = 400
  ) +
  xlim(-60,400) +
  ylim(0,460) +
  #geom_rect(data=rectangleg1, mapping=aes(xmin=x1g1, xmax=x2g1, ymin=y1g1, ymax=y2g1), color="red", alpha=0)+
  geom_rect(data=rectangleg1m, mapping=aes(xmin=x1g1m, xmax=x2g1m, ymin=y1g1m, ymax=y2g1m),fill = "red", color="red", alpha=1)+
  #geom_rect(data=rectangleg2, mapping=aes(xmin=x1g2, xmax=x2g2, ymin=y1g2, ymax=y2g2), color="blue", alpha=0)+
  geom_rect(data=rectangleg2m, mapping=aes(xmin=x1g2m, xmax=x2g2m, ymin=y1g2m, ymax=y2g2m),fill = "blue", color="blue", alpha=1)+
  geom_rect(aes(xmin=0, xmax=400, ymin=400, ymax=430),fill="lightgrey", color=NA, alpha=1)+
  geom_rect(aes(xmin=0, xmax=400, ymin=430, ymax=460),fill="grey", color=NA, alpha=1)+
  geom_rect(aes(xmin=-30, xmax=0, ymin=0, ymax=400),fill="lightgrey", color=NA, alpha=1)+
  geom_rect(aes(xmin=-60, xmax=-30, ymin=0, ymax=400),fill="grey", color=NA, alpha=1)+
  geom_text(aes(x = -15, y = 40, label = "low"),size = 5,angle=90)+
  geom_text(aes(x = -15, y = 200, label = "medium"),size = 5,angle=90)+
  geom_text(aes(x = -15, y = 360, label = "high"),size = 5,angle=90)+
  geom_text(aes(x = 40, y = 415, label = "low"),size = 5)+
  geom_text(aes(x = 200, y = 415, label = "medium"),size = 5)+
  geom_text(aes(x = 360, y = 415, label = "high"),size = 5)+
  geom_text(aes(x = 200, y = 445, label = "Potential"),size = 5)+
  geom_text(aes(x = -45, y = 200, label = "Probability of occurrence"),size = 5,angle=90)


print (thisis2)
#scenfile2 <- (paste0("myapp/pictures/2_graphicrectangle_eot/", scentext,"_onlycenter_",actualtype,".png"))
scenfile2 <- (paste0("myapp/pictures/2_graphicrectangle_eot/", scentext,"_onlycenter.png"))  
ggsave(filename = scenfile2, device = "png", width = 25, height = 25, units = "cm",limitsize = FALSE)

}

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