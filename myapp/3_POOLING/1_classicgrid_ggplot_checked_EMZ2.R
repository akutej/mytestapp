#Erstellung der Klassischen Risikomatrix für alle Szenarien 
#getrennt nach Risiko und Chance inklusive Verspeicherung der einzelnen Bilder

library(plotrix)
library(dplyr)
library(ggplot2)
library(grid)
library(RColorBrewer)
library(readxl)
library(officer)
library(flextable)
library(openxlsx)

df_summary <- data.frame(
  Mean_impact = numeric(),           # numerisch
  Mean_Occurrence = numeric(),       # numerisch
  Mean_RPZ = numeric(),       # numerisch
  Scenario_number = character(),     # Text oder ID
  Scenario_text = character(),       # Freitext
  Scenario_type = character(),       # Freitext
  Scenario_KBID = character(),       # Freitext
  stringsAsFactors = FALSE           # damit keine Faktoren entstehen
)

# Beispiel: Einlesen der Datei
answerstable <- read_excel("myapp/EMZ2/PMPL_correct.xlsx")

#answerstable <- read.csv(file = 'myapp/EMZ/raw/correct_new.csv', header=TRUE)
                     

dfall <- answerstable %>% filter(ANS2SURV_METHOD == "classic")
scenarios <- as.data.frame(table(dfall$QUES_ID))
#print(scenarios)
numberscenarios  <- nrow(scenarios)
#print (numberscenarios)

for (anz in 1:numberscenarios) {
actualscenario =as.vector(scenarios[anz,1])
#actualType =as.vector(scenarios[anz,1])
scentext <- (paste0("Scenario ", actualscenario))

df <- answerstable %>% filter(ANS2SURV_METHOD == "classic" & QUES_ID == actualscenario)
print (df)

actualtype <- (df$QUES_TYP[1])
print (actualtype)
numberofanswers <- nrow(df)
print (numberofanswers)

scenariogrid <- data.frame(table(df$ClassicGrid))
basicgrid <- data.frame(c("GRID11","GRID21","GRID31","GRID41","GRID51","GRID12","GRID22","GRID32","GRID42","GRID52","GRID13","GRID23","GRID33","GRID43","GRID53","GRID14","GRID24","GRID34","GRID44","GRID54","GRID15","GRID25","GRID35","GRID45","GRID55"))

numberbasic  <- nrow(basicgrid)
export_data <- answerstable[, c("QUES_ID", "ClassicGrid", "IMPACT")]

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
avg_impact <- mean(df$IMPACT, na.rm = TRUE)
avg_occurrence <- mean(df$OCCURRENCE, na.rm = TRUE)

# Werte auf ganze Zahlen runden (Matrix ist diskret)
avg_impact_round <- round(avg_impact)
avg_occurrence_round <- round(avg_occurrence)

df_summary <- rbind(df_summary, data.frame(
  Mean_impact = avg_impact_round,
  Mean_Occurrence = avg_occurrence_round,
  Mean_RPZ = (avg_impact_round * avg_occurrence_round),
  Scenario_number = actualscenario,
  Scenario_text = df$QUES_TEXT[1],
  Scenario_type = actualtype,
  Scenario_KBID = df$QUES2SURV_KBID[1],
  stringsAsFactors = FALSE
))


# Position in Plot-Koordinaten umrechnen
x_pos <- (avg_impact_round - 1) * 80 + 40  # 1=unbedeutend, 5=katastrophal
y_pos <- (avg_occurrence_round - 1) * 80 + 40  # 1=unwahrscheinlich, 5=sehr hoch




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
  geom_rect(aes(xmin = x_pos - 40, xmax = x_pos + 40, ymin = y_pos - 40, ymax = y_pos + 40), 
            fill = "blue", alpha = 0.2, color = "blue", size = 1.5)+
  
  
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
  
  geom_text(aes(x = -50, y = 40, label = "unwahrscheinlich"),size = 5)+
  geom_text(aes(x = -50, y = 120, label = "sehr gering"),size = 5)+
  geom_text(aes(x = -50, y = 200, label = "gering"),size = 5)+
  geom_text(aes(x = -50, y = 280, label = "hoch"),size = 5)+
  geom_text(aes(x = -50, y = 360, label = "sehr hoch"),size = 5)+
  
  geom_text(aes(x = 40 , y = 440, label = "unbedeutend"),size = 5)+
  geom_text(aes(x = 120, y = 440, label = "gering"),size = 5)+
  geom_text(aes(x = 200, y = 440, label = "spürbar"),size = 5)+
  geom_text(aes(x = 280, y = 440, label = "kritisch"),size = 5)+
  geom_text(aes(x = 360, y = 440, label = "katastrophal"),size = 5)+
  
  geom_text(aes(x = 200, y = 505, label = "Auswirkung"),size = 5)+
  geom_text(aes(x = -125, y = 200, label = "Eintrittswahrscheinlichkeit"),size = 5,angle=90)#+
  
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
    geom_rect(aes(xmin = x_pos - 40, xmax = x_pos + 40, ymin = y_pos - 40, ymax = y_pos + 40), 
              fill = "blue", alpha = 0.2, color = "blue", size = 1.5)+
    
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
    
    geom_text(aes(x = -50, y = 40, label = "unwahrscheinlich"),size = 5)+
    geom_text(aes(x = -50, y = 120, label = "sehr gering"),size = 5)+
    geom_text(aes(x = -50, y = 200, label = "gering"),size = 5)+
    geom_text(aes(x = -50, y = 280, label = "hoch"),size = 5)+
    geom_text(aes(x = -50, y = 360, label = "sehr hoch"),size = 5)+
    
    geom_text(aes(x = 40 , y = 440, label = "unbedeutend"),size = 5)+
    geom_text(aes(x = 120, y = 440, label = "gering"),size = 5)+
    geom_text(aes(x = 200, y = 440, label = "spürbar"),size = 5)+
    geom_text(aes(x = 280, y = 440, label = "positiv"),size = 5)+
    geom_text(aes(x = 360, y = 440, label = "bedeutend"),size = 5)+
    
    geom_text(aes(x = 200, y = 505, label = "Potential"),size = 5)+
    geom_text(aes(x = -125, y = 200, label = "Eintrittswahrscheinlichkeit"),size = 5,angle=90)#+
  
  
}  
 

print(thisis)
scenfile <- (paste0("myapp/EMZ2/answers/", scentext,"_",actualtype,".png"))  

ggsave(filename = scenfile, device = "png", width = 25, height = 25, units = "cm",limitsize = FALSE)
}


df_sorted <- df_summary %>%
  arrange(Scenario_type, desc(Mean_RPZ))

print (df_sorted)


doc <- read_docx()
for (i in 1:nrow(df_sorted)) {
  scenario <- df_sorted[i, ]
  
  # Bildpfad konstruieren
  image_file <- paste0("myapp/EMZ2/answers/Scenario ", scenario$Scenario_number, "_", scenario$Scenario_type, ".png")
  
  textblock <- paste0(
    "Szenario ", scenario$Scenario_number, "\n",
    "Beschreibung: ", scenario$Scenario_text, "\n",
    "Auswirkung: ", scenario$Mean_impact, "\n",
    "Eintritt: ", scenario$Mean_Occurrence, "\n",
    "RPZ: ", scenario$Mean_RPZ, "\n",
    "Typ: ", scenario$Scenario_type, "\n",
    "KBID: ", scenario$Scenario_KBID
  )
  
  
  # Abschnitt einfügen
  doc <- doc %>%
    body_add_par(paste0("Szenario ", scenario$Scenario_number), style = "heading 2") %>%
    body_add_par(paste("Beschreibung:", scenario$Scenario_text), style = "Normal") %>%
    body_add_par(paste("Auswirkung:", scenario$Mean_impact), style = "Normal") %>%
    body_add_par(paste("Eintritt:", scenario$Mean_Occurrence), style = "Normal") %>%
    body_add_par(paste("RPZ:", scenario$Mean_RPZ), style = "Normal") %>%
    body_add_par(paste("Typ:", scenario$Scenario_type), style = "Normal") %>%
    body_add_par(paste("KBID:", scenario$Scenario_KBID), style = "Normal")
  
  # Bild einfügen (wenn Datei existiert)
  if (file.exists(image_file)) {
    doc <- doc %>%
      body_add_img(src = image_file, width = 5, height = 5, style = "centered")
  } else {
    doc <- doc %>%
      body_add_par("(Bild nicht gefunden)", style = "Normal")
  }
  
  # Leerzeile für Abstand
  doc <- doc %>% body_add_par(" ", style = "Normal")
}
print(doc, target = "myapp/EMZ2/answers/risikoanalyse_bericht.docx")

wb <- createWorkbook()
addWorksheet(wb, "Risiken")
writeData(wb, sheet = "Risiken", x = df_sorted)
saveWorkbook(wb, file = "myapp/EMZ2/answers/auswertung_risiken_sortiert.xlsx", overwrite = TRUE)
