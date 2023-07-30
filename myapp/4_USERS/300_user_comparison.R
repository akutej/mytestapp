
library(plotrix)
library(dplyr)
library(ggplot2)
library(grid)
library(RColorBrewer)
library(openxlsx)
library(gplots)
library(tibble)

# Funktion um die Werte in einer .tex Datei zu speichern
save_to_latex <- function(AccID,USERID,group,answers,filename,c_median_x,c_sd_x,c_mean_x,c_median_y,c_sd_y,c_mean_y,g_median_x,g_sd_x,g_mean_x,g_median_y,g_sd_y,g_mean_y){
  sink(filename)
  actualscenario <- gsub(" ", "", actualscenario)
  cat(paste("\\expandafter\\newcommand\\csname ",AccID,"_group","\\endcsname{",group, "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",AccID,"_USERID","\\endcsname{",USERID, "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",AccID,"_answers","\\endcsname{",answers, "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",AccID,"_c_median_x","\\endcsname{",round(c_median_x, digits = 2), "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",AccID,"_c_sd_x","\\endcsname{",round(c_sd_x, digits = 2), "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",AccID,"_c_mean_x","\\endcsname{",round(c_mean_x, digits = 2), "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",AccID,"_c_median_y","\\endcsname{",round(c_median_y, digits = 2), "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",AccID,"_c_sd_y","\\endcsname{",round(c_sd_y, digits = 2), "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",AccID,"_c_mean_y","\\endcsname{",round(c_mean_y, digits = 2), "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",AccID,"_g_median_x","\\endcsname{",round(g_median_x, digits = 2), "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",AccID,"_g_sd_x","\\endcsname{",round(g_sd_x, digits = 2), "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",AccID,"_g_mean_x","\\endcsname{",round(g_mean_x, digits = 2), "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",AccID,"_g_median_y","\\endcsname{",round(g_median_y, digits = 2), "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",AccID,"_g_sd_y","\\endcsname{",round(g_sd_y, digits = 2), "}\n", sep=""))
  cat(paste("\\expandafter\\newcommand\\csname ",AccID,"_g_mean_y","\\endcsname{",round(g_mean_y, digits = 2), "}\n", sep=""))
  sink()
}


# Funktion zur Berechnung von statistischen Streumaßen
calculate_metrics <- function(x) {
  list(
    Median = median(x),
    StandardDeviation = sd(x),
    Mean = mean(x),
    FirstQuartile = quantile(x, probs = 0.25, names = FALSE),
    ThirdQuartile = quantile(x, probs = 0.75, names = FALSE),
    InterquartileRange = IQR(x),
    Minimum = min(x),
    Maximum = max(x),
    Range = max(x) - min(x)
  )
}

format_decimal <- function(x) {
  return(sprintf("%.2f", round(x, digits = 2)))
}

output <- data.frame(
  ACCID = character(0),
  USER_ID = character(0),
  group = character(0),
  answers = numeric(0),
  i_Median_classic = numeric(0),
  o_Median_classic = numeric(0),
  i_Mean_classic = numeric(0),
  o_Mean_classic = numeric(0),
  i_SD_classic = numeric(0),
  o_SD_classic = numeric(0),
  i_Median_graphic = numeric(0),
  o_Median_graphic = numeric(0),
  i_Mean_graphic = numeric(0),
  o_Mean_graphic = numeric(0),
  i_SD_graphic = numeric(0),
  o_SD_graphic = numeric(0)
  )

output2 <- data.frame(
  ACCID = character(0),
  group = character(0),
  answers = numeric(0),
  i_Median_classic = numeric(0),
  o_Median_classic = numeric(0),
  i_Mean_classic = numeric(0),
  o_Mean_classic = numeric(0),
  i_SD_classic = numeric(0),
  o_SD_classic = numeric(0),
  i_Median_graphic = numeric(0),
  o_Median_graphic = numeric(0),
  i_Mean_graphic = numeric(0),
  o_Mean_graphic = numeric(0),
  i_SD_graphic = numeric(0),
  o_SD_graphic = numeric(0)
)


answerstable <- read.csv(file = 'myapp/data/RQ1_corrected_scaled.csv', header=TRUE) #importiert das answers file
answerstable <- answerstable %>% filter(QUES_ID != "401" & QUES_ID != "402"& QUES_ID != "403")#Nimmt meine Testdatensätze aus

#answerstable <- answerstable %>% filter(QUES_ID == "281")#Nimmt meine Testdatensätze aus


df.all <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & (ACC2SURV_ROLE  == 1 | ACC2SURV_ROLE  == 2))#filtert die Daten und gibt nur die beantworteten aus #& QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")
df.coreteam <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & ACC2SURV_ROLE  == 1) # nur Kernteam
df.noncoreteam <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & ACC2SURV_ROLE  == 2) # nur nichtKernteam




#print (df.all)
numberofuser.all <- length(unique(df.all$ACC2SURV_ACCID)) #Anzahl der Gesamtuser
numberofuser.coreteam <- length(unique(df.coreteam$ACC2SURV_ACCID))
numberofuser.noncoreteam <- length(unique(df.noncoreteam$ACC2SURV_ACCID))
users <- unique(df.all$ACC2SURV_ACCID)
#print (users)

#as.data.frame(table(df.all$ACC2SURV_ACCID))
#numberofuser.coreteam <- as.data.frame(table(df.coreteam$ACC2SURV_ACCID))
#numberofuser.noncoreteam <- as.data.frame(table(df.coreteam$ACC2SURV_ACCID))

scenarios <- as.data.frame(table(df.all$QUES_ID))
numberscenarios  <- nrow(scenarios)
#print (numberofuser.all)
#print (numberofuser.coreteam)
#print (numberofuser.noncoreteam)

#Erzeugt datenframes die später benötigt werden
x.daten.user <- data.frame(ACCID = integer(), Wert = numeric())
y.daten.user <- data.frame(ACCID = integer(), Wert = numeric())
x.daten.user.all <- data.frame(ACCID = integer(), Wert = numeric())
y.daten.user.all <- data.frame(ACCID = integer(), Wert = numeric())
x.all.user <- data.frame(Wert = numeric())
x.data.overall <- data.frame(Wert = numeric())
y.all.user <- data.frame(Wert = numeric())
y.data.overall <- data.frame(Wert = numeric())
x.coreteam.user <- data.frame(Wert = numeric())
x.data.coreteam <- data.frame(Wert = numeric())
y.coreteam.user <- data.frame(Wert = numeric())
y.data.coreteam <- data.frame(Wert = numeric())
x.noncoreteam.user <- data.frame(Wert = numeric())
x.data.noncoreteam <- data.frame(Wert = numeric())
y.noncoreteam.user <- data.frame(Wert = numeric())
y.data.noncoreteam <- data.frame(Wert = numeric())



#PART FÜR ALLE USER
IMPACT <- df.all[,"scaled_IMPACT"]
OCCURRENCE <- df.all[,"scaled_OCCURRENCE"]
numberofallanswers <- length(df.all$QUES_ID)
print(paste0("Die Gesamtanzahl aller Antworten beträgt: ", numberofallanswers))
for (i in 1:numberofallanswers) {
  x.user.min <- df.all[i,"scaled_X1"]
  x.user.max <- df.all[i,"scaled_X2"]
  y.user.min <- df.all[i,"scaled_Y1"]
  y.user.max <- df.all[i,"scaled_Y2"]
  
  x.user.werte <- seq(x.user.min, x.user.max, by = 0.25)
  x.all.user <- data.frame(Wert = x.user.werte)
  x.data.overall <- rbind(x.data.overall, x.all.user)

  y.user.werte <- seq(y.user.min, y.user.max, by = 0.25)
  y.all.user <- data.frame(Wert = y.user.werte)
  y.data.overall <- rbind(y.data.overall, y.all.user)
}
classicX_metrics <- calculate_metrics(IMPACT)
classicY_metrics <- calculate_metrics(OCCURRENCE)
graphicX_metrics <- calculate_metrics(x.data.overall$Wert)
graphicY_metrics <- calculate_metrics(y.data.overall$Wert)

newdf <- data.frame(
  ACCID = "all",
  USER_ID = "all",
  group = "all",
  answers = numberofallanswers,
  i_Median_classic = round(classicX_metrics$Median, digits = 2),
  o_Median_classic = round(classicY_metrics$Median, digits = 2),
  i_Mean_classic = round(classicX_metrics$Mean, digits = 2),
  o_Mean_classic = round(classicY_metrics$Mean, digits = 2),
  i_SD_classic = round(classicX_metrics$StandardDeviation, digits = 2),
  o_SD_classic = round(classicY_metrics$StandardDeviation, digits = 2),
  i_Median_graphic = round(graphicX_metrics$Median, digits = 2),
  o_Median_graphic = round(graphicY_metrics$Median, digits = 2),
  i_Mean_graphic = round(graphicX_metrics$Mean, digits = 2),
  o_Mean_graphic = round(graphicY_metrics$Mean, digits = 2),
  i_SD_graphic = round(graphicX_metrics$StandardDeviation, digits = 2),
  o_SD_graphic = round(graphicY_metrics$StandardDeviation, digits = 2)
)

output <- rbind(output, newdf)

  scentext <- (paste0("", newdf$ACCID))
  scenfile <- (paste0("myapp/tex/300_users/",scentext,".tex")) 
  
  save_to_latex(newdf$ACCID,
                newdf$USER_ID,
                newdf$group,
                newdf$answers,
                scenfile,
                newdf$i_Median_classic,
                newdf$i_SD_classic,
                newdf$i_Mean_classic,
                newdf$o_Median_classic,
                newdf$o_SD_classic,
                newdf$o_Mean_classic,
                newdf$i_Median_graphic,
                newdf$i_SD_graphic, 
                newdf$i_Mean_graphic,
                newdf$o_Median_graphic,
                newdf$o_SD_graphic,
                newdf$o_Mean_graphic
  )  
  
#PART FÜR CORE TEAM USER
IMPACT <- df.coreteam[,"scaled_IMPACT"]
OCCURRENCE <- df.coreteam[,"scaled_OCCURRENCE"]
numberofcoreteamanswers <- length(df.coreteam$QUES_ID)
print(paste0("Die Gesamtanzahl aller CORETEAM Antworten beträgt: ", numberofcoreteamanswers))
for (i in 1:numberofcoreteamanswers) {
  x.user.min <- df.coreteam[i,"scaled_X1"]
  x.user.max <- df.coreteam[i,"scaled_X2"]
  y.user.min <- df.coreteam[i,"scaled_Y1"]
  y.user.max <- df.coreteam[i,"scaled_Y2"]
  
  x.user.werte <- seq(x.user.min, x.user.max, by = 0.25)
  x.coreteam.user <- data.frame(Wert = x.user.werte)
  x.data.coreteam <- rbind(x.data.coreteam, x.coreteam.user)
  
  y.user.werte <- seq(y.user.min, y.user.max, by = 0.25)
  y.coreteam.user <- data.frame(Wert = y.user.werte)
  y.data.coreteam <- rbind(y.data.coreteam, y.coreteam.user)
  
}
classicX_metrics <- calculate_metrics(IMPACT)
classicY_metrics <- calculate_metrics(OCCURRENCE)
graphicX_metrics <- calculate_metrics(x.data.coreteam$Wert)
graphicY_metrics <- calculate_metrics(y.data.coreteam$Wert)

newdf <- data.frame(
  ACCID = "core team",
  USER_ID = "all",
  group = "core team",
  answers = numberofcoreteamanswers,
  i_Median_classic = round(classicX_metrics$Median, digits = 2),
  o_Median_classic = round(classicY_metrics$Median, digits = 2),
  i_Mean_classic = round(classicX_metrics$Mean, digits = 2),
  o_Mean_classic = round(classicY_metrics$Mean, digits = 2),
  i_SD_classic = round(classicX_metrics$StandardDeviation, digits = 2),
  o_SD_classic = round(classicY_metrics$StandardDeviation, digits = 2),
  i_Median_graphic = round(graphicX_metrics$Median, digits = 2),
  o_Median_graphic = round(graphicY_metrics$Median, digits = 2),
  i_Mean_graphic = round(graphicX_metrics$Mean, digits = 2),
  o_Mean_graphic = round(graphicY_metrics$Mean, digits = 2),
  i_SD_graphic = round(graphicX_metrics$StandardDeviation, digits = 2),
  o_SD_graphic = round(graphicY_metrics$StandardDeviation, digits = 2)
)

output <- rbind(output, newdf)

scentext <- (paste0("", newdf$ACCID))
scenfile <- (paste0("myapp/tex/300_users/",scentext,".tex")) 

save_to_latex(newdf$ACCID,
              newdf$USER_ID,
              newdf$group,
              newdf$answers,
              scenfile,
              newdf$i_Median_classic,
              newdf$i_SD_classic,
              newdf$i_Mean_classic,
              newdf$o_Median_classic,
              newdf$o_SD_classic,
              newdf$o_Mean_classic,
              newdf$i_Median_graphic,
              newdf$i_SD_graphic, 
              newdf$i_Mean_graphic,
              newdf$o_Median_graphic,
              newdf$o_SD_graphic,
              newdf$o_Mean_graphic
)  

#PART FÜR NON CORE TEAM USER
IMPACT <- df.noncoreteam[,"scaled_IMPACT"]
OCCURRENCE <- df.noncoreteam[,"scaled_OCCURRENCE"]
numberofnoncoreteamanswers <- length(df.noncoreteam$QUES_ID)
print(paste0("Die Gesamtanzahl aller NONCORETEAM Antworten beträgt: ", numberofnoncoreteamanswers))
for (i in 1:numberofnoncoreteamanswers) {
  x.user.min <- df.noncoreteam[i,"scaled_X1"]
  x.user.max <- df.noncoreteam[i,"scaled_X2"]
  y.user.min <- df.noncoreteam[i,"scaled_Y1"]
  y.user.max <- df.noncoreteam[i,"scaled_Y2"]
  
  x.user.werte <- seq(x.user.min, x.user.max, by = 0.25)
  x.noncoreteam.user <- data.frame(Wert = x.user.werte)
  x.data.noncoreteam <- rbind(x.data.noncoreteam, x.noncoreteam.user)
  
  y.user.werte <- seq(y.user.min, y.user.max, by = 0.25)
  y.noncoreteam.user <- data.frame(Wert = y.user.werte)
  y.data.noncoreteam <- rbind(y.data.noncoreteam, y.noncoreteam.user)
  
}
classicX_metrics <- calculate_metrics(IMPACT)
classicY_metrics <- calculate_metrics(OCCURRENCE)
graphicX_metrics <- calculate_metrics(x.data.noncoreteam$Wert)
graphicY_metrics <- calculate_metrics(y.data.noncoreteam$Wert)

newdf <- data.frame(
  ACCID = "non core team",
  USER_ID = "all",
  group = "non core team",
  answers = numberofnoncoreteamanswers,
  i_Median_classic = round(classicX_metrics$Median, digits = 2),
  o_Median_classic = round(classicY_metrics$Median, digits = 2),
  i_Mean_classic = round(classicX_metrics$Mean, digits = 2),
  o_Mean_classic = round(classicY_metrics$Mean, digits = 2),
  i_SD_classic = round(classicX_metrics$StandardDeviation, digits = 2),
  o_SD_classic = round(classicY_metrics$StandardDeviation, digits = 2),
  i_Median_graphic = round(graphicX_metrics$Median, digits = 2),
  o_Median_graphic = round(graphicY_metrics$Median, digits = 2),
  i_Mean_graphic = round(graphicX_metrics$Mean, digits = 2),
  o_Mean_graphic = round(graphicY_metrics$Mean, digits = 2),
  i_SD_graphic = round(graphicX_metrics$StandardDeviation, digits = 2),
  o_SD_graphic = round(graphicY_metrics$StandardDeviation, digits = 2)
)

output <- rbind(output, newdf)

scentext <- (paste0("", newdf$ACCID))
scenfile <- (paste0("myapp/tex/300_users/",scentext,".tex")) 

save_to_latex(newdf$ACCID,
              newdf$USER_ID,
              newdf$group,
              newdf$answers,
              scenfile,
              newdf$i_Median_classic,
              newdf$i_SD_classic,
              newdf$i_Mean_classic,
              newdf$o_Median_classic,
              newdf$o_SD_classic,
              newdf$o_Mean_classic,
              newdf$i_Median_graphic,
              newdf$i_SD_graphic, 
              newdf$i_Mean_graphic,
              newdf$o_Median_graphic,
              newdf$o_SD_graphic,
              newdf$o_Mean_graphic
)  

#PART FÜR DIE EINZELUSER
for (m in 1:numberofuser.all) {
  AccId <- users[[m]]
  df.user <- df.all %>% filter(ACC2SURV_ACCID == AccId)#filtert die Daten und gibt nur die beantworteten des Users aus
  user.group <- ifelse(df.user$ACC2SURV_ROLE[1] == 1, "core team", "non core team")
  print(paste0("Der User hat die Gruppe: ", user.group))
  numberofuseranswers <- length(unique(df.user$QUES_ID))
  print(paste0("Der User hat accountID: ", AccId))
  print(paste0("Der User hat ",numberofuseranswers," Fragen beantwortet"))
  IMPACT <- df.user[,"scaled_IMPACT"]
  OCCURRENCE <- df.user[,"scaled_OCCURRENCE"]
  
  for (i in 1:numberofuseranswers) {
  x.user.min <- df.user[i,"scaled_X1"]
  x.user.max <- df.user[i,"scaled_X2"]
  y.user.min <- df.user[i,"scaled_Y1"]
  y.user.max <- df.user[i,"scaled_Y2"]
  
  x.user.werte <- seq(x.user.min, x.user.max, by = 0.25)
  x.daten.user <- data.frame(ACCID = rep(ACCID, length(x.user.werte)), Wert = x.user.werte)
  x.daten.user.all <- rbind(x.daten.user.all, x.daten.user)
  
  y.user.werte <- seq(y.user.min, y.user.max, by = 0.25)
  y.daten.user <- data.frame(ACCID = rep(ACCID, length(y.user.werte)), Wert = y.user.werte)
  y.daten.user.all <- rbind(y.daten.user.all, y.daten.user)
  
  }
  
  classicX_metrics <- calculate_metrics(IMPACT)
  classicY_metrics <- calculate_metrics(OCCURRENCE)
  graphicX_metrics <- calculate_metrics(x.daten.user.all$Wert)
  graphicY_metrics <- calculate_metrics(y.daten.user.all$Wert)
  
  
  newdf <- data.frame(
    ACCID = AccId,
    group = user.group,
    answers = numberofuseranswers,
    i_Median_classic = round(classicX_metrics$Median, digits = 2),
    o_Median_classic = round(classicY_metrics$Median, digits = 2),
    i_Mean_classic = round(classicX_metrics$Mean, digits = 2),
    o_Mean_classic = round(classicY_metrics$Mean, digits = 2),
    i_SD_classic = round(classicX_metrics$StandardDeviation, digits = 2),
    o_SD_classic = round(classicY_metrics$StandardDeviation, digits = 2),
    i_Median_graphic = round(graphicX_metrics$Median, digits = 2),
    o_Median_graphic = round(graphicY_metrics$Median, digits = 2),
    i_Mean_graphic = round(graphicX_metrics$Mean, digits = 2),
    o_Mean_graphic = round(graphicY_metrics$Mean, digits = 2),
    i_SD_graphic = round(graphicX_metrics$StandardDeviation, digits = 2),
    o_SD_graphic = round(graphicY_metrics$StandardDeviation, digits = 2)
    )
    
  output2 <- rbind(output2, newdf)

}

output2 <- output2 %>%
  arrange(ACCID) %>%
  mutate(USER_ID = row_number()) %>%
  select(ACCID, USER_ID, everything())

print (output2)

for (i in seq_len(nrow(output2))) {
  userfile <- paste0("myapp/tex/300_users/",output2$ACCID[i],".tex")
  
  save_to_latex(
    output2$ACCID[i],
    output2$USER_ID[i],
    output2$group[i],
    output2$answers[i],
    userfile,
    output2$i_Median_classic[i],
    output2$i_SD_classic[i], 
    output2$i_Mean_classic[i],
    output2$o_Median_classic[i],
    output2$o_SD_classic[i], 
    output2$o_Mean_classic[i],
    output2$i_Median_graphic[i],
    output2$i_SD_graphic[i], 
    output2$i_Mean_graphic[i],
    output2$o_Median_graphic[i],
    output2$o_SD_graphic[i],
    output2$o_Mean_graphic[i]
  )
}


#runde_wenn_numerisch <- function(x) {
#  if (is.numeric(x)) {
#    return(round(x, 2))
#  } else {
#    return(x)
#  }
#}
#gerundeter_df <- as.data.frame(lapply(output, runde_wenn_numerisch))

#scenfile <- (paste0("myapp/tex/300_users/usertable.tex")) 

#mein_dataframe_sortiert <- gerundeter_df %>%
#  arrange(ACCID) %>%
#  mutate(USER_ID = row_number())


gerundeter_df <- rbind(output, output2)


scentext <- paste0("usercomparison")
scenfile <- paste0("myapp/files/300_users/usercomparison.xlsx")

write.csv(gerundeter_df, file = paste0("myapp/files/300_users/",scentext,".csv"), row.names = TRUE)

# Erstelle einen neuen Workbook und füge den transponierten Dataframe ein
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", gerundeter_df)

# Speichern des Workbooks als XLSX-Datei
saveWorkbook(wb, file = scenfile, overwrite = TRUE)

