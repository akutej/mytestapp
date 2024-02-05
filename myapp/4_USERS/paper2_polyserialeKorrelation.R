
library(dplyr)
library(ggplot2)
library(grid)
library(RColorBrewer)
library(openxlsx)
library(gplots)
library(tibble)
library(readxl)
library(polycor)

answerstable <- read.xlsx('myapp/data/RQ1_corrected_scaled_2.xlsx') #importiert das answers file
answerstable <- answerstable %>% filter(QUES_ID != "401" & QUES_ID != "402"& QUES_ID != "403")#Nimmt meine Testdatensätze aus
answerstable <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & (ACC2SURV_ROLE  == 1 | ACC2SURV_ROLE  == 2))#filtert die Daten und gibt nur die beantworteten aus #& QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")

answerstableR <- answerstable %>% filter(QUES_TYP == "Risiko" & QUES_ID == "350")
R.QUESID <- (unique(answerstableR$QUES_ID))


for (ques_id in R.QUESID) {
  
  answerstableR.all <- answerstableR %>% filter(QUES_ID == ques_id)
  
  print (ques_id)
  classic <- (answerstableR.all$scaled_IMPACT)
  graphic.center <- (answerstableR.all$scaled_uncertainty_middle_X)

  print(classic)
  print ("\n")
  print(graphic.center)
  print ("\n")
  # Berechnung der polyseriellen Korrelation
  korrelation <- hetcor(data.frame(classic, graphic.center), polyserial=TRUE)
  print (korrelation)
  
  new <- polyserial(classic, graphic.center, ML = FALSE, control = list(), 
             std.err = FALSE, maxcor=.9999, bins=4, start, thresholds=FALSE)
  
  print (new)
  
}


