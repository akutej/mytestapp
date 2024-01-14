
library(dplyr)
library(ggplot2)
library(grid)
library(RColorBrewer)
library(openxlsx)
library(gplots)
library(tibble)
library(readxl)

answerstable <- read.xlsx('myapp/data/RQ1_corrected_scaled_2.xlsx') #importiert das answers file
answerstable <- answerstable %>% filter(QUES_ID != "401" & QUES_ID != "402"& QUES_ID != "403")#Nimmt meine Testdatensätze aus
answerstable <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & (ACC2SURV_ROLE  == 1 | ACC2SURV_ROLE  == 2))#filtert die Daten und gibt nur die beantworteten aus #& QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")


answerstableR <- answerstable %>% filter(QUES_TYP == "Risiko")
R.QUESID <- (unique(answerstableR$QUES_ID))



for (ques_id in R.QUESID) {

  answerstableR.G1 <- answerstableR %>% filter(ACC2SURV_ROLE  == 1  & QUES_ID == ques_id)
  answerstableR.G2 <- answerstableR %>% filter(ACC2SURV_ROLE  == 2  & QUES_ID == ques_id)
  classic.G1.IMPACT.R <- (answerstableR.G1$scaled_IMPACT)
  classic.G2.IMPACT.R <- (answerstableR.G2$scaled_IMPACT)
  classic.G1.OCCURRENCE.R <- (answerstableR.G1$scaled_OCCURRENCE)
  classic.G2.OCCURRENCE.R <- (answerstableR.G2$scaled_OCCURRENCE)
  cat("CLASSIC IMPACT RISK - QUES_ID:", ques_id, "\n")
  classic.result.I.R <- wilcox.test(classic.G1.IMPACT.R, classic.G2.IMPACT.R)
  print (classic.result.I.R$p.value)
  cat("CLASSIC OCCURRENCE RISK - QUES_ID:", ques_id, "\n")
  classic.result.O.R <- wilcox.test(classic.G1.OCCURRENCE.R, classic.G2.OCCURRENCE.R)
  print (classic.result.O.R$p.value)
  
}


answerstableC <- answerstable %>% filter(QUES_TYP == "Chance")

#anzahl <- nrow(answerstableR.G1)
#print (anzahl)

answerstableC.G1 <- answerstableC %>% filter(ACC2SURV_GROUPID == 1)
answerstableC.G2 <- answerstableC %>% filter(ACC2SURV_GROUPID == 2)


classic.G1.IMPACT.C <- (answerstableC.G1$scaled_IMPACT)
classic.G2.IMPACT.C <- (answerstableC.G2$scaled_IMPACT)
classic.G1.OCCURRENCE.C <- (answerstableC.G1$scaled_OCCURRENCE)
classic.G2.OCCURRENCE.C <- (answerstableC.G2$scaled_OCCURRENCE)



print ("CLASSIC IMPACT CHANCE")
classic.result.I.C <- wilcox.test(classic.G1.IMPACT.C, classic.G2.IMPACT.C)
print (classic.result.I.C)

print ("CLASSIC OCCURRENCE CHANCE")
classic.result.O.C <- wilcox.test(classic.G1.OCCURRENCE.C, classic.G2.OCCURRENCE.C)
print (classic.result.O.C)


print ("GRAPHISCH")
graphic.G1.IMPACT.R.middle <- (answerstableR.G1$scaled_uncertainty_middle_X)
graphic.G2.IMPACT.R.middle <- (answerstableR.G2$scaled_uncertainty_middle_X)

graphic.result.I.R <- wilcox.test(graphic.G1.IMPACT.R.middle, graphic.G2.IMPACT.R.middle)
print (graphic.result.I.R)


for (ques_id in R.QUESID) {
  
  answerstableR.G1 <- answerstableR %>% filter(ACC2SURV_ROLE  == 1  & QUES_ID == ques_id)
  answerstableR.G2 <- answerstableR %>% filter(ACC2SURV_ROLE  == 2  & QUES_ID == ques_id)
  graphic.G1.IMPACT.R <- (answerstableR.G1$scaled_uncertainty_middle_X)
  graphic.G2.IMPACT.R <- (answerstableR.G2$scaled_uncertainty_middle_X)
  graphic.G1.OCCURRENCE.R <- (answerstableR.G1$scaled_uncertainty_middle_Y)
  graphic.G2.OCCURRENCE.R <- (answerstableR.G2$scaled_uncertainty_middle_Y)
  cat("GRAPHIC IMPACT RISK - QUES_ID:", ques_id, "\n")
  graphic.result.I.R <- wilcox.test(graphic.G1.IMPACT.R, graphic.G2.IMPACT.R)
  print (graphic.result.I.R$p.value)
  cat("GRAPHIC OCCURRENCE RISK - QUES_ID:", ques_id, "\n")
  graphic.result.O.R <- wilcox.test(graphic.G1.OCCURRENCE.R, graphic.G2.OCCURRENCE.R)
  print (graphic.result.O.R$p.value)
  
  
}





#all_uncI <- (answerstableR$uncertaintyIPercent[answerstableR$ACC2SURV_ROLE == 1 |  answerstableR$ACC2SURV_ROLE == 2 ])
# scaled_IMPACT	
# scaled_OCCURRENCE
# scaled_uncertainty_middle_X	
# scaled_uncertainty_middle_Y

