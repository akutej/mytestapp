library(dplyr)
library(ggplot2)
library(scales)
answerstable <- read.csv(file = 'myapp/Data/answers.csv', header=TRUE, sep=";", dec=".")
answersframe=as.data.frame.matrix(answerstable)
answersframe_1 <- answersframe
#class(answersframe_1$X1PCT)
answersframe_1$X1PCT = as.numeric(sub(",", ".", sub(".", "", answersframe_1$X1PCT, fixed=TRUE), fixed=TRUE))
answersframe_1$X2PCT = as.numeric(sub(",", ".", sub(".", "", answersframe_1$X2PCT, fixed=TRUE), fixed=TRUE))
answersframe_1$Y1PCT = as.numeric(sub(",", ".", sub(".", "", answersframe_1$Y1PCT, fixed=TRUE), fixed=TRUE))
answersframe_1$Y2PCT = as.numeric(sub(",", ".", sub(".", "", answersframe_1$Y2PCT, fixed=TRUE), fixed=TRUE))
answersframe_1$X1Pixel <- ((400/100)*answersframe_1$X1PCT)
answersframe_1$X2Pixel <- ((400/100)*answersframe_1$X2PCT)
answersframe_1$Y1Pixel <- ((400/100)*answersframe_1$Y1PCT)
answersframe_1$Y2Pixel <- ((400/100)*answersframe_1$Y2PCT)
answersframe_1$ImpactUncPixel <- (answersframe_1$X2Pixel-answersframe_1$X1Pixel)
answersframe_1$ImpactUncPCT <- (answersframe_1$X2PCT-answersframe_1$X1PCT)
answersframe_1$LikelihoodUncPixel <- (answersframe_1$Y2Pixel-answersframe_1$Y1Pixel)
answersframe_1$LikelihoodUncPCT <- (answersframe_1$Y2PCT-answersframe_1$Y1PCT)
#colnames(answersframe_1)
#answersframe[["ACC2SURV_ACCID"]]
#myoutput <- answersframe_1[answersframe_1$ACC2SURV_ACCID == "22" & answersframe_1$QUES2SURV_METHOD == "graphic",  ]
uncertaintygroup1 <- answersframe_1[answersframe_1$ANS2SURV_ANSWERED == "1" & answersframe_1$ACC2SURV_GROUPID == "1" & answersframe_1$QUES2SURV_METHOD == "graphic",  ]  
uncertaintygroup2 <- answersframe_1[answersframe_1$ANS2SURV_ANSWERED == "1" & answersframe_1$ACC2SURV_GROUPID == "2" & answersframe_1$QUES2SURV_METHOD == "graphic",  ]  

mean(uncertaintygroup1$ImpactUncPixel)
mean(uncertaintygroup1$LikelihoodUncPixel)
mean(uncertaintygroup1$ImpactUncPCT)
mean(uncertaintygroup1$LikelihoodUncPCT)

mean(uncertaintygroup2$ImpactUncPixel)
mean(uncertaintygroup2$LikelihoodUncPixel)
mean(uncertaintygroup2$ImpactUncPCT)
mean(uncertaintygroup2$LikelihoodUncPCT)
