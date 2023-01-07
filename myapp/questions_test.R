library(dplyr)
library(ggplot2)
library(scales)

questions <- read.csv(file = 'myapp/Data/questions.csv', header=TRUE, sep=";", dec=".", encoding="auto")

qframe=as.data.frame.matrix(questions)
colnames(qframe)
head (questions)
newtest <- aggregate(qframe, by=list(qframe$QUES_ID), FUN=length )
newcount <- table(qframe$QUES_CATEGORY)
#nrow (qframe[duplicated(qframe$QUES_ID), ]) #zählt die Anzahl ohne Berücksichtigung der Duplikate
nrow (qframe[duplicated(qframe$QUES_CATEGORY), ]) #zählt die Anzahl ohne Berücksichtigung der Duplikate
newcount
nrow (newcount)

