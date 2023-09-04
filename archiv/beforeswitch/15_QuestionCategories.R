library(dplyr)
library(ggplot2)
library(scales)

questions <- read.csv(file = 'myapp/Data/questions.csv', header=TRUE, sep=";", dec=".", encoding="auto")

qframe=as.data.frame.matrix(questions)
colnames(qframe)
#head (questions)
#newtest <- aggregate(qframe, by=list(qframe$QUES_ID), FUN=length )
#test <- subset(qframe,QUES_CATEGORY == "Projektablauf")
df2 <- qframe %>% group_by(QUES_CATEGORY,QUES_TYP) %>% 
  summarise(total_count=n(),.groups = 'drop') %>%
  as.data.frame()

newcount <- table(qframe$QUES_CATEGORY)
#nrow (qframe[duplicated(qframe$QUES_ID), ]) #zählt die Anzahl ohne Berücksichtigung der Duplikate
nrow (qframe[duplicated(qframe$QUES_CATEGORY), ]) #zählt die Anzahl ohne Berücksichtigung der Duplikate
newcount
nrow (newcount)
#test
print (df2)
