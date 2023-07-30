library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)

answers <- read.csv(file = 'myapp/data/RQ1_corrected_scaled.csv', header = TRUE)
all.answers <- answers %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)
number.scenarios <- nrow(as.data.frame(table(all.answers$QUES_ID)))
scenarios <- as.data.frame(table(all.answers$QUES_ID))

x.daten <- data.frame(ACCID = numeric(),
                      scale = numeric(),
                      value = numeric())


for (anz in 89:89) { #1:number.scenarios) {#
  actualscenario <- as.vector(scenarios[anz, 1])
  print(actualscenario)
  actual.df <- answers %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == actualscenario)
  numberofanswers <- nrow(actual.df)
  
  # Berechnung des normalisierten Wertebereichs und der umgekehrten Gewichtung für jedes Szenario
  user_data <- data.frame(
    user_id = actual.df[,"ACC2SURV_ACCID"],
    min_range = actual.df[,"scaled_X1"],
    max_range = actual.df[,"scaled_X2"],
    distance = actual.df[,"scaled_uncertainty_X"]
    
  )
  
  user_data <- user_data %>%
    mutate(
      normalized_range = 1 - (max_range - min_range) / 100
    )
  
  total_normalized_range <- sum(user_data$normalized_range)
  
  user_data <- user_data %>%
    mutate(
      weight = normalized_range / total_normalized_range
    )
  
  
  
  for (i in 1:numberofanswers){
    ACCID <- actual.df[i,"ACC2SURV_ACCID"]
    normalized_range_user = (1 - (actual.df[i,"scaled_X2"] - actual.df[i,"scaled_X1"]) / 100)
    actualmin.x <- actual.df[i,"scaled_X1"]
    actualmax.x <- actual.df[i,"scaled_X2"]
    weight.actualvaluex <- actualmin.x
    
    
    
    while (weight.actualvaluex <= actualmax.x){
      
      weight.transformedx <- (1*user_data$weight)
      #print (transformedx)
      new_values <- data.frame(ACCID = ACCID , scale = weight.actualvaluex, value = weight.transformedx)
      x.daten <- bind_rows(x.daten, new_values)
      weight.actualvaluex = weight.actualvaluex + 0.25
      
      
    }
    
    
    
  }
  
  #print (D)

  
  
  # Ausgabe der gewichteten Daten für jedes Szenario
  #print(user_data)
  #print(x.daten)
  #options(scipen = 999) #werte voll darstellen
  grouped_x.daten <- x.daten %>% group_by(scale)
  summary_df <- grouped_x.daten %>% summarise(Sum = sum(value))
  summary_df <- summary_df %>%
    mutate(Wahrs = Sum / sum(Sum))
  
  summary_df <- summary_df %>%
    mutate(Histo = Wahrs * numberofanswers)
  
  print.data.frame (summary_df)
  
  #hist(summary_df$Histo, main = "Histogramm der Skalenwerte")
  
  
}