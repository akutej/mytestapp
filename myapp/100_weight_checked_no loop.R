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

for (anz in 1:1) {
  
  actualscenario <- as.vector(scenarios[anz, 1])
  print(actualscenario)
  actual.df <- answers %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == actualscenario)
  numberofanswers <- nrow(actual.df)
  
  user_data <- data.frame(
    user_id = actual.df[,"ACC2SURV_ACCID"],
    min_range = actual.df[,"scaled_X1"],
    max_range = actual.df[,"scaled_X2"],
    distance = actual.df[,"scaled_uncertainty_X"]
  )
  df_all <- data.frame(scale = numeric(), value = numeric())
  user_data <- user_data %>%  mutate(normalized_range = 1 - (max_range - min_range) / 100)
  total_normalized_range <- sum(user_data$normalized_range)
  user_data <- user_data %>%  mutate(weight = normalized_range / total_normalized_range)
  
  for (i in 1:numberofanswers){
    ACCID <- actual.df[i,"ACC2SURV_ACCID"]
    normalized_range_user <- (1 - (actual.df[i,"scaled_X2"] - actual.df[i,"scaled_X1"]) / 100)
    actualmin.x <- actual.df[i,"scaled_X1"]
    actualmax.x <- actual.df[i,"scaled_X2"]
    weight.actualvaluex <- actualmin.x
    values <- seq(from = 0, to = 100, by = 0.25)
    df_prod <- data.frame(scale = values, value = 0)
    df_prod <- df_prod[order(df_prod$scale), , drop = FALSE]
    
    while (weight.actualvaluex <= actualmax.x) {
      index <- which(df_prod$scale == weight.actualvaluex)
      df_prod$value[index] <- user_data$weight[i]
      weight.actualvaluex <- weight.actualvaluex + 0.25
    }
    
    df_all <- rbind(df_all, df_prod)
  }
  
  grouped_x.daten <- df_all %>% group_by(scale)
  summary_df <- grouped_x.daten %>% summarise(Sum = sum(value))
  summary_df <- summary_df %>%
    mutate(Wahrs = Sum / sum(Sum))
  
  summary_df <- summary_df %>%
    mutate(Histo = Wahrs * numberofanswers)
  
  print(summary_df)
  
  grouped <- aggregate(summary_df$Sum, by = list(Group = cut(summary_df$scale, breaks = seq(0, 100, by = 2), include.lowest = TRUE)), FUN = sum)
  
  print(grouped)
  
  #barplot(grouped$x, names.arg = grouped$Group, main = "Histogramm", xlab = "X-Skala", ylab = "Summierte Y-Werte")
  
  #barplot(summary_df$Histo, names.arg = summary_df$scale, main = "Histogramm", xlab = "X-Skala", ylab = "Summierte Y-Werte")
  
  #barplot(grouped$x, names.arg = grouped$Group, main = "Histogramm", xlab = "X-Skala", ylab = "Summierte Y-Werte", space = 0)
  
  
  # Definiere die gewünschten Skalenwerte für die X-Achse
  
  
  # Erstelle das Barplot mit manuell überschriebener X-Skala
  
  
  # Erstelle das Barplot mit manuell überschriebener X-Skala und beibehaltener Y-Skala
  
  #barplot(grouped$x, names.arg = x_scale_labels, main = "", xlab = "", ylab = "weighted Frequency", space = 0, col = "lightblue")
  
  barplot(grouped$x, names.arg = grouped$Group, main = "", xlab = "", ylab = "weighted Frequency", space = 0, col = "lightblue", xaxt = "n")
  
  
  print(sum(summary_df$Wahrs))
  print(mittelwert <- mean(summary_df$Wahrs))
  ergebnis <- apply(summary_df[, c("scale", "Wahrs")], MARGIN = 1, FUN = prod)
  print(sum(ergebnis))


 
  
  
}
