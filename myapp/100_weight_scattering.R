library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)


answers <- read.csv(file = 'myapp/data/RQ1_corrected_scaled.csv', header = TRUE)
all.answers <- answers %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)
number.scenarios <- nrow(as.data.frame(table(all.answers$QUES_ID)))
scenarios <- as.data.frame(table(all.answers$QUES_ID))


x.daten <- data.frame(ACCID = numeric(),
                      x.scale = numeric(),
                      x.value = numeric())

y.daten <- data.frame(ACCID = numeric(),
                      y.scale = numeric(),
                      y.value = numeric())

for (anz in 1:1){#number.scenarios) {
  
  actualscenario <- as.vector(scenarios[anz, 1])
  print(actualscenario)
  scentext <- (paste0("Scenario ", actualscenario))
  actual.df <- answers %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == actualscenario)
  numberofanswers <- nrow(actual.df)
  
  user_data <- data.frame(
    user_id = actual.df[,"ACC2SURV_ACCID"],
    x.min_range = actual.df[,"scaled_X1"],
    x.max_range = actual.df[,"scaled_X2"],
    x.distance = actual.df[,"scaled_uncertainty_X"],
    y.min_range = actual.df[,"scaled_Y1"],
    y.max_range = actual.df[,"scaled_Y2"],
    y.distance = actual.df[,"scaled_uncertainty_Y"]
  )
  x.df_all <- data.frame(x.scale = numeric(), x.value = numeric())
  y.df_all <- data.frame(y.scale = numeric(), y.value = numeric())
  user_data <- user_data %>%  mutate(x.normalized_range = 1 - (x.max_range - x.min_range) / 100)
  user_data <- user_data %>%  mutate(y.normalized_range = 1 - (y.max_range - y.min_range) / 100)
  x.total_normalized_range <- sum(user_data$x.normalized_range)
  y.total_normalized_range <- sum(user_data$y.normalized_range)
  user_data <- user_data %>%  mutate(x.weight = x.normalized_range / x.total_normalized_range)
  user_data <- user_data %>%  mutate(y.weight = y.normalized_range / y.total_normalized_range)
  
  for (i in 1:numberofanswers){
    ACCID <- actual.df[i,"ACC2SURV_ACCID"]
    x.normalized_range_user <- (1 - (actual.df[i,"scaled_X2"] - actual.df[i,"scaled_X1"]) / 100)
    y.normalized_range_user <- (1 - (actual.df[i,"scaled_Y2"] - actual.df[i,"scaled_Y1"]) / 100)
    actualmin.x <- actual.df[i,"scaled_X1"]
    actualmax.x <- actual.df[i,"scaled_X2"]
    actualmin.y <- actual.df[i,"scaled_Y1"]
    actualmax.y <- actual.df[i,"scaled_Y2"]
    weight.actualvaluex <- actualmin.x
    weight.actualvaluey <- actualmin.y
    x.values <- seq(from = 0, to = 100, by = 0.25)
    y.values <- seq(from = 0, to = 100, by = 0.25)
    x.df_prod <- data.frame(x.scale = values, x.value = 0)
    y.df_prod <- data.frame(y.scale = values, y.value = 0)
    x.df_prod <- x.df_prod[order(x.df_prod$x.scale), , drop = FALSE]
    y.df_prod <- y.df_prod[order(y.df_prod$y.scale), , drop = FALSE]
    
    while (weight.actualvaluex <= actualmax.x) {
      index <- which(x.df_prod$x.scale == weight.actualvaluex)
      x.df_prod$x.value[index] <- user_data$x.weight[i]
      weight.actualvaluex <- weight.actualvaluex + 0.25
    }
    
    x.df_all <- rbind(x.df_all, x.df_prod)
  
  while (weight.actualvaluey <= actualmax.y) {
    index <- which(y.df_prod$y.scale == weight.actualvaluey)
    y.df_prod$y.value[index] <- user_data$y.weight[i]
    weight.actualvaluey <- weight.actualvaluey + 0.25
  }
  
  y.df_all <- rbind(y.df_all, y.df_prod)
  }
  
  grouped_x.daten <- x.df_all %>% group_by(x.scale)
  x.summary_df <- grouped_x.daten %>% summarise(x.Sum = sum(x.value))
  x.summary_df <- x.summary_df %>% mutate(x.Wahrs = x.Sum / sum(x.Sum))
  x.summary_df <- x.summary_df %>% mutate(x.Histo = x.Wahrs * numberofanswers)
  print(x.summary_df)
  
  grouped_y.daten <- y.df_all %>% group_by(y.scale)
  y.summary_df <- grouped_y.daten %>% summarise(y.Sum = sum(y.value))
  y.summary_df <- y.summary_df %>% mutate(y.Wahrs = y.Sum / sum(y.Sum))
  y.summary_df <- y.summary_df %>%mutate(y.Histo = y.Wahrs * numberofanswers)
  print(y.summary_df)
  
  print(sum(x.summary_df$x.Wahrs))
  print(x.mittelwert <- mean(x.summary_df$x.Wahrs))
  x.ergebnis <- apply(x.summary_df[, c("x.scale", "x.Wahrs")], MARGIN = 1, FUN = prod)
  print(x.erwartungswert <- sum(x.ergebnis))
  print (x.standardabweichung <- sd(x.erwartungswert))
  print (x.median_value <- median(x.summary_df$x.Wahrs, na.rm = TRUE))
  x.gewichtete_werte <- x.summary_df$x.scale * x.summary_df$x.Wahrs
  print (x.gewichtete_werte)
  standardabweichung <- sqrt(sum((x.summary_df$x.scale  - mean(x.gewichtete_werte ))^2 * x.summary_df$x.Wahrs))
  
  # Standardabweichung ausgeben
  print(standardabweichung)

  #print(sum(y.summary_df$y.Wahrs))
  #print(y.mittelwert <- mean(y.summary_df$y.Wahrs))
  #y.ergebnis <- apply(y.summary_df[, c("y.scale", "y.Wahrs")], MARGIN = 1, FUN = prod)
  #print(sum(y.ergebnis))
  #print (y.median_value <- median(y.summary_df$y.Wahrs, na.rm = TRUE))
 
  
  
}
