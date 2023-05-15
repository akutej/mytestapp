library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)


dfkst <- data.frame(D_value_I = numeric(),D_value_O = numeric(), Label = character())

answers <- read.csv(file = 'myapp/data/RQ1_corrected_scaled.csv', header=TRUE) #importiere das answers file
all.answers <- answers %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)
number.scenarios <- nrow(as.data.frame(table(all.answers$QUES_ID)))

for (anz in 1:number.scenarios) {
  actualscenario =as.vector(scenarios[anz,1])
  print (actualscenario)
  #scentext <- (paste0("Scenario ", actualscenario))
  actual.df <- answers %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")
  data_IMPACT <- actual.df$scaled_IMPACT
  data_OCCURRENCE <- actual.df$scaled_OCCURRENCE
  ordinal_df_IMPACT <- data.frame(value = data_IMPACT, freq = 1)
  ordinal_df_OCCURRENCE <- data.frame(value = data_OCCURRENCE, freq = 1)
  ordinal_freq_IMPACT <- aggregate(freq ~ value, data = ordinal_df_IMPACT, sum)
  ordinal_freq_OCCURRENCE <- aggregate(freq ~ value, data = ordinal_df_OCCURRENCE, sum)
  
  numberofanswers <- nrow(actual.df)
  
  IMPACT <- c()
  weight.IMPACT <- c()
  LIKELIHOOD <- c()
  weight.LIKELIHOOD <- c()
  
  #sums.UNC.I <- colSums(actual.df["uncertaintyIPixel"])
  #sums.UNC.O <- colSums(actual.df["uncertaintyOPixel"])
  #print (sums.UNC.I[[1]])
  #print (sums.UNC.O[[1]])
  sum_I <- actual.df %>% mutate(scaled_uncertainty_X = 100 - scaled_uncertainty_X) %>% summarize(sum_I = sum(scaled_uncertainty_X)) %>% pull(sum_I)
  sum_O <- actual.df %>% mutate(scaled_uncertainty_Y = 100 - scaled_uncertainty_Y) %>% summarize(sum_O = sum(scaled_uncertainty_Y)) %>% pull(sum_O)
  
  
  print (sum_I)
  print (sum_O)
  
  for (i in 1:numberofanswers){
    AccId <- actual.df[i,"ACC2SURV_ACCID"]
    QuesId <- actual.df[i,"QUES_ID"]
    UncertaintyI <- actual.df[i,"scaled_uncertainty_X"]
    UncertaintyO <- actual.df[i,"scaled_uncertainty_Y"]
    Role <- actual.df[i,"ACC2SURV_ROLE"]
    GroupId <- actual.df[i,"ACC2SURV_GROUPID"]
    x.min <- actual.df[i,"scaled_X1"]
    x.max <- actual.df[i,"scaled_X2"]
    y.min <- actual.df[i,"scaled_Y1"]
    y.max <- actual.df[i,"scaled_Y2"]
    actualvaluex <- x.min
    actualvaluey <- y.min
    
    
    while (actualvaluex <= x.max){
      weight.IMPACT <- (100/sum_I)*(100-UncertaintyI)
      #transformedx <- (((actualvaluex/400)*100))
      #transformedx <- (((actualvaluex/400)*5)) #+0.5 !!!!!!!!!!!OHNE 0.5
      weight.transformedx <- ((actualvaluex)*weight.IMPACT)
      #print (transformedx)
      IMPACT <- c(IMPACT,actualvaluex)
      weight.IMPACT <- c(weight.IMPACT,weight.transformedx)
      actualvaluex = actualvaluex + 1
    }
    
    while (actualvaluey <= y.max){
      weight.OCCURRENCE <- (100/sum_O)*(100-UncertaintyO)
      #   print (weight.OCCURRENCE)
      #transformedy <- (((actualvaluey/400)*100))
      #transformedy <- (((actualvaluey/400)*5))
      weight.transformedy <- ((actualvaluey)*weight.OCCURRENCE)
      LIKELIHOOD <- c(LIKELIHOOD,actualvaluey)
      weight.LIKELIHOOD <- c(weight.LIKELIHOOD,weight.transformedy)
      actualvaluey = actualvaluey + 1
    }
    
    
    
    
    
    
  }
  
  
  #print (ordinal_freq_IMPACT)
  #print (ordinal_freq_OCCURRENCE)
  
  I <- ggplot(ordinal_freq_IMPACT, aes(x = factor(value), y = freq)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(x = "Ordinal Value", y = "Frequency") +
    labs(title = "classic IMPACT") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  O <- ggplot(ordinal_freq_OCCURRENCE, aes(x = factor(value), y = freq)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(x = "Ordinal Value", y = "Frequency") +
    labs(title = "classic OCCURRENCE") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  #plot (I)
  #plot (O)
  
  
  print (paste0("GRAPHISCH - IMPACT"))
  gr_sd_ordinal_data_I <- sd(IMPACT)
  gr_median_ordinal_data_I <- median(IMPACT)
  gr_mean_ordinal_data_I <- mean(IMPACT)
  gr_Q1_I <- quantile(IMPACT, 0.25)[[1]]
  gr_Q3_I <- quantile(IMPACT, 0.75)[[1]]
  print (paste0("Standardabweichung:",gr_sd_ordinal_data_I))
  print (paste0("Median:",gr_median_ordinal_data_I))
  print (paste0("Mittelwert:",gr_mean_ordinal_data_I))
  print (paste0("erstes Quartil:",gr_Q1_I))
  print (paste0("drittes Quartil:",gr_Q3_I))
  print (paste0("Interquartilsabstand:",gr_Q3_I - gr_Q1_I))
  print ("")
  print (paste0("GRAPHISCH - OCCURRENCE"))
  gr_sd_ordinal_data_O <- sd(LIKELIHOOD)
  gr_median_ordinal_data_O <- median(LIKELIHOOD)
  gr_mean_ordinal_data_O <- mean(LIKELIHOOD)
  gr_Q1_O <- quantile(LIKELIHOOD, 0.25)[[1]]
  gr_Q3_O <- quantile(LIKELIHOOD, 0.75)[[1]]
  print (paste0("Standardabweichung:",gr_sd_ordinal_data_O))
  print (paste0("Median:",gr_median_ordinal_data_O))
  print (paste0("Mittelwert:",gr_mean_ordinal_data_O))
  print (paste0("erstes Quartil:",gr_Q1_O))
  print (paste0("drittes Quartil:",gr_Q3_O))
  print (paste0("Interquartilsabstand:",gr_Q3_O - gr_Q1_O))
  print ("")
  
  print (paste0("gewichtet GRAPHISCH - IMPACT"))
  weight_gr_sd_ordinal_data_I <- sd(weight.IMPACT)
  weight_gr_median_ordinal_data_I <- median(weight.IMPACT)
  weight_gr_mean_ordinal_data_I <- mean(weight.IMPACT)
  weight_gr_Q1_I <- quantile(weight.IMPACT, 0.25)[[1]]
  weight_gr_Q3_I <- quantile(weight.IMPACT, 0.75)[[1]]
  print (paste0("Standardabweichung:",weight_gr_sd_ordinal_data_I))
  print (paste0("Median:",weight_gr_median_ordinal_data_I))
  print (paste0("Mittelwert:",weight_gr_mean_ordinal_data_I))
  print (paste0("erstes Quartil:",weight_gr_Q1_I))
  print (paste0("drittes Quartil:",weight_gr_Q3_I))
  print (paste0("Interquartilsabstand:",weight_gr_Q3_I - weight_gr_Q1_I))
  print ("")
  print (paste0("gewichtet GRAPHISCH - OCCURRENCE"))
  weight_gr_sd_ordinal_data_O <- sd(weight.LIKELIHOOD)
  weight_gr_median_ordinal_data_O <- median(weight.LIKELIHOOD)
  weight_gr_mean_ordinal_data_O <- mean(weight.LIKELIHOOD)
  weight_gr_Q1_O <- quantile(LIKELIHOOD, 0.25)[[1]]
  weight_gr_Q3_O <- quantile(LIKELIHOOD, 0.75)[[1]]
  print (paste0("Standardabweichung:",weight_gr_sd_ordinal_data_O))
  print (paste0("Median:",weight_gr_median_ordinal_data_O))
  print (paste0("Mittelwert:",weight_gr_mean_ordinal_data_O))
  print (paste0("erstes Quartil:",weight_gr_Q1_O))
  print (paste0("drittes Quartil:",weight_gr_Q3_O))
  print (paste0("Interquartilsabstand:",weight_gr_Q3_O - weight_gr_Q1_O))
  print ("")
  
  
  print (paste0("KLASSISCH - IMPACT"))
  sd_ordinal_data_I <- sd(data_IMPACT)
  median_ordinal_data_I <- median(data_IMPACT)
  mean_ordinal_data_I <- mean(data_IMPACT)
  Q1_I <- quantile(data_IMPACT, 0.25)[[1]]
  Q3_I <- quantile(data_IMPACT, 0.75)[[1]]
  print (paste0("Standardabweichung:",sd_ordinal_data_I))
  print (paste0("Median:",median_ordinal_data_I))
  print (paste0("Mittelwert:",mean_ordinal_data_I))
  print (paste0("erstes Quartil:",Q1_I))
  print (paste0("drittes Quartil:",Q3_I))
  print (paste0("Interquartilsabstand:",Q3_I - Q1_I))
  print ("")       
  
  sd_ordinal_data_O <- sd(data_OCCURRENCE)
  median_ordinal_data_O <- median(data_OCCURRENCE)
  mean_ordinal_data_O <- mean(data_OCCURRENCE)
  Q1_O <- quantile(data_OCCURRENCE, 0.25)[[1]]
  Q3_O <- quantile(data_OCCURRENCE, 0.75)[[1]]
  print (paste0("KLASSISCH - OCCURRENCE"))
  print (paste0("Standardabweichung:",sd_ordinal_data_O))
  print (paste0("Median:",median_ordinal_data_O))
  print (paste0("Mittelwert:",mean_ordinal_data_O))
  print (paste0("erstes Quartil:",Q1_O))
  print (paste0("drittes Quartil:",Q3_O))
  print (paste0("Interquartilsabstand:",Q3_O - Q1_O))
  
  kstx1 <- data_IMPACT
  ksty1 <- data_OCCURRENCE
  kstx2 <- IMPACT
  ksty2 <- LIKELIHOOD
  
  #print (kstx1)
  #print (kstx2)
  
  # Anwenden des Kolmogorov-Smirnov-Tests
  ks_result1 <- (ks.test(kstx1, kstx2, alternative = "two.sided", exact = NULL))
  ks_result2 <- (ks.test(ksty1, ksty2, alternative = "two.sided", exact = NULL))
  new_row <- data.frame(
    Label = paste0("Szenario_", QuesId),                    
    D_I_classicgraphic = ks_result1$statistic,
    D_O_classicgraphic = ks_result2$statistic,
    classic_SD_Occurrence = sd_ordinal_data_O,
    classic_SD_Impact = sd_ordinal_data_I,
    graphic_SD_Occurrence = gr_sd_ordinal_data_O,
    graphic_SD_Impact = gr_sd_ordinal_data_I,
    classic_median_Occurrence = median_ordinal_data_O,
    classic_median_Impact = median_ordinal_data_I,
    graphic_median_Occurrence = gr_median_ordinal_data_O,
    graphic_median_Impact = gr_median_ordinal_data_I
    
  )
  row.names(new_row) <- NULL
  dfkst <- rbind(dfkst, new_row)
  
  
  
  
  
}
####   SCHREIBT DAS FILE write.xlsx(dfkst,'myapp/files/32_Streumaße/Streumaße.xlsx', rowNames=TRUE)
#MEDIAN Vergleich
#print (dfkst)
classic_I <- dfkst$classic_median_Impact
graphic_I <- dfkst$graphic_median_Impact
print ("ANZAHL")
print (length(dfkst$classic_median_Impact))
print ("ANZAHL")
print (length(dfkst$graphic_median_Impact))
classic_O <- dfkst$classic_median_Occurrence
graphic_O <- dfkst$graphic_median_Occurrence

print ("IMPACT KLASSISCH")
print (classic_I)
print ("IMPACT GRAPHISCH")
print (graphic_I)
print ("OCCURRENCE KLASSISCH")
print (classic_O)
print ("OCCURRENCE GRAPHISCH")
print (graphic_O)
# U-Test durchführen
test_result_IMPACT <- wilcox.test(classic_I, graphic_I)
test_result_OCCURENCE <- wilcox.test(classic_O, graphic_O)
print (test_result_IMPACT)
print (test_result_OCCURENCE)

print (classic_I[1])
print (graphic_I[1])
test_result_IMPACT_one <- wilcox.test(classic_I[1], graphic_I[1])
print (test_result_IMPACT_one)


ergebnisse <- data.frame(Type = character(),Wert_X = numeric(), Wert_Y = numeric(), W = numeric(), p_value = numeric())

# Schleife über die Elemente der Vektoren
for (i in 1:length(classic_I)) {
  # U-Test durchführen
  test_result <- wilcox.test(classic_I[i], graphic_I[i])
  
  # Ergebnisse in den Dataframe schreiben
  ergebnisse[i, "Type"] <- "IMPACT"
  ergebnisse[i, "Wert_X"] <- classic_I[i]
  ergebnisse[i, "Wert_Y"] <- graphic_I[i]
  ergebnisse[i, "W"] <- test_result$statistic
  ergebnisse[i, "p_value"] <- test_result$p.value
}

for (i in 1:length(classic_O)) {
  # U-Test durchführen
  test_result <- wilcox.test(classic_O[i], graphic_O[i])
  val1 <- "OCCURRENCE"
  val2 <- classic_O[i]
  val3 <- graphic_O[i]
  val4 <- test_result$statistic
  val5 <- test_result$p.value
  
  # Ergebnisse in den Dataframe schreiben
  teilergebnis <- data.frame(Type=val1,Wert_X=val2,Wert_Y=val3,W=val4,p_value=val5)
  
  ergebnisse <- rbind(ergebnisse, teilergebnis)
}

# Ausgabe des Ergebnis-Dataframes
print(ergebnisse)

print (table(ergebnisse$W,ergebnisse$Type))


