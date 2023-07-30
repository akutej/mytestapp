library(dplyr)
library(openxlsx)
library(gplots)
library(ggplot2)

capacity <- function(rectangle) {
  # Berechnet die Breite und Höhe des Rechtecks.
  width <- abs(rectangle$x2 - rectangle$x1)
  height <- abs(rectangle$y2 - rectangle$y1)
  # Berechnet die Fläche.
  area <- width * height
  # Das Gewicht ist der Kehrwert der Fläche.
  weight <- 1 / area
  return(weight)
}

choquet_integral <- function(values, weights) {
  # Sortiert die Werte in abnehmender Reihenfolge.
  order <- order(values, decreasing = TRUE)
  values <- values[order]
  # Behält die zugehörigen Gewichte und berechnet die kumulativen Gewichte.
  weights <- weights[order]
  cumulative_weights <- cumsum(weights)
  # Fügt eine Null am Anfang der kumulativen Gewichte hinzu.
  cumulative_weights <- c(0, cumulative_weights)
  # Berechnet das Choquet-Integral.
  integral <- sum(diff(cumulative_weights) * values)
  return(integral)
}

answerstable <- read.csv(file = 'myapp/Data/RQ1_corrected_scaled.csv', header=TRUE) #importiere das answers file
dfall <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1)
scenarios <- as.data.frame(table(dfall$QUES_ID))
numberscenarios  <- nrow(scenarios)

for (anz in 1:2){ #numberscenarios) {
  actualscenario =as.vector(scenarios[anz,1])
  #print (actualscenario)
  scentext <- (paste0("Scenario ", actualscenario))
  #print (actualscenario) 
  df <- answerstable %>% filter(QUES2SURV_METHOD == "classic" & ANS2SURV_ANSWERED == 1 & QUES_ID == actualscenario)# & ACC2SURV_ACCID == "22")
  #print (df)
  numberofanswers <- nrow(df)
  
  rectangles <- list()
  
  for (i in 1:numberofanswers){
    AccId <- df[i,"ACC2SURV_ACCID"]
    #QuesId <- df[i,"QUES_ID"]
    #UncertaintyI <- df[i,"scaled_uncertainty_X"]
    #UncertaintyO <- df[i,"scaled_uncertainty_Y"]
    #Role <- df[i,"ACC2SURV_ROLE"]
    #GroupId <- df[i,"ACC2SURV_GROUPID"]
    x1 <- df[i,"scaled_X1"]
    x2 <- df[i,"scaled_X2"]
    y1 <- df[i,"scaled_Y1"]
    y2 <- df[i,"scaled_Y2"]
    
    
    rectangles[[i]] <- list(x1 = x1, y1 = y1, x2 = x2, y2 = y2)
    
  }
  
  
  


#print (rectangles)

# Berechnet die Gewichte für jedes Rechteck.
weights <- sapply(rectangles, capacity)

# Normalisiert die Gewichte, so dass sie alle zusammen eins ergeben.
weights <- weights / sum(weights)

# Berechnet das Choquet-Integral für alle x1, x2, y1 und y2 Werte.
x1_integral <- choquet_integral(sapply(rectangles, function(rectangle) rectangle$x1), weights)
x2_integral <- choquet_integral(sapply(rectangles, function(rectangle) rectangle$x2), weights)
y1_integral <- choquet_integral(sapply(rectangles, function(rectangle) rectangle$y1), weights)
y2_integral <- choquet_integral(sapply(rectangles, function(rectangle) rectangle$y2), weights)

# Das "konsolidierte" Rechteck ist nun definiert durch die gewichteten x1, x2, y1 und y2 Werte.
consolidated_rectangle <- list(x1 = x1_integral, y1 = y1_integral, x2 = x2_integral, y2 = y2_integral)

print (consolidated_rectangle)

# Berechnet das Choquet-Integral für alle x1 Werte.
x1_values <- sapply(rectangles, function(rectangle) rectangle$x1)
x1_integral <- choquet_integral(x1_values, weights)

# Berechnet die Varianz und den Standardabweichung.
x1_variance <- sum(weights * (x1_values - x1_integral)^2)
x1_std_dev <- sqrt(x1_variance)


print(x1_std_dev)


######IMPACT und OCCURRENCE ZUSAMMNENGEFASST
# Extrahiert alle X- und Y-Koordinaten

#x_values <- sapply(rectangles, function(rectangle) c(rectangle$x1, rectangle$x2))
#y_values <- sapply(rectangles, function(rectangle) c(rectangle$y1, rectangle$y2))

#x_values <- sapply(rectangles, function(rectangle) c(rectangle$x1, rectangle$x2))
#y_values <- sapply(rectangles, function(rectangle) c(rectangle$y1, rectangle$y2))

#x_values <- unlist(sapply(rectangles, function(rectangle) c(rectangle$x1, rectangle$x2)))
#y_values <- unlist(sapply(rectangles, function(rectangle) c(rectangle$y1, rectangle$y2)))
x_values <- unlist(sapply(rectangles, function(rectangle) c(rectangle$x1, rectangle$x2)))

# Combine y1 and y2 vectors into a single y_values vector
y_values <- unlist(sapply(rectangles, function(rectangle) c(rectangle$y1, rectangle$y2)))



#print (x_values)
# Doppelte Gewichte für X- und Y-Werte, da zwei Punkte pro Rechteck 
#weights_double <- rep(weights, each = 2)



# Custom function for calculating the weighted median
weighted_median <- function(x, w) {
  if (length(w) != length(x)) {
    stop("Length of weights must be equal to length of values.")
  }
  order <- order(x)
  x <- x[order]
  w <- w[order]
  cum_weights <- cumsum(w)
  cum_weights <- cum_weights / cum_weights[length(cum_weights)]
  idx <- sum(cum_weights < 0.5)
  return(x[idx])
}

# Berechnet die statistischen Maße
statistical_measures <- function(values, weights) {

    mean = sum(values * weights),
    #median = weighted_median(values, weights),
    sd = sqrt(sum(weights * (values - sum(values * weights))^2)),
    IQR = quantile(values, 0.75, type = 6, names = FALSE) - quantile(values, 0.25, type = 6, names = FALSE),
    Q1 = quantile(values, 0.25, type = 6, names = FALSE),
    Q3 = quantile(values, 0.75, type = 6, names = FALSE)
  
}


x_metrics <- statistical_measures(x_values,weights)
y_metrics <- statistical_measures(y_values,weights)

print (x_metrics)

}
