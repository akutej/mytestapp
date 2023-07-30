
# Beispielwerte
df <- data.frame(
  Skala = c(0.25, 0.50, 0.75, 1.00, 1.25, 1.50, 1.75, 2.00, 2.25, 2.50),
  User1 = c(NA, 1, 1, NA, NA, NA, NA, NA, NA, NA),
  User2 = c(NA, NA, NA, NA, 1, 1, 1, 1, 1, 1),
  User3 = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
)
längen <- sapply(df[, -1], function(x) sum(!is.na(x)))
gewichtung <- 1 / längen
gewichtung <- gewichtung / sum(gewichtung) 
print (längen)
print (gewichtung)

gewichtung_df <- data.frame(User = names(längen), Gewichtung = gewichtung)



print (df)
print (gewichtung_df)


