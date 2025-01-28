setwd("C:/Users/danim/Dev/R")
processed.data <- read.csv("./processed_data.csv")

medal.colnames <- c("Bronze", "Silver", "Gold", "Total")
drop.cols <- c(medal.colnames, "Rank", "Year", "NOC")

bronze.mdl <- lm(Bronze ~ . + Is.host*Events + Is.host*Mean.Event.Participation, data = processed.data[, !(names(processed.data) %in% setdiff(drop.cols, "Bronze"))])
silver.mdl <- lm(Silver ~ . + Is.host*Events + Is.host*Mean.Event.Participation, data = processed.data[, !(names(processed.data) %in% setdiff(drop.cols, "Silver"))])
gold.mdl   <- lm(Gold   ~ . + Is.host*Events + Is.host*Mean.Event.Participation, data = processed.data[, !(names(processed.data) %in% setdiff(drop.cols, "Gold"))])
total.mdl  <- lm(Total  ~ . + Is.host*Events + Is.host*Mean.Event.Participation, data = processed.data[, !(names(processed.data) %in% setdiff(drop.cols, "Total"))])

# Now do backwards selection
library(MASS)

bronze.red <- step(bronze.mdl)
silver.red <- step(silver.mdl)
gold.red   <- step(gold.mdl)
total.red  <- step(total.mdl)

## Prediction ##
year.location <- "Los Angeles, United States"
prev.year <- 2024
pred.medal.table <- data.frame()

last.year.data <- data.frame(processed.data[processed.data$Year == prev.year, ])
countries <- unique(last.year.data$NOC)

for (country in countries) {
  pred.data <- last.year.data[last.year.data$NOC == country, ]
  pred.data["Bronze.Last.Olympics"] <- pred.data["Bronze"]
  pred.data["Silver.Last.Olympics"] <- pred.data["Silver"]
  pred.data["Gold.Last.Olympics"] <- pred.data["Gold"]
  pred.data <- pred.data[, !(names(pred.data) %in% drop.cols)]
  
  test.name <- country # use to test if name is in host location's name
  # Special Case
  if (country == "Great Britain") { test.name <- "United Kingdom" }
  pred.data["Is.host"] <- as.integer(grepl(test.name, year.location))
  
  bronze.pred <- predict(bronze.mdl, newdata = data.frame(pred.data), interval = "confidence")
  silver.pred <- predict(silver.mdl, newdata = data.frame(pred.data), interval = "confidence") 
  gold.pred   <- predict(gold.mdl, newdata = data.frame(pred.data), interval = "confidence")
  total.pred  <- predict(total.mdl, newdata = data.frame(pred.data), interval = "confidence")
  pred.total.sum <- bronze.pred[1] + silver.pred[1] + gold.pred[1]
  # Note: this coerces all predicted values to be strings
  row <- c(country,  
           bronze.pred, silver.pred, gold.pred, total.pred, pred.total.sum)
  pred.medal.table <- rbind(pred.medal.table, row)
}

table.names <- c("NOC")
for (medal.name in medal.colnames) {
  for (metric in c("fit", "lwr", "upr")) {
    colname <- paste(medal.name, metric, sep='.')
    table.names <- c(table.names, colname)
  }
}
names(pred.medal.table) <- c(table.names, "Total.Sums")

numeric.cols <- names(pred.medal.table) != "NOC"

pred.medal.table[numeric.cols] <- lapply(pred.medal.table[numeric.cols], as.numeric)
pred.medal.table <- pred.medal.table[order(pred.medal.table$Total.fit, decreasing = TRUE), ]

pred.medal.table.rounded <- pred.medal.table[names(pred.medal.table) == "NOC" | grepl(".*\\.fit", names(pred.medal.table))]
names(pred.medal.table.rounded) <- sub("\\.fit", '', names(pred.medal.table.rounded))
numeric.cols <- names(pred.medal.table.rounded) != "NOC"
pred.medal.table.rounded[numeric.cols] <- lapply(pred.medal.table.rounded[numeric.cols],  round)

pred.medal.table.rounded.pos <- pred.medal.table.rounded
pred.medal.table.rounded.pos[numeric.cols] <- lapply(pred.medal.table.rounded.pos[numeric.cols], function(x) ifelse(x > 0, x, 0))

pred.medal.table.rounded[pred.medal.table.rounded$Bronze + pred.medal.table.rounded$Silver + pred.medal.table.rounded$Gold != pred.medal.table.rounded$Total, ]

write.csv(pred.medal.table, "./predictions/predicted_medal_table.csv", row.names = FALSE)
write.csv(pred.medal.table.rounded, "./predictions/rounded_predicted_medal_table.csv", row.names = FALSE)
write.csv(pred.medal.table.rounded.pos, "./predictions/pos_rounded_predicted_medal_table.csv", row.names = FALSE)