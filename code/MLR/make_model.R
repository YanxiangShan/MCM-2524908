setwd("C:/Users/danim/Dev/R")
processed.data <- read.csv("./processed_data.csv")

medal.colnames <- c("Bronze", "Silver", "Gold", "Total")
drop.cols <- c(medal.colnames, "Rank", "Year", "NOC")

bronze.mdl <- lm(Bronze ~ . + Is.host*Events + Is.host*Mean.Event.Participation, data = processed.data[, !(names(processed.data) %in% setdiff(drop.cols, "Bronze"))])
silver.mdl <- lm(Silver ~ . + Is.host*Events + Is.host*Mean.Event.Participation, data = processed.data[, !(names(processed.data) %in% setdiff(drop.cols, "Silver"))])
gold.mdl   <- lm(Gold   ~ . + Is.host*Events + Is.host*Mean.Event.Participation, data = processed.data[, !(names(processed.data) %in% setdiff(drop.cols, "Gold"))])
total.mdl  <- lm(Total  ~ . + Is.host*Events + Is.host*Mean.Event.Participation, data = processed.data[, !(names(processed.data) %in% setdiff(drop.cols, "Total"))])

library(stargazer)

# What do the full models look like?
bronze.mdl$AIC <- AIC(bronze.mdl)
silver.mdl$AIC <- AIC(silver.mdl)
gold.mdl$AIC <- AIC(gold.mdl)
total.mdl$AIC <- AIC(total.mdl)
bronze.mdl$BIC <- BIC(bronze.mdl)
silver.mdl$BIC <- BIC(silver.mdl)
gold.mdl$BIC <- BIC(gold.mdl)
total.mdl$BIC <- BIC(total.mdl)

stargazer(bronze.mdl, silver.mdl, gold.mdl, total.mdl, type='latex',
          keep.stat = c("n", "rsq", "adj.rsq", "aic", "bic", "ser", "f"), report = ("vc*p"))

# Now do backwards selection
library(MASS)

bronze.red <- step(bronze.mdl)
silver.red <- step(silver.mdl)
gold.red   <- step(gold.mdl)
total.red  <- step(total.mdl)

# Metrics
bronze.red$AIC <- AIC(bronze.red)
silver.red$AIC <- AIC(silver.red)
gold.red$AIC <- AIC(gold.red)
total.red$AIC <- AIC(total.red)
bronze.red$BIC <- BIC(bronze.red)
silver.red$BIC <- BIC(silver.red)
gold.red$BIC <- BIC(gold.red)
total.red$BIC <- BIC(total.red)

stargazer(bronze.red, silver.red, gold.red, total.red, type='latex',
          keep.stat = c("n", "rsq", "adj.rsq", "aic", "bic", "ser", "f"), report = ("vc*p"))

setdiff(names(coef(total.mdl)), names(coef(total.red)))
