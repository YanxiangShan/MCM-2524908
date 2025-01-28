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

# Extreme in Y
alpha <- 0.05
n <- nobs(total.red)
p <- length(coef(total.red))

processed.data.total <- processed.data[, !(names(processed.data) %in% setdiff(drop.cols, "Total"))]
y.extremes <- processed.data.total[abs(rstudent(total.red)) >= qt(1 - alpha/n, n - p - 1), ]
nrow(y.extremes)

# Extreme in X
x.extremes <- processed.data.total[hatvalues(total.red) >= 2*p, ]
nrow(x.extremes)

##  Influential points
# DFFITS
dffits.ex <- processed.data[abs(dffits(total.red)) >= 1, ] # DFFITS, cutoff |DFFITS| >= 1
nrow(dffits.ex)
write.csv(dffits.ex[, c("NOC", "Year")], "./analysis/DFFITS.csv", row.names = FALSE)

# Cook's Distance D, cutoff |D| >= F(0.5, p, n - p)
cd.ex <- processed.data[cooks.distance(total.red) >= qf(0.5, p, n - p), ] # Cook's D
nrow(cd.ex)
write.csv(cd.ex[, c("NOC", "Year")], "./analysis/cooks_distance.csv", row.names = FALSE)

# DFBETAS, cutoff |DFBETAS| >= 1
dfbetas.ex <- processed.data.total[abs(dfbetas(total.red)) >= 1]
length(dfbetas.ex)

all.values <- data.frame()
# for (model in c(bronze.red, silver.red, gold.red, total.red)) {
  model <- total.red
  preds <- names(coef(model))
  preds <- setdiff(preds, "(Intercept)")
  p.values <- rep(0, length(preds))
  names(p.values) <- preds
  
  for (pred.name in preds) {
    pred <- na.omit(unlist(processed.data[[pred.name]], use.names = FALSE))
    med <- median(pred)
    # Next, we calculate the values needed to get $s^2$ and $t^*$
    res <- resid(total.red)
    e1 <- na.omit(res[pred <= med])
    e2 <- na.omit(res[pred > med])
    d1 <- abs(e1 - median(e1))
    d2 <- abs(e2 - median(e2))
    s2 <- (sum((d1 - mean(d1))^2) + sum(d2 - mean(d2))^2) / (n-2)
    s <- sqrt(s2)
    n1 <- length(d1)
    n2 <- length(d2)
    t.star <- (mean(d1) - mean(d2)) / (s*sqrt(1/n1 + 1/n2))
    # RR: |t*| > t(1 - alpha/2; n - 2)
    # p-value, multiply by 2 for two-tailed
    p.value <- 2 * pt(abs(t.star), n - 2, lower.tail = FALSE)
    p.value
    p.values[pred.name] <- p.value
  }
  
library(stargazer)
  
stargazer(p.values, type='latex', flip = TRUE)
  
  all.values <- rbind(all.values, p.values)

bronze.slr.athletes <- lm(Bronze ~ Athletes, data = processed.data)
silver.slr.athletes <- lm(Silver ~ Athletes, data = processed.data)
gold.slr.athletes   <- lm(Gold ~ Athletes, data = processed.data)
total.slr.athletes  <- lm(Total ~ Athletes, data = processed.data)

stargazer(bronze.slr.athletes, silver.slr.athletes, gold.slr.athletes,
          type='latex', report=('vc*p'))

bronze.slr.events <- lm(Bronze ~ Events + Is.host + Events*Is.host, data = processed.data)
silver.slr.events <- lm(Silver ~ Events + Is.host + Events*Is.host, data = processed.data)
gold.slr.events <- lm(Gold ~ Events + Is.host + Events*Is.host, data = processed.data)
total.slr.events <- lm(Total ~ Events + Is.host + Events*Is.host, data = processed.data)

stargazer(bronze.slr.events, silver.slr.events, gold.slr.events, total.slr.events,
          type='latex', report=('vc*p'))

library(car)

vif(total.red, type = 'predictor')

VIFs <- cbind(vif(bronze.red), vif(silver.red), vif(gold.red), vif(total.red))
colnames(VIFs) <- medal.colnames
stargazer(VIFs, type='latex')

VIF.diffs <- VIF - vif(total.red)
write.csv(VIFs, "./analysis/vifs.csv", row.names = FALSE)
write.csv(VIF.diffs, "./analysis/vif_diffs.csv", row.names = FALSE)