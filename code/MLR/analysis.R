setwd("C:/Users/danim/Dev/R")
processed.data <- read.csv("./processed_data.csv")

medal.colnames <- c("Bronze", "Silver", "Gold", "Total")
drop.cols <- c(medal.colnames, "Rank", "Year", "NOC")

total.mdl  <- lm(Total  ~ ., data = processed.data[, !(names(processed.data) %in% setdiff(drop.cols, "Total"))], na.action = na.omit)
gold.mdl   <- lm(Gold   ~ ., data = processed.data[, !(names(processed.data) %in% setdiff(drop.cols, "Gold"))])
silver.mdl <- lm(Silver ~ ., data = processed.data[, !(names(processed.data) %in% setdiff(drop.cols, "Silver"))])
bronze.mdl <- lm(Bronze ~ ., data = processed.data[, !(names(processed.data) %in% setdiff(drop.cols, "Bronze"))])

summary(total.mdl)

# Extreme in Y
alpha <- 0.05
n <- nobs(total.mdl)
p <- length(coef(total.mdl))

processed.data.total <- processed.data[, !(names(processed.data) %in% setdiff(drop.cols, "Total"))]
y.extremes <- processed.data.total[abs(rstudent(total.mdl)) >= qt(1 - alpha/n, n - p - 1), ]
nrow(y.extremes)

# Extreme in X
x.extremes <- processed.data.total[hatvalues(total.mdl) >= 2*p, ]
nrow(x.extremes)

##  Influential points
# DFFITS
dffits.ex <- processed.data[abs(dffits(total.mdl)) >= 1, ] # DFFITS, cutoff |DFFITS| >= 1
nrow(dffits.ex)
write.csv(dffits.ex[, c("NOC", "Year")], "./analysis/DFFITS.csv", row.names = FALSE)

# Cook's Distance D, cutoff |D| >= F(0.5, p, n - p)
cd.ex <- processed.data[cooks.distance(total.mdl) >= qf(0.5, p, n - p), ] # Cook's D
nrow(cd.ex)
write.csv(cd.ex[, c("NOC", "Year")], "./analysis/cooks_distance.csv", row.names = FALSE)

# DFBETAS, cutoff |DFBETAS| >= 1
dfbetas.ex <- processed.data.total[abs(dfbetas(total.mdl)) >= 1]
length(dfbetas.ex)

all.values <- data.frame()

preds <- names(coef(total.mdl))
preds <- setdiff(preds, "(Intercept)")
p.values <- rep(0, length(preds))
names(p.values) <- preds

for (pred.name in preds) {
  pred <- na.omit(unlist(processed.data[[pred.name]], use.names = FALSE))
  med <- median(pred)
  # Next, we calculate the values needed to get $s^2$ and $t^*$
  res <- resid(total.mdl)
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

all.values <- rbind(all.values, p.values)

# Generate latex tables
library(stargazer)

stargazer(bronze.mdl, silver.mdl, gold.mdl, total.mdl, type='latex')

total.slr.sports <- lm(Total ~ Sports, data = processed.data)
total.slr.events <- lm(Total ~ Events, data = processed.data)

stargazer(total.slr.sports, total.slr.events,
          type='latex', report=('vc*p'))

library(car)

VIFs <- cbind(vif(bronze.mdl), vif(silver.mdl), vif(gold.mdl), vif(total.mdl))
colnames(VIFs) <- medal.colnames
stargazer(vif(total.mdl), type='latex', flip = TRUE)

VIF.diffs <- VIF - vif(total.mdl)
write.csv(VIFs, "./analysis/vifs.csv", row.names = FALSE)
write.csv(VIF.diffs, "./analysis/vif_diffs.csv", row.names = FALSE)