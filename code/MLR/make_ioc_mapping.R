setwd("C:\\Users\\danim\\Downloads\\2025_MCM-ICM_Problems\\2025_Problem_C_Data\\2025_Problem_C_Data") 
  
athletes     <- read.csv("summerOly_athletes.csv")
medal.tables <- read.csv("summerOly_medal_counts.csv")

countries <- sort(unique(medal.tables[, "NOC"]))

country.to.nocs <- vector("list", length(countries))
names(country.to.nocs) <- countries

for (country in countries) {
  # find matches for when team names from "athletes" have the same as country name
  # as the one from "medal_counts" in this iteration
  nocs <- unique(athletes[athletes$Team == country, "NOC"])
  if (length(nocs) != 0) {
    country.to.nocs[[country]] <- nocs
  }
}
names(Filter(is.null, country.to.nocs))

noc.col <- athletes[, "NOC"]
nocs <- sort(unique(noc.col))
noc.to.country <- vector("list", length(nocs))
names(noc.to.country) <- nocs

for (noc in nocs) {
  found <- FALSE
  # Get all team names that have participated under this NOC
  noc.team.names <- unique(athletes[noc.col == noc, "Team"])
  for (tn in noc.team.names) {
    if (tn %in% countries) {
      noc.to.country[[noc]] <- c(noc.to.country[[noc]], tn)
      found <- TRUE
      next
    }
  }
  if (!found) {
      noc.to.country[noc] <- "not found"
  }
}

# noc.to.country <- noc.to.country[!(noc.to.country == "not found")]

noc.to.country