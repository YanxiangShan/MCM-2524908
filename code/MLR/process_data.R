setwd("C:/Users/danim/Dev/R")

athletes     <- read.csv("./2025_Problem_C_Data/summerOly_athletes.csv")
medal.tables <- read.csv("./2025_Problem_C_Data/summerOly_medal_counts.csv")

hosts        <- read.csv("./2025_Problem_C_Data/summerOly_hosts.csv", encoding="UTF-8")
# From https://stackoverflow.com/a/46783671/18031673: Replace UTF-8 non-breaking spaces
# with standard ASCII spaces
hosts <- as.data.frame(lapply(hosts, function(x) {
              gsub("\u00A0", "", x) 
         })) 
colnames(hosts) <- c("Year", "Host")


countries <- unique(medal.tables$NOC)

# Create mapping from country names from "medal_counts" to IOC country codes
country.to.iocs <- vector("list", length(countries))
names(country.to.iocs) <- countries

for (country in countries) {
  # find matches for when team names from "athletes" have the same as country name
  # as the one from "medal_counts" in this iteration
  iocs <- unique(athletes[athletes$Team == country, "NOC"])
  if (length(iocs) != 0) {
    country.to.iocs[[country]] <- iocs
  }
}

# Needed operation to calculate average sex
library(dplyr)

athletes[, "Sex"]  <- recode(athletes[, "Sex"], F = 0, M = 1)


stats <- data.frame() # All data will be stored here
  
years <- unique(athletes[, "Year"])
years <- sort(years[years %% 4 == 0]) # Removes 1906 entry that does not appear in "medal_counts"
for (year in years) {
  year.athletes <- athletes[athletes$Year == year, ]
  year.stats <- data.frame()
  
  year.location <- hosts[hosts$Year == year, "Host"]
  
  # For each sport, count how many countries participated in it
  sport.names <- unique(year.athletes$Sport)
  sport.counts <- rep(0, length(sport.names))
  names(sport.counts) <- sport.names
  
  for (sport in sport.names) {
    sport.counts[sport] <- length(year.athletes[year.athletes$Sport == sport, "NOC"])
  }
  
  # For each event, count how many countries participated in it
  event.names <- unique(year.athletes$Event)
  event.counts <- rep(0, length(event.names))
  names(event.counts) <- event.names
  
  for (event in event.names) {
    event.counts[event] <- length(year.athletes[year.athletes$Event == event, "NOC"])
  }
  
  year.countries <- medal.tables[medal.tables$Year == year, "NOC"]
  nocs <- unique(year.athletes$NOC)
  for (country in year.countries) {
    iocs <- country.to.iocs[[country]]
    # Skip countries without known IOC
    if (is.null(iocs)) {
      year.countries <- year.countries[year.countries != country]
      next
    } 
    country.year.athletes <- year.athletes[year.athletes$NOC %in% iocs, ]
    
    no.athletes <- length(unique(country.year.athletes$Name))
    no.sports <- length(unique(country.year.athletes$Sport))
    no.events <- length(unique(country.year.athletes$Event))
    
    test.name <- country # use to test if name is in host location's name
    # Special Case
    if (country == "Great Britain") {
      test.name <- "United Kingdom"
    }
    is.host <- as.integer(grepl(test.name, year.location))
  
    mean.sex <- mean(country.year.athletes$Sex)
    # How many other countries participated in each sport/event? The idea is that the lower these 
    # metric, the more unique the choice of sports/events were with respect to which sports/events 
    # the country decided to participate in this year. 
    mean.sport_participation <- mean(sport.counts[country.year.athletes$Sport])
    mean.event_participation <- mean(event.counts[country.year.athletes$Event])
    
    last.year.medal.tables.row <- medal.tables[medal.tables$NOC == country & medal.tables$Year == year - 4, ]
    last.year.medals <- unlist(last.year.medal.tables.row[, c("Bronze", "Silver", "Gold")])
    if (!length(last.year.medals)) {
      last.year.medals <- rep(NA, 3)
    }
    
    medal.tables.row <- medal.tables[medal.tables$NOC == country & medal.tables$Year == year, ]
    medals <- unlist(medal.tables.row[, c("Bronze", "Silver", "Gold", "Total")])
    rank <- medal.tables.row[, "Rank"]
    
    country.year.stats <-  c(country, year,
                             no.athletes, no.sports, no.events, 
                             is.host, 
                             mean.sex, mean.sport_participation, mean.event_participation, 
                             # all.prev.bronze.medals, all.prev.silver.medals, all.prev.gold.medals, all.prev.total.medals, 
                             # bronze.medals.last.years, silver.medals.last.years, gold.medals.last.years, total.medals.last.years, 
                             last.year.medals,
                             medals,
                             rank)

    year.stats <- rbind(year.stats, country.year.stats)
    colnames(year.stats) <-  c("NOC", "Year",    
                               "Athletes", "Sports", "Events",   
                               "Is host",   
                               "Mean Sex", "Mean Sport Participation",  "Mean Event Participation",   
                               # "All Previous Bronze Medals", "All Previous Silver Medals", "All Previous Gold Medals", "All Previous Total Medals",   
                               # "Bronze Medals Last 3 Olympics", "Silver Medals Last 3 Olympics", "Gold Medals Last 3 Olympics", "Total Medals Last 3 Olympics",   
                               "Bronze Last Olympics", "Silver Last Olympics", "Gold Last Olympics",
                               "Bronze", "Silver", "Gold", "Total",   
                               "Rank")
  }
  
  stats <- rbind(stats, year.stats)
}

write.csv(stats, "./processed_data.csv", row.names = FALSE)
