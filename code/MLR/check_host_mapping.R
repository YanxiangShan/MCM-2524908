medal.tables <- read.csv("./2025_Problem_C_Data/summerOly_medal_counts.csv")
hosts     <- read.csv("./2025_Problem_C_Data/summerOly_hosts.csv", encoding="UTF-8")
# From https://stackoverflow.com/a/46783671/18031673: Replace UTF-8 non-breaking spaces
# with standard ASCII spaces
hosts <- as.data.frame(lapply(hosts, function(x) {
              gsub("\u00A0", "", x) 
         })) 
colnames(hosts) <- c("Year", "Host")

countries <- unique(medal.tables$NOC)

hosts.unique <- unique(hosts$Host)
hosts.not.cancelled <- hosts$Host[!grepl("Cancelled", hosts$Host)]
hosts.no.parentheses <- gsub("\\s*\\(.*?\\)", "", hosts.not.cancelled)
hosts.no.city <- gsub(".*,\\s*", "", hosts.no.parentheses)
host.countries <- unique(hosts.no.city)

setdiff(host.countries, countries)

# grepl(countries[1], hosts$Host[!grepl("Cancelled", hosts$Host)])
