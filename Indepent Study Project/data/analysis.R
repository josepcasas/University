rm(list=ls())
setwd("/Users/josepcasas/Documents/bgse/term3/isp/data/")

# read the data
data <- read.csv("~/Documents/bgse/term3/isp/data/FebPwtExport592015.csv")

# separate the data by country
greece <- data[which(data$RegionCode == "GRC"),]
italy <- data[which(data$RegionCode == "ITA"),]
ireland <- data[which(data$RegionCode == "IRL"),]
portugal <- data[which(data$RegionCode == "PRT"),]
spain <- data[which(data$RegionCode == "ESP"),]

# create data frame with a variable for the gdp of each country
# delete year 1950 because no info for greece
# years 1951 - 2011
periods = length(spain$YearCode)
gdp <- data.frame(rev(spain$YearCode[2:periods]), ts(rev(greece$AggValue)), ts(rev(ireland$AggValue[2:periods])), ts(rev(italy$AggValue[2:periods])), ts(rev(portugal$AggValue[2:periods])), ts(rev(spain$AggValue[2:periods])))

#rename for ease
names(gdp)[1] <- "year"
names(gdp)[2] <- "greece"
names(gdp)[3] <- "ireland"
names(gdp)[4] <- "italy"
names(gdp)[5] <- "portugal"
names(gdp)[6] <- "spain"



greece <- c(0,(gdp$greece - lag(gdp$greece) ) / lag(gdp$greece))
ireland <- c(0,(gdp$ireland - lag(gdp$ireland) ) / lag(gdp$ireland))
italy <- c(0,(gdp$italy - lag(gdp$italy) ) / lag(gdp$italy))
portugal <- c(0,(gdp$portugal - lag(gdp$portugal) ) / lag(gdp$portugal))
spain <- c(0,(gdp$spain - lag(gdp$spain) ) / lag(gdp$spain))

# use all the data until 2007 (included)
# We cannot use after 2007 because the crisis had already started
g<- data.frame(gdp$year[5:length(greece)], greece[5:length(greece)], ireland[5:length(greece)], italy[5:length(greece)], portugal[5:length(greece)], spain[5:length(greece)])

#rename for ease
names(g)[1] <- "year"
names(g)[2] <- "greece"
names(g)[3] <- "ireland"
names(g)[4] <- "italy"
names(g)[5] <- "portugal"
names(g)[6] <- "spain"

countries <- c("greece", "ireland", "italy", "portugal", "spain")

summary <- data.frame(countries)
means <- data.frame(countries)
sds <- data.frame(countries)

country.mean50 <- c()
country.sd50 <- c()
country.mean60 <- c()
country.sd60 <- c()
country.mean70 <- c()
country.sd70 <- c()
country.mean80 <- c()
country.sd80 <- c()
country.mean90 <- c()
country.sd90 <- c()
country.mean00 <- c()
country.sd00 <- c()

for (country in g[2:length(g)]){
  country.mean50 = c(country.mean50, mean(country))
  country.sd50 = c(country.sd50, sd(country))
  country.mean60 = c(country.mean60, mean(country[which(g$year >= 1960)]))
  country.sd60 = c(country.sd60, sd(country[which(g$year >= 1960)]))
  country.mean70 = c(country.mean70, mean(country[which(g$year >= 1970)]))
  country.sd70 = c(country.sd70, sd(country[which(g$year >= 1970)]))
  country.mean80 = c(country.mean80, mean(country[which(g$year >= 1980)]))
  country.sd80 = c(country.sd80, sd(country[which(g$year >= 1980)]))
  country.mean90 = c(country.mean90, mean(country[which(g$year >= 1990)]))
  country.sd90 = c(country.sd90, sd(country[which(g$year >= 1990)]))
  country.mean00 = c(country.mean00, mean(country[which(g$year >= 2000)]))
  country.sd00 = c(country.sd00, sd(country[which(g$year >= 2000)]))
}

summary$mean50 <- country.mean50
summary$sd50 <- country.sd50
summary$mean60 <- country.mean60
summary$sd60 <- country.sd60
summary$mean70 <- country.mean70
summary$sd70 <- country.sd60
summary$mean80 <- country.mean80
summary$sd80 <- country.sd60
summary$mean90 <- country.mean90
summary$sd90 <- country.sd60
summary$mean00 <- country.mean00
summary$sd00 <- country.sd60

means$mean50 <- country.mean50
means$mean60 <- country.mean60
means$mean70 <- country.mean70
means$mean80 <- country.mean80
means$mean90 <- country.mean90
means$mean00 <- country.mean00
i = 1;
for (i in 1:length(means$countries)){
  means$max[i] <- max(means[i,3:4])
  means$min[i] <- min(means[i,2:7])
}


sds$sd50 <- country.sd50
sds$sd60 <- country.sd60
sds$sd70 <- country.sd70
sds$sd80 <- country.sd80
sds$sd90 <- country.sd90
sds$sd00 <- country.sd00

# save the data as csv
write.table(gdp, file = "gdp.csv", sep = ",", col.names = names(gdp), row.names = FALSE)
write.table(g, file = "g.csv", sep = ",", col.names = names(g), row.names = FALSE)
write.table(summary, file = "giips.csv", sep = ",", col.names = names(summary), row.names = FALSE)
write.table(means, file = "mean.csv", sep = ",", col.names = names(means), row.names = FALSE)
write.table(ses, file = "se.csv", sep = ",", col.names = names(ses), row.names = FALSE)
