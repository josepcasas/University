rm(list=ls())
setwds("/Users/josepcasas/Documents/bgse/term3/isp/data/")
require(lubridate) # for the year function
require(plyr)




#------------------------------------------------------------------------------------------------------------------------------------------
# Functions
#------------------------------------------------------------------------------------------------------------------------------------------

yearlyGrowth <- function(y1, y2){
  g = (y2-y1)/y1
  return(g)
}

countryGrowth <- function(vector){
  g = rep(0, length(vector))
  i = 2
  while (i <= length(vector)){
    g[i] = yearlyGrowth(vector[i-1],vector[i])
    i = i + 1
  }
  return(g)
}


#------------------------------------------------------------------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------------------------------------------------------------------


# imports the pen world table data. 
# Column 2 is REGION CODE
# column 3 is YEAR
# Column 4 is GDP
pwt <- read.csv("/Users/josepcasas/Documents/bgse/bgse-code/Indepent Study Project/data-v2/pwt.csv")

pwt <- pwt[,2:4]

names(pwt) <- c("country", "year", "GDP")

# import the Reinhart and Trebesch database (many variables)
haircuts <- read.csv("/Users/josepcasas/Documents/bgse/bgse-code/Indepent Study Project/data-v2/haircut.csv")

# select only country code, date and haircut from the original haircut database
haircuts <- data.frame(haircuts$Code, haircuts$Date, haircuts$Preferred.Haircut.HSZ)

# change the names to make more readable
names(haircuts) <- c("country", "date", "haircut")

# make date a date format. Disregard day
haircuts$date <- as.Date(haircuts$date)

# create a year variable
haircuts$year <- year(haircuts$date)

# merge the pwt with the haircuts data frames
dataset <- merge(pwt, haircuts, by=c("country", "year"), all = TRUE)

# create a dummy for when a country restructures
dataset$restructure <- ifelse(is.na(dataset$haircut), rep(0, length(dataset$haircut)), rep(1, length(dataset$haircut)))


# calculates the growth rate variable
growth <- c()
for (country in levels(dataset$country)){
  episodes
  growth <- c(growth, countryGrowth(dataset$GDP[which(dataset$country==country)]))
}

# add the variable to the dataset data.frame
dataset$growth <- growth

# imports the IMF dataset. The data has year in columns and countries in rows
debt <- read.csv("/Users/josepcasas/Documents/bgse/bgse-code/Indepent Study Project/data-v2/debt.csv")

# transpose the dataframe
debt <- as.data.frame(t(debt))
names(debt) <- debt[1,]

# imf uses number codes for countries. This file contains the dictionary for the number codes
codes <- read.csv("/Users/josepcasas/Documents/bgse/bgse-code/Indepent Study Project/data-v2/codes.csv")

dataset$code <- mapvalues(dataset$country, from = codes$ISO3A, to = codes$IMF3N)

for 
