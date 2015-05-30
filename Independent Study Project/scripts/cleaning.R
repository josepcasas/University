rm(list=ls())
setwd("/Users/josepcasas/Documents/bgse/bgse-code/Indepent Study Project/data/")
library(lubridate) # for the year function
library(plyr)
library(plm)



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
# Read data
#------------------------------------------------------------------------------------------------------------------------------------------

# PWT 8.1

# Due to PWT8.1 limitation it is not possible to export all the data in one file
africa <- read.csv("/Users/josepcasas/Documents/bgse/bgse-code/Indepent Study Project/data/africa.csv")
asia <- read.csv("/Users/josepcasas/Documents/bgse/bgse-code/Indepent Study Project/data/asia.csv")
europe <- read.csv("/Users/josepcasas/Documents/bgse/bgse-code/Indepent Study Project/data/europe.csv")
northamerica <- read.csv("/Users/josepcasas/Documents/bgse/bgse-code/Indepent Study Project/data/northamerica.csv")
oceania <- read.csv("/Users/josepcasas/Documents/bgse/bgse-code/Indepent Study Project/data/oceania.csv")
southamerica <- read.csv("/Users/josepcasas/Documents/bgse/bgse-code/Indepent Study Project/data/southamerica.csv")

#concatenate all the data into one data frame
pwt <- rbind(africa, asia, europe, northamerica, oceania, southamerica)


# create different data frames for each one of the variables
# pop = numger on people engaged (in millions)
# hc = human capital (barro/lee 2012, Psacharopoulos 1994)
# rdgpna = real gdp 2005 usd ppp
# rkna = real capital stock 2005 usd ppp
# rtfpna = real TFP
pop <- pwt[which(pwt$VariableCode=="emp"),2:4]
hc <- pwt[which(pwt$VariableCode=="hc"),2:4]
gdp <- pwt[which(pwt$VariableCode=="rgdpna"),2:4]
capital <- pwt[which(pwt$VariableCode=="rkna"),2:4]
productivity <- pwt[which(pwt$VariableCode=="rtfpna"),2:4]

# merge all the data into one data frame where each variable is one column
dataset <- merge(pop, hc, by=c("RegionCode", "YearCode"), all = TRUE)
dataset <- merge(dataset, gdp, by=c("RegionCode", "YearCode"), all = TRUE)
dataset <- merge(dataset, capital, by=c("RegionCode", "YearCode"), all = TRUE)
dataset <- merge(dataset, productivity, by=c("RegionCode", "YearCode"), all = TRUE)

# rename for clarity
names(dataset) <- c("country", "year", "pop", "hc", "gdp", "capital", "productivity")



#--------------------------------------------------------------------------------------
# Growth
## We calculate growth from the gdp variable

# calculates the growth rate variable
growth <- c()
for (country in levels(dataset$country)){
  growth <- c(growth, countryGrowth(dataset$gdp[which(dataset$country==country)]))
}

# add the variable to the dataset data.frame
dataset$growth <- growth



#--------------------------------------------------------------------------------------
# Reinhart and Trebesh, Haircut Dataset

# import the Reinhart and Trebesch database (many variables)
haircuts <- read.csv("/Users/josepcasas/Documents/bgse/bgse-code/Indepent Study Project/data/haircut-v2.csv")

# select only country code, date and haircut from the original haircut database
haircuts <- data.frame(haircuts$Code, haircuts$Date, haircuts$Preferred.Haircut.HSZ)

# change the names to make more readable
names(haircuts) <- c("country", "date", "haircut")

# make date a date format. Disregard day
haircuts$date <- as.Date(haircuts$date)

# create a year variable
haircuts$year <- year(haircuts$date)

# merge the pwt with the haircuts data frames
dataset <- merge(dataset, haircuts, by=c("country", "year"), all = TRUE)

# create a dummy for when a country restructures
dataset$restructure <- ifelse(is.na(dataset$haircut), rep(0, length(dataset$haircut)), rep(1, length(dataset$haircut)))

#--------------------------------------------------------------------------------------
# IMF country codes (number)
## IMF uses a different set of country codes. Before merging the debt data, we need to 
## map the codes


# imf uses number codes for countries. This file contains the dictionary for the number codes
codes <- read.csv("/Users/josepcasas/Documents/bgse/bgse-code/Indepent Study Project/data/codes.csv")

# map the imf codes to the iso contry codes
dataset$code <- mapvalues(dataset$country, from = codes$ISO3A, to = codes$IMF3N)
# Returns a warning for some countries:
# The following `from` values were not present in `x`: SMR, HTI, ARE, AFG, MMR,
# TLS, ERI, LBY, SSD, SLB, KIR, VUT, PNG, WSM, TON, MHL, FSM, TUV, UVK

#--------------------------------------------------------------------------------------
# Debt levels (from IMF)

# imports the IMF dataset. The data has year in columns and countries in rows
debt <- read.csv("/Users/josepcasas/Documents/bgse/bgse-code/Indepent Study Project/data/debt.csv")

# transpose the dataframe
debt <- as.data.frame(t(debt))
names(debt) <- debt[1,]


# We need to transform the debt data from cross sectional to panel data in order to merge it with the main dataset
years = 1692:2012

debt.v2.code = c()
debt.v2.year = c()
debt.v2.debt = c()

for (i in 1:length(debt[1,])){
  code = as.character(debt[1,i])
  debt.v2.code <- c(debt.v2.code, rep(code, length(years)))
  debt.v2.year <- c(debt.v2.year, years)
  debt.v2.debt <- c(debt.v2.debt, as.numeric(as.character(debt[2:length(debt[,i]),i])))
}

debt.v2 <- data.frame(debt.v2.code, debt.v2.year, debt.v2.debt)

names(debt.v2) <- c("code", "year", "debt")

# select only the data after 1950
debt.v2 <- debt.v2[which(debt.v2$year >= "1950"),]

# merge the debt dataset with the final dataset by code and year
dataset <- merge(dataset, debt.v2, by=c("code", "year"), all = TRUE)


#--------------------------------------------------------------------------------------
# FX regime (Reinhart+Rogoff)

  
# import the reinhart and roggoff fx dataset. Use the coarse column
fx <- read.csv("/Users/josepcasas/Documents/bgse/bgse-code/Indepent Study Project/data/fx.csv")

names(fx) <- c("country.name", "code", "year", "fx", "fine")

fx <- fx[which(fx$year>=1950),1:4]

dataset <- merge(dataset, fx, by=c("code", "year"), all= TRUE)


#--------------------------------------------------------------------------------------
# Inflation data

cpi <- read.csv("/Users/josepcasas/Documents/bgse/bgse-code/Indepent Study Project/data/inflation.csv")

cpi <- t(cpi)

cpi.country <- c()
cpi.year <- c()
cpi.values <- c()

years.v2 = 1980:2011

cpi.year <- rep(years.v2, length(cpi[1,])-2)

for (i in 1:(length(cpi[1,])-2)){
  country = as.character(cpi[1,i])
  cpi.country <- c(cpi.country, rep(country, length(years.v2)))
  values = as.numeric(cpi[8:length(cpi[,1])-1,i])
  cpi.values <- c(cpi.values, values)
}

cpi.v2 <- data.frame(cpi.country,cpi.year, cpi.values)

names(cpi.v2) <- c("country", "year", "cpi")

dataset <- merge(dataset, cpi.v2, by=c("country", "year"), all=TRUE)


#------------------------------------------------------------------------------------------------------------------------------------------
# Cleanup
#------------------------------------------------------------------------------------------------------------------------------------------

# reorder columns
dataset <- dataset[c("country.name", "country", "code", "year", "date", "pop", "hc", "gdp", "capital", "productivity", "growth","restructure", "haircut", "debt", "fx", "cpi")]

# limit observations to years >= 1970
dataset <- dataset[which(dataset$year>=1970),]
dataset <- dataset[which(dataset$year<2012),]


# The data has a bunch of tiny countries that are not on the pwt8.1 (the main data source) so we remove them. 
# These countries are of the type of san marino or yemen or micronesia
dataset<- dataset[!is.na(dataset$country),]


#we also neeed to remove all the countries without gdp data
dataset <- dataset[!is.na(dataset$gdp),]

# we need to deal for the lack of data in the soviet countries
ussr <- c("ARM", "AZE", "GEO", "KAZ", "KGZ", "TJK", "TKM", "UZB", "YEM", "BIH", "BLR", "CZE", "EST", "HRV", "LTU", "LVA", "MDA", "MKD", "MNE", "RUS", "SRB", "SVK", "SVN", "UKR")

dataset <- dataset[which(!(dataset$country %in% ussr)),]

# check all countries have all years
for (country in levels(dataset$country)){
  subset = dataset[which(dataset$country == country),]
  if (!is.na(subset$year[1])){
    if (subset$year[1] != 1970){
      print(country)
    }
  }
}

# if they did not have a restructure then the haircut is 0
dataset$haircut[is.na(dataset$haircut)] <- 0

dataset<- dataset[which(dataset$code != "MAC" | dataset$code!="BMU"),]

dataset$code.v2 <- as.numeric(dataset$code)



#------------------------------------------------------------------------------------------------------------------------------------------
# Save dataset
#------------------------------------------------------------------------------------------------------------------------------------------

write.table(dataset, file = "dataset.csv", sep = ",", col.names = names(dataset), row.names = FALSE)







