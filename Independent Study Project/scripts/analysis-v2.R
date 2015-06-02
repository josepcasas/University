#------------------------------------------------------------------------------------------------------------------------------------------
# Initialize
#------------------------------------------------------------------------------------------------------------------------------------------
rm(list=ls())
setwd("/Users/josepcasas/Documents/bgse/bgse-code/Independent Study Project/results/")
library(plm)
library(stargazer)
library(sandwich)
library(lmtest)
library(ggplot2)
library(gtools)


# Use diego's dataset. Much better than mine.
#Load alternative dataset
dataset <- read.csv("~/Documents/bgse/bgse-code/Independent Study Project/data/dataset-v2.csv")
names(dataset) <- c("wb", "group", "country.name","ifs", "country", "year", "hc", "gdp", "capital", "haircut3", "haircut4", "debtres", "restructure", "restructure2", "haircut", "haircut2", "bankcrisis", "default", "pop", "workshare1", "workshare2", "institutions1", "institutions2","fxregime", "fxregime2", "log.capital", "log.hc", "log.workshare1", "growth", "cpi", "cpi20", "cpi40", "haircutsq", "haircutcube", "haircut2sq")


#------------------------------------------------------------------------------------------------------------------------------------------
# A note on robust standard errors in R
#------------------------------------------------------------------------------------------------------------------------------------------
# I was not sure on how to do the robust standard errors in R, specially in FE and Arellano Bond.
# I tried the following specification with the equivalent in Stata

# R
# test <- plm(growth ~ gdp, data = dataset, index=c("code.v2", "year"),  model="within")
# testr <- coeftest(test, vcov.=vcovHC(test))

# Stata
# xtset codev2 year
# xtreg growth gdp, fe
# xtreg growth gdp, fe r

# I obtained similar results (small difference in rounding) with the difference that I think Stata includes a constant and R does 
# demaning. But I am not 100% sure of this. 

# The regressions without r at the end of the varible name are WITHOUT robut standard errors.
# The regressions with r at the end of the variable name are WITH robust standard errors.
# Therefore, it is advised always to look at the regression that end with r. 

# Without further ado, the regressions:
#------------------------------------------------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------------------------------
# Parsimonious Fixed Effects
#------------------------------------------------------------------------------------------------------------------------------------------

par1 <- plm(growth ~ diff(log(capital)) + log(hc) + workshare1 + cpi20, data = dataset, index=c("country", "year"),  model="within", effect = "individual")
par1r <- coeftest(par1, vcov.=vcovHC(par1))
par1r

par2 <- plm(growth ~ diff(log(capital)) + log(hc) + workshare1 + cpi20 + haircut, data = dataset, index=c("country", "year"),  model="within", effect = "individual")
par2r <- coeftest(par2, vcov.=vcovHC(par2))
par2r

par3 <- plm(growth ~ diff(log(capital)) + log(hc) + workshare1 + cpi20 + haircut + I(haircut^2), data = dataset, index=c("country", "year"),  model="within", effect = "individual")
par3r <- coeftest(par3, vcov.=vcovHC(par3))
par3r

par4 <- plm(growth ~ diff(log(capital)) + log(hc) + workshare1 + cpi20 + haircut + I(haircut^2) + institutions2, data = dataset, index=c("country", "year"),  model="within", effect = "individual")
par4r <- coeftest(par4, vcov.=vcovHC(par4))
par4r

par5 <- plm(growth ~ diff(log(capital)) + log(hc) + workshare1 + cpi20 + haircut + I(haircut^2) + institutions2 + default, data = dataset, index=c("country", "year"),  model="within", effect = "individual")
par5r <- coeftest(par5, vcov.=vcovHC(par5))
par5r

par6 <- plm(growth ~ diff(log(capital)) + log(hc) + workshare1 + cpi20 + haircut + I(haircut^2) + institutions2 + default + bankcrisis, data = dataset, index=c("country", "year"),  model="within", effect = "individual")
par6r <- coeftest(par6, vcov.=vcovHC(par6))
par6r

stargazer(par1r, par2r, par3r, par4r, par5r, par6r, align=TRUE, type = "text")


#------------------------------------------------------------------------------------------------------------------------------------------
# Complete Fixed Effects
#------------------------------------------------------------------------------------------------------------------------------------------

fe1 <- plm(growth ~ log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi, data = dataset, index=c("country", "year"),  model="within", effect = "individual")
fe1r <- coeftest(fe1, vcov.=vcovHC(fe1))
fe1r

fe2 <- plm(growth ~ log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + restructure, data = dataset, index=c("country", "year"),  model="within", effect = "individual")
fe2r <- coeftest(fe2, vcov.=vcovHC(fe2))
fe2r

fe3 <- plm(growth ~ log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + restructure + haircut, data = dataset, index=c("country", "year"),  model="within", effect = "individual")
fe3r <- coeftest(fe3, vcov.=vcovHC(fe3))
fe3r

fe4 <- plm(growth ~ log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + restructure + haircut + I(haircut^2), data = dataset, index=c("country", "year"),  model="within", effect = "individual")
fe4r <- coeftest(fe4, vcov.=vcovHC(fe4))
fe4r

fe5 <- plm(growth ~ log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3), data = dataset, index=c("country", "year"),  model="within", effect = "individual")
fe5r <- coeftest(fe5, vcov.=vcovHC(fe5))
fe5r

fe6 <- plm(growth ~ log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + fxregime, data = dataset, index=c("country", "year"),  model="within", effect = "individual")
fe6r <- coeftest(fe6, vcov.=vcovHC(fe6))
fe6r

fe7 <- plm(growth ~ log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + fxregime + haircut*fxregime, data = dataset, index=c("country", "year"),  model="within", effect = "individual")
fe7r <- coeftest(fe7, vcov.=vcovHC(fe7))
fe7r

fe8 <- plm(growth ~ log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + fxregime+ haircut*fxregime + bankcrisis, data = dataset, index=c("country", "year"),  model="within", effect = "individual")
fe8r <- coeftest(fe8, vcov.=vcovHC(fe8))
fe8r

fe9 <- plm(growth ~ log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + fxregime + haircut*fxregime + bankcrisis + default, data = dataset, index=c("country", "year"),  model="within", effect = "individual")
fe9r <- coeftest(fe9, vcov.=vcovHC(fe9))
fe9r

fe10 <- plm(growth ~ log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + fxregime + haircut*fxregime + bankcrisis + default + institutions2, data = dataset, index=c("ifs", "year"),  model="within", effect = "individual")
fe10r <- coeftest(fe10, vcov.=vcovHC(fe10))
fe10r

stargazer(fe1r, fe2r, fe3r, fe4r, fe5r, fe6r, fe7r, fe8r, fe9r, align=TRUE, type = "text")



#------------------------------------------------------------------------------------------------------------------------------------------
# Complete Dynamic + Fixed Effects
#------------------------------------------------------------------------------------------------------------------------------------------

dyn1 <- plm(growth ~ lag(growth) + lag(growth, k=2) + log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi, data = dataset, index=c("country", "year"),  model="within", effect = "individual", method = "arellano")
dyn1r <- coeftest(dyn1, vcov.=vcovHC(dyn1))
dyn1r

dyn2 <- plm(growth ~ lag(growth) + lag(growth, k=2) + log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + restructure, data = dataset, index=c("country", "year"),  model="within", effect = "individual", method = "arellano")
dyn2r <- coeftest(dyn2, vcov.=vcovHC(dyn2))
dyn2r

dyn3 <- plm(growth ~ lag(growth) + lag(growth, k=2) + log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + restructure + haircut, data = dataset, index=c("country", "year"),  model="within", effect = "individual", method = "arellano")
dyn3r <- coeftest(dyn3, vcov.=vcovHC(dyn3))
dyn3r

dyn4 <- plm(growth ~ lag(growth) + lag(growth, k=2) + log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + restructure + haircut + I(haircut^2), data = dataset, index=c("country", "year"),  model="within", effect = "individual", method = "arellano")
dyn4r <- coeftest(dyn4, vcov.=vcovHC(dyn4))
dyn4r

dyn5 <- plm(growth ~ lag(growth) + lag(growth, k=2) + log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3), data = dataset, index=c("country", "year"),  model="within", effect = "individual", method = "arellano")
dyn5r <- coeftest(dyn5, vcov.=vcovHC(dyn5))
dyn5r

dyn6 <- plm(growth ~ lag(growth) + lag(growth, k=2) + log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + fxregime, data = dataset, index=c("country", "year"),  model="within", effect = "individual", method = "arellano")
dyn6r <- coeftest(dyn6, vcov.=vcovHC(dyn6))
dyn6r

dyn7 <- plm(growth ~ lag(growth) + lag(growth, k=2) + log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + fxregime + haircut*fxregime, data = dataset, index=c("country", "year"),  model="within", effect = "individual", method = "arellano")
dyn7r <- coeftest(dyn7, vcov.=vcovHC(dyn7))
dyn7r

dyn8 <- plm(growth ~ lag(growth) + lag(growth, k=2) + log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + fxregime+ haircut*fxregime + bankcrisis, data = dataset, index=c("country", "year"),  model="within", effect = "individual", method = "arellano")
dyn8r <- coeftest(dyn8, vcov.=vcovHC(dyn8))
dyn8r

dyn9 <- plm(growth ~ lag(growth) + lag(growth, k=2) + log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + fxregime + haircut*fxregime + bankcrisis + default, data = dataset, index=c("country", "year"),  model="within", effect = "individual", method = "arellano")
dyn9r <- coeftest(dyn9, vcov.=vcovHC(dyn9))
dyn9r

dyn10 <- plm(growth ~ lag(growth) + lag(growth, k=2) + log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + fxregime + haircut*fxregime + bankcrisis + default + institutions2, data = dataset, index=c("country", "year"),  model="within", effect = "individual", method = "arellano")
dyn10r <- coeftest(dyn10, vcov.=vcovHC(dyn10))
dyn10r

stargazer(dyn1, dyn2, dyn3, dyn4, dyn5, dyn6, dyn7, dyn8, dyn9, align=TRUE)
stargazer(dyn1r, dyn2r, dyn3r, dyn4r, dyn5r, dyn6r, dyn7r, dyn8r, dyn9r, align=TRUE, type = "text")



#------------------------------------------------------------------------------------------------------------------------------------------
# Robutstness Checks
#------------------------------------------------------------------------------------------------------------------------------------------


# Only >1970 data
dataset <- read.csv("~/Documents/bgse/bgse-code/Independent Study Project/data/dataset-v2.csv")
names(dataset) <- c("wb", "group", "ifs", "country", "country.name", "year", "hc", "gdp", "capital", "haircut3", "haircut4", "debtres", "restructure", "restructure2", "haircut", "haircut2", "bankcrisis", "default", "pop", "workshare1", "workshare2", "institutions1", "institutions2","fxregime", "fxregime2", "log.capital", "log.hc", "log.workshare1", "growth", "cpi", "cpi20", "cpi40", "haircutsq", "haircutcube", "haircut2sq")
dataset <- dataset[which(dataset$year >= 1970),]

par6rob1 <- plm(growth ~ diff(log(capital)) + log(hc) + workshare1 + cpi20 + haircut + I(haircut^2) + institutions2 + default + bankcrisis, data = dataset, index=c("country", "year"),  model="within", effect = "individual")
par6rob1r <- coeftest(par6rob1, vcov.=vcovHC(par6rob1))
par6rob1r

fe10rob1 <- plm(growth ~ log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + fxregime + haircut*fxregime + bankcrisis + default + institutions2, data = dataset, index=c("ifs", "year"),  model="within", effect = "individual")
fe10rob1r <- coeftest(fe10rob1, vcov.=vcovHC(fe10rob1))
fe10rob1r

dyn10rob1 <- plm(growth ~ lag(growth) + lag(growth, k=2) + log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + fxregime + haircut*fxregime + bankcrisis + default + institutions2, data = dataset, index=c("country", "year"),  model="within", effect = "individual", method = "arellano")
dyn10rob1r <- coeftest(dyn10rob1, vcov.=vcovHC(dyn10rob1))
dyn10rob1r


# OECD + HI + UMI
dataset <- read.csv("~/Documents/bgse/bgse-code/Independent Study Project/data/dataset-v2.csv")
names(dataset) <- c("wb", "group", "ifs", "country", "country.name", "year", "hc", "gdp", "capital", "haircut3", "haircut4", "debtres", "restructure", "restructure2", "haircut", "haircut2", "bankcrisis", "default", "pop", "workshare1", "workshare2", "institutions1", "institutions2","fxregime", "fxregime2", "log.capital", "log.hc", "log.workshare1", "growth", "cpi", "cpi20", "cpi40", "haircutsq", "haircutcube", "haircut2sq")
dataset <- dataset[which(dataset$group=="OECD" | dataset$group=="HI" | dataset$group=="UMI"),]

par6rob2 <- plm(growth ~ diff(log(capital)) + log(hc) + workshare1 + cpi20 + haircut + I(haircut^2) + institutions2 + default + bankcrisis, data = dataset, index=c("country", "year"),  model="within", effect = "individual")
par6rob2r <- coeftest(par6rob2, vcov.=vcovHC(par6rob2))
par6rob2r

fe10rob2 <- plm(growth ~ log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + fxregime + haircut*fxregime + bankcrisis + default + institutions2, data = dataset, index=c("ifs", "year"),  model="within", effect = "individual")
fe10rob2r <- coeftest(fe10rob2, vcov.=vcovHC(fe10rob2))
fe10rob2r

dyn10rob2 <- plm(growth ~ lag(growth) + lag(growth, k=2) + log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + fxregime + haircut*fxregime + bankcrisis + default + institutions2, data = dataset, index=c("country", "year"),  model="within", effect = "individual", method = "arellano")
dyn10rob2r <- coeftest(dyn10rob2, vcov.=vcovHC(dyn10rob2))
dyn10rob2r


# OECD + HI + UMI & Only >1970 data
dataset <- read.csv("~/Documents/bgse/bgse-code/Independent Study Project/data/dataset-v2.csv")
names(dataset) <- c("wb", "group", "ifs", "country", "country.name", "year", "hc", "gdp", "capital", "haircut3", "haircut4", "debtres", "restructure", "restructure2", "haircut", "haircut2", "bankcrisis", "default", "pop", "workshare1", "workshare2", "institutions1", "institutions2","fxregime", "fxregime2", "log.capital", "log.hc", "log.workshare1", "growth", "cpi", "cpi20", "cpi40", "haircutsq", "haircutcube", "haircut2sq")
dataset <- dataset[which(dataset$year >= 1970),]
dataset <- dataset[which(dataset$group=="OECD" | dataset$group=="HI" | dataset$group=="UMI"),]

par6rob3 <- plm(growth ~ diff(log(capital)) + log(hc) + workshare1 + cpi20 + haircut + I(haircut^2) + institutions2 + default + bankcrisis, data = dataset, index=c("country", "year"),  model="within", effect = "individual")
par6rob3r <- coeftest(par6rob3, vcov.=vcovHC(par6rob3))
par6rob3r

fe10rob3 <- plm(growth ~ log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + fxregime + haircut*fxregime + bankcrisis + default + institutions2, data = dataset, index=c("ifs", "year"),  model="within", effect = "individual")
fe10rob3r <- coeftest(fe10rob3, vcov.=vcovHC(fe10rob3))
fe10rob3r

dyn10rob3 <- plm(growth ~ lag(growth) + lag(growth, k=2) + log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + fxregime + haircut*fxregime + bankcrisis + default + institutions2, data = dataset, index=c("country", "year"),  model="within", effect = "individual", method = "arellano")
dyn10rob3r <- coeftest(dyn10rob3, vcov.=vcovHC(dyn10rob3))
dyn10rob3r

# set dataset back to normal
dataset <- read.csv("~/Documents/bgse/bgse-code/Independent Study Project/data/dataset-v2.csv")
names(dataset) <- c("wb", "group", "ifs", "country", "country.name", "year", "hc", "gdp", "capital", "haircut3", "haircut4", "debtres", "restructure", "restructure2", "haircut", "haircut2", "bankcrisis", "default", "pop", "workshare1", "workshare2", "institutions1", "institutions2","fxregime", "fxregime2", "log.capital", "log.hc", "log.workshare1", "growth", "cpi", "cpi20", "cpi40", "haircutsq", "haircutcube", "haircut2sq")

stargazer(par6, fe10, dyn10, par6rob1, fe10rob2, dyn10rob1, par6rob2, fe10rob2, dyn10rob2, par6rob3, fe10rob3, dyn10rob3, align=TRUE)
stargazer(par6r, fe10r, dyn10r, par6rob1r, fe10rob2r, dyn10rob1r, par6rob2r, fe10rob2r, dyn10rob2r, par6rob3r, fe10rob3r, dyn10rob3r, align=TRUE, type = "text")


#------------------------------------------------------------------------------------------------------------------------------------------
# HAIRCUT NON-LINEARITIES
#------------------------------------------------------------------------------------------------------------------------------------------

#including all observations
hnl1 <- ggplot(data = dataset[which(dataset$restructure==1),], aes(haircut, growth, label = country)) 
hnl1 <- hnl1 + geom_point(aes(size = gdp)) + geom_text(hjust=1.2, vjust=1.2)
hnl1 <- hnl1 + stat_smooth(method = "lm", formula = y ~ x + I(x^2),  aes(colour = "Quadratic Polynomial", fill = "Quadratic Polynomial"))
hnl1 <- hnl1 + stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), aes(colour = "Cubic Polynomial", fill =  "Cubic Polynomial"))
hnl1 <- hnl1 + scale_size(range = c(3, 13))
hnl1 <- hnl1 + labs(title = "Real GDP growth vs. Haircut", y = "Real GDP growth rate (%)", x = "Haircut size (%)")
hnl1 <- hnl1 + scale_fill_manual(name="Polynomial Fit", label = c("Cubic Polynomial", "Quadratic Polynomial"), values = c("#FF9999", "#56B4E9"))
hnl1 <- hnl1 + scale_colour_manual(name="Polynomial Fit", label = c("Cubic Polynomial", "Quadratic Polynomial"), values = c("#FF9999", "#56B4E9"))
hnl1
ggsave(file="haircut-growth-nonlinearities.png", plot = hnl1, width = 40, height = 20, units = "cm", dpi = 500)

#only high income countries
hnl2 <- ggplot(data = dataset[which(dataset$restructure==1 & (dataset$group=="OECD" | dataset$group=="HI" | dataset$group=="UMI") ),], aes(haircut, growth, label = country))
hnl2 <- hnl2 + geom_point(aes(size = gdp)) + geom_text(hjust=1.2, vjust=1.2)
hnl2 <- hnl2 + stat_smooth(method = "lm", formula = y ~ x + I(x^2),  aes(colour = "Quadratic Polynomial", fill = "Quadratic Polynomial"))
hnl2 <- hnl2 + stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), aes(colour = "Cubic Polynomial", fill =  "Cubic Polynomial"))
hnl2 <- hnl2 + scale_size(range = c(3, 13))
hnl2 <- hnl2 + labs(title = "Real GDP growth vs. Haircut", y = "Real GDP growth rate (%)", x = "Haircut size (%)")
hnl2 <- hnl2 + scale_fill_manual(name="Polynomial Fit", label = c("Cubic Polynomial", "Quadratic Polynomial"), values = c("#FF9999", "#56B4E9"))
hnl2 <- hnl2 + scale_colour_manual(name="Polynomial Fit", label = c("Cubic Polynomial", "Quadratic Polynomial"), values = c("#FF9999", "#56B4E9"))
hnl2
ggsave(file="haircut-growth-nonlinearities-high-income.png", plot = hnl2, width = 40, height = 20, units = "cm", dpi = 500)




#------------------------------------------------------------------------------------------------------------------------------------------
# Counterfactual functions
#------------------------------------------------------------------------------------------------------------------------------------------


plot.counterfactual <- function(dataset, country, year){
  
gdp <- counterfactual(country, year, dataset)
  
  country.gdp <- dataset[which(dataset$country==country & dataset$year %in% 2005:2011),]
  
  norm <- dataset$gdp[which(dataset$country==country & dataset$year == year)]
  gdp$gdp2 <- gdp$gdp/norm
  gdp$min2 <- gdp$min/norm
  gdp$max2 <- gdp$max/norm
  country.gdp$gdp2 <- country.gdp$gdp/norm
  
  colours <- c("orange", "palegreen3")  
  fxs = c(1,6)
  legendText = c("Flexible FX regime (1)", "Rigid FX regime (6)")
  cols <- c("Flexible FX regime (1)"="orange","Rigid FX regime (6)"="palegreen3")
  
  p <- ggplot()
  for (i in 1:length(fxs)){
    p <- p + geom_ribbon(data = gdp[which(gdp$fxregime == fxs[i]),], aes(show_guide = TRUE, x=year, ymin=min2, ymax=max2, fill = as.factor(fxregime)), linetype = 2,  alpha = 0.2)
    p <- p + geom_line(data = gdp[which(gdp$fxregime == fxs[i]),], aes(x = year, y=gdp2, group=haircut, colour = haircut))
    p <- p + geom_ribbon(data = gdp[which(gdp$fxregime == fxs[i]),], aes(show_guide = FALSE, x=year, ymin=min2, ymax=max2), color=colours[i], linetype = 2,  alpha = 0)
    p <- p + geom_line(data = country.gdp, aes(x = year, y = gdp2), size = 1.5, colour = "gray40")
    p <- p + geom_vline(aes(xintercept = year), linetype = 2, colour = "red", size = 1)
    p <- p + labs(title = paste (country, "counterfactual for restructure in", year, sep = " ", collapse = NULL), y = "Normalized Real GDP", x = "Year")
    p <- p + theme(axis.title.y = element_text(face = 'bold', size = 10, angle = 90))
    p <- p + theme(axis.title.x = element_text(face = 'bold', size = 10, angle = 00))
    p <- p + theme(title = element_text(face = 'bold', size = 10, angle = 00))
    
  }
  p <- p + scale_fill_manual(name="FX Regime",values=colours, labels = legendText)
  p <- p + scale_x_continuous(breaks = 2005:(year+5))
  ggsave(file=paste(country,"-",year,".png", sep = ""), width = 40, height = 20, units = "cm", dpi = 500)
  p
}


# This function will create the counterfactual graph for a given country and restructure year
counterfactual <- function(country, year, dataset){
  
  # Counterfactual for growth in t+0
  gt0 <- plm(growth ~ lag(growth) + lag(growth, k=2) + log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + fxregime + bankcrisis + default + institutions2 + haircut*fxregime, data = dataset, index=c("country", "year"),  model="within", effect = "individual", method = "arellano")
  gt0r <- coeftest(gt0, vcov.=vcovHC(gt0))
  
  # Counterfactual for growth in t+1
  gt1 <- plm(growth ~ lag(growth, k = 2) + lag(growth, k=3) + lag(log(pop)) + lag(workshare1) + lag(hc) + lag(log(gdp)) + lag(diff(log(capital))) + lag(cpi) + lag(restructure) + lag(haircut) + lag(I(haircut^2)) + lag(I(haircut^3)) + lag(fxregime) + lag(bankcrisis) + lag(default) + lag(institutions2) + lag(haircut * fxregime), data=dataset, index=c("country", "year"),  model="within", effect = "individual", methods="arellano")
  gt1r <- coeftest(gt1, vcov.=vcovHC(gt1))
  
  # Counterfactual for growth in t+2
  gt2 <- plm(growth ~ lag(growth, k = 3) + lag(growth, k=4) + lag(log(pop), k = 2) +  lag(workshare1, k = 2) + lag(hc, k = 2) + lag(log(gdp), k = 2) + lag(diff(log(capital)), k = 2) + lag(cpi, k = 2) + lag(restructure, k = 2) + lag(haircut, k = 2) + lag(I(haircut^2), k = 2) + lag(I(haircut^3), k = 2) + lag(fxregime, k = 2) + lag(bankcrisis, k = 2) + lag(default, k = 2) + lag(institutions2, k = 2) + lag(haircut * fxregime, k = 2), data=dataset, index=c("country", "year"),  model="within", effect = "individual", methods="arellano")
  gt2r <- coeftest(gt2, vcov.=vcovHC(gt2))
  
  # Counterfactual for growth in t+3
  gt3 <- plm(growth ~ lag(growth, k = 4) + lag(growth, k=5) + lag(log(pop), k = 3) + lag(workshare1, k = 3) + lag(hc, k = 3) + lag(log(gdp), k = 3) + lag(diff(log(capital)), k = 3) + lag(cpi, k = 3) + lag(restructure, k = 3) + lag(haircut, k = 3) + lag(I(haircut^2), k = 3) + lag(I(haircut^3), k = 3) + lag(fxregime, k = 3) + lag(bankcrisis, k = 3) + lag(default, k = 3) + lag(institutions2, k = 3) + lag(haircut * fxregime, k = 3), data=dataset, index=c("country", "year"),  model="within", effect = "individual", methods="arellano")
  gt3r <- coeftest(gt3, vcov.=vcovHC(gt3))
  
  # Counterfactual for growth in t+4
  gt4 <- plm(growth ~ lag(growth, k = 5) + lag(growth, k=6) + lag(log(pop), k = 4) + lag(workshare1, k = 4) + lag(hc, k = 4) + lag(log(gdp), k = 4) + lag(diff(log(capital)), k = 4) + lag(cpi, k = 4) + lag(restructure, k = 4) + lag(haircut, k = 4) + lag(I(haircut^2), k = 4) + lag(I(haircut^3), k = 4) + lag(fxregime, k = 4) + lag(bankcrisis, k = 4) + lag(default, k = 4) + lag(institutions2, k = 4) + lag(haircut * fxregime, k = 4), data=dataset, index=c("country", "year"),  model="within", effect = "individual", methods="arellano")
  gt4r <- coeftest(gt4, vcov.=vcovHC(gt4))
  
  
  growths <- data.frame(years[-1])
  results <- data.frame(c(0), c(0), c(0), c(0), c(0), c(0))
  names(results) <- c("year", "haircut", "fxregime", "gdp", "min", "max")
  
  
  # define some aux list
  years <- year:(year+5)
  haircuts <- seq(0, 1, by=.05)
  fxs <- seq(1,6, by=1)
  
  # loop through the five regressions
  # predict the growth rate
  # calculate the gdp
  
  for (k in 1:length(fxs)){
    
    for (i in 1:length(haircuts)){
      # initialize the vectors to store growth and gdp
      gs <- c()
      gdp <- c(dataset$gdp[which(dataset$country==country & dataset$year == year)])
      fe <- c()
      
      for (j in 1:(length(years))){
        
        # we need to calculate the FE for each period
        # NOTE: I do not know how to do it "nicer"
        if (j == 1){
          # obtain the country FE
          fes <- fixef(gt0, effect="individual")
          fe[j] = as.numeric(fes[which(names(fes)==country)])
          
          gs[j] <- fe[j] + predict.cf(gt0r, country.data(country, year, dataset, haircuts[i], fxs[k]))
          gdp[j+1] <- gdp[j]*(1+gs[j])
        }
        
        if (j == 2){
          # obtain the country FE
          fes <- fixef(gt1, effect="individual")
          fe[j] = as.numeric(fes[which(names(fes)==country)])
          
          gs[j] <- fe[j] + predict.cf(gt1r, country.data(country, year, dataset, haircuts[i], fxs[k]))
          gdp[j+1] <- gdp[j]*(1+gs[j])
        }
        
        if (j == 3){
          # obtain the country FE
          fes <- fixef(gt2, effect="individual")
          fe[j] = as.numeric(fes[which(names(fes)==country)])
          
          gs[j] <- fe[j] + predict.cf(gt2r, country.data(country, year, dataset, haircuts[i], fxs[k]))
          gdp[j+1] <- gdp[j]*(1+gs[j])
        }
        
        if (j == 4){
          # obtain the country FE
          fes <- fixef(gt3, effect="individual")
          fe[j] = as.numeric(fes[which(names(fes)==country)])
          
          gs[j] <- fe[j] + predict.cf(gt3r, country.data(country, year, dataset, haircuts[i], fxs[k]))
          gdp[j+1] <- gdp[j]*(1+gs[j])
        }
        
        if (j == 5){
          # obtain the country FE
          fes <- fixef(gt4, effect="individual")
          fe[j] = as.numeric(fes[which(names(fes)==country)])
          
          gs[j] <- fe[j] + predict.cf(gt4r, country.data(country, year, dataset, haircuts[i], fxs[k]))
          gdp[j+1] <- gdp[j]*(1+gs[j])
        }
      }
      
      newResults <- data.frame(years, rep(haircuts[i], length(years)), rep(fxs[k], length(years)), gdp, 0, 0)
      names(newResults) <- c("year", "haircut", "fxregime", "gdp", "min", "max")
      results <- rbind(results, newResults)
      print(paste0(as.character(round(length(results[,1])/757*100, digits = 3)),"%"))
    }
    
  }
  
  for (k in 1:length(fxs)){
    for (j in 1:(length(years))){
      results$max[which(results$fxregime == fxs[k] & results$year == years[j])] <- rep(max(results$gdp[which(results$fxregime == fxs[k] & results$year == years[j])]), length(results$max[which(results$fxregime == fxs[k] & results$year == years[j])]))
      results$min[which(results$fxregime == fxs[k] & results$year == years[j])] <- rep(min(results$gdp[which(results$fxregime == fxs[k] & results$year == years[j])]), length(results$max[which(results$fxregime == fxs[k] & results$year == years[j])]))
    }
  }
  
  #  names(results) <- c("Year", "Real", haircuts)
  #  results$max <- apply(results[,3:length(results)], 1, max)
  #  results$min <- apply(results[,3:length(results)], 1, min)
  #  print(growths)
  results <- results[2:length(results[,1]),]
  return(results)
}




# I am using ALL coefs (including the not significant ones to do the predict)
# WARNING: does not include the country fixed effects

predict.cf <- function(coefs, dataset){
  g = 0
  if(length(dataset) != length(coefs[,1])){
    return("Coefficients and data not the same lenght")
  }
  else{
    for (el in 1:length(coefs[,1])){
      if(coefs[el,4]<1){
        g0 = g
        g = g + coefs[el] * dataset[el]
        # for debugging
        # print(names(dataset)[el])
        # print(paste(as.character(g0),"+", as.character(coefs[el]), "x", as.character(dataset[el]), "=", as.character(g)))
      }
    }
  }
  return(as.numeric(g))
}


country.data <- function(country, year, dataset, haircut, fxregime){
  # growth ~ lag(growth) + lag(growth, k=2) + log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + fxregime + haircut*fxregime + bankcrisis + default + institutions2
  
  lag1.growth <- dataset$growth[which(dataset$country==country & dataset$year==year-1)]
  lag2.growth <- dataset$growth[which(dataset$country==country & dataset$year==year-2)]
  log.pop <- log(dataset$pop[which(dataset$country==country & dataset$year==year)])
  workshare <- dataset$workshare1[which(dataset$country==country & dataset$year==year)]
  log.hc <- log(dataset$hc[which(dataset$country==country & dataset$year==year)])
  log.gdp <- log(dataset$gdp[which(dataset$country==country & dataset$year==year)])
  
  diff.log.capital <- diff(log(dataset$capital))[which(dataset$country==country & dataset$year==year)]
  cpi <- dataset$cpi[which(dataset$country==country & dataset$year==year)]
  if (haircut != 0){
    restructure <- c(1)
  } else {
    restructure <- c(0)
  }
  haircut2 <- haircut^2
  haircut3 <- haircut^3
  bankcrisis <- dataset$bankcrisis[which(dataset$country==country & dataset$year==year)]
  default <- dataset$default[which(dataset$country==country & dataset$year==year)]
  institutions2 <- dataset$institutions2[which(dataset$country==country & dataset$year==year)]
  if (is.na(institutions2)){
    institutions2 = 0
  }
  fxhaircut <- haircut * fxregime
  
  dataset = data.frame(lag1.growth, lag2.growth, log.pop, workshare, log.hc, log.gdp, diff.log.capital, cpi, restructure, haircut, haircut2, haircut3, fxregime, bankcrisis, default, institutions2, fxhaircut)
  return(dataset)
}


#------------------------------------------------------------------------------------------------------------------------------------------
# Counterfactual grpahs
#------------------------------------------------------------------------------------------------------------------------------------------


countries <- c("GRC", "IRL", "ITA", "PRT", "ESP")
years <- c(2008, 2009, 2010)


for (country in countries){
  for (year in years){
    print(paste(country, year))
    plot.counterfactual(dataset, country, year)
  }
}







