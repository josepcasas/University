#------------------------------------------------------------------------------------------------------------------------------------------
# Initialize
#------------------------------------------------------------------------------------------------------------------------------------------
rm(list=ls())
setwd("/Users/josepcasas/Documents/bgse/bgse-code/Indepent Study Project/results/")
library(plm)
library(stargazer)
library(sandwich)
library(lmtest)
library(ggplot2)

# Load the dataset
dataset <- read.csv("/Users/josepcasas/Documents/bgse/bgse-code/Indepent Study Project/data/dataset.csv")

# exclude the crisis years
dataset <- dataset[which(dataset$year<= 2008),]
# note: by excluding these years we loose significance power in the haircut (in the linear quadratic and cubic terms)



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
# FE Regression
#------------------------------------------------------------------------------------------------------------------------------------------


# Standard Growth Regression (FE)
growth1 <- plm(growth ~ as.factor(code.v2) + pop + hc + log(gdp) + log(capital) + productivity + cpi, data = dataset, index=c("code.v2", "year"),  model="within")
growth1r <- coeftest(growth1, vcov.=vcovHC(growth1))

# Standard Growth Regression (FE)
growth2 <- plm(growth ~ pop + hc + log(gdp) + log(capital) + productivity + cpi + restructure, data = dataset, index=c("code.v2", "year"),  model="within")
growth2r <- coeftest(growth2, vcov.=vcovHC(growth2))

# Standard Growth Regression + Haircut (FE)
growth3 <- plm(growth ~ pop + hc + log(gdp) + log(capital) + productivity + cpi + restructure + haircut, data=dataset, index=c("code.v2", "year"),  model="within")
growth3r <- coeftest(growth3, vcov.=vcovHC(growth3))

# Standard Growth Regression + Haircut + Haircut^2 (FE)
growth4 <- plm(growth ~ pop + hc + log(gdp) + log(capital) + productivity + cpi + restructure + haircut + I(haircut^2), data=dataset, index=c("code.v2", "year"),  model="within")
growth4r <- coeftest(growth4, vcov.=vcovHC(growth4))

# Standard Growth Regression + Haircut + Haircut^2 + Haircut^3 (FE)
growth5 <- plm(growth ~ pop + hc + log(gdp) + log(capital) + productivity + cpi + restructure + haircut + I(haircut^2) + I(haircut^3), data=dataset, index=c("code.v2", "year"),  model="within")
growth5r <- coeftest(growth5, vcov.=vcovHC(growth5))

# Standard Growth Regression + Haircut + Haircut^2 + Haircut^3 + debt (FE)
growth6 <- plm(growth ~ pop + hc + log(gdp) + log(capital) + productivity + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt, data=dataset, index=c("code.v2", "year"),  model="within")
growth6r <- coeftest(growth6, vcov.=vcovHC(growth6))

# Standard Growth Regression + Haircut + Haircut^2 + Haircut^3  + debt + fx (FE)
growth7 <- plm(growth ~ pop + hc + log(gdp) + log(capital) + productivity + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt + fx, data=dataset, index=c("code.v2", "year"),  model="within")
growth7r <- coeftest(growth7, vcov.=vcovHC(growth7))

# Same as 7 but pooled
growth8 <- plm(growth ~ pop + hc + log(gdp) + log(capital) + productivity + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt + fx, data=dataset, index=c("code.v2", "year"),  model="pooling")
growth8r <- coeftest(growth8, vcov.=vcovHC(growth8))

stargazer(growth1, growth2, growth3, growth4, growth5, growth6, growth7, growth8, title="Results", align=TRUE)
stargazer(growth1r, growth2r, growth3r, growth4r, growth5r, growth6r, growth7, growth8r, title="Results", align=TRUE)


#------------------------------------------------------------------------------------------------------------------------------------------
# Dynamic + FE Regression
#------------------------------------------------------------------------------------------------------------------------------------------

# Growth Regression (FE + 2 lag dynamic)
dyn1 <- plm(growth ~ lag(growth) + lag(growth, k=2) + pop + hc + log(gdp) + log(capital) + productivity + cpi, data = dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn1r <- coeftest(dyn1, vcov.=vcovHC(dyn1))

# Growth Regression + restructure (FE + 2 lag dynamic) 
dyn2 <- plm(growth ~ lag(growth) + lag(growth, k=2) + pop + hc + log(gdp) + log(capital) + productivity + cpi + restructure , data = dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn2r <- coeftest(dyn2, vcov.=vcovHC(dyn2))

# Standard Growth Regression + Haircut (FE + 2 lag dynamic) 
dyn3 <- plm(growth ~ lag(growth) + lag(growth, k=2) + pop + hc + log(gdp) + log(capital) + productivity + cpi + restructure + haircut, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn3r <- coeftest(dyn3, vcov.=vcovHC(dyn3))

# Standard Growth Regression + Haircut + haircut^2 (FE + 2 lag dynamic) 
dyn4 <- plm(growth ~ lag(growth) + lag(growth, k=2) + pop + hc + log(gdp) + log(capital) + productivity + cpi + restructure + haircut + I(haircut^2), data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn4r <- coeftest(dyn4, vcov.=vcovHC(dyn4))

# Standard Growth Regression + Haircut + haircut^2 + haircut^3 (FE + 2 lag dynamic) 
dyn5 <- plm(growth ~ lag(growth) + lag(growth, k=2) + pop + hc + log(gdp) + log(capital) + productivity + cpi + restructure + haircut + I(haircut^2) + I(haircut^3), data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn5r <- coeftest(dyn5, vcov.=vcovHC(dyn5))

# Standard Growth Regression + Haircut + haircut^2 + haircut^3 + debt (FE + 2 lag dynamic) 
dyn6 <- plm(growth ~ lag(growth) + lag(growth, k=2) + pop + hc + log(gdp) + log(capital) + productivity + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn6r <- coeftest(dyn6, vcov.=vcovHC(dyn6))

# Standard Growth Regression + Haircut + haircut^2 + haircut^3 + debt + fx (FE + 2 lag dynamic) 
dyn7 <- plm(growth ~ lag(growth) + lag(growth, k=2) + pop + hc + log(gdp) + log(capital) + productivity + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt + fx, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn7r <- coeftest(dyn7, vcov.=vcovHC(dyn7))

#------------------------------------------------------------------------------------------------------------------------------------------
# Prefered Specification
#------------------------------------------------------------------------------------------------------------------------------------------

# Standard Growth Regression + restructure + haircut + haircut2 + haircut3 + debt + fx (FE + Arellano)
dyn8 <- plm(growth ~ lag(growth) + lag(growth, k=2) + pop + hc + log(gdp) + log(capital) + productivity + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt + fx + haircut * fx, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn8r <- coeftest(dyn8, vcov.=vcovHC(dyn8))

stargazer(dyn1, dyn2, dyn3, dyn4, dyn5, dyn6, dyn7, dyn8, title="Dynamic + FE Results", align=TRUE)
stargazer(dyn1r, dyn2r, dyn3r, dyn4r, dyn5r, dyn6r, dyn7r, dyn8r, title="Dynamic + FE Results", align=TRUE)


#------------------------------------------------------------------------------------------------------------------------------------------
# Optimization graphs
#------------------------------------------------------------------------------------------------------------------------------------------
# We want to find the optimal haircut size (i.e. the haircut that leads to the higher growth rate). We will (force) fit a quadratic 
# polinomyal even if the cubic term is also significant. This happens because of a few outliers in the higher end of the haircuts.

# UPDATE: It turns out that we need the cubic term for haircut to be significant. I believe this is because the nature of the relationship
# between haircut and growth is cubic and therefore when we try to fit a quadratic model it becomes insignificant. But I might be wrong.

# Graph (including quadratic and cubic fit) of growth over haircut for all restructure events
g1 <- ggplot(dataset[which(dataset$restructure==1),], aes(haircut, growth, label=country) ) + geom_point()  + geom_text(hjust=1, vjust=1)
g1 <- g1 + stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1)
g1 <- g1 + stat_smooth(method = "lm", formula = y ~ poly(x, 3), size = 1, color = "red") # + theme_fivethirtyeight()
g1


# Graph (including quadratic and cubic fit) of growth over haircut for rich (above average gdp) restructure events
g2 <- ggplot(dataset[which(dataset$restructure==1 & dataset$gdp > mean(dataset$gdp)),], aes(haircut, growth, label=country) ) + geom_point()  + geom_text(hjust=1, vjust=1)
g2 <- g2 + stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1)
g2 <- g2 + stat_smooth(method = "lm", formula = y ~ poly(x, 3), size = 1, color = "red") # + theme_fivethirtyeight()
g2

# NOTE: We find a very clear quadratic shape when we restric the sample to high income countries (i.e. all those above the average gdp). 
# I do not know how to argue this results given the cubic nature of the regression (which we control for GDP levels). 


#------------------------------------------------------------------------------------------------------------------------------------------
# Counterfactuals
#------------------------------------------------------------------------------------------------------------------------------------------

# Counterfactual Regressions

# Counterfactual for growth in t+0
# Standard Growth Regression + restructure + haircut + haircut2 + haircut3 + debt + fx (FE + Arellano)
gt0 <- plm(growth ~ lag(growth) + lag(growth, k=2) + pop + hc + log(gdp) + log(capital) + productivity + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt + fx + haircut * fx, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
gt0r <- coeftest(gt0, vcov.=vcovHC(gt0))

# Counterfactual for growth in t+1
gt1 <- plm(growth ~ lag(growth, k = 2) + lag(growth, k=3) + lag(pop) + lag(hc) + lag(log(gdp)) + lag(log(capital)) + lag(productivity) + lag(cpi) + lag(restructure) + lag(haircut) + lag(I(haircut^2)) + lag(I(haircut^3)) + lag(debt) + lag(fx) + lag(haircut * fx), data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
gt1r <- coeftest(gt1, vcov.=vcovHC(gt1))

# Counterfactual for growth in t+2
gt2 <- plm(growth ~ lag(growth, k = 3) + lag(growth, k=4) + lag(pop, k = 2) + lag(hc, k = 2) + lag(log(gdp), k = 2) + lag(log(capital), k = 2) + lag(productivity, k = 2) + lag(cpi, k = 2) + lag(restructure, k = 2) + lag(haircut, k = 2) + lag(I(haircut^2), k = 2) + lag(I(haircut^3), k = 2) + lag(debt, k = 2) + lag(fx, k = 2) + lag(haircut * fx, k = 2), data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
gt2r <- coeftest(gt2, vcov.=vcovHC(gt2))

# Counterfactual for growth in t+3
gt3 <- plm(growth ~ lag(growth, k = 4) + lag(growth, k=5) + lag(pop, k = 3) + lag(hc, k = 3) + lag(log(gdp), k = 3) + lag(log(capital), k = 3) + lag(productivity, k = 3) + lag(cpi, k = 3) + lag(restructure, k = 3) + lag(haircut, k = 3) + lag(I(haircut^2), k = 3) + lag(I(haircut^3), k = 3) + lag(debt, k = 3) + lag(fx, k = 3) + lag(haircut * fx, k = 3), data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
gt3r <- coeftest(gt3, vcov.=vcovHC(gt3))

# Counterfactual for growth in t+4
gt4 <- plm(growth ~ lag(growth, k = 5) + lag(growth, k=6) + lag(pop, k = 4) + lag(hc, k = 4) + lag(log(gdp), k = 4) + lag(log(capital), k = 4) + lag(productivity, k = 4) + lag(cpi, k = 4) + lag(restructure, k = 4) + lag(haircut, k = 4) + lag(I(haircut^2), k = 4) + lag(I(haircut^3), k = 4) + lag(debt, k = 4) + lag(fx, k = 4) + lag(haircut * fx, k = 4), data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
gt4r <- coeftest(gt4, vcov.=vcovHC(gt4))




