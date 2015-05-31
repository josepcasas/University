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

# Load the dataset
dataset <- read.csv("/Users/josepcasas/Documents/bgse/bgse-code/Independent Study Project/data/dataset.csv")

# exclude the crisis years
# dataset <- dataset[which(dataset$year<= 2008),]
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
# Dynamic + FE Regression
#------------------------------------------------------------------------------------------------------------------------------------------



# Growth Regression (FE + 2 lag dynamic)
dyn1 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi, data = dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn1r <- coeftest(dyn1, vcov.=vcovHC(dyn1))

# Growth Regression + restructure (FE + 2 lag dynamic) 
dyn2 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure , data = dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn2r <- coeftest(dyn2, vcov.=vcovHC(dyn2))

# Standard Growth Regression + Haircut (FE + 2 lag dynamic) 
dyn3 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn3r <- coeftest(dyn3, vcov.=vcovHC(dyn3))

# Standard Growth Regression + Haircut + haircut^2 (FE + 2 lag dynamic) 
dyn4 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2), data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn4r <- coeftest(dyn4, vcov.=vcovHC(dyn4))

# Standard Growth Regression + Haircut + haircut^2 + haircut^3 (FE + 2 lag dynamic) 
dyn5 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3), data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn5r <- coeftest(dyn5, vcov.=vcovHC(dyn5))

# Standard Growth Regression + Haircut + haircut^2 + haircut^3 + debt (FE + 2 lag dynamic) 
dyn6 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn6r <- coeftest(dyn6, vcov.=vcovHC(dyn6))

# Standard Growth Regression + Haircut + haircut^2 + haircut^3 + debt + fx (FE + 2 lag dynamic) 
dyn7 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt + fxregime, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn7r <- coeftest(dyn7, vcov.=vcovHC(dyn7))

# Standard Growth Regression + restructure + haircut + haircut2 + haircut3 + debt + fx (FE + Arellano)
dyn8 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt + fxregime + fxrate, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn8r <- coeftest(dyn8, vcov.=vcovHC(dyn8))
dyn8r

dyn9 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt + fxregime + fxrate + institutions1, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn9r <- coeftest(dyn9, vcov.=vcovHC(dyn9))
dyn9r

# Standard Growth Regression + restructure + haircut + haircut2 + haircut3 + debt + fx (FE + Arellano)
dyn10 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt + fxregime + fxrate + institutions1 + haircut * fxregime, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn10r <- coeftest(dyn10, vcov.=vcovHC(dyn10))
dyn10r


# Standard Growth Regression + restructure + haircut + haircut2 + haircut3 + debt + fx (FE + Arellano)
dyn11 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt + fxregime + fxrate + institutions1 + haircut * fxregime + productivity, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn11r <- coeftest(dyn11, vcov.=vcovHC(dyn11))
dyn11r


stargazer(dyn1r, dyn2r, dyn3r, dyn4r, dyn5r, dyn6r, dyn7r, dyn8r, dyn9r, dyn10r, dyn11r, title="Dynamic + FE Results", align=TRUE, type = "text")

#------------------------------------------------------------------------------------------------------------------------------------------
# Alternatives specifications
#------------------------------------------------------------------------------------------------------------------------------------------

# Equivalent variables
# default ~ restructure ~ restructure2
# haircut ~ haircut2
# pop ~ workshare1 ~ workshare2
# institutions1 ~ institutions2


# Standard Growth Regression + restructure + haircut + haircut2 + haircut3 + debt + fx (FE + Arellano)
dyn12 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt + fxregime + fxrate + institutions1 + haircut * fxregime, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn12r <- coeftest(dyn12, vcov.=vcovHC(dyn12))
dyn12r

# Standard Growth Regression + restructure + haircut + haircut2 + haircut3 + debt + fx (FE + Arellano)
dyn13 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure2 + haircut + I(haircut^2) + I(haircut^3) + debt + fxregime + fxrate + institutions1 + haircut * fxregime, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn13r <- coeftest(dyn13, vcov.=vcovHC(dyn13))
dyn13r

# Standard Growth Regression + restructure + haircut + haircut2 + haircut3 + debt + fx (FE + Arellano)
dyn14 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + default + haircut + I(haircut^2) + I(haircut^3) + debt + fxregime + fxrate + institutions1 + haircut * fxregime, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn14r <- coeftest(dyn14, vcov.=vcovHC(dyn14))
dyn14r

#------------------------------------------------------------------------------------------------------------------------------------------

# Standard Growth Regression + restructure + haircut + haircut2 + haircut3 + debt + fx (FE + Arellano)
dyn15 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt + fxregime + fxrate + institutions1 + haircut * fxregime, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn15r <- coeftest(dyn15, vcov.=vcovHC(dyn15))
dyn15r

# Standard Growth Regression + restructure + haircut + haircut2 + haircut3 + debt + fx (FE + Arellano)
dyn16 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut2 + I(haircut^2) + I(haircut^3) + debt + fxregime + fxrate + institutions1 + haircut * fxregime, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn16r <- coeftest(dyn16, vcov.=vcovHC(dyn16))
dyn16r

#------------------------------------------------------------------------------------------------------------------------------------------

# Standard Growth Regression + restructure + haircut + haircut2 + haircut3 + debt + fx (FE + Arellano)
dyn17 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt + fxregime + fxrate + institutions1 + haircut * fxregime, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn17r <- coeftest(dyn17, vcov.=vcovHC(dyn17))
dyn17r

# Standard Growth Regression + restructure + haircut + haircut2 + haircut3 + debt + fx (FE + Arellano)
dyn18 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare2 + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt + fxregime + fxrate + institutions1 + haircut * fxregime, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn18r <- coeftest(dyn18, vcov.=vcovHC(dyn18))
dyn18r

# Standard Growth Regression + restructure + haircut + haircut2 + haircut3 + debt + fx (FE + Arellano)
dyn19 <- plm(growth ~ lag(growth) + lag(growth, k=2) + pop + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt + fxregime + fxrate + institutions1 + haircut * fxregime, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn19r <- coeftest(dyn19, vcov.=vcovHC(dyn19))
dyn19r

#------------------------------------------------------------------------------------------------------------------------------------------

# Standard Growth Regression + restructure + haircut + haircut2 + haircut3 + debt + fx (FE + Arellano)
dyn20 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt + fxregime + fxrate + institutions1 + haircut * fxregime, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn20r <- coeftest(dyn20, vcov.=vcovHC(dyn20))
dyn20r

# Standard Growth Regression + restructure + haircut + haircut2 + haircut3 + debt + fx (FE + Arellano)
dyn21 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt + fxregime + fxrate + institutions2 + haircut * fxregime, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn21r <- coeftest(dyn21, vcov.=vcovHC(dyn21))
dyn21r

# Standard Growth Regression + restructure + haircut + haircut2 + haircut3 + debt + fx (FE + Arellano)
dyn22 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt + fxregime + fxrate + institutions2 + haircut * fxregime + productivity, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
dyn22r <- coeftest(dyn22, vcov.=vcovHC(dyn22))
dyn22r

stargazer(dyn11r, dyn12r, dyn13r, dyn14r, dyn15r, dyn17r, dyn18r, dyn19r, dyn20r, dyn21r, dyn22r, title="Dynamic + FE Results", align=TRUE, type = "text")

#------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------
# Final Specification buildup
#------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------


final1 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
final1r <- coeftest(final1, vcov.=vcovHC(final1))
final1r

final2 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
final2r <- coeftest(final2, vcov.=vcovHC(final2))
final2r

final3 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
final3r <- coeftest(final3, vcov.=vcovHC(final3))
final3r

final4 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2), data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
final4r <- coeftest(final4, vcov.=vcovHC(final4))
final4r

final4b <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3), data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
final4br <- coeftest(final4b, vcov.=vcovHC(final4b))
final4br

final5 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
final5r <- coeftest(final5, vcov.=vcovHC(final5))
final5r

final6 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt + fxregime, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
final6r <- coeftest(final6, vcov.=vcovHC(final6))
final6r

final7 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt + fxregime + fxrate, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
final7r <- coeftest(final7, vcov.=vcovHC(final7))
final7r

final8 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt + fxregime + fxrate, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
final8r <- coeftest(final8, vcov.=vcovHC(final8))
final8r

final8b <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt + fxregime + fxrate + bankcrisis, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
final8br <- coeftest(final8b, vcov.=vcovHC(final8b))
final8br

final8c <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt + fxregime + fxrate + bankcrisis + default, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
final8cr <- coeftest(final8c, vcov.=vcovHC(final8c))
final8cr

final9 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt + fxregime + fxrate + bankcrisis + default + institutions2, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
final9r <- coeftest(final9, vcov.=vcovHC(final9))
final9r

final10 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt + fxregime + fxrate + bankcrisis + default + institutions2 + haircut * fxregime, data=dataset, index=c("code.v2", "year"),  model="within", methods="arellano")
final10r <- coeftest(final10, vcov.=vcovHC(final10))
final10r

#------------------------------------------------------------------------------------------------------------------------------------------
# FINAL LATEX OUTPUT
#------------------------------------------------------------------------------------------------------------------------------------------

stargazer(final1, final2, final3, final4, final4b, final5, final6, final7, final8, final9, final10, title="Dynamic + FE Results", align=TRUE, type="text")
stargazer(final1r, final2r, final3r, final4r, final4br, final5r, final6r, final7r, final8r, final8br, final8cr, final9r, final10r, title="Dynamic + FE Results", align=TRUE, type="text")




#------------------------------------------------------------------------------------------------------------------------------------------
# HAIRCUT NON-LINEARITIES
#------------------------------------------------------------------------------------------------------------------------------------------
# We want to find the optimal haircut size (i.e. the haircut that leads to the higher growth rate). We will (force) fit a quadratic 
# polinomyal even if the cubic term is also significant. This happens because of a few outliers in the higher end of the haircuts.

# UPDATE: It turns out that we need the cubic term for haircut to be significant. I believe this is because the nature of the relationship
# between haircut and growth is cubic and therefore when we try to fit a quadratic model it becomes insignificant. But I might be wrong.

# NOTE: We find a very clear quadratic shape when we restric the sample to high income countries (i.e. all those above the average gdp). 
# I do not know how to argue this results given the cubic nature of the regression (which we control for GDP levels). 

# UPDATE v2: We have finally found an regression specification that fits a qudratic model, we have included some new controls, including
# institution quality that are accounting for that tail at the end (i.e. they are countries with low institutional quality)

hnl1 <- ggplot(data = dataset[which(dataset$restructure==1),], aes(haircut, growth, label = country)) 
hnl1 <- hnl1 + geom_point(aes(size = gdp)) + geom_text(hjust=1.2, vjust=1.2)
hnl1 <- hnl1 + stat_smooth(method = "lm", formula = y ~ x + I(x^2),  aes(colour = "Quadratic Polynomial", fill = "Quadratic Polynomial"))
hnl1 <- hnl1 + stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), aes(colour = "Cubic Polynomial", fill =  "Cubic Polynomial"))
hnl1 <- hnl1 + scale_size(range = c(3, 13))
hnl1 <- hnl1 + labs(title = "Real GDP growth vs. Haircut", y = "Real GDP growth rate (%)", x = "Haircut size (%)")
hnl1 <- hnl1 + scale_fill_manual(name="Polynomial Fit", label = c("Quadratic Polynomial", "Cubic Polynomial"), values = c("#FF9999", "#56B4E9"))
#hnl1 <- hnl1 + scale_size_manual(name="Real GDP", labels = c("tiny economy (~3"))
hnl1
ggsave(file="haircut-growth-nonlinearities.png", plot = hnl1, width = 40, height = 20, units = "cm", dpi = 500)



hnl2 <- ggplot(data = dataset[which(dataset$restructure==1 & dataset$gdp > 250000),], aes(haircut, growth, label = country))
hnl2 <- hnl2 + geom_point(aes(size = gdp)) + geom_text(hjust=1.2, vjust=1.2)
hnl2 <- hnl2 + stat_smooth(method = "lm", formula = y ~ x + I(x^2), aes(colour = "Quadratic Polynomial", fill = "Quadratic Polynomial"))
hnl2 <- hnl2 + stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), aes(colour = "Cubic Polynomial", fill =  "Cubic Polynomial"))
hnl2 <- hnl2 + scale_size(range = c(3, 13))
hnl2
ggsave(file="haircut-growth-nonlinearities-highGDP.png", plot = hnl2, width = 40, height = 20, units = "cm", dpi = 500)


#------------------------------------------------------------------------------------------------------------------------------------------
# Counterfactuals
#------------------------------------------------------------------------------------------------------------------------------------------
# Counterfactual for growth in t+0
gt0 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt + fxregime + fxrate + institutions2 + haircut * fxregime, data=dataset, index=c("country", "year"),  model="within", methods="arellano")
gt0r <- coeftest(gt0, vcov.=vcovHC(gt0))

# Counterfactual for growth in t+1
gt1 <- plm(growth ~ lag(growth, k = 2) + lag(growth, k=3) + lag(workshare1) + lag(hc) + lag(log(gdp)) + lag(log(capital)) + lag(cpi) + lag(restructure) + lag(haircut) + lag(I(haircut^2)) + lag(I(haircut^3)) + lag(debt) + lag(fxregime) + lag(fxrate) + lag(institutions2) + lag(haircut * fxregime), data=dataset, index=c("country", "year"),  model="within", methods="arellano")
gt1r <- coeftest(gt1, vcov.=vcovHC(gt1))

# Counterfactual for growth in t+2
gt2 <- plm(growth ~ lag(growth, k = 3) + lag(growth, k=4) + lag(workshare1, k = 2) + lag(hc, k = 2) + lag(log(gdp), k = 2) + lag(log(capital), k = 2) + lag(cpi, k = 2) + lag(restructure, k = 2) + lag(haircut, k = 2) + lag(I(haircut^2), k = 2) + lag(I(haircut^3), k = 2) + lag(debt, k = 2) + lag(fxregime, k = 2) + lag(fxrate, k = 2) + lag(institutions2, k = 2) + lag(haircut * fxregime, k = 2), data=dataset, index=c("country", "year"),  model="within", methods="arellano")
gt2r <- coeftest(gt2, vcov.=vcovHC(gt2))

# Counterfactual for growth in t+3
gt3 <- plm(growth ~ lag(growth, k = 4) + lag(growth, k=5) + lag(workshare1, k = 3) + lag(hc, k = 3) + lag(log(gdp), k = 3) + lag(log(capital), k = 3) + lag(cpi, k = 3) + lag(restructure, k = 3) + lag(haircut, k = 3) + lag(I(haircut^2), k = 3) + lag(I(haircut^3), k = 3) + lag(debt, k = 3) + lag(fxregime, k = 3) + lag(fxrate, k = 3) + lag(institutions2, k = 3) + lag(haircut * fxregime, k = 3), data=dataset, index=c("country", "year"),  model="within", methods="arellano")
gt3r <- coeftest(gt3, vcov.=vcovHC(gt3))

# Counterfactual for growth in t+4
gt4 <- plm(growth ~ lag(growth, k = 5) + lag(growth, k=6) + lag(workshare1, k = 4) + lag(hc, k = 4) + lag(log(gdp), k = 4) + lag(log(capital), k = 4) + lag(cpi, k = 4) + lag(restructure, k = 4) + lag(haircut, k = 4) + lag(I(haircut^2), k = 4) + lag(I(haircut^3), k = 4) + lag(debt, k = 4) + lag(fxregime, k = 4) + lag(fxrate, k = 4) + lag(institutions2, k = 4) + lag(haircut * fxregime, k = 4), data=dataset, index=c("country", "year"),  model="within", methods="arellano")
gt4r <- coeftest(gt4, vcov.=vcovHC(gt4))



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
  gt0 <- plm(growth ~ lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt + fxregime + fxrate + institutions2 + haircut * fxregime, data=dataset, index=c("country", "year"),  model="within", methods="arellano")
  gt0r <- coeftest(gt0, vcov.=vcovHC(gt0))
  
  # Counterfactual for growth in t+1
  gt1 <- plm(growth ~ lag(growth, k = 2) + lag(growth, k=3) + lag(workshare1) + lag(hc) + lag(log(gdp)) + lag(log(capital)) + lag(cpi) + lag(restructure) + lag(haircut) + lag(I(haircut^2)) + lag(I(haircut^3)) + lag(debt) + lag(fxregime) + lag(fxrate) + lag(institutions2) + lag(haircut * fxregime), data=dataset, index=c("country", "year"),  model="within", methods="arellano")
  gt1r <- coeftest(gt1, vcov.=vcovHC(gt1))
  
  # Counterfactual for growth in t+2
  gt2 <- plm(growth ~ lag(growth, k = 3) + lag(growth, k=4) + lag(workshare1, k = 2) + lag(hc, k = 2) + lag(log(gdp), k = 2) + lag(log(capital), k = 2) + lag(cpi, k = 2) + lag(restructure, k = 2) + lag(haircut, k = 2) + lag(I(haircut^2), k = 2) + lag(I(haircut^3), k = 2) + lag(debt, k = 2) + lag(fxregime, k = 2) + lag(fxrate, k = 2) + lag(institutions2, k = 2) + lag(haircut * fxregime, k = 2), data=dataset, index=c("country", "year"),  model="within", methods="arellano")
  gt2r <- coeftest(gt2, vcov.=vcovHC(gt2))
  
  # Counterfactual for growth in t+3
  gt3 <- plm(growth ~ lag(growth, k = 4) + lag(growth, k=5) + lag(workshare1, k = 3) + lag(hc, k = 3) + lag(log(gdp), k = 3) + lag(log(capital), k = 3) + lag(cpi, k = 3) + lag(restructure, k = 3) + lag(haircut, k = 3) + lag(I(haircut^2), k = 3) + lag(I(haircut^3), k = 3) + lag(debt, k = 3) + lag(fxregime, k = 3) + lag(fxrate, k = 3) + lag(institutions2, k = 3) + lag(haircut * fxregime, k = 3), data=dataset, index=c("country", "year"),  model="within", methods="arellano")
  gt3r <- coeftest(gt3, vcov.=vcovHC(gt3))
  
  # Counterfactual for growth in t+4
  gt4 <- plm(growth ~ lag(growth, k = 5) + lag(growth, k=6) + lag(workshare1, k = 4) + lag(hc, k = 4) + lag(log(gdp), k = 4) + lag(log(capital), k = 4) + lag(cpi, k = 4) + lag(restructure, k = 4) + lag(haircut, k = 4) + lag(I(haircut^2), k = 4) + lag(I(haircut^3), k = 4) + lag(debt, k = 4) + lag(fxregime, k = 4) + lag(fxrate, k = 4) + lag(institutions2, k = 4) + lag(haircut * fxregime, k = 4), data=dataset, index=c("country", "year"),  model="within", methods="arellano")
  gt4r <- coeftest(gt4, vcov.=vcovHC(gt4))
  
  
  growths <- data.frame(years[-1])
  results <- data.frame(c(0), c(0), c(0), c(0), c(0), c(0))
  names(results) <- c("year", "haircut", "fxregime", "gdp", "min", "max")

  
  # define some aux list
  years <- c(year:(year+5))
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
      print(length(results[,1]))
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
# NOTE: DECIDE WHETER TO INCLUDE ALL COEFFICIENTS OR ONLY SIGNIFICANT ONES
predict.cf <- function(coefs, dataset){
  g = 0
  if(length(dataset) != length(coefs[,1])){
    return("Coefficients and data not the same lenght")
  }
  else{
    for (el in 1:length(coefs[,1])){
      if(coefs[el,4]<1){
        g = g + coefs[el] * dataset[el]
      }
    }
  }
  return(as.numeric(g))
}


country.data <- function(country, year, dataset, haircut, fxregime){
# the data MUST follow this order:
# lag(growth) + lag(growth, k=2) + workshare1 + hc + log(gdp) + log(capital) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + debt + fxregime + fxrate + institutions2 + haircut * fx
  
  lag1.growth <- dataset$growth[which(dataset$country==country & dataset$year==year-1)]
  lag2.growth <- dataset$growth[which(dataset$country==country & dataset$year==year-2)]
  workshare <- dataset$workshare1[which(dataset$country==country & dataset$year==year)]
  hc <- dataset$hc[which(dataset$country==country & dataset$year==year)]
  log.gdp <- log(dataset$gdp[which(dataset$country==country & dataset$year==year)])
  log.capital <- log(dataset$capital[which(dataset$country==country & dataset$year==year)])
  cpi <- dataset$cpi[which(dataset$country==country & dataset$year==year)]
  if (haircut != 0){
    restructure <- c(1)
  }
  else{
    restructure <- c(0)
  }
  
  haircut2 <- haircut^2
  haircut3 <- haircut^3
  debt <- dataset$debt[which(dataset$country==country & dataset$year==year)]
  fxrate <- dataset$fxrate[which(dataset$country==country & dataset$year==year)]
  institutions2 <- dataset$institutions2[which(dataset$country==country & dataset$year==year)]
  if (is.na(institutions2)){
    institutions2 = 0
  }
  fxhaircut <- haircut * fxregime
  
  dataset = data.frame(lag1.growth, lag2.growth, workshare, hc, log.gdp, log.capital, cpi, restructure, haircut, haircut2, haircut3, debt, fxregime, fxrate, institutions2, fxhaircut)
  return(dataset)
}


#------------------------------------------------------------------------------------------------------------------------------------------
# Counterfactual grpahs
#------------------------------------------------------------------------------------------------------------------------------------------


countries <- c("GRC", "IRL", "ITA", "PRT", "ESP")
years <- c(2010)


for (country in countries){
  for (year in years){
    print(paste(country, year))
    plot.counterfactual(dataset, country, year)
  }
}





