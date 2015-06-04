dyn10 <- plm(growth ~ lag(growth) + lag(growth, k=2) + log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + restructure + haircut + I(haircut^2) + I(haircut^3) + fxregime + haircut*fxregime + bankcrisis + default + institutions2, data = dataset, index=c("country", "year"),  model="within", effect = "individual", method = "arellano")
dyn10r <- coeftest(dyn10, vcov.=vcovHC(dyn10))
dyn10r

opt1 <- plm(growth ~ lag(growth) + lag(growth, k=2) + log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + lag(restructure) + lag(haircut) + lag(I(haircut^2)) + lag(I(haircut^3)) + lag(fxregime) + lag(restructure*fxregime) + bankcrisis + default + institutions2, data = dataset, index=c("country", "year"),  model="within", effect = "individual", method = "arellano")
opt1r <- coeftest(opt1, vcov.=vcovHC(opt1))
opt1r

opt2 <- plm(growth ~ lag(growth) + lag(growth, k=2) + log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + lag(restructure, k = 2) + lag(haircut, k = 2) + lag(I(haircut^2), k = 2) + lag(I(haircut^3), k = 2) + lag(fxregime, k = 2) + lag(haircut*fxregime, k = 2) + bankcrisis + default + institutions2, data = dataset, index=c("country", "year"),  model="within", effect = "individual", method = "arellano")
opt2r <- coeftest(opt2, vcov.=vcovHC(opt2))
opt2r

opt3 <- plm(growth ~ lag(growth) + lag(growth, k=2) + log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + lag(restructure, k = 3) + lag(haircut, k = 3) + lag(I(haircut^2), k = 3) + lag(I(haircut^3), k = 3) + lag(fxregime, k = 3) + lag(haircut*fxregime, k = 3) + bankcrisis + default + institutions2, data = dataset, index=c("country", "year"),  model="within", effect = "individual", method = "arellano")
opt3r <- coeftest(opt3, vcov.=vcovHC(opt3))
opt3r

opt4 <- plm(growth ~ lag(growth) + lag(growth, k=2) + log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + lag(restructure, k = 4) + lag(haircut, k = 4) + lag(I(haircut^2), k = 4) + lag(I(haircut^3), k = 4) + lag(fxregime, k = 4) + lag(haircut*fxregime, k = 4) + bankcrisis + default + institutions2, data = dataset, index=c("country", "year"),  model="within", effect = "individual", method = "arellano")
opt4r <- coeftest(opt4, vcov.=vcovHC(opt4))
opt4r

opt5 <- plm(growth ~ lag(growth) + lag(growth, k=2) + log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + lag(restructure, k = 5) + lag(haircut, k = 5) + lag(I(haircut^2), k = 5) + lag(I(haircut^3), k = 5) + lag(fxregime, k = 5) + lag(haircut*fxregime, k = 5) + bankcrisis + default + institutions2, data = dataset, index=c("country", "year"),  model="within", effect = "individual", method = "arellano")
opt5r <- coeftest(opt5, vcov.=vcovHC(opt5))
opt5r

opt6 <- plm(growth ~ lag(growth) + lag(growth, k=2) + log(pop) + workshare1 + log(hc) + log(gdp) + diff(log(capital)) + cpi + lag(restructure, k = 6) + lag(haircut, k = 6) + lag(I(haircut^2), k = 6) + lag(I(haircut^3), k = 6) + lag(fxregime, k = 6) + lag(haircut*fxregime, k = 6) + bankcrisis + default + institutions2, data = dataset, index=c("country", "year"),  model="within", effect = "individual", method = "arellano")
opt6r <- coeftest(opt6, vcov.=vcovHC(opt6))
opt6r


stargazer(dyn10r, opt1r, opt2r, opt3r, opt4r, opt5r, opt6r, align=TRUE, type = "text")
stargazer(dyn10r, opt1, opt2, opt3, opt4, opt5, opt6, align=TRUE, type = "text")
