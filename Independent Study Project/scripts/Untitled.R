#------------------------------------------------------------------------------------------------------------------------------------------
# Initialize
#------------------------------------------------------------------------------------------------------------------------------------------
rm(list=ls())
setwd("/Users/josepcasas/Documents/bgse/bgse-code/Independent Study Project/results/")
library(ggplot2)

sus1 <- read.csv("~/Documents/bgse/bgse-code/Independent Study Project/data/sustainability.csv")
names(sus1) <- c("country", "year", "value")

p1 <- ggplot(data = sus1)
p1 <- p1 + geom_line(aes(x=year, y = value, group=country, colour=country))
p1 <- p1 + geom_hline(aes(yintercept = 1), size = 1, linetype = 2)
p1 <- p1 + scale_x_continuous(breaks=sus1$year)
p1

ggsave(file="sustainable1.png", plot = p1, width = 40, height = 20, units = "cm", dpi = 500)


#------------------------------------------------------------------------------------------------------------------------------------------


sus1 <- read.csv("~/Documents/bgse/bgse-code/Independent Study Project/data/sustainability3.csv")
names(sus1) <- c("country", "year", "value")

p1 <- ggplot(data = sus1)
p1 <- p1 + geom_line(aes(x=year, y = value, group=country, colour=country))
p1 <- p1 + scale_x_continuous(breaks=sus1$year)
p1
ggsave(file="sustainable3.png", plot = p1, width = 40, height = 20, units = "cm", dpi = 500)


#------------------------------------------------------------------------------------------------------------------------------------------


sus1 <- read.csv("~/Documents/bgse/bgse-code/Independent Study Project/data/sustainability4.csv")
names(sus1) <- c("country", "year", "value")

p1 <- ggplot(data = sus1)
p1 <- p1 + geom_line(aes(x=year, y = value, group=country, colour=country))
p1 <- p1 + scale_x_continuous(breaks=sus1$year)
p1
ggsave(file="sustainable4.png", plot = p1, width = 40, height = 20, units = "cm", dpi = 500)


#------------------------------------------------------------------------------------------------------------------------------------------


sus1 <- read.csv("~/Documents/bgse/bgse-code/Independent Study Project/data/sustainability5.csv")
names(sus1) <- c("country", "year", "value")

p1 <- ggplot(data = sus1)
p1 <- p1 + geom_line(aes(x=year, y = value, group=country, colour=country))
p1 <- p1 + scale_x_continuous(breaks=sus1$year)
p1
ggsave(file="sustainable5.png", plot = p1, width = 40, height = 20, units = "cm", dpi = 500)


