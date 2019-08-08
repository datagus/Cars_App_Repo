library(ggplot2)
library(shiny)
library(dplyr)
library(stringr)
setwd("~/Documents/Dropbox/Data Science Study/Coursera Hopkins Data Series/9 Developing Data Products/Week 4 Project")
cars <- read.csv("cars.csv")
oilP <- read.csv("oil prices.csv", header = FALSE, col.names = c("year", "pr", "adj"))
makes <- sort(unique(cars$make))
cardat <- cars %>% 
    filter(year < 2017 & (fuel == "Regular" | fuel == "Premium")) %>%
    select(year, make, disp, mpg, class) %>%
    group_by(make, year) %>% 
    summarize(m22 = round(100 * mean(mpg >= 22), 1),
              mean_disp = round(mean(disp), 1)) %>%
    mutate(oil = oilP[match(year, oilP$year), "adj"], 
           yr = str_pad(year %% 100, 2, pad = "0"))
head(cardat)
write.csv(cardat, "cardat.csv")

