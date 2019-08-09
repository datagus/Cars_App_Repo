library(ggplot2)
library(shiny)
library(dplyr)
library(stringr)

data_path <- '~/OneDrive/DS_Study/mpg-shiny-project/data'

cars <- read.csv(
    file.path(data_path, 'cars.csv')
)
oilP <- read.csv(
    file.path(data_path, 'oil_prices.csv'),
    header=FALSE,
    col.names=c('year', 'pr', 'adj')
)
makes <- sort(unique(cars$make))
cardat <- cars %>%
    filter(year < 2017 & (fuel == "Regular" | fuel == "Premium")) %>%
    select(year, make, disp, mpg, class) %>%
    group_by(make, year) %>%
    summarize(
        m22 = round(100 * mean(mpg >= 22), 1),
        mean_disp = round(mean(disp), 1)
    ) %>%
    mutate(
        oil = oilP[match(year, oilP$year), "adj"],
        yr = str_pad(year %% 100, 2, pad = "0")
    )
head(cardat)
write.csv(
    cardat,
    file.path(data_path, 'cardat.csv')
)
