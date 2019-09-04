# Script to read and transform datasets, then load to a single
# text file for the Shiny app

# %% Load libraries
#
packages <- c('ggplot2', 'shiny', 'dplyr', 'stringr')
suppressMessages(invisible(
    lapply(packages, library, character.only = TRUE, quietly = TRUE)
))

# %% Read data files
#
data_path <- '~/OneDrive/DS_Study/mpg-shiny-project/data'
cars <- read.csv(
    file.path(data_path, 'cars.csv')
)
oil_price <- read.csv(
    file.path(data_path, 'oil_prices.csv'),
    header = FALSE,
    col.names = c('year', 'pr', 'adj')
)
makes <- sort(unique(cars$make))

# %% Transform datasets
#
cardat <- cars %>%
    filter(year < 2017 & (fuel == 'Regular' | fuel == 'Premium')) %>%
    select(year, make, disp, mpg, class) %>%
    group_by(make, year) %>%
    summarize(
        m22 = round(100 * mean(mpg >= 22), 1),
        mean_disp = round(mean(disp), 1)
    ) %>%
    mutate(
        oil = oil_price[match(year, oil_price$year), 'adj'],
        yr = str_pad(year %% 100, 2, pad = '0')
    )

# %% Load to text file at top of working directory
#
write.csv(cardat, 'cardat.csv')
