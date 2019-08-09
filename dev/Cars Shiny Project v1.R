library(dplyr)
library(stringr)
carsOrig <- read.csv("vehicles.csv", header = TRUE, stringsAsFactors = FALSE)
carsDat <- carsOrig %>% transmute(year = year,
                                  make = make,
                                  model = model,
                                  mod = model,
                                  mpg = comb08,
                                  mpgE = combE,
                                  class = as.factor(VClass),
                                  cyl = cylinders,
                                  tran = str_extract(carsOrig$trany, "(Automatic|Manual)"),
                                  gear = str_extract(carsOrig$trany, "\\d+|(variable)"),                                disp = displ,
                                  drive = drive,
                                  alt = as.factor(atvType),
                                  fuel = as.factor(fuelType),
                                  co2 = co2TailpipeGpm,
                                  turbo = ifelse(is.na(tCharger), FALSE, tCharger),
                                  super = ifelse(sCharger == "S", TRUE, FALSE),
                                  guzz = ifelse(guzzler == "", FALSE, TRUE)
        )
## Locate and filter for only larger companies (makes > 40)
bigCo <- carsDat %>% 
        group_by(make) %>% 
        summarise(tot = n()) %>% 
        filter(tot > 40) %>%
        select(make) %>%
        arrange(make) %>%
        unlist(use.names = FALSE)

cars <- carsDat %>% filter(make %in% bigCo)


