#### R Shiny App - Car Maker MPG Analysis
Pulls together cars data and historical carmaker data along with historical oil price data to enable comparison of two car makers over time.
 - Data file `cardat.csv` must be at the level of server.R to deploy, run `rscript launch.r` to create this file and test the app at local `127.0.0.1:5000`
 - The `data_prep.r` file has code to load, join, clean and transform
the datasets for use in the app
 - The app is publicly <a href='https://gusmairs.shinyapps.io/mpg-shiny-app/'>hosted</a> in its current form.
