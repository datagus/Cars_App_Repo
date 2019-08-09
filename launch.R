# Convenience script to launch the app on local server
# Runs from terminal, not in R: 'Rscript launch.R'
# Creates data file if not present

# %% Produce data file if needed
#
if (!file.exists('cardat.csv')) {
    source('lib/data_prep.R')
}

# %% Launch the app on port 5000
#
shiny::runApp(port = 5000)
