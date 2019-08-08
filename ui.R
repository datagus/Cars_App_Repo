library(ggplot2)
library(shiny)

cardat <- read.csv("cardat.csv")
makes <- sort(unique(cardat$make))


shinyUI(fluidPage(
    titlePanel("Car Makes Efficiency Plot"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "make1", 
                        label = strong("Car Make 1"), 
                        choices = makes, 
                        selected = "Ford"
            ), 
            selectInput(inputId = "make2", 
                        label = strong("Car Make 2"), 
                        choices = makes, 
                        selected = "Honda"
            ), 
            numericInput(inputId = "start",
                         label = "Starting year: ",
                         value = 2001,
                         min = min(cardat$year),
                         max = max(cardat$year)
            ),
            numericInput(inputId = "span",
                         label = "Years to show: ",
                         value = 15,
                         min = 1,
                         max = max(cardat$year) - min(cardat$year) + 1
            )
        ),
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", br(), plotOutput("sctPlot")),
                        tabPanel("Info", br(), htmlOutput("infoText"))
                        ),
            verbatimTextOutput("carN")
        )
    )
))


