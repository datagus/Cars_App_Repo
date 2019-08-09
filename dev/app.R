library(ggplot2)
library(shiny)
library(dplyr)
library(stringr)
setwd("~/Dropbox/Data Science Study/Coursera Hopkins Data Series/9 Developing Data Products/Week 4 Project")
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


ui <- fluidPage(
    titlePanel("Cars Plot"),
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
                         min = min(cars$year),
                         max = max(cars$year)
            ),
            numericInput(inputId = "span",
                         label = "Years to show: ",
                         value = 15,
                         min = 1,
                         max = max(cars$year) - min(cars$year) + 1
            ),
            checkboxInput(inputId = "model", 
                          label = strong("Overlay linear model"), 
                          value = TRUE)
        ),
        mainPanel(
            plotOutput("sctPlot"),
            verbatimTextOutput("carN")
        )
    ))


server <- function(input, output) {
    carsamp <- reactive({
        filter(cardat, 
               make == input$make1 | make == input$make2,
               year >= input$start & year <= input$start + input$span - 1)
    })
    output$sctPlot <- renderPlot({
        g <- ggplot(carsamp(), aes(x = year, y = m22)) +
            geom_freqpoly(stat = "identity", aes(color = make)) + 
            geom_point(pch = 21, size = 3.0, color = "gray30", 
                       aes(fill = factor(oil < 50, labels = c("Oil over $50", "Oil under $50")))) +
            scale_color_manual(values = c("steelblue", "orangered3")) + 
            scale_fill_manual(values = c("gray80", "white")) +
            scale_x_continuous(breaks = seq(input$start + input$start %% 2, 
                                            input$start + input$span - 1,
                                            by = 2)) +
            theme(legend.position = c(0.25, 0.9),
                  legend.box = "horizontal",
                  legend.margin = margin(0, 5, 4, 5),
                  legend.key = element_rect(color = NULL, fill = "white"),
                  legend.box.background = element_rect(color = "gray40"),
                  legend.box.margin = margin(1, 1, 1, 1),
                  legend.justification = "center") + 
            guides(fill = guide_legend(title = NULL, nrow = 1),
                   color = guide_legend(title = NULL, nrow = 1)) + 
            labs(x = "Year", 
                 y = "Percent of Models with MPG > 22", 
                 title = "Efficiency Timeline for Car Makes")
        print(g)
    })
}

shinyApp(ui = ui, server = server)
