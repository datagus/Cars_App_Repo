# Server side code for mpg Shiny app

# %% Load libraries
#
packages <- c('ggplot2', 'shiny', 'dplyr', 'stringr')
suppressMessages(invisible(
    lapply(packages, library, character.only=TRUE, quietly=TRUE)
))

# %% Server code
#
cardat <- read.csv('cardat.csv')
shinyServer(function(input, output) {
    carsamp <- reactive({
        filter(
            cardat,
            make == input$make1 | make == input$make2,
            year >= input$start & year <= input$start + input$span - 1
        )
    })
    output$sctPlot <- renderPlot({
        g <- ggplot(
            carsamp(),
            aes(x = year, y = m22)
        ) +
        geom_freqpoly(
            stat = 'identity', aes(color = make)
        ) +
        geom_point(
            pch = 21, size = 3.0, color = 'gray30',
            aes(fill = factor(
                oil < 50, labels = c('Oil over $50', 'Oil under $50')
            ))
        ) +
        scale_color_manual(
            values = c('steelblue', 'orangered3')
        ) +
        scale_fill_manual(
            values = c('gray80', 'white')
        ) +
        scale_x_continuous(
            breaks = seq(
                input$start + input$start %% 2,
                input$start + input$span - 1,
                by = 2
            )
        ) +
        theme(
            legend.position = c(0.3, 0.9),
            legend.box = 'horizontal',
            legend.margin = margin(0, 5, 4, 5),
            legend.key = element_rect(color = NULL, fill = 'white'),
            legend.box.background = element_rect(color = 'gray40'),
            legend.box.margin = margin(1, 1, 1, 1),
            legend.justification = 'center'
        ) +
        guides(
            fill = guide_legend(title = NULL, nrow = 1),
            color = guide_legend(title = NULL, nrow = 1)
        ) +
        labs(
            x = 'Year',
            y = 'Percent of Models with MPG > 22',
            title = 'Efficiency Timeline for Car Makes'
        )
        print(g)
    })

    conn <- file('lib/descrip.txt', open='r')
    lines <- readLines(conn)
    txt <- ''
    for (line in lines){
       txt <- paste(txt, line, '</br>')
    }

    output$infoText <- renderUI({
        HTML(txt)
    })
})
