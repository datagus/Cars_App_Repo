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

    # str1 <- '<b>Shiny App Description</b>'
    # str2 <- 'The plot shows car make efficiency using number of cars
    #          with MPG > 22 each year.'
    # str3 <- 'The plot dots are shaded for years when the global price of
    #          oil exceeded $50 per barrel.'
    # str4 <- 'Use pull-down lists to choose another car maker.'
    # str5 <- 'Type or click to alter the range of years, from 1984 through 2016.'
    # str6 <- 'There are no input controls designed into the UI yet (e.g.
    #          years prior to 1984).'
    # str7 <- 'The refresh button will clear up most errors.'
    # str8 <- ''
    # str9 <- '<a href=\'https://github.com/gusmairs/mpg-shiny-app.git\'>
    #          GitHub Repo</a>'

    conn <- file('descrip.txt', open='r')
    lines <- readLines(conn)
    txt <- ''
    for (line in lines){
       txt <- paste(txt, line, '</br>')
    }

    output$infoText <- renderUI({
        HTML(txt)
        # HTML(paste(
            # str1, str2, str3, str4, str5, str6, str7, str8, str9,
            # sep = '<br/>'
        # ))
    })
})

# conn <- file('descrip.txt', open='r')
# lines <- readLines(conn)
# txt <- ''
# for (line in lines){
#    txt <- paste(txt, line, '</br>')
# }
# print(HTML(txt))
# close(conn)
