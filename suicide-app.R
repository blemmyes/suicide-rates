#!/usr/bin/env Rscript

library('shiny')
library('tidyverse')

read_suicide_data <- function(csv_file_path) {
    data_frame <- read.csv(csv_file_path)
    tbbl <- as_tibble(data_frame)
    return(tbbl)
}

ui <- fluidPage(titlePanel('DASB: Suicide Rates'),
                fluidRow(column(12, sliderInput(inputId='minMeasurements',
                                                label='Min. Measurements',
                                                min=1,
                                                max=100, # TODO: highest from data set
                                                value=10)),
                         column(6, plotOutput(outputId='histogram')))
)

# TODO: filter by input$minMeasurements

data <- read_suicide_data('./data/master.csv')
by_country_year <- data %>% group_by(country, year) %>% summarize(n())
by_country <- by_country_year %>% group_by(country) %>% summarize(count=n())

server <- function(input, output) {
    observeEvent(input$minMeasurements, {
        output$histogram <- renderPlot(ggplot(data=by_country, mapping=aes(count)) +
                                       # TODO: replace 35 by...
                                       scale_x_continuous(breaks = seq(1, 35, 1)) +
                                       geom_histogram(binwidth=1))
    })
}

shinyApp(ui=ui, server=server, options=list(port=1337))
