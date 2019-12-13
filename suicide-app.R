#!/usr/bin/env Rscript

library('shiny')
library('tidyverse')

read_suicide_data <- function(csv_file_path) {
    data_frame <- read.csv(csv_file_path)
    tbbl <- as_tibble(data_frame)
    return(tbbl)
}

height <- '1024px'
ui <- navbarPage('DASB: Suicide Rates', id='tabs',
                 tabPanel('Overview',
                          sidebarLayout(sidebarPanel(h1('Overview'),
                                                     p('The diagram on the right hand side shows...')),
                                        mainPanel(plotOutput(outputId='overview', height=height)))),
                 tabPanel('Histogram',
                          sidebarLayout(sidebarPanel(h1('Histogram'),
                                                     p('The histogram on the right hand side...')),
                                        mainPanel(plotOutput(outputId='histogram', height=height)))),
                 tabPanel('Country Data',
                          sidebarLayout(sidebarPanel(h1('Country Data'),
                                                    p('The number of data per country and year...')),
                                       mainPanel(plotOutput(outputId='countryData', height=height))))
)

data <- read_suicide_data('./data/master.csv')
by_country_year <- data %>% group_by(country, year) %>% summarize(n())
by_country <- by_country_year %>% group_by(country) %>% summarize(count=n())

server <- function(input, output) {
    max <- max(by_country$count)
    output$overview <- renderPlot(ggplot(data=by_country_year, mapping=aes(x=year, y=country)) +
                                  geom_tile() +
                                  xlab('Year') +
                                  ylab('Country'))
    output$histogram <- renderPlot(ggplot(data=by_country, mapping=aes(count)) +
                                   scale_x_continuous(breaks = seq(1, max, 1)) +
                                   geom_histogram(binwidth=1) +
                                   xlab('Number of Years Covered') +
                                   ylab('Number of Countries'))
    output$countryData <- renderPlot(ggplot(data=by_country, mapping=aes(x=country, y=count)) +
                                     geom_point() +
                                     theme(axis.text.x=element_text(angle=90, hjust=1, vjust=+0.25)) +
                                     xlab('Country') +
                                     ylab('Number of Years Covered'))
}

shinyApp(ui=ui, server=server, options=list(port=1337))
