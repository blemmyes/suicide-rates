#!/usr/bin/env Rscript

library('cluster')
library('factoextra')
library('shiny')
library('tidyverse')

read_suicide_data <- function(csv_file_path) {
    data_frame <- read.csv(csv_file_path)
    tbbl <- as_tibble(data_frame)
    return(tbbl)
}

height <- '920px'
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
                                       mainPanel(plotOutput(outputId='countryData', height=height)))),
                 tabPanel('GDP per Capita',
                          sidebarLayout(sidebarPanel(h1('GDP per Capita'),
                                                     p('This plot shows...')),
                                        mainPanel(plotOutput(outputId='gdpPerCapita', height=height)))),
                 tabPanel('Clustering',
                          sidebarLayout(sidebarPanel(h1('Clustering'),
                                                     p('This slider...'),
                                                     sliderInput('nClusters', 'Clusters', 2, 10, 4),
                                                     p('This plot...')),
                                        mainPanel(plotOutput(outputId='clustering', height=height)))),
                 tabPanel('Conclusion', h1('Foobar'), p('foobar'))
)

data <- read_suicide_data('./data/master.csv')
by_country_year <- data %>% group_by(country, year) %>% summarize(n())
by_country <- by_country_year %>% group_by(country) %>% summarize(count=n()) 
valid_countries <- by_country %>% filter(by_country$count>=10) %>% select(country) %>% unlist() %>% as.character() %>% c()
data_clean <- filter(data, country %in% valid_countries)
suicide_gdp <- data_clean %>%
    group_by(country, year, suicides.100k.pop, gdp_per_capita....) %>%
    summarize(n()) %>%
    group_by(country, year) %>%
    mutate(total_suicides_100k_pop=sum(suicides.100k.pop)) %>%
    group_by(country, year, total_suicides_100k_pop, gdp_per_capita....) %>%
    summarize(count=n())
clustered <- suicide_gdp %>%
    group_by(country) %>%
    summarize(total_suicides_100k_pop=mean(total_suicides_100k_pop), gdp_per_capita....=mean(gdp_per_capita....)) %>%
    remove_rownames() %>%
    column_to_rownames(var='country')
clustered$total_suicides_100k_pop <- scale(clustered$total_suicides_100k_pop, center=TRUE, scale=TRUE)
clustered$gdp_per_capita.... <- scale(clustered$gdp_per_capita...., center=TRUE, scale=TRUE)

server <- function(input, output) {
    max <- max(by_country$count)
    output$overview <- renderPlot(ggplot(data=by_country_year, mapping=aes(x=year, y=country)) +
                                  geom_tile() +
                                  xlab('Year') +
                                  ylab('Country'))
    output$histogram <- renderPlot(ggplot(data=by_country, mapping=aes(count)) +
                                   scale_x_continuous(breaks=seq(1, max, 1)) +
                                   geom_histogram(binwidth=1) +
                                   xlab('Number of Years Covered') +
                                   ylab('Number of Countries'))
    output$countryData <- renderPlot(ggplot(data=by_country, mapping=aes(x=country, y=count)) +
                                     geom_point() +
                                     theme(axis.text.x=element_text(angle=90, hjust=1, vjust=+0.25)) +
                                     xlab('Country') +
                                     ylab('Number of Years Covered'))
    output$gdpPerCapita <- renderPlot(ggplot(data=suicide_gdp,
                                             mapping=aes(x=gdp_per_capita...., y=total_suicides_100k_pop)) +
                                      geom_point(alpha=0.25) +
                                      theme(legend.position='none') +
                                      geom_smooth(method='lm') +
                                      xlab('GDP per Capita') +
                                      ylab('Suicides (per 100k Inhabitants)'))
    observeEvent(input$nClusters, {
        clusters <- kmeans(clustered, centers=input$nClusters, nstart=25)
        output$clustering <- renderPlot(fviz_cluster(clusters, data=clustered,
                                                     xlab='Suicides (per 100k Inhabitants)',
                                                     ylab='GDP per Capita',
                                                     main=''))
    })
}

shinyApp(ui=ui, server=server, options=list(port=1337))
