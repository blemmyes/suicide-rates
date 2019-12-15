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
                                                     p('The data for this project origins from the Kaggle website:'),
                                                     a('Suicide Rates Overview 1985 to 2016', 
                                                       href='https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016/data'),
                                                     p(),
                                                     p('This data set contains data about suicides from different countries in different years. 
                                                       The data is further distinguished by age, generation, GDP, population and gender.'),
                                                     h4('Research questions:'),
                                                     p('The hypothesis is, based on the available information, there should be a correlation between the GDP per capita and the number of suicides in a country. 
                                                       There should also be different suicide rates depending on the generation.'),
                                                     p('The diagram on the right hand side shows which country has data available for which years. 
                                                       Apparently, some countries have only few years covered. 
                                                       These countries should not be taken into consideration when it comes to reason about the correlation, as those are not as significant as the other countries with more data available.')),
                                        mainPanel(plotOutput(outputId='overview', height=height)))),
                 tabPanel('Histogram',
                          sidebarLayout(sidebarPanel(h1('Histogram'),
                                                     p('To decide which countries should not be taken into consideration, we take a look at the histogram on the right hand side. 
                                                       It shows the number of years where data is available per country. 
                                                       We decided to focus on countries with at least 10 years of data.')),
                                        mainPanel(plotOutput(outputId='histogram', height=height)))),
                 tabPanel('Country Data',
                          sidebarLayout(sidebarPanel(h1('Country Data'),
                                                    p('On the right hand side, the count of years per country is visualized. 
                                                      It shows that there are many countries with more than 20 years of data, only a few have a small amount of years covered. 
                                                      These are not useful for comparison.')),
                                       mainPanel(plotOutput(outputId='countryData', height=height)))),
                 tabPanel('GDP per Capita',
                          sidebarLayout(sidebarPanel(h1('GDP per Capita'),
                                                     withMathJax(p('This plot shows the correlation between the count of suicides per country and the GDP per capita. 
                                                                   As the blue line shows, there is no correlation between these two attributes. 
                                                                   The \\(R^2\\) score for this correlation is 0.00166 (p-value of 0.686), that means, only 0.166% of the variance gets explained with the model. 
                                                                   This clearly shows no dependency, and the hypothesis of a correlation can be refused.'))),
                                        mainPanel(plotOutput(outputId='gdpPerCapita', height=height)))),
                 tabPanel('Clustering',
                          sidebarLayout(sidebarPanel(h1('Clustering'),
                                                     p('With the slider below, different counts of clusters can be chosen.'),
                                                     sliderInput('nClusters', 'Clusters', 2, 10, 4),
                                                     p('This plot shows how the different countries can be clustered together, based on the number of suicides and the GDP per capita of the country. 
                                                       Between a cluster count of 4 and 7, Switzerland is in a cluster with Qatar, Luxembourg, Norway and Denmark. 
                                                       But interestingly, Switzerland has the highest suicide rate of all countries in this cluster.')),
                                        mainPanel(plotOutput(outputId='clustering', height=height)))),
                 tabPanel('Generation',
                          sidebarLayout(sidebarPanel(h1('Generation'),
                                                     p('This plot shows the suicide rates grouped by generation. 
                                                       Each country is represented by a dot. 
                                                       It is interesting to see that, the older a generation is, the wider the spread of suicide rates becomes.'),
                                                     h4('Legend:'),
                                                     p('Boomers (born between 1946 - 1964)'),
                                                     p('G.I. Generation (born in early 1900s - mid to late 1920s)'),
                                                     p('Generation X (born between 1965 - 1975)'),
                                                     p('Generation Z (born between 1997 - 2012)'),
                                                     p('Millenials (born between 1981 - 1998)'),
                                                     p('Silent (born in mid 1920s - early 1940s)'),
                                                     p('------------------------'),
                                                     p('Possible reasons for this distribution can be seen in the next tab, Generations over years.')),
                                        mainPanel(plotOutput(outputId='generation', height=height)))),
                 tabPanel('Generations over years',
                          sidebarLayout(sidebarPanel(h1('Generations over years'),
                                                     h4('Legend:'),
                                                     p('G.I. Generation (born in early 1900s - mid to late 1920s)', style='color:red'),
                                                     p('Silent (born in mid 1920s - early 1940s)', style='color:purple'),
                                                     p('Boomers (born between 1946 - 1964)', style='color:blue'),
                                                     p('Generation X (born between 1965 - 1975)', style='color:green'),
                                                     p('Millenials (born between 1981 - 1998)', style='color:orange'),
                                                     p('Generation Z (born between 1997 - 2012)'),
                                                     p('------------------------'),
                                                     p('In this chart, data of all countries is assembled. 
                                                       For every generation (see legend) a timeseries is plotted in the line chart. 
                                                       The line chart shows how many suicides there were for each generation over the years.'),
                                                     p('This shows that the last years (maybe from 2010) should not be taken into consideration, as they seem to have missing data. 
                                                       But we can see, for example, that the G.I. generation had the last suicide in the year 2000. 
                                                       This makes sense when considering the G.I.\'s are around 100 years old in 2000! 
                                                       Also the youngest generation, generation Z, did not have as many suicides until now. 
                                                       The first suicides happened around the year 2007, where the persons were 10 years old, at the most. 
                                                       It is intersting to see how regularly the suicide rate of each generation is tending to increase over the years.')),
                                        mainPanel(plotOutput(outputId='generationOverYears', height=height)))),
                 tabPanel('Conclusion', 
                          sidebarLayout(sidebarPanel(h1('Conclusion'), 
                                                     h4('Corellation GDP / suicide rate'),
                                                     p('The data provided does not lead to the conclusion that there is a correlation between GDP per capita and the suicide rate of a country.
                                                       This means the hypothesis from the beginning is invalid.'), 
                                                     h4('Suicide rate per generation'),
                                                     p('The second question about the suicide rate per generation is interesting. 
                                                       There is no year span where the suicide rate of all generation rises or drops. 
                                                       The different generation\'s suicide rates are independent from each other. 
                                                       Interestingly, you can see that the Millenials and the Generation Z have their first suicide at age of 10.'),
                                                     h4('Data inconsistency'),
                                                     p('But as already seen in the plots before, there is a lot of missing data.
                                                        Even after cleaning up insignificant data, the data set remained inconsistent
                                                        If the data set would be more consistent and contained more significant data, more meaningful and precise statements could be made.')),
                                        mainPanel(),
)))

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
suicide_generation = data_clean %>% group_by(generation, suicides.100k.pop)

data_boom <- dplyr::filter(data, grepl('Boomer', generation))
data_boom <- aggregate(data_boom$suicides_no, by=list(Year=data_boom$year), FUN=sum)
data_gi <- dplyr::filter(data, grepl('G.I. Generation', generation))
data_gi <- aggregate(data_gi$suicides_no, by=list(Year=data_gi$year), FUN=sum)
data_genx <- dplyr::filter(data, grepl('Generation X', generation))
data_genx <- aggregate(data_genx$suicides_no, by=list(Year=data_genx$year), FUN=sum)
data_genz <- dplyr::filter(data, grepl('Generation Z', generation))
data_genz <- aggregate(data_genz$suicides_no, by=list(Year=data_genz$year), FUN=sum)
data_mill <- dplyr::filter(data, grepl('Millenials', generation))
data_mill <- aggregate(data_mill$suicides_no, by=list(Year=data_mill$year), FUN=sum)
data_silent <- dplyr::filter(data, grepl('Silent', generation))
data_silent <- aggregate(data_silent$suicides_no, by=list(Year=data_silent$year), FUN=sum)

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
    output$generation <- renderPlot(ggplot(data=suicide_generation,
                                           mapping=aes(x=generation, y=suicides.100k.pop)) +
                                    geom_point(alpha=0.25) +
                                    xlab('Generation') +
                                    ylab('Suicides (per 100k Inhabitants)'))
    output$generationOverYears <- renderPlot(ggplot() +
                                               geom_line(data = data_gi, aes(x = Year, y = x), color = "red")+
                                               geom_line(data = data_boom, aes(x=Year, y=x), color = "blue")+
                                               geom_line(data = data_genx, aes(x=Year, y=x), color = "green")+
                                               geom_line(data = data_genz, aes(x=Year, y=x), color = "black")+
                                               geom_line(data = data_mill, aes(x=Year, y=x), color = "orange")+
                                               geom_line(data = data_silent, aes(x=Year, y=x), color = "purple")+
                                               xlab('Year')+
                                               ylab('Count of suicides')+
                                               scale_color_discrete(name = "Generations", labels = c("Generation G. I.", "Boomer", 'Generation X', "Generation Y", "Millenaials", "Silent")) +
                                               geom_point())
    observeEvent(input$nClusters, {
        clusters <- kmeans(clustered, centers=input$nClusters, nstart=25)
        output$clustering <- renderPlot(fviz_cluster(clusters, data=clustered,
                                                     xlab='Suicides (per 100k Inhabitants)',
                                                     ylab='GDP per Capita',
                                                     main=''))
    })
}

shinyApp(ui=ui, server=server, options=list(port=1337))
