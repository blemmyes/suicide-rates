#!/usr/bin/env Rscript

library('tidyverse')

read_suicide_data <- function(csv_file_path) {
    data_frame <- read.csv(csv_file_path)
    tbbl <- as_tibble(data_frame)
    return(tbbl)
}

## On Windows:
data <- read_suicide_data('C:\\Users\\Larissa\\Documents\\GitHub\\suicide-rates\\data\\master.csv')
## On Mac:
data <- read_suicide_data('/Users/fabianmeyer/Library/Mobile Documents/com~apple~CloudDocs/HSLU/07 HS19/DASB/Projekt/suicide-rates/data/master.csv') #
## Everywhere:
## just make sure the project dir is your working dir
## data <- read_suicide_data('./data/master.csv')

## data: 1 observation per year per country per sex per age

## Larissa needs this!
## print(data)
data <- data %>% rename(country = ?..country)
## print(data)

## data2: Summarize year (1 observation per every year per country)

data2 <- data %>%
  group_by(country, year) %>%
  summarize(n())

data2 <- as_tibble(data2)

## Plot has year (yes/no) per country (y) vs time (x)
## Note that some countries have two entries in year ~ 2015

ggplot(data = data2, mapping = aes(x = year, y = country)) +
    theme(tex = element_text(size=6)) +
    geom_bin2d()

## V2 with tiles(): returns true/false color mapping
ggplot(data = data2, mapping = aes(x = year, y = country)) +
    theme(tex = element_text(size=6)) +
    xlab("Year") +
    ylab("Country") +
    geom_tile()
ggsave('doc/1-available-data.png', dpi = 300)


## Delete countrys with too few suicides

data_clean <- data %>%
  group_by(country, year) %>%
  summarize(n())

print(data_clean)

data1_clean <- data_clean %>%
  group_by(country) %>%
  summarize(n())

print(data1_clean)

data1_clean<-data1_clean[order(-data1_clean$`n()`),]
print(data1_clean)

data_full_clean <- data1_clean %>% filter(data1_clean$'n()'>=10)
print(data_full_clean)


## data3: How many entries per country? (number of years)
## How many entries per country? (number of years)

data3 <- data2 %>%
  group_by(country) %>%
  summarize(count = n())

data3 <- as_tibble(data3)

plot(data3)

## A closer look at the data quality: Show with a histogram the number of observations each country has

ggplot(data = data3, mapping = aes(count)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(1, 35, 1)) +
  xlab('Years with Data') +
  ylab('Number of Countries')
ggsave('doc/2-observations-histogram.png', dpi = 300)

## what is the max? 32. Some countries have 32 observations / data about suicide from 32 different years

max(data3$count)

## TODO: print top ten countries (with the most number of observations)

## Plot number of different years per country with custom ticks

ggplot(data = data3, mapping = aes(x = country, y = count)) +
    theme(tex = element_text(size=6)) +
    geom_point() + theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust=+0.25)) +
    xlab('Country') +
    ylab('Years with Data')
ggsave('doc/3-years-with-data.png', dpi = 300)

## Test: Plotting suicide vs. gdp per capita (plotting pretty much all rows)

data4 <- data %>%
  group_by(country, year, suicides.100k.pop, gdp_per_capita....) %>%
  summarize(n())

data4 <- as_tibble(data4)

ggplot(data = data4, mapping = aes(x = gdp_per_capita...., y = suicides.100k.pop)) +
  geom_point(alpha = 0.25) + theme(legend.position="none") +
  geom_smooth()

## The right way: Plotting 1 observation per every year per every country vs corresponding gdp per capita

data5 <- data4 %>%
  group_by(country, year) %>%
  mutate(total_suicides_100k_pop = sum(suicides.100k.pop)) %>%
  group_by(country, year, total_suicides_100k_pop, gdp_per_capita....) %>%
  summarize(count = n())

data5 <- as_tibble(data5)

ggplot(data = data5, mapping = aes(x = gdp_per_capita...., y = total_suicides_100k_pop)) +
  geom_point(alpha = 0.25) + theme(legend.position="none") +
  geom_smooth() +
  xlab('GDP per Capita') +
  ylab('Suicides (per 100k Inhabitants)')
ggsave('doc/4-regression-curve.png', dpi = 300)

## Computed 1 observation per country with average of suicides and average of gdp over all years

data6 <- data5 %>%
  group_by(country) %>%
  summarize(total_suicides_100k_pop = mean(total_suicides_100k_pop), gdp_per_capita.... = mean(gdp_per_capita....))

ggplot(data = data6, mapping = aes(x = gdp_per_capita...., y = total_suicides_100k_pop)) +
  geom_point(alpha = 0.25) + theme(legend.position="none") +
  geom_smooth() +
  xlab('GDP per Capita') +
  ylab('Suicides (per 100k Inhabitants)')
ggsave('doc/5-another-regression-curve.png', dpi = 300)
## TODO: what is the difference to the plot before?

## Clustering

library(cluster)
library(factoextra)

head(data6) # Problem: 'country' has to be an index, not rowname

d6_scale <- data6

d6_scale <- d6_scale %>%
  remove_rownames() %>%
  column_to_rownames(var = 'country')

## normalize data!

d6_scale$total_suicides_100k_pop <- scale(d6_scale$total_suicides_100k_pop, center= TRUE, scale=TRUE)
d6_scale$gdp_per_capita.... <- scale(d6_scale$gdp_per_capita...., center= TRUE, scale=TRUE)

clusters <- kmeans(d6_scale, centers=4, nstart = 25)

str(clusters)

clusters

clusters_visualized <- fviz_cluster(clusters, data = d6_scale)

clusters_visualized2 <- fviz_cluster(clusters, data = d6_scale, labelsize = 6,
                                     xlab='Suicides (per 100k Inhabitangs)',
                                     ylab='GDP per Capita', main='') +
  theme(tex = element_text(size=6))
ggsave('doc/6-cluster.png', dpi = 300)

clusters_visualized
clusters_visualized2

## Problem: Data is still normalized!

clusters_unscale <- clusters

clusters_unscale$centers[,1] <- clusters$centers[,1] * attr(d6_scale$total_suicides_100k_pop, 'scaled:scale') + attr(d6_scale$total_suicides_100k_pop, 'scaled:center')
clusters_unscale$centers[,2] <- clusters$centers[,2] * attr(d6_scale$gdp_per_capita...., 'scaled:scale') + attr(d6_scale$gdp_per_capita...., 'scaled:center')

data6 <- data6 %>%
  remove_rownames() %>%
  column_to_rownames(var = 'country')

clusters_visualized2 <- fviz_cluster(clusters_unscale, data = data6)

clusters_visualized2

## TODO: Denormalization does not work