#!/usr/bin/env Rscript

library('tidyverse')

read_suicide_data <- function(csv_file_path) {
    data_frame <- read.csv(csv_file_path)
    tbbl <- as_tibble(data_frame)
    return(tbbl)
}

data <- read_suicide_data('data/master.csv')
## On Mac:
# data <- read_suicide_data('/Users/fabianmeyer/Library/Mobile Documents/com~apple~CloudDocs/HSLU/07 HS19/DASB/Projekt/suicide-rates/data/master.csv') #

print(data)

## Preparing data: Summarize year (one row per every year per every country)

data2 <- data %>%
  group_by(country, year) %>%
  summarize(n())

data2 <- as_tibble(data2)

## Plot has year (yes/no) per country (y) vs time (x)
## Note that some countries have two entries in year ~ 2015

ggplot(data = data2, mapping = aes(x = year, y = country)) +
  geom_bin2d()

## V2 with tiles(): returns true/fals color mapping

ggplot(data = data2, mapping = aes(x = year, y = country)) +
  geom_tile()

## How many entries per country? (number of years)

data3 <- data2 %>%
  group_by(country) %>%
  summarize(count = n())

data3 <- as_tibble(data3)

## what is the max? 32

max(data3$count)

## Plot number of different years per country with custom ticks

ggplot(data = data3, mapping = aes(x = count, y = country)) +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 35, 5))

## Test: Plotting suicide vs. gdp per capita (plotting pretty much all rows)

data4 <- data %>%
  group_by(country, year, suicides.100k.pop, gdp_per_capita....) %>%
  summarize(n())

data4 <- as_tibble(data4)

ggplot(data = data4, mapping = aes(x = gdp_per_capita...., y = suicides.100k.pop)) +
  geom_point(alpha = 0.25) + theme(legend.position="none") +
  geom_smooth()

## The right way

data5 <- data4 %>%
  group_by(country, year) %>%
  mutate(total_suicides_100k_pop = sum(suicides.100k.pop)) %>%
  group_by(country, year, total_suicides_100k_pop, gdp_per_capita....) %>%
  summarize(count = n())

data5 <- as_tibble(data5)

ggplot(data = data5, mapping = aes(x = gdp_per_capita...., y = total_suicides_100k_pop)) +
  geom_point(alpha = 0.25) + theme(legend.position="none") +
  geom_smooth()
