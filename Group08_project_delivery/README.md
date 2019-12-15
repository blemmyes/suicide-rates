# Group 8: Suicide Rates

## How to Run

- run `suicide-app.R` using `Rscript` ‒ or just type `./suicide-app.R`
- visit [localhost:1337](http://localhost:1337)

The file `analysis.R` contains more code that was used for the data analysis.

## Data Source

[Suicide Rates 1985-2016](https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016/download)

## Description of Data

The datasource gives an overview of suicides in different countries, mostly
America, Europe and Australia. The suicides are further distinguished by
gender, gdp, population, age and generation.

### Attributes

- `country`
- `year`
- `sex`
- `age`
- `suicides_no`
- `population`
- `suicides/100k pop`
- `country-year`
- `HDI for year`
- `gdp_for_year ($)`
- `gdp_per_capita ($)`
- `generation`

## Research Questions

Based on the information available we assume there to be a correlation between
gdp per capita and the number of suicides in a country. There should also be
different suicide rates depending on generation. The countries can also be
compared with each other. 
