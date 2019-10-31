#!/usr/bin/env Rscript

library('tibble')

read_suicide_data <- function(csv_file_path) {
    data_frame <- read.csv(csv_file_path)
    tbbl <- as_tibble(data_frame)
    return(tbbl)
}

data <- read_suicide_data('data/master.csv')
print(data)
