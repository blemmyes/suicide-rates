#!/usr/bin/env Rscript

library('tibble')

read_suicide_data <- function(csv_file_path) {
    data_frame <- read.csv(csv_file_path)
    return as_tibble(data_frame)
}

read_suicide_data('data/master.csv')
