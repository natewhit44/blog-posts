# Identify periods of missing data ----

# Load libraries ----
library(tidyverse)
library(tidyquant)
library(lubridate)
library(plotly)

# Source preparation functions ----
source('Scripts/weather_data_viz_functions.R')
source('Scripts/generation_data_viz_functions.R')

# Load data ----
plant_1_weather_tbl <- read_csv('Data/Plant_1_Weather_Sensor_Data.csv')
plant_2_weather_tbl <- read_csv('Data/Plant_2_Weather_Sensor_Data.csv')

plant_1_generation_tbl <- read_csv('Data/Plant_1_Generation_Data.csv')
plant_2_generation_tbl <- read_csv('Data/Plant_2_Generation_Data.csv')


# Prepare data tibbles ----
plant_1_weather_tbl <- clean_weather_input_tibble(data = plant_1_weather_tbl)
plant_2_weather_tbl <- clean_weather_input_tibble(data = plant_2_weather_tbl)

plant_1_generation_tbl <- clean_generation_input_tibble(data = plant_1_generation_tbl, date_type = 'type_1')
plant_2_generation_tbl <- clean_generation_input_tibble(data = plant_2_generation_tbl, date_type = 'type_2')


# Identify missing periods in weather data ----
find_missing_weather_periods <- function(data) {
    
    missing_periods_tbl <- data %>%
        
        # Project timestamp column
        select(date_time) %>%
        
        # Get next timestamp
        mutate(next_date_time = lead(date_time)) %>%
        
        # Compute time difference
        mutate(diff = next_date_time - date_time) %>%
        
        # Filter periods longer than sample rate
        filter(diff > minutes(15)) %>%
        
        # Sort by timestamp
        arrange(date_time)
    
    return(missing_periods_tbl)
}

find_missing_weather_periods(data = plant_1_weather_tbl)
find_missing_weather_periods(data = plant_2_weather_tbl)

# Identify missing periods in generation data ----
find_missing_generation_periods <- function(data) {
    
    missing_periods_tbl <- data %>%
        
        # Project time column
        select(date_time, source_key) %>%
        
        # Group by inverter ID
        group_by(source_key) %>%
        
        # Get next timestamp
        mutate(next_date_time = lead(date_time)) %>%
        
        # Compute time difference
        mutate(diff = next_date_time - date_time) %>%
        
        # Filter periods longer than sample rate
        filter(diff > minutes(15)) %>%
        
        # Sort by inverter ID and timestamp
        arrange(source_key, date_time)
        
    return(missing_periods_tbl)
}

find_missing_generation_periods(data = plant_1_generation_tbl)
find_missing_generation_periods(data = plant_2_generation_tbl)

# Export functions
function_names <- c('find_missing_weather_periods',
                    'find_missing_generation_periods')

dump(function_names, file = "Scripts/identify_missing_periods_functions.R")
