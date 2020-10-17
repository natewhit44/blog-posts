# Load libraries
library(tidyverse)
library(tidyquant)
library(lubridate)
library(plotly)
library(timetk)
library(modeltime)
library(tidymodels)

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

### Clean up code below
plant_1_tbl <- plant_1_weather_tbl %>%
    left_join(plant_1_generation_tbl, by = 'date_time') %>%
    select(plant_id, source_key, date_time, time, everything())

plant_1_tbl <- plant_1_tbl %>%
    group_by(plant_id, day) %>%
    summarize(
        avg_ambient_temperature = mean(ambient_temperature),
        avg_module_temperature = mean(module_temperature),
        avg_irradiation = mean(irradiation),
        total_dc_power = sum(dc_power, na.rm = TRUE),
        total_ac_power = sum(ac_power, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(! is.na(day))

plant_1_tbl %>%
    plot_time_series(day, total_ac_power)

# Split data 80/20
splits <- initial_time_split(plant_1_tbl, prop = 0.8)

# Model 1: auto_arima ----
model_fit_arima_no_boost <- arima_reg() %>%
    set_engine(engine = 'auto_arima') %>%
    fit(total_dc_power ~ day, data = training(splits))

# Model 2: arima_boost ----
model_fit_arima_boosted <- arima_boost(min_n = 2, learn_rate = 0.015) %>%
    set_engine(engine = 'auto_arima_xgboost') %>%
    fit(total_dc_power ~ day + as.numeric(day) + factor(month(day, label = TRUE), ordered = FALSE),
        data = training(splits))

# Model 3: Exponential smoothing ----
model_fit_ets <- exp_smoothing() %>%
    set_engine(engine = 'ets') %>%
    fit(total_dc_power ~ day, data = training(splits))

# Model 4: Prophet ----
model_fit_prophet <- prophet_reg() %>%
    set_engine(engine = 'prophet') %>%
    fit(total_dc_power ~ day, data = training(splits))

# Model 5: Linear regression ----
model_fit_lm <- linear_reg() %>%
    set_engine(engine = 'lm') %>%
    fit(total_dc_power ~ as.numeric(day) + factor(month(day, label = TRUE), ordered = FALSE),
        data = training(splits))

# Add fitted models to table
models_tbl <- modeltime_table(
    model_fit_arima_no_boost,
    model_fit_arima_boosted,
    model_fit_ets,
    model_fit_prophet,
    model_fit_lm
)

# Visualize forecasts
calibration_tbl %>%
    modeltime_forecast(
        new_data    = testing(splits),
        actual_data = plant_1_tbl
    ) %>%
    plot_modeltime_forecast(
        .legend_max_width = 25, # For mobile screens
        .interactive      = TRUE
    )

# Validate models on testing set
calibration_tbl <- models_tbl %>%
    modeltime_calibrate(new_data = testing(splits))

# Validation table
calibration_tbl %>%
    modeltime_accuracy() %>%
    table_modeltime_accuracy(
        .interactive = TRUE
    )

# Refit to all data and foreacst
refit_tbl <- calibration_tbl %>%
    modeltime_refit(data = plant_1_tbl)

refit_tbl %>%
    modeltime_forecast(h = "12 days", actual_data = plant_1_tbl) %>%
    plot_modeltime_forecast(
        .legend_max_width = 25, # For mobile screens
        .interactive      = TRUE
    )
