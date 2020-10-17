# Visualizing environment data ----

# Load libraries ----
library(tidyverse)
library(tidyquant)
library(lubridate)
library(plotly)

# Load data ----
plant_1_weather_tbl <- read_csv('Data/Plant_1_Weather_Sensor_Data.csv')
plant_2_weather_tbl <- read_csv('Data/Plant_2_Weather_Sensor_Data.csv')

plant_1_weather_tbl %>%
    slice(1:5)

# Clean up input tibbles ----
clean_weather_input_tibble <- function(data) {
    
    clean_tbl <- data %>%
        
        # Lower case column names
        rename_with(tolower) %>%
        
        # Project desired columns
        select(date_time, ambient_temperature, module_temperature, irradiation)
    
    return(clean_tbl)
}

# Prepare data tibbles ----
plant_1_weather_tbl <- clean_weather_input_tibble(data = plant_1_weather_tbl)
plant_2_weather_tbl <- clean_weather_input_tibble(data = plant_2_weather_tbl)

plant_1_weather_tbl %>%
    slice(1:5)

# Compute weather data statistics by plant ---
compute_weather_statistics_by_plant <- function() {
    
    plant_weather_stats_tbl <- plant_1_weather_tbl %>%
        
        # Add plant ID
        mutate(plant_id = 'Plant 1 - 4135001' ) %>%
        
        # Combine plant data
        bind_rows(plant_2_weather_tbl %>% mutate(plant_id = 'Plant 2 - 4136001')) %>%
        
        # Compute farenheit 
        mutate(
            ambient_temperature = (ambient_temperature * 9/5) + 32,
            module_temperature = (module_temperature * 9/5) + 32
        ) %>%
        
        # Group and summarize
        group_by(plant_id) %>% 
        summarize(
            avg_ambient_temperature = round(mean(ambient_temperature), 2),
            std_ambient_temperature = round(sd(ambient_temperature), 2),
            avg_module_temperature = round(mean(module_temperature), 2),
            std_module_temperature = round(sd(module_temperature), 2),
            avg_irradiation = round(mean(irradiation), 2),
            std_irradiation = round(sd(irradiation), 2)
        ) %>%
        ungroup()
    
    return(plant_weather_stats_tbl)
}

compute_weather_statistics_by_plant()

# Average hourly measurements ----
prepare_weather_data <- function(data) {
    
    avg_by_day_h_tbl <- data %>%
        
        # Prepare day and hour of day columns
        mutate(
            day = floor_date(date_time, unit='day') %>% ymd(),
            hour = hour(date_time)
        ) %>%
        
        # Group by day and hour and summarize
        group_by(day, hour) %>%
        summarize(
            ambient_temp_celsius = mean(ambient_temperature),
            ambient_temp_farenheit = (mean(ambient_temperature) * 9/5) + 32,
            module_temp_celsius = mean(module_temperature),
            module_temp_farenheit = (mean(module_temperature) * 9/5) + 32,
            irradiation = mean(irradiation)
        ) %>%
        ungroup()
    
    return(avg_by_day_h_tbl)
}

prepare_weather_data(data = plant_1_weather_tbl)
prepare_weather_data(data = plant_2_weather_tbl)

# Average hourly ambient temperature heatmap ----
plot_ambient_temp_heatmap <- function(data, plant_name, date_format = '%B %d, %Y', interactive = TRUE) {
    
    # Data Manipulation
    avg_day_h_tbl <- prepare_weather_data(data = data)
    
    # Create plot
    g <- avg_day_h_tbl %>%
        mutate(label_text = str_glue("Date: {format(day, date_format)}
                                     Hour: {hour}
                                     Ambient Temp (C): {round(ambient_temp_celsius, digits = 1)}
                                     Ambient Temp (F): {round(ambient_temp_farenheit, digits = 1)}")) %>%
        ggplot(aes(x = hour, y = day)) +
        
            # Geometries
            geom_tile(aes(fill = ambient_temp_farenheit)) +
            geom_text(aes(label = round(ambient_temp_farenheit), 
                                        text = label_text),
                      size = 2) +
            
            # Formatting
            scale_fill_gradient(low = '#62d7f5', high = '#eb4034') +
            scale_x_continuous(breaks = 0:23) +
            theme_tq() +
            labs(
                title = str_c(plant_name, ' Ambient Temperature Heatmap'),
                subtitle = 'Temperature in degrees farenheit',
                x = 'Hour of Day',
                y = NULL,
                fill = 'Degrees Farenheit'
            )
    
    # Interactive vs. static
    if (interactive) {
        return(ggplotly(g, tooltip = 'text'))
    } else {
        return(g)
    }
}

plot_ambient_temp_heatmap(data = plant_1_weather_tbl, plant_name = 'Plant 1')
plot_ambient_temp_heatmap(data = plant_2_weather_tbl, plant_name = 'Plant 2')

# Average hourly module temperature heatmap ----
plot_module_temp_heatmap <- function(data, plant_name, date_format = '%B %d, %Y', interactive = TRUE) {
    
    # Data Manipulation
    avg_day_h_tbl <- prepare_weather_data(data = data)
    
    # Create plot
    g <- avg_day_h_tbl %>%
        mutate(label_text = str_glue("Date: {format(day, date_format)}
                                     Hour: {hour}
                                     Module Temp (C): {round(module_temp_celsius, digits = 1)}
                                     Module Temp (F): {round(module_temp_farenheit, digits = 1)}")) %>%
        ggplot(aes(x = hour, y = day)) +
        
            # Geometries
            geom_tile(aes(fill = module_temp_farenheit)) +
            geom_text(aes(label = round(module_temp_farenheit), 
                          text = label_text),
                      size = 2) +
            
            # Formatting
            scale_fill_gradient(low = '#62d7f5', high = '#eb4034') +
            scale_x_continuous(breaks = 0:23) +
            theme_tq() +
            labs(
                title = str_c(plant_name, ' Module Temperature Heatmap'),
                subtitle = 'Temperature in degrees farenheit',
                x = 'Hour of Day',
                y = NULL,
                fill = 'Degrees Farenheit'
            )
    
    # Interactive vs. static
    if (interactive) {
        return(ggplotly(g, tooltip = 'text'))
    } else {
        return(g)
    }
}

plot_module_temp_heatmap(data = plant_1_weather_tbl, plant_name = 'Plant 1')
plot_module_temp_heatmap(data = plant_2_weather_tbl, plant_name = 'Plant 2')

# Average hourly irradiation heatmap ----
plot_irradiation_heatmap <- function(data, plant_name, date_format = '%B %d, %Y', interactive = TRUE) {
    
    # Data Manipulation
    avg_day_h_tbl <- prepare_weather_data(data = data)
    
    # Create plot
    g <- avg_day_h_tbl %>%
        mutate(label_text = str_glue("Date: {format(day, date_format)}
                                     Hour: {hour}
                                     Module Temp (C): {round(irradiation, digits = 1)}
                                     Module Temp (F): {round(irradiation, digits = 1)}")) %>%
        ggplot(aes(x = hour, y = day)) +
        
            # Geometries
            geom_tile(aes(fill = irradiation)) +
            geom_text(aes(label = round(irradiation, digits=2), 
                          text = label_text),
                      size = 2) +
            
            # Formatting
            scale_fill_gradient(low = '#fcdf03', high = '#eb4034') +
            scale_x_continuous(breaks = 0:23) +
            theme_tq() +
            labs(
                title = str_c(plant_name, ' Irradiation Heatmap'),
                subtitle = 'Watts per square meter (W/m^2)',
                x = 'Hour of Day',
                y = NULL,
                fill = 'Watts per Sq. Meter'
            )
    
    # Interactive vs. static
    if (interactive) {
        return(ggplotly(g, tooltip = 'text'))
    } else {
        return(g)
    }
}

plot_irradiation_heatmap(data = plant_1_weather_tbl, plant_name = 'Plant 1')
plot_irradiation_heatmap(data = plant_2_weather_tbl, plant_name = 'Plant 2')

# Average hourly ambient vs. module temperature ----
plot_avg_hourly_ambient_module_temp <- function(data, plant_name, interactive = TRUE) {
    
    # Data manipulation
    avg_hourly_temp_tbl <- prepare_weather_data(data = data) %>%
        
        # Group by hour and compute mean temps
        group_by(hour) %>%
        summarize(
            avg_ambient_temp_farenheit = mean(ambient_temp_farenheit),
            avg_module_temp_farenheit = mean(module_temp_farenheit)
        ) %>%
        ungroup() %>%
        
        # Create label text
        mutate(label_text = str_glue("Hour: {hour}
                                     Ambient Temp (F): {round(avg_ambient_temp_farenheit, digits = 1)}
                                     Module Temp (F): {round(avg_module_temp_farenheit, digits = 1)}"))
    
    # Create plot
    g <- avg_hourly_temp_tbl %>%
        ggplot(aes(x = hour, group = 1)) +
        
        # Geometries
        geom_line(aes(y = avg_ambient_temp_farenheit, text = label_text, color = 'Ambient Temp'), 
                  size = 1) +
        geom_line(aes(y = avg_module_temp_farenheit, text = label_text, color = 'Module Temp'), 
                  size = 1) +
        
        # Formatting
        scale_color_manual(name = 'Source',
                           values = c('Ambient Temp' = '#2c3e50','Module Temp' = 'red')) +
        scale_x_continuous(breaks = c(0:23)) +
        theme_tq() +
        labs(
            title = str_c(plant_name, ' Average Hourly Ambient and Module Temperature (F)'),
            x = 'Hour of Day',
            y = 'Average Hourly Temperature (F)'
        )
    
    # Interactive vs. static
    if (interactive) {
        return(ggplotly(g, tooltip = 'text'))
    } else {
        return(g)
    }
}

plot_avg_hourly_ambient_module_temp(data = plant_1_weather_tbl, plant_name = 'Plant 1')
plot_avg_hourly_ambient_module_temp(data = plant_2_weather_tbl, plant_name = 'Plant 2')

# Export functions
function_names <- c('clean_weather_input_tibble', 
                    'compute_weather_statistics_by_plant',
                    'prepare_weather_data',
                    'plot_ambient_temp_heatmap',
                    'plot_module_temp_heatmap',
                    'plot_irradiation_heatmap',
                    'plot_avg_hourly_ambient_module_temp')

dump(function_names, file = "Scripts/weather_data_viz_functions.R")
