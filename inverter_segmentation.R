# Clustering inverters ---- 

# Load libraries ----
library(tidyverse)
library(tidyquant)
library(tidymodels)
library(umap)
library(lubridate)
library(plotly)

# Source preparation functions ----
source('Scripts/generation_data_viz_functions.R')

# Load data ----
plant_1_generation_tbl <- read_csv('Data/Plant_1_Generation_Data.csv')
plant_2_generation_tbl <- read_csv('Data/Plant_2_Generation_Data.csv')


# Prepare data tibbles ----
plant_1_generation_tbl <- clean_generation_input_tibble(data = plant_1_generation_tbl, date_type = 'type_1')
plant_2_generation_tbl <- clean_generation_input_tibble(data = plant_2_generation_tbl, date_type = 'type_2')


plant_1_generation_tbl %>% 
slice(1:5)

# Segment inverters based on DC power
segment_inverters <- function(data, k = 5, seed = 7) {
    
    plant_dc_spread_tbl <- data %>%
        
        # Group by inverter and compute average DC power
        group_by(plant_id, source_key, time) %>%
        summarize(avg_dc_power = mean(dc_power)) %>%
        ungroup() %>%
        
        # Transform int matrix
        spread(key = time, value = avg_dc_power) 
    
    # K-means clustering
    set.seed(seed)
    kmeans_obj <- plant_dc_spread_tbl %>%
        select(- plant_id, - source_key) %>%
        kmeans(centers = k, nstart = 100)
    
    kmeans_tbl <- kmeans_obj %>%
        augment(plant_dc_spread_tbl) %>%
        select(plant_id, source_key, .cluster)
    
    # Umap
    umap_config <- umap.defaults
    umap_config$random_state <- seed
    
    umap_obj <- plant_dc_spread_tbl %>%
        select(- plant_id, - source_key) %>%
        as.matrix() %>%
        umap(config = umap_config)
    
    umap_tbl <- umap_obj %>%
        pluck('layout') %>%
        as_tibble() %>%
        set_names(c('x', 'y')) %>%
        bind_cols(plant_dc_spread_tbl %>% select(plant_id, source_key))
    
    # Combine k-means and umap
    combined_tbl <- umap_tbl %>%
        left_join(kmeans_tbl, by = c('plant_id', 'source_key')) %>%
        mutate(label_text = str_glue("Plant ID: {plant_id}
                                     Inverter ID: {source_key}
                                     Cluster ID: {.cluster}"))
    
    return(combined_tbl)
}

segment_inverters(data = plant_1_generation_tbl, k = 5)

# K-means clustering
kmeans_mapper <- function(data = plant_1_generation_tbl, centers) {
    
    kmeans_obj <- data %>%
        
        # Compute conversion percentage
        mutate(
            conversion_pct = ac_power / dc_power,
            conversion_pct = replace_na(conversion_pct, replace = 0)
        ) %>%
        
        # Group by inverter and compute average DC power
        group_by(plant_id, source_key, time) %>%
        summarize(conversion_pct = mean(conversion_pct)) %>%
        ungroup() %>%
        
        # Transform int matrix
        spread(key = time, value = conversion_pct) %>%
        
        # Remove name variable
        select(- plant_id, - source_key) %>%
        
        # Run kmeans algo
        kmeans(centers = centers, nstart = 100)
    
    return(kmeans_obj)
}

# Map function to many arguments
kmeans_mapped_tbl <- tibble(k=1:9) %>%
    mutate(k_means = map(data = plant_2_generation_tbl, k, .f = kmeans_mapper)) %>%
    mutate(glance = map(k_means, glance))

kmeans_mapped_tbl

# Skree Plot ----
kmeans_mapped_tbl %>%
    
    # Unpack condensed data
    unnest(glance) %>%
    
    # Visualize stability
    ggplot(aes(x = k, y = tot.withinss)) +
        geom_point(color='#2c3e50', size=4) +
        geom_line(color='#2c3e50', size=1) +
        ggrepel::geom_label_repel(aes(label=k), color='#2c3e50') +
        
        # Formatting
        theme_tq() +
        labs(
            title='Skree Plot',
            subtitle='Measures distance of each customer from centroid',
            x='Number of Clusters',
            y='Within Sum of Squared Error',
            caption='Conclusion: Based on the Skree plot, 4 clusters would be segement the customer base'
        )


# Visualize inverter segments
plot_inverter_segments <- function(data, k = 5, interactive = TRUE) {
    
    # Data manipulation
    inverter_segments_tbl <- segment_inverters(data = data, k = k)
    
    # Create chart
    g <- inverter_segments_tbl %>%
        ggplot(aes(x = x, y = y, color = .cluster)) +
        
            # Geometries
            geom_point(aes(text = label_text), size = 3) +
            
            # Formatting
            theme_tq() +
            scale_color_tq() +
            labs(
                title = 'Inverter Segmentation: 2D Projection',
                subtitle = 'UMAP 2D Projection with K-Means Cluster Assignment'
            ) +
            theme(legend.position = 'none')
    
    # Interactive vs. Static
    if (interactive) {
        ggplotly(g, tooltip = 'text')
    } else {
        g + ggrepel::geom_label_repel(aes(label = label_text), size = 2)
        return(g)
    }
}

plot_inverter_segments(data = plant_2_generation_tbl, k = 3)

# Identifying inverts with unusual zero power readings
plant_1_generation_tbl %>%
    mutate(zero_reading = case_when(
        dc_power == 0 & 
            time >= hms('8:00:00') &
            time <= hms('16:00:00') ~ 1,
        TRUE ~ 0
        )
    ) %>%
    filter(zero_reading == 1) %>%
    distinct(plant_id, source_key, day, zero_reading) %>%
    left_join(plant_1_generation_tbl, by = c('day', 'source_key')) %>%
    ggplot(aes(x = time, y = dc_power, color = source_key)) +
        geom_line() +
        facet_wrap(~ source_key + day) + 
        theme_tq() +
        scale_color_tq() +
        theme(
            legend.position = 'none'
        ) +
        labs(
            title = 'Questionable Daily DC Readings',
            subtitle = 'Measurements drop to zero mid day',
            x = NULL,
            y = 'DC Power (kW)'
        )
