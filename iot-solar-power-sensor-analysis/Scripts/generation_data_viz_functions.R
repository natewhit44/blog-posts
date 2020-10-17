clean_generation_input_tibble <-
function(data, date_type = 'type_1') {
    clean_tbl <- data %>%
        rename_with(tolower) %>%
        select(date_time, plant_id, source_key, dc_power, ac_power, daily_yield, total_yield) %>%
        mutate(
            source_key = as_factor(source_key),
            plant_id = case_when(
                date_type == 'type_1' ~ str_glue('Plant 1 - {plant_id}'),
                TRUE ~ str_glue('Plant 2 - {plant_id}')
            ),
            date_time = case_when(
                date_type == 'type_1' ~ dmy_hm(date_time),
                date_type == 'type_2' ~ ymd_hms(date_time)
            ),
            day = floor_date(date_time, unit='day') %>% ymd(),
            time = hms::as_hms(date_time)
            
        ) %>%
        select(day, date_time, everything())
    
    return(clean_tbl)
}
compute_generation_statistics_by_plant <-
function() {
    
    plant_generation_stats_tbl <- plant_1_generation_tbl %>%
        
        # Combine plant data
        bind_rows(plant_2_generation_tbl) %>%
        
        # Group and summarize
        group_by(plant_id) %>% 
        summarize(
            avg_dc_power = round(mean(dc_power), 2),
            std_dc_power = round(sd(dc_power), 2),
            avg_ac_power = round(mean(ac_power), 2),
            std_ac_power = round(sd(ac_power), 2),
            avg_daily_yield = round(mean(daily_yield), 2),
            std_daily_yield = round(sd(daily_yield), 2)
        ) %>%
        ungroup()
    
    return(plant_generation_stats_tbl)
}
prepare_avg_ac_dc_data <-
function(data) {
    
    avg_ac_dc_tbl <- data %>%
        
        # Group by time and summarize
        group_by(plant_id, time) %>%
        summarize(
            avg_dc_power = mean(dc_power),
            avg_ac_power = mean(ac_power)
        ) %>%
        ungroup() %>% 
        
        # Transform to long form
        gather(key = 'power_type', value = 'power_value', -plant_id, -time) %>%
        
        # Create label text
        mutate(
            power_type = case_when(
                power_type == 'avg_dc_power' ~ 'Average DC Power',
                power_type == 'avg_ac_power' ~ 'Average AC Power'
            ),
            label_text = str_glue("Time: {time}
                                  Power Type: {power_type}
                                  Power Value: {round(power_value, digits = 2)}")
        )
    
    return(avg_ac_dc_tbl)
}
plot_ac_dc_by_plant <-
function(data, plant_name, interactive = TRUE) {
    
    # Data manipulation
    avg_ac_dc_tbl <- prepare_avg_ac_dc_data(data = data)

    # Create chart
    g <- avg_ac_dc_tbl %>%
        ggplot(aes(x = time, y = power_value, color = power_type)) +
        
            # Geometries
            geom_point(aes(text = label_text), alpha = 0.5, size = 2) +
            
            # Formatting
            theme_tq() +
            scale_color_tq() +
            labs(
                title = str_c(plant_name, ' Average AC and DC Power'),
                subtitle = 'Data sampled every 15 mintues',
                x = NULL,
                y = 'Measured Power (kW)',
                color = 'Power Type'
            )
    
    # Interactive vs. Static
    if (interactive) {
        return(ggplotly(g, tooltip = 'text'))
    } else {
        return(g)
    }
}
prepare_avg_conversion_pct_data <-
function(data) {
    
    avg_conversion_pct_tbl <- data %>%
        
        # Group by time and summarize
        group_by(plant_id, time) %>%
        summarize(conversion_pct = mean(ac_power) / mean(dc_power)) %>%
        ungroup() %>%
        
        # Create label text
        mutate(
            label_text = str_glue("Time: {time}
                                  Conversion Percentage: {scales::percent(conversion_pct, accuracy = 0.1)}")
        )
        
    return(avg_conversion_pct_tbl)
}
plot_conversion_pct_by_plant <-
function(data, plant_name, interactive = TRUE) {
    
    # Data manipulation
    avg_conversion_pct_tbl <- prepare_avg_conversion_pct_data(data = data)
    
    # Create plot
    g <- avg_conversion_pct_tbl %>%
        ggplot(aes(x = time, y = conversion_pct, group = 1)) +
        
            # Geometries
            geom_line(aes(text = label_text)) +
            
            # Formatting
            theme_tq() +
            scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
            labs(
                title = str_c(plant_name, ' Average Energy Conversion Percentage'),
                subtitle = 'DC to AC success',
                x = NULL, 
                y = 'DC to AC Conversion Percentage'
            )
    
    # Interactive vs. Static
    if (interactive) {
        return(ggplotly(g, tooltip = 'text'))
    } else {
        return(g)
    }
}
prepare_avg_inverter_ac_dc_data <-
function(data) {
    
    avg_inverter_ac_dc_tbl <- data %>%
        
        # Group by time and summarize
        group_by(plant_id, source_key, time) %>%
        summarize(
            avg_dc_power = mean(dc_power),
            avg_ac_power = mean(ac_power)
        ) %>%
        ungroup() %>% 
        
        # Transform to long form
        gather(key = 'power_type', value = 'power_value', -plant_id, -source_key, -time) %>%
        
        # Create label text
        mutate(
            power_type = case_when(
                power_type == 'avg_dc_power' ~ 'Average DC Power',
                power_type == 'avg_ac_power' ~ 'Average AC Power'
            ),
            label_text = str_glue("Inverter ID: {source_key}
                                  Time: {time}
                                  Power Type: {power_type}
                                  Power Value: {round(power_value, digits = 2)}")
        )
    
    return(avg_inverter_ac_dc_tbl)
}
plot_ac_dc_by_inverter <-
function(data, plant_name, interactive = TRUE) {
    
    # Data manipulation
    avg_inverter_ac_dc_tbl <- prepare_avg_inverter_ac_dc_data(data = data)
    
    # Create plot
    g <- avg_inverter_ac_dc_tbl %>%
        ggplot(aes(x = time, y = power_value, color = source_key, group = 1)) +
        
            # Geometries
            geom_line(aes(text = label_text)) +
            facet_wrap(~ power_type, ncol = 1, scales = 'free_y') +
            
            # Formatting
            theme_tq() +
            scale_color_tq() +
            labs(
                title = str_c(plant_name, ' Average AC and DC Power by Inverter'),
                subtitle = 'Data sampled every 15 mintues',
                x = NULL,
                y = 'Measured Power (kW)',
                color = 'Inverter ID'
            )
            
    # Interactive vs. Static
    if (interactive) {
        return(ggplotly(g, tooltip = 'text'))
    } else {
        return(g)
    }
}
prepare_avg_inverter_conversion_pct_data <-
function(data) {
    
    avg_inverter_conversion_pct_tbl <- data %>%
        
        # Group by time and summarize
        group_by(plant_id, source_key, time) %>%
        summarize(conversion_pct = mean(ac_power) / mean(dc_power)) %>%
        ungroup() %>%
        
        # Create label text
        mutate(
            label_text = str_glue("Inverter ID: {source_key}
                                  Time: {time}
                                  Conversion Percentage: {scales::percent(conversion_pct, accuracy = 0.1)}")
        )
    
    return(avg_inverter_conversion_pct_tbl)
}
plot_conversion_pct_by_inverter <-
function(data, plant_name, interactive = TRUE) {
    
    # Data manipulation
    avg_inverter_conversion_pct_tbl <- prepare_avg_inverter_conversion_pct_data(data = data)
    
    # Create plot
    g <- avg_inverter_conversion_pct_tbl %>%
        ggplot(aes(x = time, y = conversion_pct, color = source_key, group = 1)) +
        
            # Geometries
            geom_line(aes(text = label_text)) +
            
            # Formatting
            theme_tq() +
            scale_color_tq() +
            scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
            labs(
                title = str_c(plant_name, ' Average Energy Conversion Percentage by Interter'),
                subtitle = 'DC to AC success',
                x = NULL, 
                y = 'DC to AC Conversion Percentage',
                color = 'Inverter ID'
            )
    
    # Interactive vs. Static
    if (interactive) {
        return(ggplotly(g, tooltip = 'text'))
    } else {
        return(g)
    }
}
prepare_avg_inverter_daily_yield <-
function(data) {
    
    # Data manipulation
    avg_daily_yield_tbl <- data %>%
        
        # Compute daily yield by inverter
        group_by(source_key, day) %>%
        mutate(ac_daily_yield = cumsum(ac_power)) %>%
        ungroup() %>%
        
        # Group by inverter and compute avg daily yield 
        group_by(source_key, time) %>%
        summarize(avg_ac_daily_yield = mean(ac_daily_yield)) %>%
        ungroup() %>%
        
        # Create tooltip text
        mutate(label_text = str_glue("Inverter ID; {source_key}
                                     Time: {time}
                                     Average Daily Yield: {avg_ac_daily_yield}"))
    
    return(avg_daily_yield_tbl)
}
plot_avg_daily_yield_by_inverter <-
function(data, plant_name, interactive = TRUE) {
    
    # Data manipulation
    avg_daily_yield_tbl <- prepare_avg_inverter_daily_yield(data = data)
    
    # Create chart
    g <- avg_daily_yield_tbl %>% 
        ggplot(aes(x = time, y = avg_ac_daily_yield, color = source_key, group = 1)) +
        
        # Geometries
        geom_line(aes(text = label_text)) +
        
        # Formatting
        theme_tq() +
        scale_color_tq() +
        labs(
            title = str_c(plant_name, ' Average Daily AC Power Yield'),
            subtitle = 'Cumulative sum of power generated during target day',
            x = NULL, 
            y = 'Average Daily Power Yield (kW)',
            color = 'Inverter ID'
        )
    
    # Interactive vs. Static
    if (interactive) {
        return(ggplotly(g, tooltip = 'text'))
    } else {
        return(g)
    }
}
