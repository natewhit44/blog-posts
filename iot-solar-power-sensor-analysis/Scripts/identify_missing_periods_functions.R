find_missing_weather_periods <-
function(data) {
    
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
find_missing_generation_periods <-
function(data) {
    
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
