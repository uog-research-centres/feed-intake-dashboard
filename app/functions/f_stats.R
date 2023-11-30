#Function to calculate the daily stats 
daily_stats <- function(data, ndays){
  # will take the cleaned and corrected data as input
  
  # remove all negative values before analysis
  data <- data %>%
    filter(start_weight_kg >= 0,
           end_weight_kg >= 0,
           intake_kg >= 0)
  
  # summary statistics of total daily intake
  cow_daily_intakes <- 
    data %>% 
    group_by(animal_id, date) %>%
    summarise(across(c(contains('intake'), contains('duration'), where(is.logical)),
                     list(sum = ~sum(.x, na.rm=TRUE), 
                          mean = ~mean(.x, na.rm=TRUE),
                          sd = ~sd(.x, na.rm=TRUE))))
  
  # Fill the missing day's data with NAs
  cow_daily_intakes <- 
    cow_daily_intakes %>%
    complete(date = seq(min(date), max(date), by = "1 day")) %>%
    arrange(animal_id, date) %>% 
    mutate(across(c(intake_kg_sum, intake_kg_mean, intake_kg_sd, duration_sec_sum, duration_sec_mean, duration_sec_sd), 
                  ~ifelse(is.na(.), NA, .)))
  
  #calculating daily statistics along with % intake for each cow 
  cow_daily_stats <- cow_daily_intakes %>% 
    group_by(animal_id) %>%
    arrange(animal_id, date) %>% 
    mutate(rolling_mean = zoo::rollapplyr(intake_kg_sum, width = ndays, FUN = function(x) mean(x, na.rm = TRUE), fill = NA, align = "right"),
           rolling_sd = zoo::rollapplyr(intake_kg_sum, width = ndays, FUN = function(x) sd(x, na.rm = TRUE), fill = NA, align = "right"),
           rolling_min= zoo::rollapplyr(intake_kg_sum, width = ndays, FUN = min, fill = NA, align = "right"),
           rolling_max = zoo::rollapplyr(intake_kg_sum, width = ndays, FUN = max, fill = NA, align = "right"),
           rolling_lower_q = zoo::rollapplyr(intake_kg_sum, width = ndays, FUN = function(x) quantile(x, 0.25, na.rm = TRUE), fill = NA, align = "right"),
           rolling_upper_q = zoo::rollapplyr(intake_kg_sum, width = ndays, FUN = function(x) quantile(x, 0.75, na.rm = TRUE), fill = NA, align = "right")) %>%
    complete(date = seq(min(dates), max(dates), by = "1 day")) %>%
    filter(date == max(dates),
           !(is.na(rolling_mean))) %>% 
    rename("intake_kg" = intake_kg_sum) %>% 
        select(animal_id, date, intake_kg, rolling_mean, rolling_sd, rolling_min, rolling_max, rolling_lower_q, rolling_upper_q) %>%
    mutate(pct_of_rolling_mean = intake_kg / rolling_mean * 100) %>% 
    relocate(pct_of_rolling_mean, .after = rolling_mean)

  return(cow_daily_stats)
}
