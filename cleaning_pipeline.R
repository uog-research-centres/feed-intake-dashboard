library(lubridate)
library(tidyverse)
library(IntakeInspectR)
library(data.table)
library(tidyr)
library(dplyr)

#Using September month's data
start_date <- as.Date("2023-09-01")
end_date <- as.Date("2023-09-30")

config <- rjson::fromJSON(file="app/.config.json")

cleaning_pipeline <- function(start_date, end_date,config) {
  dates <- seq.Date(from = start_date, to = end_date, by = 1)
  
  
  files <- lapply(dates, function (i) {
    data <- fread(format(i, config$apollo_path), header = F)
    data <- data[,1:10]
    data$date <- i
    colnames(data) <- c("transponder_id", "animal_id", "bin_id", "start_time", "end_time", "duration_sec", "start_weight_kg", "end_weight_kg", "diet", "intake_kg", "date")
    data <- relocate(data, date, .before=transponder_id) %>%
      mutate(animal_id = as.numeric(animal_id))
    return(data)
    
  })
  
  data <- rbindlist(files,use.names = TRUE, fill = TRUE)
  
  
  
  
  #Converting start_time and end_time datatype to be compatible with the function 
  
  data$start_time <- as.POSIXct(paste(data$date, data$start_time), format = "%Y-%m-%d %H:%M:%S") 
  data$end_time <- as.POSIXct(paste(data$date, data$end_time), format = "%Y-%m-%d %H:%M:%S")
  
  ##Cleaning Function
  #Applying the by bin cleaning function
  
  #Cleaning row  by row errors within the data collected by the feed bin
  #The majority of stages group the data by feed bin ID and day, allowing for separate analysis of each feed bin and day. 
  #In the last phase, the data is aggregated by cow ID to determine whether the cow visited another feed bin while she was still being recorded at that feed bin. It also checks for wrongly recorded end timings.
  #Additionally, it creates a log file that summarises each error by percent and count. This is stored in the temporary file path that the output list returns.
  list_cleaned <-
    IntakeInspectR::f_by_bin_clean(
      data,
      zero_thresh = 0.3, 
      feedout_thresh = 10, 
      col_bin_ID = bin_id,
      col_cow_ID = animal_id,
      col_date = date,
      col_start_time = start_time,
      col_end_time = end_time,
      col_start_weight = start_weight_kg,
      col_end_weight = end_weight_kg,
      col_intake = intake_kg,
      log = FALSE
    )
  
  
  ##  by cow cleaning 
  #Function to iterate each cow's data through the 'by cow' cleaning function
  by_cow_list_out <- 
    IntakeInspectR::f_iterate_cows(
      list_cleaned$df_cleaned,
      col_cow_id = animal_id,
      col_bin_id = bin_id,
      col_date = date,
      col_start_time =  start_time,
      col_intake =  corrected_intake_bybin,
      col_duration = corrected_feed_duration_seconds,
      sd_thresh = 5, 
      shiny.session = NULL, # use NULL if not inside a shiny app
      log = TRUE
    )
  # 2 Step outlier detection process 
  merged_by_cow <- 
    by_cow_list_out$nested_out %>% 
    IntakeInspectR:::f_merge_corrected_outlier_data()
  
 
  
  #keeping all the corrections (final_intake_kg and final_duration_sec), additionally a new column is added to check for any modifications from pre-calculated flags
  simplified_final_df <- 
    merged_by_cow %>% 
    #Keep only corrected end weight and end time
    # calculate new intakes and durations:
    mutate(
      selected_final_intake_kg = start_weight_kg - corrected_end_weight_bybin,
      selected_final_duration_sec = corrected_end_time - start_time
      
    ) %>% 
    dplyr::rowwise() %>%
    dplyr::mutate(
      # overall flag for if anything was modified in the event
      is_modified = any(is_corrected_intake_bybin, is_end_time_overlap_error, is_outlier, na.rm=TRUE)
    ) %>% 
    dplyr::ungroup() %>% 
    # dplyr::select(
    #   bin_id, animal_id, date, start_time, corrected_end_time, start_weight_kg, corrected_end_weight_bybin,
    #   final_intake_kg, 
    #   final_duration_sec, 
    #   is_modified
    #   
    # ) %>% 
    # remove unwanted columns:
    dplyr::select(
      bin_id, animal_id, date, diet ,start_time, corrected_end_time, start_weight_kg, corrected_end_weight_bybin,
      selected_final_intake_kg, selected_final_duration_sec
    )
  
  
  #converting the time variables back into its original format 
  simplified_final_df$start_time <- as.character(simplified_final_df$start_time)
  simplified_final_df$corrected_end_time <- as.character(simplified_final_df$corrected_end_time)


  
  return(simplified_final_df)
  
  
}


cleaned_data <- cleaning_pipeline(start_date, end_date, config)

#write.csv(cleaning_pipeline(start_date, end_date), "cleaned_cow_data.csv", row.names = FALSE)


#Function to calculate the daily stats 
daily_stats <- function(data, width_parameter){
  #will take the cleaned and corrected data as input
  
  #remove all negative values before analysis
  data <- data %>%
    filter(start_weight_kg >= 0)
  
  #summary statistics of total daily intake
  cow_daily_intakes <- 
    data %>% 
    group_by(animal_id, date) %>%
    summarise(across(c(contains('intake'), contains('duration'), where(is.logical)),
                     list(sum = ~sum(.x, na.rm=TRUE), 
                          mean = ~mean(.x, na.rm=TRUE),
                          sd = ~sd(.x, na.rm=TRUE))))
  
  #Fill the missing day's data with NAs
  cow_daily_intakes <- 
    cow_daily_intakes %>%
    complete(date = seq(min(date), max(date), by = "1 day")) %>%
    arrange(animal_id, date) %>% 
    mutate(across(c(selected_final_intake_kg_sum, selected_final_intake_kg_mean, selected_final_intake_kg_sd, selected_final_duration_sec_sum,selected_final_duration_sec_mean,selected_final_duration_sec_sd), 
                  ~ifelse(is.na(.), NA, .)))
  
  
  
#calculating daily statistics along with % intake for each cow 
  cow_daily_stats <- cow_daily_intakes %>% 
      group_by(animal_id) %>%
      arrange(animal_id, date) %>% 
      mutate(mean = zoo::rollapplyr(selected_final_intake_kg_sum, width = width_parameter, FUN = function(x) mean(x, na.rm = TRUE), fill = NA, align = "right"),
             sd = zoo::rollapplyr(selected_final_intake_kg_sum, width = width_parameter, FUN = function(x) sd(x, na.rm = TRUE), fill = NA, align = "right"),
             min= zoo::rollapplyr(selected_final_intake_kg_sum, width = width_parameter, FUN = min, fill = NA, align = "right"),
             max = zoo::rollapplyr(selected_final_intake_kg_sum, width = width_parameter, FUN = max, fill = NA, align = "right"),
             lower_q = zoo::rollapplyr(selected_final_intake_kg_sum, width = width_parameter, FUN = function(x) quantile(x, 0.25, na.rm = TRUE), fill = NA, align = "right"),
             upper_q = zoo::rollapplyr(selected_final_intake_kg_sum, width = width_parameter, FUN = function(x) quantile(x, 0.75, na.rm = TRUE), fill = NA, align = "right")) %>%
      select(animal_id, date, selected_final_intake_kg_sum, mean, sd, min, max, lower_q, upper_q)
    
   
  cow_daily_stats <- cow_daily_stats %>%
    mutate(percentage_intake_of_day = selected_final_intake_kg_sum / mean * 100) %>%
    mutate(flag_intake_category = case_when(
      percentage_intake_of_day > 100 ~ "High Intake",
      percentage_intake_of_day < 75 ~ "Low Intake",
      TRUE ~ "Normal Intake"))
  
  

 
  return(list(cow_daily_intakes=cow_daily_intakes,cow_daily_stats= cow_daily_stats))
  

}

width_parameter=3
daily_result <- daily_stats(cleaned_data, width_parameter)





