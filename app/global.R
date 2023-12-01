# Load packages ----
## Basic shiny framework
library(shiny)
library(shinyWidgets)
library(bs4Dash)

## Data wrangling
library(data.table)
library(DT)
library(dplyr)
library(lubridate)
library(rjson)
library(tidyr)

## User experience
library(shinycssloaders)

# Server-side functions/objects/dependencies ----
## Define a list to wrap ui/server for app tabs
tab_insentec <- list()

## Global Reactive values
db <- reactiveValues()

## Load config file
config <- rjson::fromJSON(file = "./.config.json")

## Load functions
# source("./functions/f_clean_data.R")

## Helper objects
### Options for dropdown pickerInput
optionsMenu <- pickerOptions(actionsBox = T,
                             liveSearch = T,
                             liveSearchNormalize = T,
                             noneSelectedText = "Select at least one",
                             noneResultsText = "Option not found",
                             selectedTextFormat = "count > 10",
                             selectOnTab = T,
                             size = 10,
                             header = "Close menu"
)

### Options for DT::datatable rendering
optionsList <- list(scrollX = T,
                    deferRender = TRUE,
                    scrollY = 600,
                    scroller = TRUE
)

## takes raw insentec VR data as input

clean_data <- function(data) {
  
  # Convert start_time and end_time datatype to be compatible with the function 
  data$start_time <- as.POSIXct(paste(data$date, data$start_time), format = "%Y-%m-%d %H:%M:%S") 
  data$end_time <- as.POSIXct(paste(data$date, data$end_time), format = "%Y-%m-%d %H:%M:%S")
  
  ## Cleaning Function
  # Applying the by bin cleaning function
  
  # Cleaning row  by row errors within the data collected by the feed bin
  # The majority of stages group the data by feed bin ID and day, allowing for separate analysis of each feed bin and day. 
  # In the last phase, the data is aggregated by cow ID to determine whether the cow visited another feed bin while she was still being recorded at that feed bin. It also checks for wrongly recorded end timings.
  # Additionally, it creates a log file that summarises each error by percent and count. This is stored in the temporary file path that the output list returns.
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
  
  
  ## Cleaning by cow
  # Function to iterate each cow's data through the 'by cow' cleaning function
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
  
  # keeping all the corrections (final_intake_kg and final_duration_sec), additionally a new column is added to check for any modifications from pre-calculated flags
  simplified_final_df <- 
    merged_by_cow %>% 
    # Keep only corrected end weight and end time
    #  calculate new intakes and durations:
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
    # keep relevant columns:
    dplyr::select(
      date, transponder_id, animal_id, bin_id, start_time, corrected_end_time, selected_final_duration_sec, start_weight_kg, corrected_end_weight_bybin, diet,
      selected_final_intake_kg
    )
  
  # converting the time variables back into its original format 
  simplified_final_df$start_time <- as.character(simplified_final_df$start_time)
  simplified_final_df$corrected_end_time <- as.character(simplified_final_df$corrected_end_time)
  
  cleaned_data <- simplified_final_df %>% 
    rename("end_time" = corrected_end_time,
           "end_weight_kg" = corrected_end_weight_bybin,
           "intake_kg" = selected_final_intake_kg,
           "duration_sec" = selected_final_duration_sec)
  
  return(cleaned_data)
}
