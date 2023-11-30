tab_insentec$server <- function(input, output, session) {
  
  ## Query full data for given date range and animals ----
  insentec_data <- reactive({
    req(input$insentec_date_range_1, input$insentec_date_range_2)
    
    if (ymd(input$insentec_date_range_1) > ymd(input$insentec_date_range_2)) {
      dates <- seq.Date(from = ymd(input$insentec_date_range_2), to = ymd(input$insentec_date_range_1), by = 1)
    } else {
      dates <- seq.Date(from = ymd(input$insentec_date_range_1), to = ymd(input$insentec_date_range_2), by = 1)
    }
    
    files <- lapply(dates, function (i) {
      data <- fread(format(i, config$apollo_path), header = F)
      data <- data[,1:10]
      data$date <- i
      colnames(data) <- c("transponder_id", "animal_id", "bin_id", "start_time", "end_time", "duration_sec", "start_weight_kg", "end_weight_kg", "diet", "intake_kg", "date")
      data <- relocate(data, date, .before=transponder_id) %>%
        mutate(animal_id = as.numeric(animal_id))
      return(data)
    })
    
    data <- rbindlist(files,
                      use.names = TRUE,
                      fill = TRUE)
  })
  
  clean_data <- reactive({
    req(insentec_data())
    data <- insentec_data()
    
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
  })
  
  ## Feed Summary ----
  observe({
    req(insentec_data(), clean_data())
    data <- insentec_data()
    clean_data <- clean_data()
    
    if (ymd(input$insentec_date_range_1) > ymd(input$insentec_date_range_2)) {
      dates <- seq.Date(from = ymd(input$insentec_date_range_2), to = ymd(input$insentec_date_range_1), by = 1)
    } else {
      dates <- seq.Date(from = ymd(input$insentec_date_range_1), to = ymd(input$insentec_date_range_2), by = 1)
    }
    
    ## Aggregate data for feed summary report and use filters for feed intake range
    feed <- data %>%
      group_by(date,animal_id) %>%
      summarise(mean_intake = round(mean(intake_kg), 2),
                mean_duration_seconds = round(mean(duration_sec), 2),
                total_intake = round(sum(intake_kg), 2),
                .groups = "keep")
    
    feed_summary <- feed %>%
      filter(total_intake <= input$insentec_filter_upper_intake & total_intake >= input$insentec_filter_lower_intake)
    
    ndays <- input$insentec_filter_ndays
    
    clean_feed <- clean_data %>%
      filter(start_weight_kg >= 0,
             end_weight_kg >= 0,
             intake_kg >= 0) %>% 
      group_by(animal_id, date) %>%
      summarise(across(c(contains('intake'), contains('duration'), where(is.logical)),
                       list(sum = ~sum(.x, na.rm=TRUE), 
                            mean = ~mean(.x, na.rm=TRUE),
                            sd = ~sd(.x, na.rm=TRUE)))) %>%
      complete(date = seq(min(date), max(date), by = "1 day")) %>%
      arrange(animal_id, date) %>% 
      mutate(across(c(intake_kg_sum, intake_kg_mean, intake_kg_sd, duration_sec_sum, duration_sec_mean, duration_sec_sd), 
                    ~ifelse(is.na(.), NA, .))) %>% 
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
      relocate(pct_of_rolling_mean, .after = rolling_mean) %>%
      mutate_at(vars(c(contains("rolling"), contains("intake"))), ~round(., 2))
    
    clean_feed_summary <- clean_feed %>% 
      filter(pct_of_rolling_mean <= input$insentec_filter_pct_normal)
    
    ## Render feed summary report
    output$insentec_feed_summary <- DT::renderDataTable({
      DT::datatable(feed_summary,
                    extensions = c('Scroller', 'RowGroup'),
                    class = "compact row-border nowrap",
                    rownames = FALSE,
                    options = list(
                      dom = 'ltip',
                      scrollX = T,
                      deferRender = TRUE,
                      rowGroup = list(dataSrc = 1), # Group by animal id
                      orderFixed = list(1, 'asc'), # Sort animal ids ascending
                      columnDefs = list(list(visible=FALSE, targets=c(1))) # Hide animal_id column as already shown on groups
                    )
      )
    })
    
    ## Render feed summary report
    output$insentec_feed_summary_pct <- DT::renderDataTable({
      DT::datatable(clean_feed_summary,
                    extensions = c('Scroller', 'RowGroup'),
                    class = "compact row-border nowrap",
                    rownames = FALSE,
                    options = list(
                      dom = 'ltip',
                      scrollX = T,
                      deferRender = TRUE,
                      # rowGroup = list(dataSrc = 0), # Group by animal id
                      orderFixed = list(1, 'asc') # Sort animal ids ascending
                      # columnDefs = list(list(visible=FALSE, targets=c(1))) # Hide animal_id column as already shown on groups
                    )
      )
    })
    ## Render choices and pre-select for animal filter in the 'all visits per animal' tabs
    output$out_insentec_vr_filter <- renderUI({
      all_animals <- data %>%
        select(animal_id) %>%
        unique() %>%
        arrange(animal_id)
      
      selected_animals <- feed_summary %>%
        ungroup() %>%
        select(animal_id) %>%
        unique() %>%
        arrange(animal_id)
      
      div(style="text-align: center;",
          pickerInput(inputId = "insentec_vr_filter",
                      label = "Filter Animal ID",
                      choices = as.character(all_animals$animal_id), # all animals
                      selected = as.character(selected_animals$animal_id), # only animals shown in the aggregated table
                      width = "100%",
                      multiple = TRUE,
                      options = optionsMenu
          )
      )
    })
    
    ## Render raw VR files based on filtered animals
    output$insentec_vr_file <- DT::renderDataTable({
      req(input$insentec_vr_filter)
      
      data <- as.data.frame(data) %>%
        filter(as.character(animal_id) %in% input$insentec_vr_filter) %>%
        arrange(animal_id)
      
      DT::datatable(data,
                    extensions = c('Scroller'),
                    class = "compact row-border nowrap",
                    rownames = FALSE,
                    options = list(
                      dom = 'ltip',
                      scrollX = T,
                      deferRender = TRUE
                    )
      )
    })
    
  })
  
  ## Stealers Summary ----
  observe({
    req(insentec_data())
    data <- insentec_data()
    
    ## Aggregate data for stealers summary report and use filters for feed intake
    stealers_summary <- data %>%
      group_by(date, animal_id, diet) %>%
      summarise(total_intake = round(sum(intake_kg), 2),
                total_n_visits = n(), # total_n_visits per diet, per animal, per day
                .groups = "drop") %>%
      unique() %>%
      filter(total_intake >= input$insentec_filter_stealer_intake) %>%
      add_count(date, animal_id) %>% # in practice, count number of different diets an animal ate on a given day. count goes to column named 'n'
      filter(n >= 2) %>%
      select(-n)
    
    ## Render Stealers Report
    output$insentec_stealers_summary <- DT::renderDataTable({
      DT::datatable(stealers_summary,
                    extensions = c('Scroller', 'RowGroup'),
                    class = "compact row-border nowrap",
                    rownames = FALSE,
                    options = list(
                      dom = 'ltip',
                      scrollX = T,
                      deferRender = TRUE,
                      rowGroup = list(dataSrc = 1), # Group by animal id
                      orderFixed = list(1, 'asc'), # Sort animal ids ascending
                      columnDefs = list(list(visible=FALSE, targets=c(1))) # Hide animal_id column as already shown on groups
                    )
      )
    })
  })
  
  ## Bin Duration Summary ----
  observe({
    req(insentec_data())
    data <- insentec_data()
    
    ## Aggregate data for duration summary report and use filters for bin duration
    duration_summary <- data %>%
      group_by(date,bin_id) %>%
      mutate(duration_min = duration_sec / 60) %>%
      summarise(total_n_visits = n(),
                mean_duration_minutes = round(mean(duration_min), 2),
                max_duration_minutes = round(max(duration_min), 2),
                .groups = "keep") %>%
      ungroup() %>%
      filter(max_duration_minutes >= as.numeric(input$insentec_filter_duration)) %>%
      arrange(desc(max_duration_minutes))
    
    ## Render bin duration summary
    output$insentec_bin_duration_summary <- DT::renderDataTable({
      DT::datatable(duration_summary,
                    extensions = c('Scroller'),
                    class = "compact row-border nowrap",
                    rownames = FALSE,
                    options = list(
                      dom = 'ltip',
                      scrollX = T,
                      deferRender = TRUE
                    )
      )
    })
    
    ## Aggregate data for negative intakes summary report and use filters for negative intakes
    bin_summary <- data %>%
      filter(intake_kg < 0) %>%
      group_by(date,bin_id) %>%
      summarise(n_visits_with_neg_intake = n(),
                lowest_intake = round(min(intake_kg), 2),
                total_neg_intake = round(sum(intake_kg), 2),
                .groups = "keep") %>%
      ungroup() %>%
      filter(total_neg_intake <= input$insentec_filter_bin_neg_intake) %>%
      arrange(total_neg_intake)
    
    ## Render Neg intake summary
    output$insentec_bin_neg_intake_summary <- DT::renderDataTable({
      DT::datatable(bin_summary,
                    extensions = c('Scroller'),
                    class = "compact row-border nowrap",
                    rownames = FALSE,
                    options = list(
                      dom = 'ltip',
                      scrollX = T,
                      deferRender = TRUE
                    )
      )
    })
    
    ## Render choices and pre-select bins for filter in the 'all visits per bin' tabs
    output$out_insentec_bin_filter <- renderUI({
      all_bins <- data %>%
        select(bin_id) %>%
        unique() %>%
        arrange(bin_id)
      
      selected_bins1 <- duration_summary %>%
        ungroup() %>%
        select(bin_id)
      
      selected_bins2 <- bin_summary %>%
        ungroup() %>%
        select(bin_id)
      
      # Combine bins from duration and neg intake summaries
      selected_bins <- rbind(selected_bins1, selected_bins2) %>%
        unique() %>%
        arrange(bin_id)
      
      div(style="text-align: center;",
          pickerInput(inputId = "insentec_bin_filter",
                      label = "Filter Bin Number",
                      choices = all_bins$bin_id,
                      selected = selected_bins$bin_id,
                      width = "100%",
                      multiple = TRUE,
                      options = optionsMenu
          )
      )
    })
    
    ## Render raw VR files based on filtered bins
    output$insentec_bin_records <- DT::renderDataTable({
      req(input$insentec_bin_filter)
      
      bin_records <- data %>%
        filter(bin_id %in% input$insentec_bin_filter)
      
      DT::datatable(bin_records,
                    extensions = c('Scroller', 'RowGroup'),
                    class = "compact row-border nowrap",
                    rownames = FALSE,
                    options = list(
                      dom = 'ltip',
                      scrollX = T,
                      deferRender = TRUE
                    )
      )
    })
  })
  
}