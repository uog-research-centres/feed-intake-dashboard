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
  
  ## Feed Summary ----
  observe({
    req(insentec_data())
    data <- insentec_data()
    
    ## Aggregate data for feed summary report and use filters for feed intake range
    feed <- data %>%
      group_by(date,animal_id) %>%
      summarise(mean_intake = round(mean(intake_kg), 2),
                mean_duration_seconds = round(mean(duration_sec), 2),
                total_intake = round(sum(intake_kg), 2),
                .groups = "keep")
    feed_summary <- feed %>%
      filter(total_intake <= input$insentec_filter_upper_intake & total_intake >= input$insentec_filter_lower_intake)
    
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