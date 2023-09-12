# Content for 'App' tab ----
tab_insentec$body <- tabItem(
  tags$style(".nav-pills .nav-link.active {color: #fff; background-color: #3c8dbc;}"),
  
  tabName = "app-insentec",
    # Page Layout ----
    ## Date Filter ----
    fluidPage(
      fluidRow(
        accordion(
          id = "accordion",
          accordionItem(
            title = tagList(icon("filter"), tags$b("Date Range")),
            status = "lightblue",
            collapsed = FALSE,
            
            ### From input ----
            div(style = "display:inline-block",
                dateInput(
                  "insentec_date_range_1",
                  "From",
                  format = "M dd, yyyy",
                  value = Sys.Date() - 1,
                  max = Sys.Date(),
                  width = 210
                )
            ),
            ### To input ----
            div(style = "display:inline-block",
                dateInput(
                  "insentec_date_range_2",
                  "To",
                  format = "M dd, yyyy",
                  value = Sys.Date() - 1,
                  max = Sys.Date(),
                  width = 210
                )
            )
          )
        )
      )
    ),
    ## Main Content ----
    fluidRow(
      
      #### Animal Reports ----
      column(
        width = 6,
        
        tabBox(
          title = tags$b("Animals"),
          width = 12,
          status = "lightblue",
          solidHeader = TRUE,
          collapsible = FALSE,
          collapsed = FALSE,
          maximizable = TRUE,
          type = "tabs",
          elevation = 2,
          side = "right",
          
          ##### Daily Feed Intake ----  
          tabPanel(
            title = "Daily Feed Intake",
            fluidRow(
              column(12,
                     div(style="text-align: center;",
                         div(style = "display:inline-block",
                             tags$b("Animals with total intake between")
                         ),
                         div(style = "display:inline-block",
                             numericInput("insentec_filter_lower_intake",
                                          "",
                                          value = 1,
                                          step = 1,
                                          width = 80)
                         ),
                         div(style = "display:inline-block",
                             tags$b("and")
                         ),
                         div(style = "display:inline-block",
                             numericInput("insentec_filter_upper_intake",
                                          "",
                                          value = 10,
                                          step = 1,
                                          width = 80)
                         ),
                         div(style = "display:inline-block",
                             tags$b("Kgs across all diets")
                         )
                     )
              )
            ),
            
            hr(),
            
            fluidRow(
              style = "min-height: 800px;",
              column(12,
                     shinycssloaders::withSpinner(
                       DT::dataTableOutput("insentec_feed_summary")
                     )
              ),
              column(2,
              )
            )
          ),
          
          ##### Stealers ----
          tabPanel(
            title = "Stealers",
            fluidRow(
              column(12,
                     div(style="text-align: center;",
                         div(style = "display:inline-block",
                             tags$b("Animals with total intake >=")
                         ),
                         div(style = "display:inline-block",
                             numericInput("insentec_filter_stealer_intake",
                                          "",
                                          value = 0.5,
                                          step = 0.1,
                                          width = 80)
                         ),
                         div(style = "display:inline-block",
                             tags$b("Kgs from one or more diets different than its own animal number")
                         )
                     )
              )
            ),
            
            hr(),
            
            fluidRow(
              style = "min-height: 800px;",
              column(12,
                     shinycssloaders::withSpinner(
                       DT::dataTableOutput("insentec_stealers_summary")
                     )
              )
            )
          ),
          
          ##### All visits ----
          tabPanel(
            title = "All visits (Raw VR File)",
            fluidRow(
              style = "min-height: 910px;",
              column(12,
                     uiOutput("out_insentec_vr_filter"),
                     shinycssloaders::withSpinner(
                       DT::dataTableOutput("insentec_vr_file")
                     )
              )
            )
          )
        )
      ),
      
      #### Bin Reports ----
      column(
        width = 6,
        tabBox(
          title = tags$b("Bins"),
          width = 12,
          status = "lightblue",
          solidHeader = TRUE,
          collapsible = FALSE,
          collapsed = FALSE,
          maximizable = TRUE,
          type = "tabs",
          elevation = 2,
          side = "right",
          
          ##### Daily Feed Duration ----
          tabPanel(
            title = "Daily Feed Duration",
            fluidRow(
              column(12,
                     div(style="text-align: center;",
                         div(style = "display:inline-block",
                             tags$b("Bins with maximum feeding duration >=")
                         ),
                         div(style = "display:inline-block",
                             numericInput("insentec_filter_duration",
                                          "",
                                          value = 30,
                                          step = 1,
                                          width = 80)
                         ),
                         div(style = "display:inline-block",
                             tags$b("minutes")
                         )
                     )
              )
            ),
            
            hr(),
            
            fluidRow(
              style = "min-height: 800px;",
              column(12,
                     shinycssloaders::withSpinner(
                       DT::dataTableOutput("insentec_bin_duration_summary")
                     )
              )
            )
          ),
          
          ##### Negative Intakes ----
          tabPanel(
            title = "Negative Intakes",
            fluidRow(
              column(12,
                     div(style="text-align: center;",
                         div(style = "display:inline-block",
                             tags$b("Bind with total negative intake <=")
                         ),
                         div(style = "display:inline-block",
                             numericInput("insentec_filter_bin_neg_intake",
                                          "",
                                          value = -1,
                                          step = 1,
                                          width = 80)
                         ),
                         div(style = "display:inline-block",
                             tags$b("Kgs")
                         )
                     )
              )
            ),
            
            hr(),
            
            fluidRow(
              style = "min-height: 800px;",
              column(12,
                     shinycssloaders::withSpinner(
                       DT::dataTableOutput("insentec_bin_neg_intake_summary")
                     )
              )
            )
          ),
          
          ##### All visits ----
          tabPanel(
            title = "All visits (Raw VR File)",
            fluidRow(
              style = "min-height: 910px;",
              column(12,
                     uiOutput("out_insentec_bin_filter"),
                     shinycssloaders::withSpinner(
                       DT::dataTableOutput("insentec_bin_records")
                     )
              )
            )
          )
        )
      )
    )
)
