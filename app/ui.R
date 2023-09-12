# Source the tab's body R file ----
source('./tabs/insentec/tab_insentec_body.R')

# UI for app ----
ui <- shinyUI(
  ## Page ----
  dashboardPage( 
    skin = "lightblue",
    title = "Feed Intake Monitoring Dashboard",
    scrollToTop = TRUE,
    fullscreen = TRUE,
    dark = NULL,
    help = NULL,
    
    ### Header ----
    header = dashboardHeader(
      title = tagList(br(),""),
      status = "white",
      div(style = "text-align: center;",
          tags$b("ODRC - Feed Intake Monitoring Dashboard")
      )
    ),
    
    ### Sidebar ----
    sidebar = dashboardSidebar(
      collapsed = TRUE,
      elevation = 2,
      fixed = FALSE,
      minified = FALSE,
      skin = "light",
      status = "lightblue",
      
      #### Siebar Menu ----
      sidebarMenu(id = "sidebarID", 
                  ##### Insentec Reports ----
                  menuItem("Insentec Reports",
                           tabName = "app-insentec",
                           icon = icon("file-lines")
                  )
      )
    ),
    
    ### Body ----
    body = dashboardBody(
      tabItems(
        tab_insentec$body
      )
    )
  ))