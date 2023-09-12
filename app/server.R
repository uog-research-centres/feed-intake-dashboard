# Source the tab's main R file here ----
source('./tabs/insentec/tab_insentec_server.R')

shinyServer(function(input, output, session) {
  tab_insentec$server(input, output, session)
})
