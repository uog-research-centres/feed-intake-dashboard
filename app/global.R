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

## User experience
library(shinycssloaders)

# Server-side functions/objects/dependencies ----
## Define a list to wrap ui/server for app tabs
tab_insentec <- list()

## Global Reactive values
db <- reactiveValues()

## Load config file
config <- rjson::fromJSON(file = "./.config.json")

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