# This is MRMProcessor shiny GUI app
# Barry Song
# 250731

library(shiny)
library(shinyFiles)
library(bslib)
library(bsicons)
library(shinyjs)
source(system.file("GUI", "pages", "1_Load_Data.R", package = "MRMProcessor"))
source(system.file("GUI", "pages", "2_Find_Peaks.R", package = "MRMProcessor"))

ui <- page_navbar(
  theme = bs_theme(version = 5, preset = "shiny"),
  useShinyjs(),

  # add head
  tags$head(
    tags$style(HTML("
    .btn-clicked {
    background-color: gray;
    color: white;
    border: 1px solid gray;
    box-shadow: inset 0 3px 5px rgba(0,0,0,.125);
    }"))
  ),
  includeScript(path = system.file("GUI", "draggable-card.js", package = "MRMProcessor")),

  title = "MRMProcessor",
  selected = "Load Data",
  navbar_options = navbar_options(collapsible = TRUE),
  nav_panel(
    title = "Load Data",
    load_data_ui(id = "load_data")
  ),
  nav_panel(
    title = "Find Peaks",
    find_peaks_ui(id = "find_peaks")
  ),
  nav_spacer(),
  # nav_menu(
  #   title = "Options",
  #   numericInput(label = "thread", inputId = "parallel_threads", value = 1, min = 1, max = 1)
  # ),
  nav_item(
    popover(
      bs_icon("gear"),
      title = "Options",
      selectInput(label = "rt unit", choices = c("min", "sec"), selected = "min", inputId = "rtUnit"),
      numericInput(label = "thread", inputId = "parallel_threads", value = 1, min = 1, max = 1)
    ),
  )
)

server <- function(input, output, session){
  # Global Variable
  {
    values <- reactiveValues()
    values$threads <- NULL
    values$rtUnit <- NULL
    values$dataDir <- NULL
    values$dataPath <- NULL
    values$windowInfoPath <- NULL
    values$windowInfo <- NULL
    values$windowNameVector <- NULL
    values$sampleInfoPath <- NULL
    values$sampleInfo <- NULL
    values$chr_grid <- NULL
    values$current_i <- 1
    values$current_j <- 1
    values$current_analyteName <- NULL
    values$current_sampleName <- NULL
    values$chr_grid_change <- 1 # 用于反应chr_grid的变化
  }
  # Initialize app
  observe({
    maxThreads <- BiocParallel::snowWorkers()
    updateNumericInput(session = session, inputId = "parallel_threads", value = maxThreads, max = maxThreads)
    message("Your machine has a maximum of ", maxThreads, " threads")
  })
  # Get rt unit in options
  observe({
    values$rtUnit <- input$rtUnit
    if(values$rtUnit != "min" & values$rtUnit != "sec") stop("rtUnit is wrong!")
  })
  # Get global threads in options
  observeEvent(input$parallel_threads, {
    values$threads <- input$parallel_threads
  })
  # Page1: Load Data
  load_data_server(id = "load_data", values = values)
  # Page2: Find Peaks
  find_peaks_server(id = "find_peaks", values = values)
}

# Run the application
shinyApp(ui = ui, server = server)

