# This is MRMProcessor shiny GUI app
# Barry Song
# 250731

library(shiny)
library(shinyFiles)
library(bslib)
library(shinyjs)
source(system.file("GUI", "pages", "1_Load_Data.R", package = "MRMProcessor"))
source(system.file("GUI", "pages", "2_Find_Peaks.R", package = "MRMProcessor"))

ui <- page_navbar(
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
  theme = bs_theme(),
  nav_panel(
    title = "Load Data",
    load_data_ui(id = "load_data")
  ),
  nav_panel(
    title = "Find Peaks",
    find_peaks_ui(id = "find_peaks")
  ),
  nav_spacer(),
  nav_item(
    class = "dropdown",
    tags$a(href = "#", class = "nav-link dropdown-toggle",`data-bs-toggle` = "dropdown", role = "button",icon("gear"), "Options"),
    tags$div(
      class = "dropdown-menu dropdown-menu-end",
      tags$div(
        style = "text-align: center; margin-bottom: 6px;",
        tags$strong("Global Options", style = "font-size: 18px;")
      ),
      tags$div(
        style = "text-align: left; margin: 1px;",
        tags$p("threads:", style = "display: inline-block; font-size: 16px; width: 40%;"),
        tags$input(id = "parallel_threads", type = "number", value = 1, min = 1, max = 1, style = "display: inline-block; height: 16px; width: 50%;")
      ),
    )
  ),
)

server <- function(input, output, session){
  # Global Variable
  {
    values <- reactiveValues() # TODO: 还是将所有values的监视变量放在一起
    values$threads <- NULL
    values$rtUnit <- NULL
    values$dataDir <- NULL
    values$dataPath <- NULL
    values$windowInfoPath <- NULL
    values$windowInfo <- NULL
    values$sampleInfoPath <- NULL
    values$sampleInfo <- NULL
    values$chr_grid <- NULL
  }
  # Initialize app
  observe({
    maxThreads <- BiocParallel::snowWorkers()
    updateNumericInput(session = session, inputId = "parallel_threads", value = maxThreads, max = maxThreads)
    message("Your machine has a maximum of ", maxThreads, " threads")
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

