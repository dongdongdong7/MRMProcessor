# 2. Find Peaks Page UI
# Barry Song
# 250803

library(shiny)
library(shinyFiles)
library(bslib)
library(DT)
library(shinyjs)

find_peaks_ui <- function(id){
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      position = "left",
      open = TRUE,
      selectInput(label = "Batch Name", choices = "none", selected = "none", inputId = ns("find_peaks_batchName")),
      selectInput(label = "Sample Name", choices = "none", selected = "none", inputId = ns("find_peaks_sampleName")),
      selectInput(label = "Analyte Name", choices = "none", selected = "none", inputId = ns("find_peaks_analyteName")),
      numericInput(label = "Target RT", inputId = ns("find_peaks_targetRt"), min = 0, max = 1000, value = 100, step = 0.5),
      actionButton(label = "Find Peaks", inputId = ns("find_peaks_findPeaks")),
      actionButton(label = "Extract Target", inputId = ns("find_peaks_extractTarget")),
      actionButton(label = "Correct RT", inputId = ns("find_peaks_correctRt")),
      actionButton(label = "Make Blank", inputId = ns("find_peaks_makeBlank"))
    ),
    layout_columns(
      col_widths = c(8, 4, 12),
      row_heights = c(1,1,1),
      card(
        plotly::plotlyOutput(outputId = ns("find_peaks_currentAnalyte"))
      ),
      card(
        h4("ok")
      ),
      card(
        plotly::plotlyOutput(outputId = ns("find_peaks_rtdifference"))
      )
    ),
    # 卡片1
    card(
      class = "position-absolute draggable-card",
      style = css(
        width = "300px",
        height = "120px",
        z_index = 1000,
        right = "20px",
        bottom = "20px",
      ),
      card_header(
        class = "user-select-none",
        style = "cursor: move;",
        "卡片1"
      ),
      card_body(
        actionButton(label = "browser", inputId = ns("browser"))
      )
    ),

  )
}

find_peaks_server <- function(id, values){
  moduleServer(
    id,
    function(input, output, session){
      # browser
      observeEvent(input$browser, {
        browser()
      })
      # Initilize
      observe({
        print(2)
        updateSelectInput(session = session, inputId = "find_peaks_analyteName", choices = values$chr_grid$windowInfo$analyteName)
      })
      # batchName and sampleName
      observe({
        print(3)
        updateSelectInput(session = session, inputId = "find_peaks_batchName", choices = values$batchNameVector, selected = values$batchNameVector[1])
        sampleName_tmp <- isolate(values$chr_grid$sampleInfo$sampleName[values$chr_grid$sampleInfo$batchName == input$find_peaks_batchName])
        updateSelectInput(session = session, inputId = "find_peaks_sampleName", choices = sampleName_tmp, selected = sampleName_tmp[1])
      })

      # find_peaks_findPeaks
      observeEvent(input$find_peaks_findPeaks, {
        if(!is.null(values$chr_grid)){
          addClass(id = "find_peaks_findPeaks", class = "btn-clicked")
          progress <- Progress$new(min = 0, max = length(values$chr_grid$chrs_list))
          progress$set(message = "Begain to finding peaks...", value = 0)
          on.exit(progress$close())
          values$chr_grid$findPeaks_ChrGrid(thread = values$threads, shinyProgress = progress)
          values$chr_grid$extend_ChrGrid()
        }
      })
      # find_peaks_extractTarget
      observeEvent(input$find_peaks_extractTarget, {
        browser()
        values$chr_grid$extractTarget
      })

      # find_peaks_currentAnalyte
      observe({
        output$find_peaks_currentAnalyte <- plotly::renderPlotly({
          values$chr_grid$get(1,1)$plot_chr()
        })
      })
    }
  )
}
