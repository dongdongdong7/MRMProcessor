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
    tags$head(
      tags$style(HTML("
      .dpad {
        display: inline-block;
        position: relative;
        width: 45%;
        height: 45%;
      }
      .dpad-btn {
        position: absolute;
        width: 50px;
        height: 50px;
        background: #333;
        color: white;
        border: none;
        border-radius: 5px;
        font-size: 20px;
        display: flex;
        align-items: center;
        justify-content: center;
        cursor: pointer;
      }
      .dpad-btn:active {
        background: #555;
      }
      #find_peaks-btn_up { top: 0; left: 50px; }
      #find_peaks-btn_left { top: 50px; left: 0; }
      #find_peaks-btn_center { top: 50px; left: 50px; background: #555; }
      #find_peaks-btn_right { top: 50px; left: 100px; }
      #find_peaks-btn_down { top: 100px; left: 50px; }
      .find_peaks_ij {
      display: inline-block;
      width: 45%;
      height: 45%;
      }
    "))
    ),
    tags$script(HTML("
    $(document).on('keydown', function(e) {
      const key = e.which;
      // 上(38)、下(40)、左(37)、右(39)
      if ([37, 38, 39, 40].includes(key)) {
        e.preventDefault();
        $('#find_peaks-btn_' + ['left', 'up', 'right', 'down'][[37,38,39,40].indexOf(key)]).click();
      }
    });
    ")),
    sidebar = sidebar(
      position = "left",
      open = TRUE,
      selectInput(label = "Batch Name", choices = "none", selected = "none", inputId = ns("find_peaks_batchName")),
      selectInput(label = "Sample Name", choices = "none", selected = "none", inputId = ns("find_peaks_sampleName"), width = "100%"),
      selectInput(label = "Analyte Name", choices = "none", selected = "none", inputId = ns("find_peaks_analyteName"), width = "100%"),
      numericInput(label = "Target RT", inputId = ns("find_peaks_targetRt"), min = 0, max = 1000, value = 100, step = 0.5),
      actionButton(label = "Find Peaks", inputId = ns("find_peaks_findPeaks")),
      actionButton(label = "Extract Target", inputId = ns("find_peaks_extractTarget")),
      actionButton(label = "Correct RT", inputId = ns("find_peaks_correctRt")),
      actionButton(label = "Make Blank", inputId = ns("find_peaks_makeBlank")),
      actionButton(label = "browser", inputId = ns("browser"))
    ),
    layout_columns(
      col_widths = c(8, 4, 12),
      row_heights = c(1,1,1),
      card(
        plotly::plotlyOutput(outputId = ns("find_peaks_currentAnalyte"))
      ),
      card(
        div(
          style = "height: 100%; witdh: 100%;",
          div(class = "dpad",
              shinyWidgets::actionBttn(inputId = ns("btn_up"), label = "↑", class = "dpad-btn"),
              shinyWidgets::actionBttn(ns("btn_left"), "←", class = "dpad-btn"),
              div(class = "dpad-btn", id = ns("btn_center"), "○"),
              shinyWidgets::actionBttn(ns("btn_right"), "→", class = "dpad-btn"),
              shinyWidgets::actionBttn(ns("btn_down"), "↓", class = "dpad-btn")
          ),
          div(class = "find_peaks_ij",
              shinyWidgets::numericInputIcon(inputId = "find_peaks_i", label = "i", value = 1, min = 1, max = 1 ,step = 1, width = "100%"),
              shinyWidgets::numericInputIcon(inputId = "find_peaks_j", label = "j", value = 1, min = 1, max = 1, step = 1, width = "100%")
          ),
        ),
        verbatimTextOutput(ns("direction_output"))
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
        height = "300px",
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
        h4("ok")
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
        if(!is.null(values$chr_grid)){
          updateSelectInput(session = session, inputId = "find_peaks_analyteName", choices = values$chr_grid$windowInfo$analyteName)
          batchName <- unique(values$sampleInfo$batchName)
          updateSelectInput(session = session, inputId = "find_peaks_batchName", choices = batchName, selected = batchName[1])
        }
      })
      # Observe find_peaks_batchName
      observeEvent(input$find_peaks_batchName, {
        if(input$find_peaks_batchName != "none"){
          sampleName_tmp <- values$chr_grid$sampleInfo$sampleName[values$chr_grid$sampleInfo$batchName == input$find_peaks_batchName]
          updateSelectInput(session = session, inputId = "find_peaks_sampleName", choices = sampleName_tmp, selected = sampleName_tmp[1])
        }
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
        if(!is.null(values$chr_grid)){
          output$find_peaks_currentAnalyte <- plotly::renderPlotly({
            values$chr_grid$get(1,1)$plot_chr()
          })
        }
      })

      # direction keys
      {
        observeEvent(input$btn_up, {
          output$direction_output <- renderText("向上移动")
        })
        observeEvent(input$btn_down, {
          output$direction_output <- renderText("向下移动")
        })
        observeEvent(input$btn_left, {
          output$direction_output <- renderText("向左移动")
        })
        observeEvent(input$btn_right, {
          output$direction_output <- renderText("向右移动")
        })
      }
    }
  )
}
