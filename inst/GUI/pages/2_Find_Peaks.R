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
        position: relative;
        width: 180px;
        height: 180px;
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
      width: 60px;
      height: 180px;
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
          style = "display: flex; flex-wrap: wrap; gap: 10px;",
          div(class = "dpad",
              shinyWidgets::actionBttn(inputId = ns("btn_up"), label = "↑", class = "dpad-btn"),
              shinyWidgets::actionBttn(ns("btn_left"), "←", class = "dpad-btn"),
              div(class = "dpad-btn", id = ns("btn_center"), "○"),
              shinyWidgets::actionBttn(ns("btn_right"), "→", class = "dpad-btn"),
              shinyWidgets::actionBttn(ns("btn_down"), "↓", class = "dpad-btn")
          ),
          div(class = "find_peaks_ij",
              shinyWidgets::numericInputIcon(inputId = ns("find_peaks_i_start"), label = "i start", value = 1, min = 1, max = 1 ,step = 1, width = "100%"),
              shinyWidgets::numericInputIcon(inputId = ns("find_peaks_j_start"), label = "j start", value = 1, min = 1, max = 1, step = 1, width = "100%")
          ),
          div(class = "find_peaks_ij",
              shinyWidgets::numericInputIcon(inputId = ns("find_peaks_i_end"), label = "i end", value = 1, min = 1, max = 1 ,step = 1, width = "100%"),
              shinyWidgets::numericInputIcon(inputId = ns("find_peaks_j_end"), label = "j end", value = 1, min = 1, max = 1, step = 1, width = "100%")
          )
        ),
        sliderInput(inputId = ns("find_peaks_peakwidth"), label = "peak witdh", min = 1, max = 30, value = c(5, 20), step = 1),
        sliderInput(inputId = ns("find_peaks_snthresh"), label = "sn thresh", min = 0, max = 10, step = 1, value = 3),
        numericInput(inputId = ns("find_peaks_noise"), label = "noise", min = 1, max = 10000, value = 100, step = 1),
        shinyWidgets::switchInput(inputId = ns("find_peaks_estimateNoise"), label = "estimate noise", value = TRUE, onLabel = "TRUE", offLabel = "FALSE"),
        sliderInput(inputId = ns("find_peaks_r2thresh"), label = "r2 thresh", min = 0, max = 1, value = 0.6, step = 0.1),
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
        message("Initialize Find Peaks Page")
        if(!is.null(values$chr_grid)){
          # Initialize sampleName
          updateSelectInput(session = session, inputId = "find_peaks_sampleName", choices = values$chr_grid$sampleInfo$sampleName, selected = values$chr_grid$sampleInfo$sampleName[1])
          # Initialize analyteName
          updateSelectInput(session = session, inputId = "find_peaks_analyteName", choices = values$chr_grid$windowInfo$analyteName, selected = values$chr_grid$windowInfo$analyteName[1])
          # Initialize i_start i_end
          max_i <- values$chr_grid$dim[1]
          max_j <- values$chr_grid$dim[2]
          updateNumericInput(session = session, inputId = "find_peaks_i_start", value = 1, min = 1, max = max_i)
          updateNumericInput(session = session, inputId = "find_peaks_i_end", value = max_i, min = 1, max = max_i)
          updateNumericInput(session = session, inputId = "find_peaks_j_start", value = 1, min = 1, max = max_j)
          updateNumericInput(session = session, inputId = "find_peaks_j_end", value = max_j, min = 1, max = max_j)
        }
      })

      # direction keys
      observeEvent(input$btn_up, {
        values$current_i <- values$current_i - 1
        values$current_analyteName <- values$chr_grid$windowInfo$analyteName[values$current_i]
        updateSelectInput(session = session, inputId = "find_peaks_analyteName", selected = values$current_analyteName)
        updateNumericInput(session = session, inputId = "find_peaks_i_start", value = values$current_i)
        updateNumericInput(session = session, inputId = "find_peaks_i_end", value = values$current_i)
      })
      observeEvent(input$btn_down, {
        values$current_i <- values$current_i + 1
        values$current_analyteName <- values$chr_grid$windowInfo$analyteName[values$current_i]
        updateSelectInput(session = session, inputId = "find_peaks_analyteName", selected = values$current_analyteName)
        updateNumericInput(session = session, inputId = "find_peaks_i_start", value = values$current_i)
        updateNumericInput(session = session, inputId = "find_peaks_i_end", value = values$current_i)
      })
      observeEvent(input$btn_left, {
        values$current_j <- values$current_j - 1
        values$current_sampleName <- values$chr_grid$sampleInfo$sampleName[values$current_j]
        updateSelectInput(session = session, inputId = "find_peaks_sampleName", selected = values$current_sampleName)
        updateNumericInput(session = session, inputId = "find_peaks_j_start", value = values$current_j)
        updateNumericInput(session = session, inputId = "find_peaks_j_end", value = values$current_j)
      })
      observeEvent(input$btn_right, {
        values$current_j <- values$current_j + 1
        values$current_sampleName <- values$chr_grid$sampleInfo$sampleName[values$current_j]
        updateSelectInput(session = session, inputId = "find_peaks_sampleName", selected = values$current_sampleName)
        updateNumericInput(session = session, inputId = "find_peaks_j_start", value = values$current_j)
        updateNumericInput(session = session, inputId = "find_peaks_j_end", value = values$current_j)
      })

      # find_peaks_analyteName
      observeEvent(input$find_peaks_analyteName, {
        if(input$find_peaks_analyteName != "none" & !is.na(input$find_peaks_i_start)){
          values$current_analyteName <- input$find_peaks_analyteName
          values$current_i <- which(values$chr_grid$windowInfo$analyteName == values$current_analyteName)
          if(which(values$chr_grid$windowInfo$analyteName == input$find_peaks_analyteName) != input$find_peaks_i_start){
            updateNumericInput(session = session, inputId = "find_peaks_i_start", value = values$current_i)
            updateNumericInput(session = session, inputId = "find_peaks_i_end", value = values$current_i)
          }
        }
      })
      # find_peaks_sampleName
      observeEvent(input$find_peaks_sampleName, {
        if(input$find_peaks_sampleName != "none" & !is.na(input$find_peaks_j_start)){
          values$current_sampleName <- input$find_peaks_sampleName
          values$current_j <- which(values$chr_grid$sampleInfo$sampleName == values$current_sampleName)
          if(which(values$chr_grid$sampleInfo$sampleName == input$find_peaks_sampleName) != input$find_peaks_j_start){
            updateNumericInput(session = session, inputId = "find_peaks_j_start", value = values$current_j)
            updateNumericInput(session = session, inputId = "find_peaks_j_end", value = values$current_j)
          }
        }
      })
      # find_peaks_i_start
      observeEvent(input$find_peaks_i_start, {
        if(input$find_peaks_analyteName != "none" & !is.na(input$find_peaks_i_start)){
          values$current_i <- input$find_peaks_i_start
          values$current_analyteName <- values$chr_grid$windowInfo$analyteName[values$current_i]
          if(which(values$chr_grid$windowInfo$analyteName == input$find_peaks_analyteName) != input$find_peaks_i_start){
            updateSelectInput(session = session, inputId = "find_peaks_analyteName", selected = values$current_analyteName)
          }
        }
      })
      # find_peaks_j_start
      observeEvent(input$find_peaks_j_start, {
        if(input$find_peaks_sampleName != "none" & !is.na(input$find_peaks_j_start)){
          values$current_j <- input$find_peaks_j_start
          values$current_sampleName <- values$chr_grid$sampleInfo$sampleName[values$current_j]
          if(which(values$chr_grid$sampleInfo$sampleName == input$find_peaks_sampleName) != input$find_peaks_j_start){
            updateSelectInput(session = session, inputId = "find_peaks_sampleName", selected = values$current_sampleName)
          }
        }
      })

      # find_peaks_findPeaks
      observeEvent(input$find_peaks_findPeaks, { # TODO: 给extend加上进度条, 少量find peaks 时, 进度条好像不对
        if(!is.null(values$chr_grid)){
          addClass(id = "find_peaks_findPeaks", class = "btn-clicked")
          progress <- Progress$new(min = 0, max = length(values$chr_grid$chrs_list))
          progress$set(message = "Begain to finding peaks...", value = 0)
          on.exit(progress$close())
          i_seq <- input$find_peaks_i_start:input$find_peaks_i_end
          j_seq <- input$find_peaks_j_start:input$find_peaks_j_end
          # if(length(i_seq) * length(j_seq) <= 10){
          #   for(i in i_seq){
          #     for(j in j_seq){
          #       values$chr_grid$get(i, j)$findPeaks_ChrGrid()
          #     }
          #   }
          # }
          values$chr_grid$findPeaks_ChrGrid(i = i_seq,
                                            j = j_seq,
                                            peakwidth = c(input$find_peaks_peakwidth[1], input$find_peaks_peakwidth[2]),
                                            snthresh = input$find_peaks_snthresh,
                                            noise = input$find_peaks_noise, estimateNoise = input$find_peaks_estimateNoise,
                                            r2thresh = input$find_peaks_r2thresh,
                                            thread = values$threads, shinyProgress = progress)
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
          if(!is.na(values$current_i) & !is.na(values$current_j)){
            chr <- values$chr_grid$get(values$current_i,values$current_j)
            peakwidth <- chr$peakwidth
            snthresh <- chr$snthresh
            noise <- chr$noise
            estimateNoise <- chr$estimateNoise
            r2thresh <- chr$r2thresh
            if(all(sapply(list(peakwidth, snthresh, noise ,estimateNoise, r2thresh), function(x) {!is.null(x)}))){
              updateSliderInput(session = session, inputId = "find_peaks_peakwidth", value = c(chr$peakwidth[1], chr$peakwidth[2]))
              updateSliderInput(session = session, inputId = "find_peaks_snthresh", value = snthresh)
              updateNumericInput(session = session, inputId = "find_peaks_noise", value = noise)
              shinyWidgets::updateSwitchInput(session = session, inputId = "find_peaks_estimateNoise", value = estimateNoise)
              updateSliderInput(session = session, inputId = "find_peaks_r2thresh", value = r2thresh)
            }
            output$find_peaks_currentAnalyte <- plotly::renderPlotly({
              chr$plot_chr()
            })
          }
        }
      })
    }
  )
}
