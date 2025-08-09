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
      width: 80px;
      height: 180px;
      }
      .custom-btn-smooth {
      display: flex;
      justify-content: center;
      align-items: center;
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
      numericInput(label = "Target RT", inputId = ns("find_peaks_targetRt"), min = 0, max = 1000, value = 100, step = 1),
      numericInput(label = "RT Difference Tolerance", inputId = ns("find_peaks_rt_diff_tol"), min = 0, max = 60, value = 10, step = 1),
      layout_columns(
        fillable = TRUE,
        fill = TRUE,
        gap = "2px",
        actionButton(inputId = ns("find_peaks_smooth"), label = "smooth", class = "custom-btn-smooth"),
        actionButton(inputId = ns("find_peaks_desmooth"), label = "desmooth", class = "custom-btn-smooth")
      ),
      actionButton(label = "Find Peaks", inputId = ns("find_peaks_findPeaks")),
      actionButton(label = "Extract Target", inputId = ns("find_peaks_extractTarget")),
      actionButton(label = "Correct RT", inputId = ns("find_peaks_correctRt")),
      actionButton(label = "Make Blank", inputId = ns("find_peaks_makeBlank")),
      actionButton(label = "browser", inputId = ns("browser"))
    ),
    layout_columns(
      col_widths = c(7, 5, 12),
      row_heights = c(1,1,1),
      card(
        div(style = "display: flex; width: 100%; height: 100%; gap: 10px;",
          div(style = "width: 50px; height: 25px;",
              shinyWidgets::dropdownButton(
                shinyWidgets::switchInput(inputId = ns("find_peaks_showTarget"), label = "Show Target", value = FALSE, onLabel = "ON", offLabel = "OFF"),
                circle = FALSE,
                size = "sm",
                status = "primary",
                icon = icon("gear"), width = "100px"
              ),
          ),
          div(style = "width: 100%; height: 100%;",
              plotly::plotlyOutput(outputId = ns("find_peaks_currentAnalyte"), width = "100%", height = "100%")
          )
        )
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
          values$windowNameVector <- unique(values$chr_grid$windowInfo$windowName)
          # Initialize sampleName
          updateSelectInput(session = session, inputId = "find_peaks_sampleName", choices = values$chr_grid$sampleInfo$sampleName, selected = values$chr_grid$sampleInfo$sampleName[1])
          # Initialize analyteName
          if(values$chr_grid$extended){
            updateSelectInput(session = session, inputId = "find_peaks_analyteName", choices = values$chr_grid$windowInfo$analyteName, selected = values$chr_grid$windowInfo$analyteName[1])
          }else{
            updateSelectInput(session = session, inputId = "find_peaks_analyteName", choices = values$windowNameVector, selected = values$windowNameVector[1])
          }
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
        if(!is.null(values$chr_grid)){
          values$current_i <- values$current_i - 1
          if(values$chr_grid$extended){
            values$current_analyteName <- values$chr_grid$windowInfo$analyteName[values$current_i]
          }else{
            values$current_analyteName <- values$windowNameVector[values$current_i]
          }
          updateSelectInput(session = session, inputId = "find_peaks_analyteName", selected = values$current_analyteName)
          updateNumericInput(session = session, inputId = "find_peaks_i_start", value = values$current_i)
          updateNumericInput(session = session, inputId = "find_peaks_i_end", value = values$current_i)
        }
      })
      observeEvent(input$btn_down, {
        if(!is.null(values$chr_grid)){
          values$current_i <- values$current_i + 1
          if(values$chr_grid$extended){
            values$current_analyteName <- values$chr_grid$windowInfo$analyteName[values$current_i]
          }else{
            values$current_analyteName <- values$windowNameVector[values$current_i]
          }
          updateSelectInput(session = session, inputId = "find_peaks_analyteName", selected = values$current_analyteName)
          updateNumericInput(session = session, inputId = "find_peaks_i_start", value = values$current_i)
          updateNumericInput(session = session, inputId = "find_peaks_i_end", value = values$current_i)
        }
      })
      observeEvent(input$btn_left, {
        if(!is.null(values$chr_grid)){
          values$current_j <- values$current_j - 1
          values$current_sampleName <- values$chr_grid$sampleInfo$sampleName[values$current_j]
          updateSelectInput(session = session, inputId = "find_peaks_sampleName", selected = values$current_sampleName)
          updateNumericInput(session = session, inputId = "find_peaks_j_start", value = values$current_j)
          updateNumericInput(session = session, inputId = "find_peaks_j_end", value = values$current_j)
        }
      })
      observeEvent(input$btn_right, {
        if(!is.null(values$chr_grid)){
          values$current_j <- values$current_j + 1
          values$current_sampleName <- values$chr_grid$sampleInfo$sampleName[values$current_j]
          updateSelectInput(session = session, inputId = "find_peaks_sampleName", selected = values$current_sampleName)
          updateNumericInput(session = session, inputId = "find_peaks_j_start", value = values$current_j)
          updateNumericInput(session = session, inputId = "find_peaks_j_end", value = values$current_j)
        }
      })

      # find_peaks_analyteName
      observeEvent(input$find_peaks_analyteName, {
        if(input$find_peaks_analyteName != "none" & !is.na(input$find_peaks_i_start)){
          values$current_analyteName <- input$find_peaks_analyteName
          if(values$chr_grid$extended){
            values$current_i <- which(values$chr_grid$windowInfo$analyteName == values$current_analyteName)
          }else{
            values$current_i <- which(values$windowNameVector == values$current_analyteName)
          }
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
          if(values$chr_grid$extended){
            values$current_analyteName <- values$chr_grid$windowInfo$analyteName[values$current_i]
          }else{
            values$current_analyteName <- values$windowNameVector[values$current_i]
          }
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

      # find_peaks_smooth
      observeEvent(input$find_peaks_smooth, {
        if(!is.null(values$chr_grid)){
          addClass(id = "find_peaks_smooth", class = "btn-clicked")
          i_seq <- input$find_peaks_i_start:input$find_peaks_i_end
          j_seq <- input$find_peaks_j_start:input$find_peaks_j_end
          if(length(i_seq) * length(j_seq) <= 20){
            progress <- Progress$new(min = 0, max = length(i_seq) * length(j_seq))
            progress$set(message = "smooth...", value = 0)
            on.exit(progress$close(), add = TRUE)
            nn <- 1
            maxValue <- progress$getMax()
            for(i in i_seq){
              for(j in j_seq){
                progress$set(value = nn, message = "Smooth...", detail = paste0(nn, " / ", maxValue))
                nn <- nn + 1
                values$chr_grid$get(i, j)$desmooth_chr()
                values$chr_grid$get(i, j)$smooth_chr()
              }
            }
          }else{
            progress_desmooth <- Progress$new(min = 0, max = length(j_seq))
            progress_desmooth$set(message = "Begain to desmooth...", value = 0)
            on.exit(progress_desmooth$close(), add = TRUE)
            values$chr_grid$desmooth_ChrGrid(i = i_seq, j = j_seq)
            progress_smooth <- Progress$new(min = 0, max = length(j_seq))
            progress_smooth$set(message = "Begain to smooth...", value = 0)
            on.exit(progress_smooth_ChrGrid$close(), add = TRUE)
            values$chr_grid$smooth(i = i_seq, j = j_seq)
          }
          values$chr_grid_change <- values$chr_grid_change + 1
        }
      })
      # find_peaks_desmooth
      observeEvent(input$find_peaks_desmooth, {
        if(!is.null(values$chr_grid)){
          addClass(id = "find_peaks_desmooth", class = "btn-clicked")
          i_seq <- input$find_peaks_i_start:input$find_peaks_i_end
          j_seq <- input$find_peaks_j_start:input$find_peaks_j_end
          if(length(i_seq) * length(j_seq) <= 20){
            progress <- Progress$new(min = 0, max = length(i_seq) * length(j_seq))
            progress$set(message = "Desmooth...", value = 0)
            on.exit(progress$close(), add = TRUE)
            nn <- 1
            maxValue <- progress$getMax()
            for(i in i_seq){
              for(j in j_seq){
                progress$set(value = nn, message = "Demooth...", detail = paste0(nn, " / ", maxValue))
                nn <- nn + 1
                values$chr_grid$get(i, j)$desmooth_chr()
              }
            }
          }else{
            progress <- Progress$new(min = 0, max = length(j_seq))
            progress$set(message = "Begain to desmooth...", value = 0)
            on.exit(progress$close(), add = TRUE)
            values$chr_grid$desmooth_ChrGrid(i = i_seq, j = j_seq)
          }
          values$chr_grid_change <- values$chr_grid_change + 1
        }
      })
      # find_peaks_findPeaks
      observeEvent(input$find_peaks_findPeaks, {
        if(!is.null(values$chr_grid)){
          addClass(id = "find_peaks_findPeaks", class = "btn-clicked")
          i_seq <- input$find_peaks_i_start:input$find_peaks_i_end
          j_seq <- input$find_peaks_j_start:input$find_peaks_j_end
          progress <- Progress$new(min = 0, max = length(i_seq) * length(j_seq))
          progress$set(message = "Begain to finding peaks...", value = 0)
          on.exit(progress$close(), add = TRUE)
          if(length(i_seq) * length(j_seq) <= 10){
            nn <- 1
            maxValue <- progress$getMax()
            for(i in i_seq){
              for(j in j_seq){
                progress$set(value = nn, message = "Find peaks...", detail = paste0(nn, " / ", maxValue))
                nn <- nn + 1
                values$chr_grid$get(i, j)$findPeaks_chr(peakwidth = c(input$find_peaks_peakwidth[1], input$find_peaks_peakwidth[2]),
                                                        snthresh = input$find_peaks_snthresh,
                                                        noise = input$find_peaks_noise, estimateNoise = input$find_peaks_estimateNoise,
                                                        r2thresh = input$find_peaks_r2thresh)
              }
            }
          }else{
            values$chr_grid$findPeaks_ChrGrid(i = i_seq,
                                              j = j_seq,
                                              peakwidth = c(input$find_peaks_peakwidth[1], input$find_peaks_peakwidth[2]),
                                              snthresh = input$find_peaks_snthresh,
                                              noise = input$find_peaks_noise, estimateNoise = input$find_peaks_estimateNoise,
                                              r2thresh = input$find_peaks_r2thresh,
                                              thread = values$threads, shinyProgress = progress)
          }
          if(length(i_seq) == values$chr_grid$dim[1] & length(j_seq) == values$chr_grid$dim[2] & !values$chr_grid$extended){
            # 没有被extend, 且正在对全部窗口寻峰
            progress2 <- Progress$new(min = 0, max = nrow(values$chr_grid$sampleInfo))
            progress2$set(message = "Begain to extend ChrGrid...", value = 0)
            on.exit(progress2$close(), add = TRUE)
            values$chr_grid$extend_ChrGrid(thread = values$threads, shinyProgress = progress2)
          }
          values$chr_grid_change <- values$chr_grid_change + 1
        }
      })
      # find_peaks_extractTarget
      observeEvent(input$find_peaks_extractTarget, {
        if(!is.null(values$chr_grid)){
          addClass(id = "find_peaks_extractTarget", class = "btn-clicked")
          i_seq <- input$find_peaks_i_start:input$find_peaks_i_end
          j_seq <- input$find_peaks_j_start:input$find_peaks_j_end
          if(length(i_seq) * length(j_seq) <= 20){
            progress <- Progress$new(min = 0, max = length(i_seq) * length(j_seq))
            progress$set(message = "Begain to extract target...", value = 0)
            on.exit(progress$close(), add = TRUE)
            nn <- 1
            maxValue <- progress$getMax()
            for(i in i_seq){
              for(j in j_seq){
                progress$set(value = nn, message = "Extract target...", detail = paste0(nn, " / ", maxValue))
                nn <- nn + 1
                values$chr_grid$get(i, j)$extract_targetPeak_chr(rt = input$find_peaks_targetRt, rt_diff_tol = input$find_peaks_rt_diff_tol)
              }
            }
          }else{
            progress <- Progress$new(min = 0, max = length(j_seq))
            progress$set(message = "Begain to extract target...", value = 0)
            on.exit(progress$close(), add = TRUE)
            values$chr_grid$extract_targetPeak_ChrGrid(i = i_seq, j = j_seq,
                                                       rt = input$find_peaks_targetRt, rt_diff_tol = input$find_peaks_rt_diff_tol,
                                                       thread = values$threads, shinyProgress = progress)
          }
        }
        values$chr_grid_change <- values$chr_grid_change + 1
      })

      # find_peaks_currentAnalyte
      observe({
        if(!is.null(values$chr_grid)){
          if(!is.na(values$current_i) & !is.na(values$current_j)){
            values$chr_grid_change # 观察chr_grid是否改变
            chr <- values$chr_grid$get(values$current_i,values$current_j)
            if(!is.null(chr)){
              # 动态更新寻峰参数
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
              # 动态更新目标保留时间和保留时间差值容忍度
              targetRt <- chr$targetRt
              rt_diff_tol <- chr$rt_diff_tol
              if(is.null(targetRt)) targetRt <- chr$expectRt
              if(is.null(rt_diff_tol)) rt_diff_tol <- 10
              if(!is.null(targetRt) & !is.null(rt_diff_tol)){
                updateNumericInput(session = session, inputId = "find_peaks_targetRt", value = targetRt)
                updateNumericInput(session = session, inputId = "find_peaks_rt_diff_tol", value = rt_diff_tol)
              }
              output$find_peaks_currentAnalyte <- plotly::renderPlotly({
                chr$plot_chr(target = input$find_peaks_showTarget)
              })
            }
          }
        }
      })
    }
  )
}
