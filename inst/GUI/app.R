# This is MRMProcessor shiny GUI
library(shiny)
library(bslib)
library(DT)
library(gridlayout)

# UI
{
  ui <- page_navbar(
    title = "MRMProcessor",
    selected = "Data Loader",
    collapsible = TRUE,
    theme = bs_theme(),
    nav_panel(
      title = "Data Loader",
      grid_container(
        layout = c(
          "num_chicks area1"
        ),
        gap_size = "0px",
        col_sizes = c(
          "250px",
          "1fr"
        ),
        row_sizes = c(
          "1.5000000000000004fr"
        ),
        grid_card(
          area = "num_chicks",
          card_body(
            selectInput(
              label = "Select rt unit",
              choices = c("min", "seconds"),
              selected = "min",
              inputId = "DataLoader_rtUnit"
            ),
            shinyFiles::shinyDirButton('DataLoader_folder', 'Folder select', 'Please select a folder', FALSE, style="color: #000000; background-color: #FFFFFF; border-color: #555555"),
            verbatimTextOutput(outputId = "DataLoader_text1", placeholder = TRUE),
            shinyFiles::shinyFilesButton('DataLoader_sampleInfo', 'Sample Information', 'Please select a sampleInfo', FALSE, style="color: #000000; background-color: #FFFFFF; border-color: #555555"),
            verbatimTextOutput(outputId = "DataLoader_text2", placeholder = TRUE),
            shinyFiles::shinyFilesButton('DataLoader_windowInfo', 'Window Information', 'Please select a windowInfo', FALSE, style="color: #000000; background-color: #FFFFFF; border-color: #555555"),
            verbatimTextOutput(outputId = "DataLoader_text3", placeholder = TRUE),
            shinyFiles::shinyFilesButton('DataLoader_MChromatogramsUpload', "Upload MChromatograms", 'Please upload a MChromatograms.rds', FALSE, style="color: #000000; background-color: #FFFFFF; border-color: #555555"),
            verbatimTextOutput(outputId = "DataLoader_text4", placeholder = TRUE),
            downloadButton("DataLoader_download", label = "Object Save")
          )
        ),
        grid_card(
          area = "area1",
          card_body(
            tabsetPanel(
              nav_panel(
                title = "sampleInfo",
                DTOutput(
                  outputId = "DataLoader_sampleInfoDT",
                  width = "100%",
                  height = "100%"
                )
              ),
              nav_panel(
                title = "windowInfo",
                DTOutput(
                  outputId = "DataLoader_windowInfoDT",
                  width = "100%",
                  height = "100%"
                )
              )
            )
          )
        )
      )
    ),
    nav_panel(
      title = "IS Checking",
      grid_container(
        layout = c(
          "area1 area2 area3"
        ),
        gap_size = "0px",
        col_sizes = c(
          "0.5fr",
          "1.49fr",
          "0.5000000000000001fr"
        ),
        row_sizes = c(
          "1.5fr"
        ),
        grid_card(
          area = "area1",
          card_body(
            tabsetPanel(
              nav_panel(
                title = "Window",
                selectInput(
                  label = "Batch Name",
                  choices = "none",
                  selected = "none",
                  inputId = "ISCheck_batchName"
                ),
                selectInput(
                  label = "Sample Name",
                  choices = "none",
                  selected = "none",
                  inputId = "ISCheck_sampleName"
                ),
                selectInput(
                  label = "Analyte Name",
                  choices = "none",
                  selected = "none",
                  inputId = "ISCheck_analyteName"
                ),
                numericInput("ISCheck_expectRt", label = "Expect Rtime", min = 0, max = 1000, value = 100, step = 0.5),
                radioButtons("ISCheck_targetPeak", label = "Show Target", choices = c("TRUE" = "TRUE", "FALSE" = "FALSE"), selected = "FALSE", inline = TRUE),
                radioButtons("ISCheck_PlotType", label = "Plot Type", choices = c("Single" = "Single", "Compare" = "Compare"), selected = "Single", inline = TRUE),
                actionButton(
                  label = "Prepare",
                  inputId = "ISCheck_prepare"
                ),
                actionButton(
                  label = "Extract IS",
                  inputId = "ISCheck_extractIS_all"
                ),
                actionButton(
                  label = "Correction IS",
                  inputId = "ISCheck_correctionIS_all"
                ),
                actionButton(
                  label = "debug",
                  inputId = "ISCheck_debug"
                )
              ),
              nav_panel(
                title = "Parameter",
                sliderInput("ISCheck_sn", label = "Select sn", min = 0, max = 10, step = 1, value = 3),
                radioButtons("ISCheck_above", label = "Select above method", choices = c("baseline" = "baseline", "noise" = "noise"), selected = "baseline"),
                sliderInput("ISCheck_preNum", label = "Select preNum", min = 3, max = 10, step = 1, value = 8),
                sliderInput("ISCheck_extend", label = "Select extend", min = 1, max = 10, step = 1, value = 5),
                sliderInput("ISCheck_tolM", label = "Select tol_m", min = 0, max = 50, step = 0.5, value = 10),
                sliderInput("ISCheck_fwhm", label = "Select fwhm", min = 0, max = 50, step = 1, value = 0),
                sliderInput("ISCheck_peakWidth", label = "Select peak width", min = 0, max = 50, value = c(0, 10), step = 1),
                sliderInput("ISCheck_snthresh", label = "Select snthresh", min = 0, max = 10, step = 0.5, value = 0.5),
                radioButtons("ISCheck_xcms", label = "Select a method", choices = c("BOTH" = "BOTH", "ORIGN" = "ORIGN", "CentWave" = "CentWave", "MatchedFilter" = "MatchedFilter"), selected = "BOTH")
              )
            )
          )
        ),
        grid_card(
          area = "area2",
          uiOutput("ISCheck_Plot1UI"),
          #plotly::plotlyOutput(outputId = "ISCheck_Plot1", width = "100%"),
          plotly::plotlyOutput(outputId = "ISCheck_Plot2", width = "100%")
        ),
        grid_card(
          area = "area3",
          tabsetPanel(
            nav_panel(
              title = "noise",
              numericInput("ISCheck_noise", label = "Select noise", min = -1, max = 100000000, step = 1, value = 200),
              sliderInput("ISCheck_noiseMag", label = "Select noiseMag", min = 2, max = 4, step = 1, value = 3),
              actionButton("ISCheck_peakPicking", label = "Peak Picking"),
              actionButton("ISCheck_peakPickingBatch", label = "Peak Picking Batch"),
              actionButton("ISCheck_extract", label = "Extract"),
              actionButton("ISCheck_extractBatch", label = "Extract Batch"),
              actionButton("ISCheck_correction", label = "Correction"),
              actionButton("ISCheck_correctionBatch", label = "Correction Batch"),
              actionButton("ISCheck_blank", label = "Blank"),
              actionButton("ISCheck_blankBatch", label = "Blank Batch")
            ),
            nav_panel(
              title = "smoothPara",
              radioButtons("ISCheck_smooth", label = "Smooth or not", choices = c("TRUE" = "TRUE", "FALSE" = "FALSE"), selected = "TRUE"),
              radioButtons("ISCheck_smoothMethod", label = "Select a smooth method", choices = c("mean" = "mean", "sg" = "sg"), selected = "mean"),
              sliderInput("ISCheck_smoothSize", label = "Select smooth size", min = 3, max = 11, step = 2, value = 3),
              sliderInput("ISCheck_smoothP", label = "Select smooth p", min = 1, max = 10, step = 1, value = 3),
              sliderInput("ISCheck_smoothM", label = "Select smooth m", min = 0, max = 10, step = 1, value = 0),
              sliderInput("ISCheck_smoothTs", label = "Select smooth th", min = 1, max = 10, step = 1, value = 1)
            ),
            nav_panel(
              title = "baselinePara",
              sliderInput("ISCheck_baselineThreshold", label = "Select baseline threshold", min = 1, max = 10, step = 1, value = 1),
              sliderInput("ISCheck_baselineTolM", label = "Select baseline tol_m", min = 0, max = 50, step = 0.5, value = 30),
              sliderInput("ISCheck_loops", label = "Select loops", min = 1, max = 10, step = 1, value = 6)
            )
          )
        )
      )
    ),
    nav_panel(
      title = "Analyte Checking",
      grid_container(
        layout = c(
          "area1 area2 area3"
        ),
        gap_size = "0px",
        col_sizes = c(
          "0.5fr",
          "1.49fr",
          "0.5000000000000001fr"
        ),
        row_sizes = c(
          "1.5fr"
        ),
        grid_card(
          area = "area1",
          card_body(
            tabsetPanel(
              nav_panel(
                title = "Window",
                selectInput(
                  label = "Batch Name",
                  choices = "none",
                  selected = "none",
                  inputId = "AnalyteCheck_batchName"
                ),
                selectInput(
                  label = "Sample Name",
                  choices = "none",
                  selected = "none",
                  inputId = "AnalyteCheck_sampleName"
                ),
                selectInput(
                  label = "Analyte Name",
                  choices = "none",
                  selected = "none",
                  inputId = "AnalyteCheck_analyteName"
                ),
                numericInput("AnalyteCheck_expectRt", label = "Expect Rtime", min = 0, max = 1000, value = 100, step = 0.5),
                numericInput("AnalyteCheck_deltaRt", label = "Delta Rtime", min = 0, max = 100, value = 5, step = 0.5),
                radioButtons("AnalyteCheck_targetPeak", label = "Show Target", choices = c("TRUE" = "TRUE", "FALSE" = "FALSE"), selected = "FALSE", inline = TRUE),
                radioButtons("AnalyteCheck_PlotType", label = "Plot Type", choices = c("Single" = "Single", "Compare" = "Compare"), selected = "Single", inline = TRUE),
                actionButton(
                  label = "Correct Analyte",
                  inputId = "AnalyteCheck_correctionAnalyte_all"
                ),
                actionButton(
                  label = "Extract Analyte",
                  inputId = "AnalyteCheck_extractAnalyte_all"
                ),
                actionButton(
                  label = "Standard Curve",
                  inputId = "AnalyteCheck_standardCurve"
                ),
                actionButton(
                  label = "debug",
                  inputId = "AnalyteCheck_debug"
                )
              ),
              nav_panel(
                title = "Parameter",
                sliderInput("AnalyteCheck_sn", label = "Select sn", min = 0, max = 10, step = 0.5, value = 3),
                radioButtons("AnalyteCheck_above", label = "Select above method", choices = c("baseline" = "baseline", "noise" = "noise"), selected = "baseline"),
                sliderInput("AnalyteCheck_preNum", label = "Select preNum", min = 3, max = 10, step = 1, value = 3),
                sliderInput("AnalyteCheck_extend", label = "Select extend", min = 1, max = 10, step = 1, value = 5),
                sliderInput("AnalyteCheck_tolM", label = "Select tol_m", min = 0, max = 50, step = 0.5, value = 10),
                sliderInput("AnalyteCheck_fwhm", label = "Select fwhm", min = 0, max = 50, step = 1, value = 0),
                sliderInput("AnalyteCheck_peakWidth", label = "Select peak width", min = 0, max = 50, value = c(0, 10), step = 1),
                sliderInput("AnalyteCheck_snthresh", label = "Select snthresh", min = 0, max = 10, step = 0.5, value = 0.5),
                radioButtons("AnalyteCheck_xcms", label = "Select a method", choices = c("BOTH" = "BOTH", "ORIGN" = "ORIGN", "CentWave" = "CentWave", "MatchedFilter" = "MatchedFilter"), selected = "BOTH")
              )
            )
          )
        ),
        grid_card(
          area = "area2",
          tabsetPanel(
            nav_panel(
              title = "Chromatogram",
              uiOutput("AnalyteCheck_Plot1UI"),
              plotly::plotlyOutput(outputId = "AnalyteCheck_Plot2", width = "100%")
            ),
            nav_panel(
              title = "Standard Curve",
              plotly::plotlyOutput(outputId = "AnalyteCheck_Plot3", width = "100%"),
              checkboxGroupInput(inputId = "AnalyteCheck_delete", label = "Delete Points", choices = NULL, selected = NULL, inline = TRUE),
              radioButtons(inputId = "AnalyteCheck_weights", label = "Weights", choices = c("none" = "none", "1/x" = "1/x", "1/x^2" = "1/x^2"), selected = "1/x^2", inline = TRUE),
              radioButtons(inputId = "AnalyteCheck_zero", label = "Zero Point", choices = c("TRUE" = "TRUE", "FALSE" = "FALSE"), selected = "FALSE"),
              actionButton(inputId = "AnalyteCheck_stdCurve_single", label = "Update stdCurve", width = "20%")
            )
          )
        ),
        grid_card(
          area = "area3",
          tabsetPanel(
            nav_panel(
              title = "noise",
              numericInput("AnalyteCheck_noise", label = "Select noise", min = -1, max = 100000000, step = 1, value = 200),
              sliderInput("AnalyteCheck_noiseMag", label = "Select noiseMag", min = 2, max = 4, step = 1, value = 3),
              actionButton("AnalyteCheck_peakPicking", label = "Peak Picking"),
              actionButton("AnalyteCheck_peakPickingBatch", label = "Peak Picking Batch"),
              actionButton("AnalyteCheck_correction", label = "Correction"),
              actionButton("AnalyteCheck_correctionBatch", label = "Correction Batch"),
              actionButton("AnalyteCheck_extract", label = "Extract"),
              actionButton("AnalyteCheck_extractBatch", label = "Extract Batch"),
              actionButton("AnalyteCheck_blank", label = "Blank"),
              actionButton("AnalyteCheck_blankBatch", label = "Blank Batch")
            ),
            nav_panel(
              title = "smoothPara",
              radioButtons("AnalyteCheck_smooth", label = "Smooth or not", choices = c("TRUE" = "TRUE", "FALSE" = "FALSE"), selected = "TRUE"),
              radioButtons("AnalyteCheck_smoothMethod", label = "Select a smooth method", choices = c("mean" = "mean", "sg" = "sg"), selected = "mean"),
              sliderInput("AnalyteCheck_smoothSize", label = "Select smooth size", min = 3, max = 11, step = 2, value = 3),
              sliderInput("AnalyteCheck_smoothP", label = "Select smooth p", min = 1, max = 10, step = 1, value = 3),
              sliderInput("AnalyteCheck_smoothM", label = "Select smooth m", min = 0, max = 10, step = 1, value = 0),
              sliderInput("AnalyteCheck_smoothTs", label = "Select smooth th", min = 1, max = 10, step = 1, value = 1)
            ),
            nav_panel(
              title = "baselinePara",
              sliderInput("AnalyteCheck_baselineThreshold", label = "Select baseline threshold", min = 1, max = 10, step = 1, value = 1),
              sliderInput("AnalyteCheck_baselineTolM", label = "Select baseline tol_m", min = 0, max = 50, step = 0.5, value = 10),
              sliderInput("AnalyteCheck_loops", label = "Select loops", min = 1, max = 10, step = 1, value = 6)
            )
          )
        )
      )
    ),
    nav_panel(
      title = "Result Output",
      grid_container(
        layout = c(
          "area1 area2"
        ),
        gap_size = "0px",
        col_sizes = c(
          "250px",
          "1fr"
        ),
        row_sizes = c(
          "1.50fr"
        ),
        grid_card(
          area = "area1",
          card_body(
            actionButton("ResultOutput_areaOutput", label = "Generate Area Table"),
            downloadButton("ResultOutput_download_areaTb", label = "Download Area Table"),
            actionButton("ResultOutput_calculateCon", label = "Calculate concentration"),
            downloadButton("ResultOutput_download", label = "Download Concentration Table")
          )
        ),
        grid_card(
          area = "area2",
          card_body(
            tabsetPanel(
              nav_panel(
                title = "Area Table",
                DTOutput(
                  outputId = "ResultOutput_areaTb",
                  width = "100%",
                  height = "100%"
                )
              ),
              nav_panel(
                title = "Concentration Table",
                DTOutput(
                  outputId = "ResultOutput_conTb",
                  width = "100%",
                  height = "100%"
                )
              )
            )
          )
        )
      )
    )
  )
}
# Server
{
  server <- function(input, output, session){
    # Interactive Module
    {
      notify <- function(msg, id = NULL){
        showNotification(msg, id = id, duration = NULL, closeButton = FALSE)
      }
    }

    # Global Variable
    {
      values <- reactiveValues()
      # Initial Variable
      {
        values$ncore <- NULL
        values$parallel <- TRUE
        values$rtUnit <- NULL
      }
      # Data Loader
      {
        values$dataDir <- NULL
        values$dataPath <- NULL
        values$MChromatograms <- NULL
        values$sampleNameVector <- NULL
        values$analyteNameVector <- NULL
        values$windowInfoPath <- NULL
        values$sampleInfoPath <- NULL
        values$windowInfo <- NULL
        values$sampleInfo <- NULL
        values$rows_IS <- NULL
        values$rows_Quant <- NULL
        values$rows_Qual <- NULL
        values$cols_batchs <- NULL
        values$batchNameVector <- NULL
        values$prepared <- FALSE
      }
      # IS Checking
      {
        values$IS_extracted <- FALSE
        values$IS_corrected <- FALSE
      }
      # Analyte Checking
      {
        values$Analyte_corrected <- FALSE
        values$Analyte_extracted <- FALSE
        values$stdCurved <- FALSE
      }
      # Result Output
      {
        values$areaTb <- NULL
        values$conTb <- NULL
      }
    }

    # Data Loader
    {
      volumes <- .getVolumes()
      basedir <- volumes
      basedirNames <- names(volumes)
      basedirNames <- gsub("[:]", "",  stringr::str_extract(basedirNames, "(?<=\\().+?(?=\\))"))
      names(basedir) <- basedirNames
      nowsep <- .Platform$file.sep
      shinyFiles::shinyDirChoose(input, 'DataLoader_folder', roots=basedir)
      shinyFiles::shinyFileChoose(input, 'DataLoader_sampleInfo', roots=basedir )
      shinyFiles::shinyFileChoose(input, 'DataLoader_windowInfo', roots=basedir )
      shinyFiles::shinyFileChoose(input, 'DataLoader_parameterInfo', roots=basedir )
      shinyFiles::shinyFileChoose(input, "DataLoader_MChromatogramsUpload", roots=basedir)

      values$ncore <- parallel::detectCores()

      # DataLoader_rtUnit
      {
        observeEvent(input$DataLoader_rtUnit, {
          if(input$DataLoader_rtUnit == "min") values$rtUnit <- "min"
          else if(input$DataLoader_rtUnit == "seconds") values$rtUnit <- "seconds"
          else stop("rtUnit is wrong!")
        })
      }

      # DataLoader_folder
      {
        observeEvent(input$DataLoader_folder, {
          id <- notify("Load Data...")
          on.exit(removeNotification(id), add = TRUE)
          if("path" %in% names(input$DataLoader_folder)){
            folder <- input$DataLoader_folder$path
            root <- input$DataLoader_folder$root
            n <- length(folder)
            root <- paste0(root, ":")
            folderPath <- paste0(folder[2:n], collapse = nowsep)
            folderPath <- paste0(root, nowsep, folderPath, nowsep)
            patterns <- c(".mzXML", ".mzxml", ".mzML", ".mzml")
            patterns <- paste0(patterns, collapse = "|")
            file_path <- list.files(folderPath, pattern = patterns)
            file_path <- paste0(folderPath, file_path)
            if(length(file_path) == 0){
              folderPath <- NULL;message <- "There are no *.mzML files in this folder!"
            }else{
              message <- folderPath;values$dataDir <- folderPath;values$dataPath <- file_path
              id <- notify("Loading Raw Data...")
              on.exit(removeNotification(id), add = TRUE)
              values$MChromatograms <- MRMProcessor::readMRMData(file_path, thread = round(values$ncore * 0.4))
            }
          }
          else message <- NULL
          output$DataLoader_text1 <- renderText({message})
        })
      }

      # DataLoader_sampleInfo
      {
        observeEvent(input$DataLoader_sampleInfo, {
          if("files" %in% names(input$DataLoader_sampleInfo)){
            n <- length(input$DataLoader_sampleInfo$files$`0`)
            file <- input$DataLoader_sampleInfo$files$`0`
            file_name <- input$DataLoader_sampleInfo$files$`0`[[n]]
            root <- paste0(input$DataLoader_sampleInfo$root, ":")
            text <- paste0(file[2:(n-1)], collapse = nowsep)
            text <- paste0(root, nowsep, text, nowsep, file_name)
            values$sampleInfoPath <- text
            values$sampleInfo <- MRMProcessor::read_sampleInfo(values$sampleInfoPath)
            values$batchNameVector <- unique(values$sampleInfo$batchName)
            message <- text
          }else message <- NULL
          output$DataLoader_text2 <- renderText({message})
          if(!is.null(values$sampleInfo)){
            output$DataLoader_sampleInfoDT <- renderDT(
              {values$sampleInfo},options = list(columnDefs = list(list(className = "dt-center", targets = "_all")), pageLength = 10)
            )
          }
        })
      }

      # DataLoader_windowInfo
      {
        observeEvent(input$DataLoader_windowInfo, {
          if("files" %in% names(input$DataLoader_windowInfo)){
            n <- length(input$DataLoader_windowInfo$files$`0`)
            file <- input$DataLoader_windowInfo$files$`0`
            file_name <- input$DataLoader_windowInfo$files$`0`[[n]]
            root <- paste0(input$DataLoader_windowInfo$root, ":")
            text <- paste0(file[2:(n-1)], collapse = nowsep)
            text <- paste0(root, nowsep, text, nowsep, file_name)
            values$windowInfoPath <- text
            values$windowInfo <- MRMProcessor::read_windowInfo(values$windowInfoPath)
            message <- text
          }else message <- NULL
          output$DataLoader_text3 <- renderText({message})
          if(!is.null(values$windowInfo)){
            output$DataLoader_windowInfoDT <- renderDT(
              {values$windowInfo},options = list(columnDefs = list(list(className = "dt-center", targets = "_all")), pageLength = 10)
            )
          }
        })
      }

      # DataLoader_MChromatogramsUpload
      {
        observeEvent(input$DataLoader_MChromatogramsUpload, {
          id <- notify("Upload...")
          on.exit(removeNotification(id), add = TRUE)
          if("files" %in% names(input$DataLoader_MChromatogramsUpload)){
            n <- length(input$DataLoader_MChromatogramsUpload$files$`0`)
            file <- input$DataLoader_MChromatogramsUpload$files$`0`
            file_name <- input$DataLoader_MChromatogramsUpload$files$`0`[[n]]
            root <- paste0(input$DataLoader_MChromatogramsUpload$root, ":")
            text <- paste0(file[2:(n-1)], collapse = nowsep)
            text <- paste0(root, nowsep, text, nowsep, file_name)
            if(is.null(values$MChromatograms)){
              values$MChromatograms <- readRDS(text)
              message <- text
              if(!is.null(attributes(values$MChromatograms)$prepared)) values$prepared <- attributes(values$MChromatograms)$prepared
              if(!is.null(attributes(values$MChromatograms)$IS_extracted)) values$IS_extracted <- attributes(values$MChromatograms)$IS_extracted
              if(!is.null(attributes(values$MChromatograms)$IS_corrected)) values$IS_corrected <- attributes(values$MChromatograms)$IS_corrected
              if(!is.null(attributes(values$MChromatograms)$Analyte_corrected)) values$Analyte_corrected <- attributes(values$MChromatograms)$Analyte_corrected
              if(!is.null(attributes(values$MChromatograms)$Analyte_extracted)) values$Analyte_extracted <-attributes(values$MChromatograms)$Analyte_extracted
              if(!is.null(attributes(values$MChromatograms)$stdCurved)) values$stdCurved <- attributes(values$MChromatograms)$stdCurved
            }
          }else message <- NULL
          output$DataLoader_text4 <- renderText({message})
        })
      }

      # DataLoader_download
      {
        output$DataLoader_download <- downloadHandler(
          filename = function() {
            paste0("MChromatograms", ".rds")
          },
          content = function(file){
            id <- notify("Download...")
            on.exit(removeNotification(id), add = TRUE)
            if(!is.null(values$MChromatograms)){
              if(values$prepared) attributes(values$MChromatograms)$prepared <- TRUE
              if(values$IS_extracted) attributes(values$MChromatograms)$IS_extracted <- TRUE
              if(values$IS_corrected) attributes(values$MChromatograms)$IS_corrected <- TRUE
              if(values$Analyte_corrected) attributes(values$MChromatograms)$Analyte_corrected <- TRUE
              if(values$Analyte_extracted) attributes(values$MChromatograms)$Analyte_extracted <- TRUE
              if(values$stdCurved) attributes(values$MChromatograms)$stdCurved <- TRUE
              saveRDS(values$MChromatograms, file = file)
            }
          }
        )
      }
    }

    # IS Checking
    {
      # ISCheck_prepare
      {
        observeEvent(input$ISCheck_prepare, {
          id <- notify("Peak picking and prepare...")
          on.exit(removeNotification(id), add = TRUE)
          if(!is.null(values$MChromatograms) & !is.null(values$windowInfo) & !is.null(values$sampleInfo) & !values$prepared){
            if(input$ISCheck_fwhm == 0) fwhm <- NA
            else fwhm <- input$ISCheck_fwhm
            if(any(input$ISCheck_peakWidth == 0)) peakWidth <- NA
            else peakWidth <- input$ISCheck_peakWidth
            peakPara <- MRMProcessor::get_peakPara(sn = input$ISCheck_sn, above = input$ISCheck_above, preNum = input$ISCheck_preNum, extend = input$ISCheck_extend, tol_m = input$ISCheck_tolM, fwhm = fwhm, peakWidth = peakWidth, snthresh = input$ISCheck_snthresh, xcms = input$ISCheck_xcms)
            smoothPara <- MRMProcessor::get_smoothPara(smooth = as.logical(input$ISCheck_smooth), method = input$ISCheck_smoothMethod, size = input$ISCheck_smoothSize, p = input$ISCheck_smoothP, m = input$ISCheck_smoothM, ts = input$ISCheck_smoothTs)
            baselinePara <- MRMProcessor::get_baselinePara(threshold = input$ISCheck_baselineThreshold, tol_m = input$ISCheck_baselineTolM, loops = input$ISCheck_loops)
            if(input$ISCheck_noise < 0) noise <- NA
            else noise <- input$ISCheck_noise
            #browser()
            values$MChromatograms <- MRMProcessor::peakPicking_MChromatograms(MChromatograms = values$MChromatograms, thread = round(values$ncore * 0.8), unit = values$rtUnit, noise = noise, noiseMag = input$ISCheck_noiseMag, smoothPara = smoothPara, baselinePara = baselinePara, peakPara = peakPara)
            values$MChromatograms <- MRMProcessor::prepare_MChromatograms(MChromatograms = values$MChromatograms,
                                                            windowInfo = values$windowInfo, sampleInfo = values$sampleInfo,
                                                            unit = values$rtUnit, thread = round(values$ncore * 0.4))
            values$prepared <- TRUE
          }
        })
        observeEvent(values$prepared, {
          if(values$prepared & !is.null(values$batchNameVector)){
            values$cols_batchs <- lapply(values$batchNameVector, function(x) MRMProcessor:::.getCol4batchName(MChromatograms = values$MChromatograms, batchName = x))
            names(values$cols_batchs) <- values$batchNameVector
            values$rows_IS <- MRMProcessor:::.getRow4analyteType(MChromatograms = values$MChromatograms, analyteType = "IS")
            values$rows_Quant <- MRMProcessor:::.getRow4analyteType(MChromatograms = values$MChromatograms, analyteType = "Quant")
            values$rows_Qual <- MRMProcessor:::.getRow4analyteType(MChromatograms = values$MChromatograms, analyteType = "Qual")
            updateSelectInput(session, "ISCheck_batchName", choices = values$batchNameVector, selected = values$batchNameVector[1])
            values$sampleNameVector <- sapply(1:ncol(values$MChromatograms), function(j) {
              strsplit(basename(attributes(values$MChromatograms[1, j])$sample_name), split = ".", fixed = TRUE)[[1]][1]
            })
            values$analyteNameVector <- sapply(1:nrow(values$MChromatograms), function(i) {
              attributes(values$MChromatograms[i, 1])$analyteName
            })
            updateSelectInput(session, "ISCheck_analyteName", choices = values$analyteNameVector[values$rows_IS], selected = values$analyteNameVector[values$rows_IS][1])
          }
        })
      }
      # ISCheck_sampleName
      {
        observeEvent(input$ISCheck_batchName, {
          if(input$ISCheck_batchName != "none" & !is.null(values$sampleNameVector)){
            updateSelectInput(session, "ISCheck_sampleName", choices = values$sampleNameVector[values$cols_batchs[[input$ISCheck_batchName]]], selected = values$sampleNameVector[values$cols_batchs[[input$ISCheck_batchName]]][1])
          }
        })
      }
      # ISCheck_Plot and update UI
      {
        observe({
          if(values$prepared & input$ISCheck_sampleName != "none" & input$ISCheck_analyteName != "none" & !is.null(values$MChromatograms)){
            #row <- MRMProcessor:::.getRow4analyteName(MChromatograms = values$MChromatograms, analyteNameVec = input$ISCheck_analyteName)
            row <- which(values$analyteNameVector == input$ISCheck_analyteName)
            #col <- MRMProcessor:::.getCol4sampleName(MChromatograms = values$MChromatograms, sampleNameVec = input$ISCheck_sampleName)
            col <- which(values$sampleNameVector == input$ISCheck_sampleName)
            peakPara <- attributes(values$MChromatograms[row, col])$peakPara
            updateSliderInput(session, inputId = "ISCheck_sn", value = peakPara$sn)
            updateRadioButtons(session, inputId = "ISCheck_above", selected = peakPara$above)
            updateSliderInput(session, inputId = "ISCheck_preNum", value = peakPara$preNum)
            updateSliderInput(session, inputId = "ISCheck_extend", value = peakPara$extend)
            updateSliderInput(session, inputId = "ISCheck_tolM", value = peakPara$tol_m)
            if(is.na(peakPara$fwhm)) fwhm <- 0
            else fwhm <- peakPara$fwhm
            updateSliderInput(session, inputId = "ISCheck_fwhm", value = fwhm)
            if(any(is.na(peakPara$peakWidth))) peakWidth <- c(0, 0)
            else peakWidth <- peakPara$peakWidth
            updateSliderInput(session, inputId = "ISCheck_peakWidth", value = peakWidth)
            updateSliderInput(session, inputId = "ISCheck_snthresh", value = peakPara$snthresh)
            updateRadioButtons(session, inputId = "ISCheck_xcms", selected = peakPara$xcms)
            smoothPara <- attributes(values$MChromatograms[row, col])$smoothPara
            updateRadioButtons(session, inputId = "ISCheck_smooth", selected = as.character(smoothPara$smooth))
            updateRadioButtons(session, inputId = "ISCheck_smoothMethod", selected = smoothPara$method)
            updateSliderInput(session, inputId = "ISCheck_smoothSize", value = smoothPara$size)
            updateSliderInput(session, inputId = "ISCheck_smoothP", value = smoothPara$p)
            updateSliderInput(session, inputId = "ISCheck_smoothM", value = smoothPara$m)
            updateSliderInput(session, inputId = "ISCheck_smoothTs", value = smoothPara$ts)
            baselinePara <- attributes(values$MChromatograms[row, col])$baselinePara
            updateSliderInput(session, inputId = "ISCheck_baselineThreshold", value = baselinePara$threshold)
            updateSliderInput(session, inputId = "ISCheck_baselineTolM", value = baselinePara$tol_m)
            updateSliderInput(session, inputId = "ISCheck_loops", value = baselinePara$loops)
            updateNumericInput(session, inputId = "ISCheck_expectRt", value = attributes(values$MChromatograms[row, col])$expectRt)
            updateNumericInput(session, inputId = "ISCheck_noise", value = round(attributes(values$MChromatograms[row, col])$noise))
            updateSliderInput(session, inputId = "ISCheck_noiseMag", value = attributes(values$MChromatograms[row, col])$noiseMag)
            output$ISCheck_Plot1UI <- renderUI({
              if(input$ISCheck_PlotType == "Single"){
                plotly::plotlyOutput(outputId = "ISCheck_Plot1", width = "100%")
                output$ISCheck_Plot1 <- plotly::renderPlotly({
                  MRMProcessor:::.plotChromatogram_interactive(Chromatogram = values$MChromatograms[row, col], targetPeak = as.logical(input$ISCheck_targetPeak))
                })
              }else{
                plotOutput(outputId = "ISCheck_Plot1")
                cols <- values$cols_batchs[[input$ISCheck_batchName]]
                areaVec <- sapply(cols, function(j) {
                  if(!is.null(attributes(values$MChromatograms[row, j])$targetPeak)){
                    targetPeak_tmp <- attributes(values$MChromatograms[row, j])$targetPeak[[1]]
                    as.numeric(targetPeak_tmp["area"])
                  }else return(0)
                })
                standard_cols <- cols[which.max(areaVec)]
                output$ISCheck_Plot1 <- renderPlot({
                  MRMProcessor::plotMChromatograms(MChromatograms = values$MChromatograms, rows = row, cols = c(standard_cols, col), targetPeak = as.logical(input$ISCheck_targetPeak))
                })
              }
            })
            # output$ISCheck_Plot2 <- plotly::renderPlotly({
            #   MRMProcessor::plotHeatMap_MChromatogramsRow(MChromatograms = values$MChromatograms, row = row,cols = values$cols_batchs[[input$ISCheck_batchName]])
            # })
          }
        })
      }
      # ISCheck_Plot2
      {
        observe({
          if(values$prepared & values$IS_extracted & input$ISCheck_batchName != "none" & input$ISCheck_analyteName != "none" & !is.null(isolate(values$MChromatograms))){
            #row <- MRMProcessor:::.getRow4analyteName(MChromatograms = values$MChromatograms, analyteNameVec = input$ISCheck_analyteName)
            row <- which(values$analyteNameVector == input$ISCheck_analyteName)
            input$ISCheck_blank;input$ISCheck_blankBatch;input$ISCheck_correction;input$ISCheck_correctionBatch;input$ISCheck_extract;input$ISCheck_extractBatch;values$IS_corrected
            output$ISCheck_Plot2 <- plotly::renderPlotly({
              MRMProcessor::plotHeatMap_MChromatogramsRow(MChromatograms = isolate(values$MChromatograms), row = row,cols = values$cols_batchs[[input$ISCheck_batchName]])
            })
          }
        })
      }
      # ISCheck_extractIS_all
      {
        observeEvent(input$ISCheck_extractIS_all, {
          id <- notify("Extract IS...")
          on.exit(removeNotification(id), add = TRUE)
          if(values$prepared & !is.null(values$MChromatograms)){
            #values$MChromatograms <- extractTargetPeak_MChromatograms(values$MChromatograms, rows = values$rows_IS, cols = 1:ncol(values$MChromatograms), targetRt = NA, tolRt = 10)
            values$MChromatograms <- MRMProcessor:::extractTargetPeak_IS(MChromatograms = values$MChromatograms)
            values$IS_extracted <- TRUE
          }
        })
      }
      # ISCheck_peakPicking
      {
        observeEvent(input$ISCheck_peakPicking, {
          id <- notify("Peak Picking...")
          on.exit(removeNotification(id), add = TRUE)
          if(values$prepared & input$ISCheck_sampleName != "none" & input$ISCheck_analyteName != "none" & !is.null(values$MChromatograms)){
            #row <- MRMProcessor:::.getRow4analyteName(MChromatograms = values$MChromatograms, analyteNameVec = input$ISCheck_analyteName)
            row <- which(values$analyteNameVector == input$ISCheck_analyteName)
            #col <- MRMProcessor:::.getCol4sampleName(MChromatograms = values$MChromatograms, sampleNameVec = input$ISCheck_sampleName)
            col <- which(values$sampleNameVector == input$ISCheck_sampleName)
            if(input$ISCheck_fwhm == 0) fwhm <- NA
            else fwhm <- input$ISCheck_fwhm
            if(any(input$ISCheck_peakWidth == 0)) peakWidth <- NA
            else peakWidth <- input$ISCheck_peakWidth
            peakPara <- MRMProcessor::get_peakPara(sn = input$ISCheck_sn, above = input$ISCheck_above, preNum = input$ISCheck_preNum, extend = input$ISCheck_extend, tol_m = input$ISCheck_tolM, fwhm = fwhm, peakWidth = peakWidth, snthresh = input$ISCheck_snthresh, xcms = input$ISCheck_xcms)
            smoothPara <- MRMProcessor::get_smoothPara(smooth = as.logical(input$ISCheck_smooth), method = input$ISCheck_smoothMethod, size = input$ISCheck_smoothSize, p = input$ISCheck_smoothP, m = input$ISCheck_smoothM, ts = input$ISCheck_smoothTs)
            baselinePara <- MRMProcessor::get_baselinePara(threshold = input$ISCheck_baselineThreshold, tol_m = input$ISCheck_baselineTolM, loops = input$ISCheck_loops)
            if(input$ISCheck_noise < 0) noise <- NA
            else noise <- input$ISCheck_noise
            values$MChromatograms <- MRMProcessor::peakPicking_MChromatograms2(MChromatograms = values$MChromatograms,
                                                                 rows = row, cols = col, noise = noise, noiseMag = input$ISCheck_noiseMag,
                                                                 peakPara = peakPara, smoothPara = smoothPara, baselinePara = baselinePara)
            #browser()
          }
        })
      }
      # ISCheck_extract
      {
        observeEvent(input$ISCheck_extract, {
          id <- notify("Extract target peak...")
          on.exit(removeNotification(id), add = TRUE)
          if(values$prepared & input$ISCheck_sampleName != "none" & input$ISCheck_analyteName != "none" & !is.null(values$MChromatograms)){
            #row <- MRMProcessor:::.getRow4analyteName(MChromatograms = values$MChromatograms, analyteNameVec = input$ISCheck_analyteName)
            row <- which(values$analyteNameVector == input$ISCheck_analyteName)
            #col <- MRMProcessor:::.getCol4sampleName(MChromatograms = values$MChromatograms, sampleNameVec = input$ISCheck_sampleName)
            col <- which(values$sampleNameVector == input$ISCheck_sampleName)
            values$MChromatograms <- MRMProcessor::extractTargetPeak_MChromatograms(values$MChromatograms, rows = row, cols = col, targetRt = input$ISCheck_expectRt)
          }
        })
      }
      # ISCheck_peakPickingBatch
      {
        observeEvent(input$ISCheck_peakPickingBatch, {
          id <- notify("Peak Picking Batch...")
          on.exit(removeNotification(id), add = TRUE)
          if(values$prepared & input$ISCheck_sampleName != "none" & input$ISCheck_analyteName != "none" & !is.null(values$MChromatograms)){
            #row <- MRMProcessor:::.getRow4analyteName(MChromatograms = values$MChromatograms, analyteNameVec = input$ISCheck_analyteName)
            row <- which(values$analyteNameVector == input$ISCheck_analyteName)
            cols <- values$cols_batchs[[input$ISCheck_batchName]]
            if(input$ISCheck_fwhm == 0) fwhm <- NA
            else fwhm <- input$ISCheck_fwhm
            if(any(input$ISCheck_peakWidth == 0)) peakWidth <- NA
            else peakWidth <- input$ISCheck_peakWidth
            peakPara <- MRMProcessor::get_peakPara(sn = input$ISCheck_sn, above = input$ISCheck_above, preNum = input$ISCheck_preNum, extend = input$ISCheck_extend, tol_m = input$ISCheck_tolM, fwhm = fwhm, peakWidth = peakWidth, snthresh = input$ISCheck_snthresh, xcms = input$ISCheck_xcms)
            smoothPara <- MRMProcessor::get_smoothPara(smooth = as.logical(input$ISCheck_smooth), method = input$ISCheck_smoothMethod, size = input$ISCheck_smoothSize, p = input$ISCheck_smoothP, m = input$ISCheck_smoothM, ts = input$ISCheck_smoothTs)
            baselinePara <- MRMProcessor::get_baselinePara(threshold = input$ISCheck_baselineThreshold, tol_m = input$ISCheck_baselineTolM, loops = input$ISCheck_loops)
            if(input$ISCheck_noise < 0) noise <- NA
            else noise <- input$ISCheck_noise
            values$MChromatograms <- MRMProcessor::peakPicking_MChromatograms2(MChromatograms = values$MChromatograms,
                                                                 rows = row, cols = cols, noise = noise, noiseMag = input$ISCheck_noiseMag,
                                                                 peakPara = peakPara, smoothPara = smoothPara, baselinePara = baselinePara)
          }
        })
      }
      # ISCheck_extractBatch
      {
        observeEvent(input$ISCheck_extractBatch, {
          id <- notify("Extract Batch...")
          on.exit(removeNotification(id), add = TRUE)
          if(values$prepared & input$ISCheck_sampleName != "none" & input$ISCheck_analyteName != "none" & !is.null(values$MChromatograms)){
            #row <- MRMProcessor:::.getRow4analyteName(MChromatograms = values$MChromatograms, analyteNameVec = input$ISCheck_analyteName)
            row <- which(values$analyteNameVector == input$ISCheck_analyteName)
            cols <- values$cols_batchs[[input$ISCheck_batchName]]
            values$MChromatograms <-MRMProcessor:: extractTargetPeak_MChromatograms(values$MChromatograms, rows = row, cols = cols, targetRt = input$ISCheck_expectRt)
          }
        })
      }
      # ISCheck_correctionIS_all
      {
        observeEvent(input$ISCheck_correctionIS_all, {
          id <- notify("Correction IS...")
          on.exit(removeNotification(id), add = TRUE)
          if(values$prepared & values$IS_extracted & input$ISCheck_sampleName != "none" & input$ISCheck_analyteName != "none" & !is.null(values$MChromatograms)){
            values$MChromatograms <- MRMProcessor::rtCorrection_IS(MChromatograms = values$MChromatograms, rows = NA, cols = 1:ncol(values$MChromatograms))
            values$IS_corrected <- TRUE
          }
        })
      }
      # ISCheck_correcrion
      {
        observeEvent(input$ISCheck_correction, {
          id <- notify("Correction...")
          on.exit(removeNotification(id), add = TRUE)
          if(values$prepared & values$IS_extracted & input$ISCheck_sampleName != "none" & input$ISCheck_analyteName != "none" & !is.null(values$MChromatograms)){
            #row <- MRMProcessor:::.getRow4analyteName(MChromatograms = values$MChromatograms, analyteNameVec = input$ISCheck_analyteName)
            row <- which(values$analyteNameVector == input$ISCheck_analyteName)
            #col <- MRMProcessor:::.getCol4sampleName(MChromatograms = values$MChromatograms, sampleNameVec = input$ISCheck_sampleName)
            col <- which(values$sampleNameVector == input$ISCheck_sampleName)
            values$MChromatograms <- MRMProcessor::rtCorrection_IS(MChromatograms = values$MChromatograms, rows = row, cols = col)
          }
        })
      }
      # ISCheck_correctionBatch
      {
        observeEvent(input$ISCheck_correctionBatch, {
          id <- notify("Correction Batch...")
          on.exit(removeNotification(id), add = TRUE)
          if(values$prepared & values$IS_extracted & input$ISCheck_sampleName != "none" & input$ISCheck_analyteName != "none" & !is.null(values$MChromatograms)){
            #row <- MRMProcessor:::.getRow4analyteName(MChromatograms = values$MChromatograms, analyteNameVec = input$ISCheck_analyteName)
            row <- which(values$analyteNameVector == input$ISCheck_analyteName)
            cols <- values$cols_batchs[[input$ISCheck_batchName]]
            values$MChromatograms <- MRMProcessor::rtCorrection_IS(MChromatograms = values$MChromatograms, rows = row, cols = cols)
          }
        })
      }
      # ISCheck_blank
      {
        observeEvent(input$ISCheck_blank, {
          id <- notify("Blank...")
          on.exit(removeNotification(id), add = TRUE)
          if(values$prepared & values$IS_extracted & input$ISCheck_sampleName != "none" & input$ISCheck_analyteName != "none" & !is.null(values$MChromatograms)){
            #row <- MRMProcessor:::.getRow4analyteName(MChromatograms = values$MChromatograms, analyteNameVec = input$ISCheck_analyteName)
            row <- which(values$analyteNameVector == input$ISCheck_analyteName)
            #col <- MRMProcessor:::.getCol4sampleName(MChromatograms = values$MChromatograms, sampleNameVec = input$ISCheck_sampleName)
            col <- which(values$sampleNameVector == input$ISCheck_sampleName)
            values$MChromatograms <- MRMProcessor:::blank_MChromatograms(values$MChromatograms, rows = row, cols = col)
          }
        })
      }
      # ISCheck_blankBatch
      observeEvent(input$ISCheck_blankBatch, {
        id <- notify("Blank Batch...")
        on.exit(removeNotification(id), add = TRUE)
        if(values$prepared & values$IS_extracted & input$ISCheck_sampleName != "none" & input$ISCheck_analyteName != "none" & !is.null(values$MChromatograms)){
          #row <- MRMProcessor:::.getRow4analyteName(MChromatograms = values$MChromatograms, analyteNameVec = input$ISCheck_analyteName)
          row <- which(values$analyteNameVector == input$ISCheck_analyteName)
          cols <- values$cols_batchs[[input$ISCheck_batchName]]
          values$MChromatograms <- MRMProcessor:::blank_MChromatograms(values$MChromatograms, rows = row, cols = cols)
        }
      })
      # ISCheck_debug
      {
        observeEvent(input$ISCheck_debug, {
          browser()
        })
      }
    }

    # Analyte Checking
    {
      # Analyte Checking initial
      {
        observeEvent(values$IS_corrected, {
          if(values$IS_corrected & !is.null(values$MChromatograms)){
            updateSelectInput(session, "AnalyteCheck_batchName", choices = values$batchNameVector, selected = values$batchNameVector[1])
            updateSelectInput(session, "AnalyteCheck_analyteName", choices = values$analyteNameVector[c(values$rows_Quant, values$rows_Qual)], selected = values$analyteNameVector[c(values$rows_Quant, values$rows_Qual)][1])
          }
        })
      }
      # AnalyteCheck_sampleName
      {
        observeEvent(input$AnalyteCheck_batchName, {
          if(input$AnalyteCheck_batchName != "none" & !is.null(values$sampleNameVector)){
            updateSelectInput(session, "AnalyteCheck_sampleName", choices = values$sampleNameVector[values$cols_batchs[[input$AnalyteCheck_batchName]]], selected = values$sampleNameVector[values$cols_batchs[[input$AnalyteCheck_batchName]]][1])
          }
        })
      }
      # AnalyteCheck_correctionAnalyte_all
      {
        observeEvent(input$AnalyteCheck_correctionAnalyte_all, {
          id <- notify("Correct Analyte...")
          on.exit(removeNotification(id), add = TRUE)
          if(values$IS_corrected & !is.null(values$MChromatograms)){
            values$MChromatograms <- MRMProcessor::rtCorrection_analyte(MChromatograms = values$MChromatograms, rows = NA, cols = 1:ncol(values$MChromatograms), thread = round(values$ncore * 0.4))
            values$Analyte_corrected <- TRUE
          }
        })
      }
      # AnalyteCheck_extractAnalyte_all
      {
        observeEvent(input$AnalyteCheck_extractAnalyte_all, {
          id <- notify("Extract Analyte...")
          on.exit(removeNotification(id), add = TRUE)
          if(values$Analyte_corrected & !is.null(values$MChromatograms)){
            values$MChromatograms <- MRMProcessor::extractTargetPeak_MChromatograms(MChromatograms = values$MChromatograms, rows = c(values$rows_Quant, values$rows_Qual), cols = 1:ncol(values$MChromatograms), targetRt = NA, tolRt = 5)
            values$Analyte_extracted <- TRUE
          }
        })
      }
      # AnalyteCheck_standardCurve
      {
        observeEvent(input$AnalyteCheck_standardCurve, {
          id <- notify("Calculate standard curve...")
          on.exit(removeNotification(id), add = TRUE)
          if(values$IS_corrected & values$IS_extracted){
            values$MChromatograms <- MRMProcessor:::GetStdCurve_MChromatograms(values$MChromatograms, sampleInfo = values$sampleInfo)
            values$stdCurved <- TRUE
          }
        })
      }
      # AnalyteCheck_plot and update UI
      {
        observe({
          if(values$IS_corrected & values$Analyte_corrected & !is.null(values$MChromatograms) & input$AnalyteCheck_sampleName != "none" & input$AnalyteCheck_analyteName != "none"){
            #row <- MRMProcessor:::.getRow4analyteName(MChromatograms = values$MChromatograms, analyteNameVec = input$AnalyteCheck_analyteName)
            row <- which(values$analyteNameVector == input$AnalyteCheck_analyteName)
            #col <- MRMProcessor:::.getCol4sampleName(MChromatograms = values$MChromatograms, sampleNameVec = input$AnalyteCheck_sampleName)
            col <- which(values$sampleNameVector == input$AnalyteCheck_sampleName)
            peakPara <- attributes(values$MChromatograms[row, col])$peakPara
            updateSliderInput(session, inputId = "AnalyteCheck_sn", value = peakPara$sn)
            updateRadioButtons(session, inputId = "AnalyteCheck_above", selected = peakPara$above)
            updateSliderInput(session, inputId = "AnalyteCheck_preNum", value = peakPara$preNum)
            updateSliderInput(session, inputId = "AnalyteCheck_extend", value = peakPara$extend)
            updateSliderInput(session, inputId = "AnalyteCheck_tolM", value = peakPara$tol_m)
            if(is.na(peakPara$fwhm)) fwhm <- 0
            else fwhm <- peakPara$fwhm
            updateSliderInput(session, inputId = "AnalyteCheck_fwhm", value = fwhm)
            if(any(is.na(peakPara$peakWidth))) peakWidth <- c(0, 0)
            else peakWidth <- peakPara$peakWidth
            updateSliderInput(session, inputId = "AnalyteCheck_peakWidth", value = peakWidth)
            updateSliderInput(session, inputId = "AnalyteCheck_snthresh", value = peakPara$snthresh)
            updateRadioButtons(session, inputId = "AnalyteCheck_xcms", selected = peakPara$xcms)
            smoothPara <- attributes(values$MChromatograms[row, col])$smoothPara
            updateRadioButtons(session, inputId = "AnalyteCheck_smooth", selected = as.character(smoothPara$smooth))
            updateRadioButtons(session, inputId = "AnalyteCheck_smoothMethod", selected = smoothPara$method)
            updateSliderInput(session, inputId = "AnalyteCheck_smoothSize", value = smoothPara$size)
            updateSliderInput(session, inputId = "AnalyteCheck_smoothP", value = smoothPara$p)
            updateSliderInput(session, inputId = "AnalyteCheck_smoothM", value = smoothPara$m)
            updateSliderInput(session, inputId = "AnalyteCheck_smoothTs", value = smoothPara$ts)
            baselinePara <- attributes(values$MChromatograms[row, col])$baselinePara
            updateSliderInput(session, inputId = "AnalyteCheck_baselineThreshold", value = baselinePara$threshold)
            updateSliderInput(session, inputId = "AnalyteCheck_baselineTolM", value = baselinePara$tol_m)
            updateSliderInput(session, inputId = "AnalyteCheck_loops", value = baselinePara$loops)
            updateNumericInput(session, inputId = "AnalyteCheck_expectRt", value = attributes(values$MChromatograms[row, col])$expectRt)
            if(is.null(attributes(values$MChromatograms[row, col])$deltaRt)) deltaRt <- 0
            else deltaRt <- round(attributes(values$MChromatograms[row, col])$deltaRt, 4)
            updateNumericInput(session, inputId = "AnalyteCheck_deltaRt", value = deltaRt)
            updateNumericInput(session, inputId = "AnalyteCheck_noise", value = round(attributes(values$MChromatograms[row, col])$noise))
            updateSliderInput(session, inputId = "AnalyteCheck_noiseMag", value = attributes(values$MChromatograms[row, col])$noiseMag)
            output$AnalyteCheck_Plot1UI <- renderUI({
              if(input$AnalyteCheck_PlotType == "Single"){
                plotly::plotlyOutput(outputId = "AnalyteCheck_Plot1", width = "100%")
                output$AnalyteCheck_Plot1 <- plotly::renderPlotly({
                  MRMProcessor:::.plotChromatogram_interactive(Chromatogram = values$MChromatograms[row, col], targetPeak = as.logical(input$AnalyteCheck_targetPeak))
                })
              }else{
                plotOutput(outputId = "AnalyteCheck_Plot1")
                cols <- values$cols_batchs[[input$AnalyteCheck_batchName]]
                areaVec <- sapply(cols, function(j) {
                  if(!is.null(attributes(values$MChromatograms[row, j])$targetPeak)){
                    targetPeak_tmp <- attributes(values$MChromatograms[row, j])$targetPeak[[1]]
                    as.numeric(targetPeak_tmp["area"])
                  }else return(0)
                })
                standard_cols <- cols[which.max(areaVec)]
                output$AnalyteCheck_Plot1 <- renderPlot({
                  MRMProcessor::plotMChromatograms(MChromatograms = values$MChromatograms, rows = row, cols = c(standard_cols, col), targetPeak = as.logical(input$AnalyteCheck_targetPeak))
                })
              }
            })
            # output$AnalyteCheck_Plot2 <- plotly::renderPlotly({
            #   MRMProcessor::plotHeatMap_MChromatogramsRow(MChromatograms = values$MChromatograms, row = row, cols = values$cols_batchs[[input$AnalyteCheck_batchName]])
            # })
          }
          if(values$stdCurved){
            #row <- MRMProcessor:::.getRow4analyteName(MChromatograms = values$MChromatograms, analyteNameVec = input$AnalyteCheck_analyteName)
            row <- which(values$analyteNameVector == input$AnalyteCheck_analyteName)
            batchName <- input$AnalyteCheck_batchName
            if(row %in% values$rows_Quant){
              stdCurveRes <- attributes(values$MChromatograms[row, values$cols_batchs[[batchName]][1]])$stdCurveRes
              delete_injectOrder <- stdCurveRes$df$injectOrder
              names(delete_injectOrder) <- as.character(stdCurveRes$df$injectOrder)
              delete_injectOrder <- c("none" = "none", delete_injectOrder)
              if(is.null(stdCurveRes$delete)) select <- "none"
              else{
                select <- as.character(stdCurveRes$delete)
              }
              updateCheckboxGroupInput(session, inputId = "AnalyteCheck_delete", choices = delete_injectOrder, selected = select)
              updateRadioButtons(session, inputId = "AnalyteCheck_weights", selected = stdCurveRes$weights)
              updateRadioButtons(session, inputId = "AnalyteCheck_zero", selected = as.character(stdCurveRes$zero))
              output$AnalyteCheck_Plot3 <- plotly::renderPlotly({
                MRMProcessor::plotStdCurve(attributes(values$MChromatograms[row, values$cols_batchs[[batchName]][1]])$stdCurveRes)
              })
            }else{
              output$AnalyteCheck_Plot3 <- plotly::renderPlotly({
                plotly::ggplotly(ggplot2::ggplot(data = NULL))
              })
            }
          }
        })
      }
      # AnalyteCheck_Plot2
      {
        observe({
          if(values$IS_corrected & values$Analyte_corrected & !is.null(isolate(values$MChromatograms)) & input$AnalyteCheck_batchName != "none" & input$AnalyteCheck_analyteName != "none"){
            #row <- MRMProcessor:::.getRow4analyteName(MChromatograms = values$MChromatograms, analyteNameVec = input$AnalyteCheck_analyteName)
            row <- which(values$analyteNameVector == input$AnalyteCheck_analyteName)
            input$AnalyteCheck_blank;input$AnalyteCheck_blankBatch;input$AnalyteCheck_correction;input$AnalyteCheck_correctionBatch;input$AnalyteCheck_extract;input$AnalyteCheck_extractBatch;values$Analyte_extracted
            output$AnalyteCheck_Plot2 <- plotly::renderPlotly({
              MRMProcessor::plotHeatMap_MChromatogramsRow(MChromatograms = isolate(values$MChromatograms), row = row, cols = values$cols_batchs[[input$AnalyteCheck_batchName]])
            })
          }
        })
      }
      # AnalyteCheck_peakPicking
      {
        observeEvent(input$AnalyteCheck_peakPicking, {
          id <- notify("Peak Picking...")
          on.exit(removeNotification(id), add = TRUE)
          if(values$Analyte_corrected & values$Analyte_extracted & input$AnalyteCheck_sampleName != "none" & input$AnalyteCheck_analyteName != "none" & !is.null(values$MChromatograms)){
            #row <- MRMProcessor:::.getRow4analyteName(MChromatograms = values$MChromatograms, analyteNameVec = input$AnalyteCheck_analyteName)
            row <- which(values$analyteNameVector == input$AnalyteCheck_analyteName)
            #col <- MRMProcessor:::.getCol4sampleName(MChromatograms = values$MChromatograms, sampleNameVec = input$AnalyteCheck_sampleName)
            col <- which(values$sampleNameVector == input$AnalyteCheck_sampleName)
            if(input$AnalyteCheck_fwhm == 0) fwhm <- NA
            else fwhm <- input$AnalyteCheck_fwhm
            if(any(input$AnalyteCheck_peakWidth == 0)) peakWidth <- NA
            else peakWidth <- input$AnalyteCheck_peakWidth
            peakPara <- MRMProcessor::get_peakPara(sn = input$AnalyteCheck_sn, above = input$AnalyteCheck_above, preNum = input$AnalyteCheck_preNum, extend = input$AnalyteCheck_extend, tol_m = input$AnalyteCheck_tolM, fwhm = fwhm, peakWidth = peakWidth, snthresh = input$AnalyteCheck_snthresh, xcms = input$AnalyteCheck_xcms)
            smoothPara <- MRMProcessor::get_smoothPara(smooth = as.logical(input$AnalyteCheck_smooth), method = input$AnalyteCheck_smoothMethod, size = input$AnalyteCheck_smoothSize, p = input$AnalyteCheck_smoothP, m = input$AnalyteCheck_smoothM, ts = input$AnalyteCheck_smoothTs)
            baselinePara <- MRMProcessor::get_baselinePara(threshold = input$AnalyteCheck_baselineThreshold, tol_m = input$AnalyteCheck_baselineTolM, loops = input$AnalyteCheck_loops)
            if(input$AnalyteCheck_noise < 0) noise <- NA
            else noise <- input$AnalyteCheck_noise
            values$MChromatograms <- MRMProcessor::peakPicking_MChromatograms2(MChromatograms = values$MChromatograms,
                                                                 rows = row, cols = col, noise = noise, noiseMag = input$AnalyteCheck_noiseMag,
                                                                 peakPara = peakPara, smoothPara = smoothPara, baselinePara = baselinePara)
          }
        })
      }
      # AnalyteCheck_peakPickingBatch
      {
        observeEvent(input$AnalyteCheck_peakPickingBatch, {
          id <- notify("Peak Picking Batch...")
          on.exit(removeNotification(id), add = TRUE)
          if(values$Analyte_corrected & values$Analyte_extracted & input$AnalyteCheck_sampleName != "none" & input$AnalyteCheck_analyteName != "none" & !is.null(values$MChromatograms)){
            #row <- MRMProcessor:::.getRow4analyteName(MChromatograms = values$MChromatograms, analyteNameVec = input$AnalyteCheck_analyteName)
            row <- which(values$analyteNameVector == input$AnalyteCheck_analyteName)
            cols <- values$cols_batchs[[input$AnalyteCheck_batchName]]
            if(input$AnalyteCheck_fwhm == 0) fwhm <- NA
            else fwhm <- input$AnalyteCheck_fwhm
            if(any(input$AnalyteCheck_peakWidth == 0)) peakWidth <- NA
            else peakWidth <- input$AnalyteCheck_peakWidth
            peakPara <- MRMProcessor::get_peakPara(sn = input$AnalyteCheck_sn, above = input$AnalyteCheck_above, preNum = input$AnalyteCheck_preNum, extend = input$AnalyteCheck_extend, tol_m = input$AnalyteCheck_tolM, fwhm = fwhm, peakWidth = peakWidth, snthresh = input$AnalyteCheck_snthresh, xcms = input$AnalyteCheck_xcms)
            smoothPara <- MRMProcessor::get_smoothPara(smooth = as.logical(input$AnalyteCheck_smooth), method = input$AnalyteCheck_smoothMethod, size = input$AnalyteCheck_smoothSize, p = input$AnalyteCheck_smoothP, m = input$AnalyteCheck_smoothM, ts = input$AnalyteCheck_smoothTs)
            baselinePara <- MRMProcessor::get_baselinePara(threshold = input$AnalyteCheck_baselineThreshold, tol_m = input$AnalyteCheck_baselineTolM, loops = input$AnalyteCheck_loops)
            if(input$AnalyteCheck_noise < 0) noise <- NA
            else noise <- input$AnalyteCheck_noise
            values$MChromatograms <- MRMProcessor::peakPicking_MChromatograms2(MChromatograms = values$MChromatograms,
                                                                 rows = row, cols = cols, noise = noise, noiseMag = input$AnalyteCheck_noiseMag,
                                                                 peakPara = peakPara, smoothPara = smoothPara, baselinePara = baselinePara)
          }
        })
      }
      # AnalyteCheck_correction
      {
        observeEvent(input$AnalyteCheck_correction, {
          id <- notify("Correction...")
          on.exit(removeNotification(id), add = TRUE)
          if(values$Analyte_corrected & values$Analyte_extracted & input$AnalyteCheck_sampleName != "none" & input$AnalyteCheck_analyteName != "none" & !is.null(values$MChromatograms)){
            #row <- MRMProcessor:::.getRow4analyteName(MChromatograms = values$MChromatograms, analyteNameVec = input$AnalyteCheck_analyteName)
            row <- which(values$analyteNameVector == input$AnalyteCheck_analyteName)
            #col <- MRMProcessor:::.getCol4sampleName(MChromatograms = values$MChromatograms, sampleNameVec = input$AnalyteCheck_sampleName)
            col <- which(values$sampleNameVector == input$AnalyteCheck_sampleName)
            values$MChromatograms <- MRMProcessor::rtCorrection_analyte(MChromatograms = values$MChromatograms, rows = row, cols = col, thread = 1,deltaRt = input$AnalyteCheck_deltaRt)
          }
        })
      }
      # AnalyteCheck_correctionBatch
      {
        observeEvent(input$AnalyteCheck_correctionBatch, {
          id <- notify("Correction Batch...")
          on.exit(removeNotification(id), add = TRUE)
          if(values$prepared & values$IS_extracted & input$AnalyteCheck_sampleName != "none" & input$AnalyteCheck_analyteName != "none" & !is.null(values$MChromatograms)){
            #row <- MRMProcessor:::.getRow4analyteName(MChromatograms = values$MChromatograms, analyteNameVec = input$AnalyteCheck_analyteName)
            row <- which(values$analyteNameVector == input$AnalyteCheck_analyteName)
            cols <- values$cols_batchs[[input$AnalyteCheck_batchName]]
            values$MChromatograms <- MRMProcessor::rtCorrection_analyte(MChromatograms = values$MChromatograms, rows = row, cols = cols, thread = round(values$ncore) * 0.2, deltaRt = input$AnalyteCheck_deltaRt)
          }
        })
      }
      # AnalyteCheck_extract
      {
        observeEvent(input$AnalyteCheck_extract, {
          id <- notify("Extract target peak...")
          on.exit(removeNotification(id), add = TRUE)
          if(values$prepared & input$AnalyteCheck_sampleName != "none" & input$AnalyteCheck_analyteName != "none" & !is.null(values$MChromatograms)){
            #row <- MRMProcessor:::.getRow4analyteName(MChromatograms = values$MChromatograms, analyteNameVec = input$AnalyteCheck_analyteName)
            row <- which(values$analyteNameVector == input$AnalyteCheck_analyteName)
            #col <- MRMProcessor:::.getCol4sampleName(MChromatograms = values$MChromatograms, sampleNameVec = input$AnalyteCheck_sampleName)
            col <- which(values$sampleNameVector == input$AnalyteCheck_sampleName)
            values$MChromatograms <- MRMProcessor::extractTargetPeak_MChromatograms(values$MChromatograms, rows = row, cols = col, targetRt = input$AnalyteCheck_expectRt, tolRt = 5)
          }
        })
      }
      # AnalyteCheck_extractBatch
      {
        observeEvent(input$AnalyteCheck_extractBatch, {
          id <- notify("Extract Batch...")
          on.exit(removeNotification(id), add = TRUE)
          if(values$prepared & input$AnalyteCheck_sampleName != "none" & input$AnalyteCheck_analyteName != "none" & !is.null(values$MChromatograms)){
            #row <- MRMProcessor:::.getRow4analyteName(MChromatograms = values$MChromatograms, analyteNameVec = input$AnalyteCheck_analyteName)
            row <- which(values$analyteNameVector == input$AnalyteCheck_analyteName)
            cols <- values$cols_batchs[[input$AnalyteCheck_batchName]]
            values$MChromatograms <- MRMProcessor::extractTargetPeak_MChromatograms(values$MChromatograms, rows = row, cols = cols, targetRt = input$AnalyteCheck_expectRt, tolRt = 5)
          }
        })
      }
      # AnalyteCheck_stdCurve_single
      {
        observeEvent(input$AnalyteCheck_stdCurve_single, {
          id <- notify("Update stdCurve...")
          on.exit(removeNotification(id), add = TRUE)
          #row <- MRMProcessor:::.getRow4analyteName(MChromatograms = values$MChromatograms, analyteNameVec = input$AnalyteCheck_analyteName)
          row <- which(values$analyteNameVector == input$AnalyteCheck_analyteName)
          batchName <- input$AnalyteCheck_batchName
          if(row %in% values$rows_Quant & values$stdCurved){
            if(any(input$AnalyteCheck_delete == "none")){
              delete <- c()
            }else{
              delete <- as.integer(input$AnalyteCheck_delete)
            }
            stdCurveRes_new <- MRMProcessor::GetStdCurve(values$MChromatograms, row = row, batchName = batchName, weights = input$AnalyteCheck_weights, delete = delete, zero = as.logical(input$AnalyteCheck_zero), rows_IS = values$rows_IS)
            attributes(values$MChromatograms[row, values$cols_batchs[[batchName]][1]])$stdCurveRes <- stdCurveRes_new
          }
        })
      }
      # AnalyteCheck_blank
      {
        observeEvent(input$AnalyteCheck_blank, {
          id <- notify("Blank...")
          on.exit(removeNotification(id), add = TRUE)
          if(values$Analyte_extracted & input$AnalyteCheck_sampleName != "none" & input$AnalyteCheck_analyteName != "none" & !is.null(values$MChromatograms)){
            #row <- MRMProcessor:::.getRow4analyteName(MChromatograms = values$MChromatograms, analyteNameVec = input$AnalyteCheck_analyteName)
            row <- which(values$analyteNameVector == input$AnalyteCheck_analyteName)
            #col <- MRMProcessor:::.getCol4sampleName(MChromatograms = values$MChromatograms, sampleNameVec = input$AnalyteCheck_sampleName)
            col <- which(values$sampleNameVector == input$AnalyteCheck_sampleName)
            values$MChromatograms <- MRMProcessor:::blank_MChromatograms(values$MChromatograms, rows = row, cols = col)
          }
        })
      }
      # AnalyteCheck_blankBatch
      observeEvent(input$AnalyteCheck_blankBatch, {
        id <- notify("Blank Batch...")
        on.exit(removeNotification(id), add = TRUE)
        if(values$Analyte_extracted & input$AnalyteCheck_sampleName != "none" & input$AnalyteCheck_analyteName != "none" & !is.null(values$MChromatograms)){
          #row <- MRMProcessor:::.getRow4analyteName(MChromatograms = values$MChromatograms, analyteNameVec = input$AnalyteCheck_analyteName)
          row <- which(values$analyteNameVector == input$AnalyteCheck_analyteName)
          cols <- values$cols_batchs[[input$AnalyteCheck_batchName]]
          values$MChromatograms <- MRMProcessor:::blank_MChromatograms(values$MChromatograms, rows = row, cols = cols)
        }
      })
      # AnalyteCheck debug
      {
        observeEvent(input$AnalyteCheck_debug, {
          browser()
        })
      }
    }

    # Result Output
    {
      observeEvent(input$ResultOutput_calculateCon, {
        id <- notify("Calculate Concentration...")
        on.exit(removeNotification(id), add = TRUE)
        if(values$stdCurved){
          values$conTb <- MRMProcessor:::cal_concentration(values$MChromatograms, sampleInfo = values$sampleInfo)
          output$ResultOutput_conTb <- renderDT({
            values$conTb
          }, options = list(columnDefs = list(list(className = "dt-center", targets = "_all")), pageLength = 12))
        }
      })
      output$ResultOutput_download <- downloadHandler(
        filename = function() {
          paste0("concentration", ".xlsx")
        },
        content = function(file){
          if(!is.null(values$conTb)) openxlsx::write.xlsx(values$conTb, file = file)
        }
      )
      observeEvent(input$ResultOutput_areaOutput, {
        id <- notify("Generate Area Table...")
        on.exit(removeNotification(id), add = TRUE)
        if(!is.null(values$MChromatograms) & values$Analyte_extracted){
          values$areaTb <- MRMProcessor:::generate_area(values$MChromatograms)
          output$ResultOutput_areaTb <- renderDT({
            values$areaTb
          }, options = list(columnDefs = list(list(className = "dt-center", targets = "_all")), pageLength = 12))
        }
      })
      output$ResultOutput_download_areaTb <- downloadHandler(
        filename = function() {
          paste0("area", ".xlsx")
        },
        content = function(file){
          if(!is.null(values$areaTb)) openxlsx::write.xlsx(values$areaTb, file = file)
        }
      )
    }
  }
}

.getVolumes <- function(){
  if(Sys.info()["sysname"] != 'Windows') stop("OS must be Windows!")
  else if(Sys.info()["sysname"] == 'Windows'){
    wmic <- paste0(Sys.getenv("SystemRoot"), "\\System32\\Wbem\\WMIC.exe")
    tmp <- tryCatch({
      volumes <- system(paste(wmic, "logicaldisk get Caption"),
                        intern = TRUE, ignore.stderr = TRUE)
      volumes <- sub(" *\\r$", "", volumes)
      keep <- !tolower(volumes) %in% c("caption",
                                       "")
      volumes <- volumes[keep]
      volNames <- system(paste(wmic, "/FAILFAST:1000 logicaldisk get VolumeName"),
                         intern = TRUE, ignore.stderr = TRUE)
      volNames <- sub(" *\\r$", "", volNames)
      volNames <- volNames[keep]
      volNames <- paste0(volNames, ifelse(volNames == "",
                                          "", " "))
      volNames <- paste0(volNames, "(", volumes,
                         ")")
      list(volumes, volNames)
    }, error = function(e) {
      warnings(e)
      volumes_info <- system2("powershell", "$dvr=[System.IO.DriveInfo]::GetDrives();Write-Output $dvr.length $dvr.name $dvr.VolumeLabel;",
                              stdout = TRUE)
      num = as.integer(volumes_info[1])
      if (num == 0)
        return(NULL)
      mat <- matrix(volumes_info[-1], nrow = num, ncol = 2)
      mat[, 1] <- gsub(":\\\\$", ":/", mat[,
                                           1])
      sel <- mat[, 2] == ""
      mat[sel, 2] <- mat[sel, 1]
      volumes <- mat[, 1]
      volNames <- mat[, 2]
      volNames <- paste0(volNames, " (", gsub(":/$",
                                              ":", volumes), ")")
      list(volumes, volNames)
    })
    volumes <- tmp[[1]]
    names(volumes) <- tmp[[2]]
    volumes <- gsub(":$", ":/", volumes)
  }else stop("Do not know sysname!")
  volumes
}

# Run the application
shinyApp(ui = ui, server = server)
