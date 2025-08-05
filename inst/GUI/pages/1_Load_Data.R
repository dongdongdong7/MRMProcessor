# 1. Load Data page UI
# Barry Song
# 250731

library(shiny)
library(shinyFiles)
library(bslib)
library(DT)
library(shinyjs)

load_data_ui <- function(id){
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      position = "left",
      title = strong("Select data and tables"),
      open = TRUE,
      selectInput(label = "Select rt unit", choices = c("min", "sec"), selected = "min", inputId = ns("load_data_rtUnit")),
      shinyDirButton(id = ns("load_data_folder"), label = "Select Folder", title = "Please select a folder"),
      verbatimTextOutput(outputId = ns("load_data_folderText"), placeholder = TRUE),
      shinyFilesButton(id = ns("load_data_windowInfo"), label = "Window Information", title = "Please select a windowInfo", multiple = FALSE),
      verbatimTextOutput(outputId = ns("load_data_windowInfoText"), placeholder = TRUE),
      shinyFilesButton(id = ns("load_data_sampleInfo"), label = "Sample Information", title = "Please select a sampleInfo", multiple = FALSE),
      verbatimTextOutput(outputId = ns("load_data_sampleInfoText"), placeholder = TRUE),
      actionButton(label = "Load Data", inputId = ns("load_data_loadDataBt")),
      verbatimTextOutput(outputId = ns("load_data_loadDataText"), placeholder = TRUE),
      shinyFilesButton(id = ns("load_data_ChrGridUpload"), label = "Upload ChrGrid", title = "Please upload a ChrGrid object rds", multiple = FALSE),
      verbatimTextOutput(outputId = ns("load_data_ChrGridText"), placeholder = TRUE),
      downloadButton(outputId = ns("load_data_ChrGridDownload"), label = "Download ChrGrid")
    ),
    card(
      tabsetPanel(
        nav_panel(
          title = "window information",
          DTOutput(
            outputId = ns("load_data_windowInfoDT")
          )
        ),
        nav_panel(
          title = "sample information",
          DTOutput(
            outputId = ns("load_data_sampleInfoDT")
          )
        )
      )
    )
  )
}

load_data_server <- function(id, values){
  moduleServer(
    id,
    function(input, output, session){
      # Generate volumes information
      {
        volumes <- .getVolumes()
        basedir <- volumes
        basedirNames <- names(volumes)
        basedirNames <- gsub("[:]", "",  stringr::str_extract(basedirNames, "(?<=\\().+?(?=\\))"))
        names(basedir) <- basedirNames
        nowsep <- .Platform$file.sep
        shinyFiles::shinyDirChoose(input, 'load_data_folder', roots=basedir)
        shinyFiles::shinyFileChoose(input, 'load_data_windowInfo', roots=basedir )
        shinyFiles::shinyFileChoose(input, 'load_data_sampleInfo', roots=basedir )
        shinyFiles::shinyFileChoose(input, "load_data_ChrGridUpload", roots=basedir)
      }

      # load_data_rtUnit
      observe({
        values$rtUnit <- input$load_data_rtUnit
        if(values$rtUnit != "min" & values$rtUnit != "sec") stop("rtUnit is wrong!")
      })

      # load_data_folder
      observeEvent(input$load_data_folder, {
        if("path" %in% names(input$load_data_folder)){
          folder <- input$load_data_folder$path
          root <- input$load_data_folder$root
          n <- length(folder)
          root <- paste0(root, ":")
          folderPath <- paste0(folder[2:n], collapse = nowsep)
          folderPath <- paste0(root, nowsep, folderPath, nowsep)
          patterns <- c(".mzML", ".mzml")
          patterns <- paste0(patterns, collapse = "|")
          file_path <- list.files(folderPath, pattern = patterns)
          file_path <- paste0(folderPath, file_path)
          if(length(file_path) == 0){
            folderPath <- NULL;message <- "There are no *.mzML or *.mzml files in this folder!"
          }else{
            message <- folderPath;values$dataDir <- folderPath;values$dataPath <- file_path
          }
        }else message <- NULL
        output$load_data_folderText <- renderText({message})
      })

      # load_data_windowInfo
      observeEvent(input$load_data_windowInfo, {
        if("files" %in% names(input$load_data_windowInfo)){
          n <- length(input$load_data_windowInfo$files$`0`)
          file <- input$load_data_windowInfo$files$`0`
          file_name <- input$load_data_windowInfo$files$`0`[[n]]
          root <- paste0(input$load_data_windowInfo$root, ":")
          text <- paste0(file[2:(n-1)], collapse = nowsep)
          text <- paste0(root, nowsep, text, nowsep, file_name)
          values$windowInfoPath <- text
          values$windowInfo <- openxlsx::read.xlsx(values$windowInfoPath)
          message <- text
        }else message <- NULL
        output$load_data_windowInfoText <- renderText({message})
      })

      # load_data_sampleInfo
      observeEvent(input$load_data_sampleInfo, {
        if("files" %in% names(input$load_data_sampleInfo)){
          n <- length(input$load_data_sampleInfo$files$`0`)
          file <- input$load_data_sampleInfo$files$`0`
          file_name <- input$load_data_sampleInfo$files$`0`[[n]]
          root <- paste0(input$load_data_sampleInfo$root, ":")
          text <- paste0(file[2:(n-1)], collapse = nowsep)
          text <- paste0(root, nowsep, text, nowsep, file_name)
          values$sampleInfoPath <- text
          values$sampleInfo <- openxlsx::read.xlsx(values$sampleInfoPath)
          message <- text
        }else message <- NULL
        output$load_data_sampleInfoText <- renderText({message})
      })

      # Update windowInfo and sampleInfo
      observe({
        if(!is.null(values$windowInfo)){
          output$load_data_windowInfoDT <- renderDT({
            values$windowInfo
          }, options = list(columnDefs = list(list(className = "dt-center", targets = "_all")), pageLength = 10))
        }
        if(!is.null(values$sampleInfo)){
          output$load_data_sampleInfoDT <- renderDT({
            values$sampleInfo
          }, options = list(columnDefs = list(list(className = "dt-center", targets = "_all")), pageLength = 10))
        }
      })

      # load_data_loadDataBt
      observeEvent(input$load_data_loadDataBt, {
        if(!is.null(values$dataPath) & !is.null(values$windowInfo) & !is.null(values$sampleInfo)){
          addClass(id = "load_data_loadDataBt", class = "btn-clicked")
          progress <- Progress$new(min = 0, max = nrow(values$sampleInfo))
          progress$set(message = "Begain to reading MRM data", value = 0)
          on.exit(progress$close())
          values$chr_grid <- readMRMData(files = values$dataPath, unit = values$rtUnit,
                                         windowInfo = values$windowInfo, sampleInfo = values$sampleInfo,
                                         thread = values$threads, shinyProgress = progress)
          message <- paste0("ChrGrid with dimensions: "  ,values$chr_grid$dim[1], " x ",  values$chr_grid$dim[2])
          values$windowInfo <- values$chr_grid$windowInfo
          values$sampleInfo <- values$chr_grid$sampleInfo
        }else message <- NULL
        output$load_data_loadDataText <- renderText({message})
      })

      #  load_data_ChrGridUpload
      observeEvent(input$load_data_ChrGridUpload, {
        if("files" %in% names(input$load_data_ChrGridUpload)){
          id <- showNotification("Load ChrGrid object...", duration = NULL, closeButton = FALSE)
          addClass(id = "load_data_ChrGridUpload", class = "btn-clicked")
          n <- length(input$load_data_ChrGridUpload$files$`0`)
          file <- input$load_data_ChrGridUpload$files$`0`
          file_name <- input$load_data_ChrGridUpload$files$`0`[[n]]
          root <- paste0(input$load_data_ChrGridUpload$root, ":")
          text <- paste0(file[2:(n-1)], collapse = nowsep)
          text <- paste0(root, nowsep, text, nowsep, file_name)
          values$chr_grid <- readRDS(text)
          values$windowInfo <- values$chr_grid$windowInfo
          values$sampleInfo <- values$chr_grid$sampleInfo
          updateSelectInput(inputId = "load_data_rtUnit", selected = values$chr_grid$rtUnit)
          message <- text
          removeNotification(id)
          showNotification("Load ChrGrid successful", type = "message")
        }else message <- NULL
        output$load_data_ChrGridText <- renderText({message})
      })

      # load_data_ChrGridDownload
      output$load_data_ChrGridDownload <- downloadHandler(
        filename = function() {
          paste0("chr_grid", ".rds")
        },
        content = function(file){
          id <- showNotification("Download ChrGrid object...", duration = NULL, closeButton = FALSE)
          if(!is.null(values$chr_grid)){
            saveRDS(values$chr_grid, file = file)
          }
          removeNotification(id)
        }
      )
    }
  )
}

# Connect volumes
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
