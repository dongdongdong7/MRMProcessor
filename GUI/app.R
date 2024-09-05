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
            actionButton(
              label = "Link Data",
              inputId = "DataLoader_linkData"
            )
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
        values$windowNameVector <- NULL
        values$windowInfoPath <- NULL
        values$sampleInfoPath <- NULL
        values$parameterInfoPath <- NULL
        values$windowInfo <- NULL
        values$sampleInfo <- NULL
        values$parameterInfo <- NULL
        values$rows_IS <- NULL
        values$rows_Quant <- NULL
        values$rows_Qual <- NULL
        values$cols_batchs <- NULL
        values$batchNameVector <- NULL
        values$prepared <- FALSE
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

      values$ncore <- parallel::detectCores()

      # DataLoader_rtUnit
      {
        observeEvent(input$DataLoader_rtUnit, {
          if(input$DataLoader_rtUnit == "min") values$rtUnit <- 60
          else if(input$DataLoader_rtUnit == "seconds") values$rtUnit <- 1
          else stop("rtUnit is wrong!")
        })
      }

      # DataLoader_folder
      {
        observeEvent(input$DataLoader_folder, {
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
              values$MChromatograms <- readMRMData(file_path, thread = round(values$ncore * 0.4))
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
            values$sampleInfo <- read_sampleInfo(values$sampleInfoPath)
            values$batchNameVector <- unique(values$sampleInfo$batchName)
            diffSampleName <- setdiff(values$sampleNameVector, values$sampleInfo$sampleName)
            if(length(diffSampleName) != 0){
              warning(paste0("please check: ", paste0(diffSampleName, collapse = ", ")))
              values$sampleInfo <- NULL
              message <- paste0("please check: ", paste0(diffSampleName, collapse = ", "))
            }else{
              values$sampleNameVector <- values$sampleNameVector
              message <- text
            }
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
            values$windowInfo <- read_windowInfo(values$windowInfoPath)
            diffWindowName <- setdiff(values$windowNameVector, values$windowInfo$windowName)
            if(length(diffWindowName) != 0){
              warning(paste0("please check: ", paste0(diffWindowName, collapse = ", ")))
              values$windowInfo <- NULL
              message <- paste0("please check: ", paste0(diffWindowName, collapse = ", "))
            }else{
              values$windowNameVector <- values$windowNameVector
              message <- text
            }
          }else message <- NULL
          output$DataLoader_text3 <- renderText({message})
          if(!is.null(values$windowInfo)){
            output$DataLoader_windowInfoDT <- renderDT(
              {values$windowInfo},options = list(columnDefs = list(list(className = "dt-center", targets = "_all")), pageLength = 10)
            )
          }
        })
      }

      # DataLoader_linkData
      {
        observeEvent(input$DataLoader_linkData, {
          id <- notify("Update parameterInfo...")
          on.exit(removeNotification(id), add = TRUE)
          if(!is.null(values$MChromatograms) & !is.null(values$windowInfo) & !is.null(values$sampleInfo)){
            values$MChromatograms <- prepare_MChromatograms(MChromatograms = MChromatograms,
                                                            windowInfo = values$windowInfo,
                                                            sampleInfo = values$sampleInfo,
                                                            thread = round(values$ncore * 0.2))
            values$cols_batchs <- lapply(values$batchNameVector, function(x) .getCol4batchName(MChromatograms = MChromatograms, batchName = x))
            names(values$cols_batchs) <- values$batchNameVector
            values$rows_IS <- .getRow4analyteType(MChromatograms = MChromatograms, analyteType = "IS")
            values$rows_Quant <- .getRow4analyteType(MChromatograms = MChromatograms, analyteType = "Quant")
            values$rows_Qual <- .getRow4analyteType(MChromatograms = MChromatograms, analyteType = "Qual")
            values$prepared <- TRUE
          }
        })
      }
    }
  }
}

.getVolumes <- function(){
  if(Sys.info()["sysname"] != 'Windows') stop("OS must be Windows!")
  volumes_info <- system2("powershell", "$dvr=[System.IO.DriveInfo]::GetDrives();Write-Output $dvr.length $dvr.name $dvr.VolumeLabel;",
                          stdout = TRUE)
  num = as.integer(volumes_info[1])
  if (num == 0)
    return(NULL)
  mat <- matrix(volumes_info[-1], nrow = num, ncol = 2)
  mat[, 1] <- gsub(":\\\\$", ":/", mat[, 1])
  sel <- mat[, 2] == ""
  mat[sel, 2] <- mat[sel, 1]
  volumes <- mat[, 1]
  volNames <- mat[, 2]
  volNames <- paste0(volNames, " (", gsub(":/$", ":",
                                          volumes), ")")
  names(volumes) <- volNames
  volumes <- gsub(":$", ":/", volumes)
  return(volumes)
}

# Run the application
shinyApp(ui = ui, server = server)
