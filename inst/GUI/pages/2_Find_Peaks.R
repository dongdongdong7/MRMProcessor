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
      open = TRUE
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
        p("第一个可拖动卡片")
      )
    ),

    # 卡片2
    card(
      class = "position-absolute draggable-card",
      style = css(
        width = "300px",
        z_index = 1000,
        left = "400px",
        top = "50px"
      ),
      card_header(
        class = "user-select-none",
        style = "cursor: move;",
        "卡片2"
      ),
      card_body(
        p("第二个可拖动卡片")
      )
    ),
  )
}

find_peaks_server <- function(id, values){
}
