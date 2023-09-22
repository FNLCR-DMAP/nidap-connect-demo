library(cookies)
library(shiny)
#library(shinyjs)
library(DT)

myShinyUI <-  cookies::add_cookie_handlers(
  fluidPage(
    #shinyjs::useShinyjs(),
    titlePanel("My Cool Shiny <-> NIDAP App!"),
    mainPanel(
      br(),
      textOutput("display_query_params_box"),
      br(),
      DT::dataTableOutput("input_data"),
      br(),
      actionButton("upload", "Upload To NIDAP"),
      textOutput("upload_error_box")
    )
  )
)
