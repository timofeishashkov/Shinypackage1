#' @title Shiny App function
#' @description Shiny app implementation as a function
#'
#' @param data A data frame to work with. Defaults to mtcars.
#'
#' @return Returns in interactive application that studies the dataset mtcars
#' @export
shinyFunction <- function(){ ... }
#'
#' @import shiny magrittr bslib
#'
#' @examples if (interactive()) {
#'   shinyFunction()
#' }


shinyFunction <- function(data = datasets::mtcars){
histogramModuleUI <- function(id) {
ns <- NS(id)
sidebarLayout(
  # Sidebar panel for input controls
  sidebarPanel(
    selectInput(ns("var"), "Variable", choices = names(data)),
    sliderInput(ns("cells"), "Number of bins:", min = 1, max = 50, value = 30),
    textInput(inputId = ns("label_x"), label = "Label for the x-axis:"),
    textInput(inputId = ns("title"), label = "Title for the graph:"),
    actionButton(inputId = ns("make_graph"), label = "Make the plot!", icon = icon("drafting-compass"))
  ),

  # Main panel with tabset for plot and summary table
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput(ns("distPlot"))),
      tabPanel("Summary statistics", tableOutput(ns("tabStats")))
    )
  )
)
}

# Define the server logic for the histogram and summary module
histogramModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Reactive expression for the selected variable data
    x <- reactive({
      datasets::mtcars[[input$var]]
    }) %>% bindEvent(input$make_graph)

    # Reactive expression for histogram breaks based on the number of bins
    breaks <- reactive({
      seq(min(x(), na.rm = TRUE), max(x(), na.rm = TRUE), length.out = input$cells + 1)
    }) %>% bindEvent(input$make_graph)

    # Generate the histogram plot
    output$distPlot <- renderPlot({
      graphics::hist(
        x(),
        breaks = breaks(),
        col = 'darkgray',
        border = 'white',
        xlab = input$label_x,
        main = input$title
      )
    })

    # Generate the summary statistics table
    output$tabStats <- renderTable({
      t(summary(x()))
    })
  })
}

# Define the main UI of the app
ui <- fluidPage(
  theme = bs_theme(bootswatch = "superhero", font_scale = 1.5),
  titlePanel("Mtcars Data"),

  # Call the module UI function within the main app UI
  histogramModuleUI("histogram1")
)

# Define the main server logic of the app
server <- function(input, output, session) {
  # Call the module server function
  histogramModuleServer("histogram1")
}

# Run the application
shinyApp(ui = ui, server = server)
}

