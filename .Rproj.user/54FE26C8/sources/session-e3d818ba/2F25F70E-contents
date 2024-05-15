#install.packages("shiny)
#devtools::install_github("probably-jaden/Pricer", force = TRUE)

library(shiny)
library(Pricer)
library(bslib)

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Pricing - Monopoly"),
  #theme = bs_theme(
  #  bootswatch = "darkly",
  #  base_font = font_google("Inter"),
  #  navbar_bg = "#25443B"
  #),

  # Sidebar layout with input and output definitions ----
  #fluidRow(
  sidebarLayout(
    # Sidebar panel for inputs ----
    #column(4,
    sidebarPanel(

      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),

      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),

      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
    ),
    mainPanel(
           dataTableOutput("contents")
           )
  ),
  fluidRow(
    column(4,
           wellPanel(
             selectInput("wtpCol", "Willingness to Pay Column Name", choices = c("FILE NEEDS TO BE UPLOADED")),
             selectInput("regressType", "Regression Transformation", choices = c("Linear", "Exponential", "Sigmoid")),
             numericInput("pop", "Customer Population Size", min = 0, value = 1000)
           )
           ),
    column(4,
           plotOutput("demand_Plot_fit")
           ),
    column(4,
           plotOutput("revenue_Plot_fit")
           )
  ),
  fluidRow(
    column(4,
           wellPanel(
             sliderInput("price", "Price", min = 0, max = 10, value = 3, step = .25),
             numericInput("var", "Variable Cost of Product", min = 0, value = 10),
             numericInput("fix", "Fixed Cost (Overhead)", min = 0, value = 1000)
           )
           ),
    column(4,
           plotOutput("profit_Plot_func"),
           ),
    column(4,
           plotOutput("profit_Plot")
           )
    )
)



server <- function(input, output) {

  userData <- reactive({
    req(input$file1)
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    df
  })

  userWTPCol <- reactive({
    if(input$wtpCol == "FILE NEEDS TO BE UPLOADED"){
      return(NULL)
    } else {
      return(userData()[[input$wtpCol]])
    }
  })

  userCleanData <- reactive({
    if(is.null(userWTPCol())){
      return(NULL)
    }
    demandDurable(userData(), input$wtpCol)
  })


  output$contents <- renderDataTable(userData(), options = list(pageLength = 5))

  observeEvent(input$file1, {
    updateSelectInput(inputId = "wtpCol", choices = names(userData()))
  })

  observeEvent(input$wtpCol, {
    #browser()
    if(is.null(userWTPCol())){
      return(NULL)
    }
    updateSliderInput(inputId = "price", min = min(userWTPCol()), max = max(userWTPCol()), value = median(userWTPCol()))
  })

  output$demand_Plot_fit <- renderPlot({
    if(is.null(userCleanData())){
      return(NULL)
    }
    demandPlot(data = userCleanData(), type = input$regressType, population = input$pop, sample = nrow(userCleanData()))
  })

  output$revenue_Plot_fit <- renderPlot({
    if(is.null(userCleanData())){
      return(NULL)
    }
    revenuePlot(data = userCleanData(), type = input$regressType, population = input$pop, sample = nrow(userCleanData()))
  })

  output$profit_Plot <- renderPlot({
    if(is.null(userCleanData())){
      return(NULL)
    }
    profitPlot(data = userCleanData(), type = input$regressType, variable = input$var, fixed = input$fix, population = input$pop, sample = nrow(userCleanData()))
  })

  output$profit_Plot_func <- renderPlot({
    if(is.null(userCleanData())){
      return(NULL)
    }
    profitFunctionPlot(price = input$price, data = userCleanData(), type = input$regressType, variable = input$var, fixed = input$fix, population = input$pop, sample = nrow(userCleanData()))
  })
}

# Create Shiny app ----
shinyApp(ui, server)

