#install.packages("shiny)
#devtools::install_git("https://github.com/probably-jaden/Pricer", force = TRUE, upgrade = "always")

library(shiny)
library(Pricer)
library(bslib)
#library(dplyr)

# Define UI for data upload app ----
ui <- fluidPage(
  withMathJax(),
  # App title ----
  titlePanel("Pricing - Monopoly"),
  #theme = bs_theme(``
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

  tags$head(
    tags$style(HTML("
      .scrollable-text {
        max-height: 400px;
        overflow-y: auto;
        white-space: pre-wrap;  /* preserve formatting */
      }
    "))
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
           #plotOutput("revenue_Plot_fit")
           tabsetPanel(
             tabPanel("Interpretations",  align = "center",
                      br(), br(), br(),
                               uiOutput("demand_math_formula"),
                      br(), br(),
                      div(style = "font-size: 14px;",
                          uiOutput("intercept_interpretation")
                          ),
                      #br(),
                      div(style = "font-size: 14px;",
                          uiOutput("slope_interpretation")
                          )
                      ),

             tabPanel("Summary",
                      div(class = "scrollable-text", verbatimTextOutput("demand_summary", placeholder = TRUE))
             )
           )
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

  output$contents <- renderDataTable(userData(), options = list(pageLength = 5))

  observeEvent(input$file1, {
    updateSelectInput(inputId = "wtpCol", choices = names(userData()))
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

  userPop <- reactive(input$pop)
  userSample <- reactive(nrow(userCleanData()))
  userType <- reactive(input$regressType)

  observeEvent(input$wtpCol, {
    #browser()
    if(is.null(userWTPCol())){
      return(NULL)
    }
    updateSliderInput(inputId = "price", min = min(userWTPCol()), max = max(userWTPCol()), value = mean(userWTPCol()))
  })

  userPrice <- reactive(input$price)

  observeEvent(input$wtpCol, {
    if(is.null(userWTPCol())){
      return(NULL)
    }
    updateNumericInput(inputId = "var", value = roundLog(mean(userWTPCol()) * .4))
    updateNumericInput(inputId = "fix", value = roundLog(mean(userWTPCol()) * userPop() * .1))
  })

  output$demand_Plot_fit <- renderPlot({
    if(is.null(userCleanData())){
      return(NULL)
    }
    demandPlot(price = userPrice(), data = userCleanData(), type = userType(), population = userPop(), sample = userSample())
  })

  output$revenue_Plot_fit <- renderPlot({
    if(is.null(userCleanData())){
      return(NULL)
    }
    revenuePlot(data = userCleanData(), type = userType(), population = userPop(), sample = userSample())
  })

  output$demand_summary <- renderPrint({
    if(is.null(userCleanData())){
      return(NULL)
    }
    demandSummary(data = userCleanData(), type = userType())
  })

  output$intercept_interpretation <- renderUI({
    if(is.null(userCleanData())){
      return(NULL)
    }

    intText <- demandInterpret(data = userCleanData(),
                             type = userType(),
                             population = userPop(),
                             sample = userSample())[[1]]

    HTML(intText)
  })

  output$slope_interpretation <- renderText({
    if(is.null(userCleanData())){
      return(NULL)
    }

    slopeText <- demandInterpret(data = userCleanData(),
                    type = userType(),
                    population = userPop(),
                    sample = userSample())[[2]]
    HTML(slopeText)
  })


  output$demand_math_formula <- renderUI({
    if(is.null(userCleanData())){
      return(NULL)
    }
    latexMaths <- demandFormula2(data = userCleanData(), type = userType(), population = userPop(), sample = userSample())

    withMathJax(
      helpText(paste0("$$\\large{\\text{Quantity Sold} \\ = \\ ", latexMaths, "}$$"))
    )
  })

  output$demand_formula <- renderPrint({
    if(is.null(userCleanData())){
      return(NULL)
    }
    demandFormula(data = userCleanData(), type = userType(), population = userPop(), sample = userSample())
  })


  userVar <- reactive(input$var)
  userFix <- reactive(input$fix)

  output$profit_Plot <- renderPlot({
    if(is.null(userCleanData())){
      return(NULL)
    }
    profitPlot(data = userCleanData(), type = userType(), variable = userVar(), fixed = userFix(), population = userPop(), sample = userSample())
  })

  output$profit_Plot_func <- renderPlot({
    if(is.null(userCleanData())){
      return(NULL)
    }
    profitFunctionPlot(price = userPrice(), data = userCleanData(), type = userType(), variable = userVar(), fixed = userFix(), population = userPop(), sample = userSample())
  })
}

# Create Shiny app ----
shinyApp(ui, server)

#library(tidyverse)
#cp <- read_csv("~/Desktop/CupcakesTest.csv")
#cpC <- demandDurable(cp, "cupcakes")
#demand
#demandFormula(cpC, "Linear", 1, 1)
#demandFormula2(cpC, "Exponential", 1, 1)[[1]]
#demandInterpret(cpC, "Linear", 1, 1)[[1]]
