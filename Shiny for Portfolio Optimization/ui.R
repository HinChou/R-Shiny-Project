library(shiny)


shinyUI(fluidPage(

  # Track all the page view and button clicking
  tags$head(includeScript("google_analytics.js")),
  
  titlePanel("Portfolio Optimization"),
  
    sidebarLayout(
      sidebarPanel(
      
      br(),

      numericInput("nInputs", "Number of Assets:", 5, 1),
      
      numericInput("tau", "Risk Tolerance:", 1, 1),
      
      numericInput("budget", "Budget:", 100000, 1),
    
    
      dateRangeInput("dates", 
                   "Date range",
                   # Default data from past three months
                   start = Sys.Date() - 90, 
                   end = Sys.Date()),
    
    actionButton("goButton","Generate"),
    
    br(),
    br(),
    
    uiOutput("stock"),
    uiOutput("PlotButton")
    
       
  ),
  
 mainPanel(
   
  
  tabsetPanel(
    tabPanel("General", verbatimTextOutput("number"),
             plotOutput("plot"),
             textOutput("ExpR"),
             tableOutput("table")), 
    tabPanel("Long Only", plotOutput("lowB")), 
    tabPanel("Short Only", plotOutput("UpB")),
    tabPanel("Efficient Frontier", plotOutput("EF"))
  )
  )
    
  )
)
)


