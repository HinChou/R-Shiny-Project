library(shiny)


shinyUI(fluidPage(
  titlePanel("Portfolio Optimization"),
  
    sidebarLayout(
      sidebarPanel(
      
      br(),
      #selectInput("stock_num","Stock",c("2","3","4")),
      
      numericInput("nInputs", "Number of Assets:", 5, 1),
      
      numericInput("tau", "Risk Tolerance:", 1, 1),
      
      numericInput("budget", "Budget:", 100000, 1),
    
    
      dateRangeInput("dates", 
                   "Date range",
                   start = "2015-01-30", 
                   end = "2015-03-29"),
    
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


