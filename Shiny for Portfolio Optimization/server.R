library(quantmod)
library(quadprog)
#source("helpers.R")



#switch(input$stock_num,
       
       #"2" = lapply(1:2, function(i)
       #{
         #textInput(as.character(i), "Symbol", "AAPL")
       #}),
       #"3" = lapply(1:3, function(i)
       #{
         #textInput("i", "Symbol", "AAPL")
       #}),
       #"4" = lapply(1:4, function(i)
       #{
         #textInput("i", "Symbol", "AAPL")
       #}),
#)


shinyServer( function(input, output, session) {
  
  
  #values <- reactiveValues(shouldShow = FALSE)
  
  nStock <- reactive({
    
    if(input$goButton)
    {
      
    isolate( 
      lapply(1:input$nInputs, function(i)
      {
        fluidRow(
          column(width = 10,offset = 1, wellPanel(h4(paste("No.", i,"Input Values")),textInput(paste("tInput",i), "Ticker",""),numericInput(paste("min",i),"Min",value=NA),numericInput(paste("max",i),"Max",value=NA),numericInput(paste("expect",i),"Expected Return",value=NA))))
      #tnam <- paste("tInput",i)
      #column(width = 3,numericInput(paste("min",i),"Min",value=NA))
      #tags$style(type="text/css", "#tnam { width: 150px; }")
      
      })
    )
      
    }
    
  })
  
  
  output$stock <- renderUI({
    
    
    if(input$nInputs < 2)
    {
      output$number <- renderText({"Please Input the Number of Assets Greater Than 2!"})
      return()
    }
    if(input$tau < 0)
    {
      output$number <- renderText({paste("Invalid Tau Input")})
      return()
    }
    if(input$budget < 0)
    {
      output$number <- renderText({paste("Invalid Budget Input")})
      return()
    }
    output$number <- renderText({"Please Click Generate Button to Start!"})
    
      nStock()
    
    
  })
  output$PlotButton <- renderUI({
    
    if(input$goButton)
    {
      fluidRow(
        column(12,actionButton("PlotButton","Plot"))
      )
    }
  })
  
  vCalculation <- reactive({
    dataInput <- list()
    input$PlotButton
    
    for (i in 1:input$nInputs)
    { 
      if(input[[paste("tInput",i)]] == "")
      {
        output$number <- renderText({paste("Please Input No.",i,"Stock Ticker")})
        return()
      }
      else
      {
        output$number <- renderText({paste("Click Plot to Generate Graph")})
        dataInput[[i]] <- getSymbols(input[[paste("tInput",i)]], src = "yahoo", 
                                     from = input$dates[1],
                                     to = input$dates[2],
                                     auto.assign = FALSE)
      }
      
    }
    
    stock.p <- matrix(0,(length(dataInput[[1]][,2])),input$nInputs + 1)
    stock.r <- matrix(0,(length(dataInput[[1]][,2])-1),input$nInputs)
    stock.er <- numeric()
    covariance <- matrix(0,input$nInputs,input$nInputs)
    average.r <- matrix(0,input$nInputs,1)
    
      tau <- isolate(input$tau)
    
    lbound <- numeric()
    ubound <- numeric()
    
    for (i in 1:input$nInputs)
    {
      if(is.na(input[[paste("expect",i)]]))
      {
        stock.er[i] <- 999
      }
      else
        stock.er[i] <- input[[paste("expect",i)]]/252
      if(is.na(input[[paste("min",i)]]))
      {
        lbound[i] <- -999
      }
      else
        lbound[i] <- input[[paste("min",i)]]
      if(is.na(input[[paste("max",i)]]))
      {
        ubound[i] <- 999
      }
      else
        ubound[i] <- input[[paste("max",i)]]
    }
    
    
    for(i in 1 : input$nInputs)
    {
      for(j in 1 : length(stock.p[,1]))
      {
        stock.p[j,i+1] <- dataInput[[i]][j,4]
      }
    }
    
    

    #A <- matrix(1,1,input$nInputs)
    notional <- isolate(input$budget)
    
    #last day price
    price <- stock.p[(length(stock.p[,1])),2:length(stock.p[1,])]
    
    
    #return
    for (j in 1:input$nInputs)
      for (i in 1:length(stock.r[,1]))
      {
        stock.r[i,j] <- ((stock.p[i+1,j+1])-(stock.p[i,j+1]))/(stock.p[i,j+1])
      }
    
    #covariance
    for (j in 1:input$nInputs)
      for(i in 1:input$nInputs)
      {
        covariance[i,j] <- cov(stock.r[,i],stock.r[,j])
      }
    
    #average return
    for (i in 1:input$nInputs)
    {
      average.r[i,] <- mean(stock.r[,i])
    }
    
    #replace with user inputs
    for (i in 1:input$nInputs)
    {
      if (as.numeric(stock.er[i]) != 999)
      {
          average.r[i,] <- stock.er[i]
      }
    }
    
    Dmat <- covariance*252*252
    dvec <- average.r*tau*252
    Aeq <- matrix(c(rep(1, input$nInputs)), ncol=1)
    Amat <- cbind(Aeq, dvec, diag(input$nInputs), -diag(input$nInputs))
    bvec <- c(1, 0, lbound, -ubound)
    qp <- solve.QP(Dmat, dvec, Amat, bvec, meq=1)
    op.solution <- t(qp$solution)
    volume <- t(op.solution*notional/price)
    
    volume <- as.numeric(t(volume)) 
    
    volume
    
    
  })
  
  PortfolioReturn <- reactive({
    dataInput <- list()
    input$PlotButton
    
    for (i in 1:input$nInputs)
    { 
      if(input[[paste("tInput",i)]] == "")
      {
        output$number <- renderText({paste("Please Input No.",i,"Stock Ticker")})
        return()
      }
      else
      {
        output$number <- renderText({paste("Click Plot to Generate Graph")})
        dataInput[[i]] <- getSymbols(input[[paste("tInput",i)]], src = "yahoo", 
                                     from = input$dates[1],
                                     to = input$dates[2],
                                     auto.assign = FALSE)
      }
      
    }
    
    stock.p <- matrix(0,(length(dataInput[[1]][,2])),input$nInputs + 1)
    stock.r <- matrix(0,(length(dataInput[[1]][,2])-1),input$nInputs)
    stock.er <- numeric()
    covariance <- matrix(0,input$nInputs,input$nInputs)
    average.r <- matrix(0,input$nInputs,1)
    
    tau <- isolate(input$tau)
    
    lbound <- numeric()
    ubound <- numeric()
    
    for (i in 1:input$nInputs)
    {
      if(is.na(input[[paste("expect",i)]]))
      {
        stock.er[i] <- 999
      }
      else
        stock.er[i] <- input[[paste("expect",i)]]/252
      if(is.na(input[[paste("min",i)]]))
      {
        lbound[i] <- -999
      }
      else
        lbound[i] <- input[[paste("min",i)]]
      if(is.na(input[[paste("max",i)]]))
      {
        ubound[i] <- 999
      }
      else
        ubound[i] <- input[[paste("max",i)]]
    }
    
    
    for(i in 1 : input$nInputs)
    {
      for(j in 1 : length(stock.p[,1]))
      {
        stock.p[j,i+1] <- dataInput[[i]][j,4]
      }
    }
    
    #A <- matrix(1,1,input$nInputs)
    notional <- isolate(input$budget)
    
    #last day price
    price <- stock.p[(length(stock.p[,1])),2:length(stock.p[1,])]
    
    
    #return
    for (j in 1:input$nInputs)
      for (i in 1:length(stock.r[,1]))
      {
        stock.r[i,j] <- ((stock.p[i+1,j+1])-(stock.p[i,j+1]))/(stock.p[i,j+1])
      }
    
    #covariance
    for (j in 1:input$nInputs)
      for(i in 1:input$nInputs)
      {
        covariance[i,j] <- cov(stock.r[,i],stock.r[,j])
      }
    
    #average return
    for (i in 1:input$nInputs)
    {
      average.r[i,] <- mean(stock.r[,i])
    }
    
    #replace with user inputs
    for (i in 1:input$nInputs)
    {
      if (as.numeric(stock.er[i]) != 999)
      {
        average.r[i,] <- stock.er[i]
      }
    }
    
    Dmat <- covariance*252*252
    dvec <- average.r*tau*252
    Aeq <- matrix(c(rep(1, input$nInputs)), ncol=1)
    Amat <- cbind(Aeq, dvec, diag(input$nInputs), -diag(input$nInputs))
    bvec <- c(1, 0, lbound, -ubound)
    qp <- solve.QP(Dmat, dvec, Amat, bvec, meq=1)
    op.solution <- t(qp$solution)
    p.return <- op.solution%*%average.r*252
    p.return 
  })
  
  nNames <- reactive({
    stock <- numeric()
    
    for(i in 1:input$nInputs)
    {
      stock[i] <- input[[paste("tInput",i)]]
    }
    stock
  })
  
 
  
  ShortOnly <- reactive({
    dataInput <- list()
    for (i in 1:input$nInputs)
    { 
      if(input[[paste("tInput",i)]] == "")
      {
        output$number <- renderText({paste("Please Input No.",i,"Stock Ticker")})
        return()
      }
      else
      {
        output$number <- renderText({paste("Click Plot to Generate Graph")})
        dataInput[[i]] <- getSymbols(input[[paste("tInput",i)]], src = "yahoo", 
                                     from = input$dates[1],
                                     to = input$dates[2],
                                     auto.assign = FALSE)
      }
      
    }
    
    stock.p <- matrix(0,(length(dataInput[[1]][,2])),input$nInputs + 1)
    stock.r <- matrix(0,(length(dataInput[[1]][,2])-1),input$nInputs)
    stock.er <- numeric()
    covariance <- matrix(0,input$nInputs,input$nInputs)
    average.r <- matrix(0,input$nInputs,1)
    
    tau <- input$tau
    
    lbound <- numeric()
    ubound <- numeric()
    
    for (i in 1:input$nInputs)
    {
      if(is.na(input[[paste("expect",i)]]))
      {
        stock.er[i] <- 999
      }
      else
        stock.er[i] <- input[[paste("expect",i)]]/252
      
      if(is.na(input[[paste("max",i)]]))
      {
        lbound[i] <- -999
      }
      else
        lbound[i] <- input[[paste("max",i)]] * -1
      if(!is.na(input[[paste("min",i)]]))
      {
        if(input[[paste("min",i)]] > 0)
          ubound[i] <- input[[paste("min",i)]] * -1
      }
      else
        ubound[i] <- 0
      
    }
    
    
    for(i in 1 : input$nInputs)
    {
      for(j in 1 : length(stock.p[,1]))
      {
        stock.p[j,i+1] <- dataInput[[i]][j,4]
      }
    }
    
    
    
    #A <- matrix(1,1,input$nInputs)
    notional <- input$budget
    
    #last day price
    price <- stock.p[(length(stock.p[,1])),2:length(stock.p[1,])]
    
    
    #return
    for (j in 1:input$nInputs)
      for (i in 1:length(stock.r[,1]))
      {
        stock.r[i,j] <- ((stock.p[i+1,j+1])-(stock.p[i,j+1]))/(stock.p[i,j+1])
      }
    
    #covariance
    for (j in 1:input$nInputs)
      for(i in 1:input$nInputs)
      {
        covariance[i,j] <- cov(stock.r[,i],stock.r[,j])
      }
    
    #average return
    for (i in 1:input$nInputs)
    {
      average.r[i,] <- mean(stock.r[,i])
    }
    
    #replace with user inputs
    for (i in 1:input$nInputs)
    {
      if (as.numeric(stock.er[i]) != 999)
      {
        average.r[i,] <- stock.er[i]
      }
    }
    
    Dmat <- covariance*252*252
    dvec <- average.r*tau*252
    Aeq <- matrix(c(rep(1, input$nInputs)), ncol=1)
    Amat <- cbind(Aeq, dvec, diag(input$nInputs), -diag(input$nInputs))
    bvec <- c(-1, -10, lbound, -ubound)
    qp <- solve.QP(Dmat, dvec, Amat, bvec, meq=1)
    op.solution <- t(qp$solution)
    op.solution
    
  })
  
  LongOnly <- reactive({
    dataInput <- list()
    for (i in 1:input$nInputs)
    { 
      if(input[[paste("tInput",i)]] == "")
      {
        output$number <- renderText({paste("Please Input No.",i,"Stock Ticker")})
        return()
      }
      else
      {
        output$number <- renderText({paste("Click Plot to Generate Graph")})
        dataInput[[i]] <- getSymbols(input[[paste("tInput",i)]], src = "yahoo", 
                                     from = input$dates[1],
                                     to = input$dates[2],
                                     auto.assign = FALSE)
      }
      
    }
    
    stock.p <- matrix(0,(length(dataInput[[1]][,2])),input$nInputs + 1)
    stock.r <- matrix(0,(length(dataInput[[1]][,2])-1),input$nInputs)
    stock.er <- numeric()
    covariance <- matrix(0,input$nInputs,input$nInputs)
    average.r <- matrix(0,input$nInputs,1)
    
      tau <- input$tau
    
    lbound <- numeric()
    ubound <- numeric()
    
    for (i in 1:input$nInputs)
    {
      if(is.na(input[[paste("expect",i)]]))
      {
        stock.er[i] <- 999
      }
      else
        stock.er[i] <- input[[paste("expect",i)]]/252
      
      if(!is.na(input[[paste("min",i)]])&&input[[paste("min",i)]] > 0)
        lbound[i] <- input[[paste("min",i)]]
      else
        lbound[i] <- 0
      
      
      if(is.na(input[[paste("max",i)]]))
      {
        ubound[i] <- 999
      }
      else
        ubound[i] <- input[[paste("max",i)]]
    }
    
    
    for(i in 1 : input$nInputs)
    {
      for(j in 1 : length(stock.p[,1]))
      {
        stock.p[j,i+1] <- dataInput[[i]][j,4]
      }
    }
    
    
    
    #A <- matrix(1,1,input$nInputs)
    notional <- input$budget
    
    #last day price
    price <- stock.p[(length(stock.p[,1])),2:length(stock.p[1,])]
    
    
    #return
    for (j in 1:input$nInputs)
      for (i in 1:length(stock.r[,1]))
      {
        stock.r[i,j] <- ((stock.p[i+1,j+1])-(stock.p[i,j+1]))/(stock.p[i,j+1])
      }
    
    #covariance
    for (j in 1:input$nInputs)
      for(i in 1:input$nInputs)
      {
        covariance[i,j] <- cov(stock.r[,i],stock.r[,j])
      }
    
    #average return
    for (i in 1:input$nInputs)
    {
      average.r[i,] <- mean(stock.r[,i])
    }
    
    #replace with user inputs
    for (i in 1:input$nInputs)
    {
      if (as.numeric(stock.er[i]) != 999)
      {
        average.r[i,] <- stock.er[i]
      }
    }
    
    Dmat <- covariance*252*252
    dvec <- average.r*tau*252
    Aeq <- matrix(c(rep(1, input$nInputs)), ncol=1)
    Amat <- cbind(Aeq, dvec, diag(input$nInputs), -diag(input$nInputs))
    bvec <- c(1, 0, lbound, -ubound)
    qp <- solve.QP(Dmat, dvec, Amat, bvec, meq=1)
    op.solution <- t(qp$solution)
    for(i in 1:length(op.solution))
    {
      if(op.solution[i] < 0)
        op.solution[i] = 0
    }
    op.solution
    
  })
  
  output$EF <- renderPlot({
    dataInput <- list()
    input$PlotButton
    
    for (i in 1:input$nInputs)
    { 
      if(input[[paste("tInput",i)]] == "")
      {
        output$number <- renderText({paste("Please Input No.",i,"Stock Ticker")})
        return()
      }
      else
      {
        output$number <- renderText({paste("Click Plot to Generate Graph")})
        dataInput[[i]] <- getSymbols(input[[paste("tInput",i)]], src = "yahoo", 
                                     from = input$dates[1],
                                     to = input$dates[2],
                                     auto.assign = FALSE)
      }
      
    }
    
    stock.p <- matrix(0,(length(dataInput[[1]][,2])),input$nInputs + 1)
    stock.r <- matrix(0,(length(dataInput[[1]][,2])-1),input$nInputs)
    stock.er <- numeric()
    A <- matrix(1,1,input$nInputs)
    covariance <- matrix(0,input$nInputs,input$nInputs)
    average.r <- matrix(0,input$nInputs,1)
    for (i in 1:input$nInputs)
    {
      if(is.na(input[[paste("expect",i)]]))
      {
        stock.er[i] <- 999
      }
      else
        stock.er[i] <- input[[paste("expect",i)]]/252
    }
     
    for(i in 1 : input$nInputs)
    {
      for(j in 1 : length(stock.p[,1]))
      {
        stock.p[j,i+1] <- dataInput[[i]][j,4]
      }
    }
    
    
    #return
    for (j in 1:input$nInputs)
      for (i in 1:length(stock.r[,1]))
      {
        stock.r[i,j] <- ((stock.p[i+1,j+1])-(stock.p[i,j+1]))/(stock.p[i,j+1])
      }
    
    #covariance
    for (j in 1:input$nInputs)
      for(i in 1:input$nInputs)
      {
        covariance[i,j] <- cov(stock.r[,i],stock.r[,j])
      }
    
    #average return
    for (i in 1:input$nInputs)
    {
      average.r[i,] <- mean(stock.r[,i])
    }
    
    #replace with user inputs
    for (i in 1:input$nInputs)
    {
      if (as.numeric(stock.er[i]) != 999)
      {
        average.r[i,] <- stock.er[i]
      }
    }
    
    
    mu <- average.r*252;
    Q <- covariance*252*252;
    A2 <- t(as.matrix(rep(1, input$nInputs)));
    b2 <- 1;
    Z2 <- rep(0, dim(A)[1]);
    M <- rbind(cbind(Q, t(A2)), cbind(A2, Z2));
    Mi <- solve(M)
    tau2 <- seq(0, 1, by=0.01);
    N <- length(tau2);
    rtn <- rep(NA, N);
    rsk <- rep(NA, N);
    for (k in 1:N)
    {
      f <- c(tau2[k]*mu, b2)
      h <- (Mi %*% f)[1:input$nInputs]
      rtn[k] <- t(mu) %*% h;
      rsk[k] <- sqrt(t(h) %*% Q %*% h)
    }
    plot(rsk,rtn,"l",col = "red")
  })
  
  
  nPlotB <- eventReactive(input$PlotButton, {
    
    result <- data.frame("stock"=nNames(),"shares"=round(vCalculation()))
    bar <- barplot(vCalculation(),names.arg=nNames())
    
    output$table <- renderTable({
      result
    })
    output$ExpR <- renderText({
      paste("The Portfolio Return is ", PortfolioReturn()*100,"%")
    })
    
    output$lowB <- renderPlot({
      input$PlotButton
      isolate(
        pct <- round(LongOnly()/sum(LongOnly())*100))
        lbls <- paste(nNames(), pct)
        lbls <- paste(lbls,"%",sep="")
        pie(LongOnly(),radius= 1,labels = lbls, col=rainbow(length(lbls)),
          main="Long Only")
    })
    output$UpB <- renderPlot({
      input$PlotButton
      isolate(
        pct <- round(abs(ShortOnly())/sum(abs(ShortOnly()))*100))
      lbls <- paste(nNames(), pct)
      lbls <- paste(lbls,"%",sep="")
      pie(abs(ShortOnly()),radius= 1,labels = lbls, col=rainbow(length(lbls)),
          main="Short Only")
      
    })
    
    
    
  })
  
  
  output$plot <- renderPlot({
    nPlotB()
    
  })
})
  
  
