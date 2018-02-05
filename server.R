


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  output$industry <- renderUI({
    
    industry <- subset(sector, Sectorall == input$sector)  

    selectInput("industry", label = "请选择股票行业 (Industry)：",
                choices = c("全部",industry$industryall))
    
    })
  
  stockindustry <- eventReactive(input$searchus, {
    
    output$explain <- renderUI({
      h6("感谢使用财报天下，更多数据添加中，敬请期待")
    })  
    
    subsector <- subset(sector, Sectorall == input$sector)
    
    sector_eng <- unique(subsector$Sector)
    
    url <- paste0("http://www.nasdaq.com/screening/companies-by-industry.aspx?industry=",sector_eng,"&render=download")
    dat <- read.csv(URLencode(url))
    
    subindustry <- subset(subsector, industryall == input$industry)  
    
    industry_eng <- unique(subindustry$Industry)
    
    if (input$industry == "全部"){
      
      dat
      
    } else {
      
      dat_industry <- subset(dat, Industry == industry_eng)
      dat_industry
      
    }
    
    
    
    
  })
  
  #output$test = DT::renderDataTable({
  #      data.frame(print(input$industry), print(input$sector))
  #    })
  
  
  output$info = DT::renderDataTable({
    mytable <- stockindustry()
    mytable$MarketCap <- round(mytable$MarketCap/1000000,2)
    
    colnames(mytable) <- c("股票代码", "股票名称","最新价格","市值(百万元)","ADR.TSO",
                           "上市年份","分类","行业","链接","X")
    mytable[,c("股票代码", "股票名称","最新价格","市值(百万元)")]
  },rownames= FALSE)
  

  
  output$report <- downloadHandler(
    filename = function() { 
      paste0('CaiBaoTianXia-',input$stockname,  '.html') 
    },
    content = function(file) {
      
      out = render('myreport.Rmd', clean = TRUE)
      file.rename(out, file) # move pdf to file for downloading
      
    },
    contentType = 'application/html'
    
  )
  
  
  output$example <- renderUI({
    
    if ( input$market == "台湾") {
      h5("请输入四位股票代码，例如大立光电，输入“3008” ")
    } else if (input$market == "美国") {
      h5("请输入股票代码，例如亚马逊，输入“AMZN”；阿里巴巴，输入“BABA”，
         好市多，输入“COST”")
      
    } else if (input$market == "香港"){
      h5("请输入五位股票代码，例如腾讯，“00700” ")
    }
    
    
  })
  
  
  
  observeEvent(input$random_search, {
  
    output$rate1 <- renderUI({
      
      actionButton("good", "好股票", width = "100%")
     
      })
    
    
    output$rate2 <- renderUI({
      
      actionButton("nutral", "一般股票", width = "100%")

    })
    
    
    output$rate3 <- renderUI({
      
      actionButton("bad", "差股票", width = "100%")
      
    })
    
    output$reveal_info <- renderUI({
      
      h5("根据财报判断股票，右侧判断股票好坏后显示股票信息，重新抽取财报请点击左侧“随机抽取财报”")
      
    })
    
    
    
    
    
    })
  
  
  observeEvent(input$searchstock, {
  
    
    output$historyprice <- renderTable({
        
        values$history_price
      }, rownames = TRUE)
    
    values$search_number <- values$search_number + 1
    
    
    output$stockinfo <- renderUI({
    
      fluidRow(
        column(
          4,      
          h5(paste0("公司名：", paste(values$stockinformation[1: (length(values$stockinformation)-3)], collapse = ' ')
)),
          h5(paste0("今日股价：", values$price ))

          
        ),
        column(
          8,   
          # h5(paste0("历年股价(12月31日截止)")),
          tableOutput("historyprice")
          
          
          
        )
        )
        
      
      
      })
    
  output$xiazai <- renderUI({
    downloadButton("report", "下载打印财报", width = "100%")
  })
  
  output$unit <- renderUI({
    
    #stockname <- paste0(market_note(input$market),as.character(input$stockname))
    
    #cash <- paste0("http://financials.morningstar.com/ajax/ReportProcess4CSV.html?t=",stockname,"&reportType=cf&period=12&dataType=A&order=asc&columnYear=5&number=3") %>%
    #  read.csv(skip=1)
    
    #stockunit <- colnames(cash)[1] %>% strsplit("[.]")
    

    h6(paste0("单位：百万元（", values$unit,"）"))
    
  })
  
  
  })
  
  values <- reactiveValues(
    search_number = 0, 
    random_number = 0,
    weight  = 1,
    unit = NA,
    stockinformation = NA,
    PETTM = NA,
    price = NA,
    eps = NA,
    random_company_name = NA,
    random_stock_name = NA,
    initial = 0,
    history_price =0
    
  )
  
  randomnumber <- sample(1:10,1)
  show_yanzheng <- sample(0:1,1)
  
  output$yanzheng <- renderUI({
    
    if (show_yanzheng == 1) {
        
        if   (randomnumber == 1) {
          question <- "验证问题：请输入2+1= "
        } else if (randomnumber == 2) {
          question <- "验证问题：请输入3+2= "
        } else if (randomnumber == 3) {
          question <- "验证问题：请输入4+3= "
        } else if (randomnumber == 4) {
          question <- "验证问题：请输入5+4= "
        } else if (randomnumber == 5) {
          question <- "验证问题：请输入6+5= "
        } else if (randomnumber == 6) {
          question <- "验证问题：请输入7+6= "
        } else if (randomnumber == 7) {
          question <- "验证问题：请输入8+7= "
        } else if (randomnumber == 8) {
          question <- "验证问题：请输入9+8= "
        } else if (randomnumber == 9) {
          question <- "验证问题：请输入10+9= "
        } else if (randomnumber == 10) {
          question <- "验证问题：请输入11+10= "
        }
      textInput("inputyanzheng", label= question )
    }
  })
  
  

  
  userid <- sample(1:100000,1)
  
  stockpage <- eventReactive(input$searchstock, {
    
    searchid <- sample(20:25,1)
    
    if (values$search_number > searchid) {
      #js$refresh()
      values$weight <- round(runif(1,0,1),2)
    }
    
    #if (show_yanzheng == 1) {
      
    #    solution <- 1 + 2*randomnumber
    #    inputsolu <- as.numeric(as.character(input$inputyanzheng))
        #print(solution)
        #print(inputsolu)
        
  #      if ( inputsolu != solution) {
     #     js$refresh()
    #    } 
      
      #}
    
    stockname <- paste0(market_note(input$market),as.character(input$stockname))
    
    inputdata <- data.frame(date = Sys.time(), symbol=stockname, 
                            searchid = values$search_number, weight = values$weight,
                            user = userid, download=0)
    ss <- gs_add_row(ss,  ws = "stock", input = inputdata)
    
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "正在读取财务报表", value = 0)
  
    
    # Create a Progress object
    #progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    #on.exit(progress$close())
    
    #progress$set(message = "正在生成资产负债比率表", value = 0)
    
    values$stockinformation <- read.table(paste0("http://financials.morningstar.com/ajax/ReportProcess4CSV.html?t=",stockname,"&reportType=bs&period=12&dataType=A&order=asc&columnYear=5&number=3"),
                                   nrows=1)
    
    values$stockinformation <- apply(values$stockinformation, 2, as.character)
    
    
    tempprice <- readLines(paste0("http://quotes.morningstar.com/stockq/c-header?&t=",stockname))
    
    values$price <-tempprice[12] %>%  gsub(" ", "", ., fixed = TRUE) %>% gsub(",", "", .) %>% as.numeric

    values$PETTM <- readHTMLTable(paste0("http://financials.morningstar.com/valuate/current-valuation-list.action?&t=",stockname))

    balance <- try(paste0("http://financials.morningstar.com/ajax/ReportProcess4CSV.html?t=",stockname,"&reportType=bs&period=12&dataType=A&order=asc&columnYear=5&number=3") %>% 
      read.csv(skip=1) %>% trans_fina)
    
    counttest <- 0
    
    while ((class(balance) == "try-error") &&  (counttest < 10)) {
      balance <- try(paste0("http://financials.morningstar.com/ajax/ReportProcess4CSV.html?t=",stockname,"&reportType=bs&period=12&dataType=A&order=asc&columnYear=5&number=3") %>% 
                       read.csv(skip=1) %>% trans_fina)
      #print("retry")
      
      counttest <- counttest+1 
      #Sys.sleep(0.2)
    }
    
    #balance <- try(trans_fina(balance))
    
    
    
    # Create a Progress object
    #progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    #on.exit(progress$close())
    
    #progress$set(message = "正在生成现金流量状况表", value = 0)
    
    
    # xianjin
    cash <- try(paste0("http://financials.morningstar.com/ajax/ReportProcess4CSV.html?t=",stockname,"&reportType=cf&period=12&dataType=A&order=asc&columnYear=5&number=3") %>%
      read.csv(skip=1))
    
    
    counttest <- 0
    while ( (class(cash) == "try-error") &&  (counttest < 10) ) {
      cash <- try(paste0("http://financials.morningstar.com/ajax/ReportProcess4CSV.html?t=",stockname,"&reportType=cf&period=12&dataType=A&order=asc&columnYear=5&number=3") %>%
                    read.csv(skip=1))
     # print("retry")
      counttest <- counttest+1 
      #Sys.sleep(0.2)
      
      }
    
    tempunit <- colnames(cash)[1] %>% strsplit("[.]")

    values$unit <- tempunit[[1]][7]
    
    
    cash <- trans_fina(cash)
    
    
    # Create a Progress object
    #progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    #on.exit(progress$close())
    
    #progress$set(message = "正在生成财务比率分析表", value = 0)
    
    
    keyratio <- try( paste0("http://financials.morningstar.com/ajax/exportKR2CSV.html?t=", stockname) %>% 
      read.csv(skip = 2))
    
    counttest <- 0
    while ((class(balance) == "try-error") &&  (counttest < 10)) {
      keyratio <- try( paste0("http://financials.morningstar.com/ajax/exportKR2CSV.html?t=", stockname) %>% 
                         read.csv(skip = 2))
      #print("retry")
      
      counttest <- counttest+1 
      #Sys.sleep(0.2)
      }
    
    keyratio <- keyratio[, c(1, 7:11) ]
    
    finance <- list(balance, cash, keyratio)
    
    
    history_price <-  matrix( "-" , 1 , 5)
    colnames(history_price) <- c( 2012:2016)
    
    rownames(history_price)<- "历年股价(12月31日截止)"
    
    for (i in 1:5){
      price <- year_price(stockname, 2011+i)
      history_price[1,i] <- ifelse(is.null(price), "-", price)
    }
    
    values$history_price <- history_price
    
    finance
    
  })
  

  output$zichan <- renderTable({
    # zichan
    
    #allcol <- getcol(stockpage())
    
    #allcol_name <- getcolname(stockpage()) %>% substr(4,5) %>% paste0("20",.)
    
    balance <- stockpage()[[1]]
    
    if (class(balance) == "try-error") {
      
      zichan_table <- "读取数据错误，请重试"
      
    } else {
      
      zichan_table <- matrix( "-" ,11 ,nrow(balance))
      
      rownames(zichan_table) <- c("现金与约当现金","短期投资","应收账款", "存货", 
                                  "流动资产", "总资产",  "应付账款", 
                                  "流动负债", "长期负债", "股东权益", "总负债+股东权益")
      
      colnames(zichan_table) <- substr(rownames(balance), 2,5)
      
      
      for (i in 1:nrow(balance)){
        zichan_table[,i] <- get_zichan(balance,i ,values$weight) %>% round(.,1)
      }
      
      zichan_table
    }
    
  }, rownames = TRUE)
  
  output$cash <- renderTable({
    
    
    cash <- stockpage()[[2]]
    
    
    if (class(cash) == "try-error") {
      
      cash_table <- "读取数据错误，请重试"
      
    } else {
      
      cash_table <- matrix( "-" ,4 , nrow(cash)-1)
      
      colnames(cash_table) <- substr(rownames(cash)[1:(nrow(cash)-1)], 2,5)
      
      rownames(cash_table) <- c("营业活动现金流量(来自损益表)",
                                "投资活动现金流量(来自资产负债表左边)",
                                "理财活动现金流量(来自资产负债表右边)",
                                "自由现金流量")
      
      
      for (i in 1: (nrow(cash)-1) ){
        cash_table[,i] <- get_cash(cash, i  ) * values$weight %>% round(.,1)
      }
      
      cash_table    
      
      }
    
    
  }, rownames = TRUE)
  

  output$financial <- renderTable({
    
    
    #stockname <- paste0(market_note(input$market),as.character(input$stockname))
    # lirun 
    #income <- paste0("http://financials.morningstar.com/ajax/ReportProcess4CSV.html?t=",stockname,"&reportType=is&period=12&dataType=A&order=asc&columnYear=5&number=3") %>% 
    # read.csv(skip=1)
    
    #income <- trans_fina(income)
    
    balance <- stockpage()[[1]]
    
    keyratio <- stockpage()[[3]]
    
    
    if ( (class(balance) == "try-error") | (class(keyratio) == "try-error")  ) {
      
      fin_table <- "读取数据错误，请重试"
      
    } else {
      
      fin_table <- matrix( "-" ,20 ,ncol(keyratio))
      
      colnames(fin_table) <- c("财务比率",substr(colnames(keyratio)[2:(ncol(keyratio))], 2,5) )
      
      rownames(fin_table)<- c("财务结构", "","偿债能力","",
                              "经营能力","","","","","",
                              "获利能力","","","","","","",
                              "现金流量","","")
      
      fin_table[,1] <- c("负债占资产比率(%) = 资产负债率",
                         "长期资金占不动产/厂房及设备比率(%) = 长期资产适合率",
                         "流动比率(%)","速动比率(%)",
                         "应收款项周转率(次)","平均收现日数",
                         "存货周转率(次)","平均销货日数(平均在库天数)",
                         "不动产/厂房及设备周转率(次) = 固定资产周转率",
                         "总资产周转率(次)",
                         "资产报酬率(%) RoA = 总资产收益率",
                         "权益报酬率(%) RoE = 净资产收益率",
                         "税前纯益占实收资本比率(%)","营业毛利率(%)",
                         "营业利益率(%)","纯益率(%) = 净利率",
                         paste0( "每股盈余(元) = 每股收益（货币：", values$unit,"）"),
                         "现金流量比率(%)","现金流量允当比率(%)","现金再投资比率(%)")
      
      
      #balance <- stockpage()
      
      for (i in 1: (ncol(keyratio) - 1) ){
        fin_table[,i+1] <- get_finance(keyratio, balance ,i ) * values$weight %>% round(.,1)
      }
      
      values$eps <- as.numeric(fin_table[17,ncol(keyratio)])
      
      fin_table
      
    }
        
  }, rownames = TRUE)
  
  
  
  
  ###### for random stock
  
  observeEvent(input$random_search, {
    # change the value "data_next" when you press "All"
    values$random_number <- values$random_number + 1
    values$initial <- 0
    
    
    selected <- random_stock(input$market_random)
    
    stockname <- paste0(market_note(input$market_random),as.character(selected$code))
    
    balance <- try(paste0("http://financials.morningstar.com/ajax/ReportProcess4CSV.html?t=",stockname,"&reportType=bs&period=12&dataType=A&order=asc&columnYear=5&number=3") %>% 
                     read.csv(skip=1) %>% trans_fina)
    
    counttest <- 0
    
    while ((class(balance) == "try-error") &&  (counttest < 10)) {
      
      selected <- random_stock(input$market_random)
      
      stockname <- paste0(market_note(input$market_random),as.character(selected$code))
      
      balance <- try(paste0("http://financials.morningstar.com/ajax/ReportProcess4CSV.html?t=",stockname,"&reportType=bs&period=12&dataType=A&order=asc&columnYear=5&number=3") %>% 
                       read.csv(skip=1) %>% trans_fina)
      #print("retry")
      
      counttest <- counttest+1 
      Sys.sleep(0.2)
    }
    
    
    values$random_stock_name <- as.character(selected$code)
    values$random_company_name <- as.character(selected$name)
    
  })  
  
  
  observeEvent(input$random_search, {
    # change the value "data_next" when you press "All"
    values$random_number <- values$random_number + 1
    values$initial <- 0
    
    
    selected <- random_stock(input$market_random)
    
    stockname <- paste0(market_note(input$market_random),as.character(selected$code))
    
    balance <- try(paste0("http://financials.morningstar.com/ajax/ReportProcess4CSV.html?t=",stockname,"&reportType=bs&period=12&dataType=A&order=asc&columnYear=5&number=3") %>% 
                     read.csv(skip=1) %>% trans_fina)
    
    counttest <- 0
    
    while ((class(balance) == "try-error") &&  (counttest < 10)) {
      
      selected <- random_stock(input$market_random)
      
      stockname <- paste0(market_note(input$market_random),as.character(selected$code))
      
      balance <- try(paste0("http://financials.morningstar.com/ajax/ReportProcess4CSV.html?t=",stockname,"&reportType=bs&period=12&dataType=A&order=asc&columnYear=5&number=3") %>% 
                       read.csv(skip=1) %>% trans_fina)
      #print("retry")
      
      counttest <- counttest+1 
      Sys.sleep(0.2)
    }
    
    
    values$random_stock_name <- as.character(selected$code)
    values$random_company_name <- as.character(selected$name)
    
  })  
  
  
  output$reveal_stock1 <- renderUI({
    if (values$initial > 0) h5(paste0("该股票代码为：",values$random_stock_name))
  })  
  
  output$reveal_stock2 <- renderUI({
    if (values$initial > 0) h5(paste0("该公司名称为：",values$random_company_name))
  })  

  observeEvent(input$good, {

    values$initial <- values$initial + 1
    
    inputdata <- data.frame(date = Sys.time(), 
                            symbol=paste0(market_note(input$market_random),values$random_stock_name),
                            searchid = values$random_number,
                            user = userid, rate = "good")
    
    ss_random <- gs_add_row(ss_random,  ws = "stock", input = inputdata)
    
    
    
      })  
  
  observeEvent(input$nutral, {
    values$initial <- values$initial + 1
    
    inputdata <- data.frame(date = Sys.time(), 
                            symbol=paste0(market_note(input$market_random),values$random_stock_name),
                            searchid = values$random_number,
                            user = userid, rate = "neutral")
    ss_random <- gs_add_row(ss_random,  ws = "stock", input = inputdata)

    
  })  
  
  
  observeEvent(input$bad, {
    values$initial <- values$initial + 1
    
    inputdata <- data.frame(date = Sys.time(),
                            symbol=paste0(market_note(input$market_random),values$random_stock_name),
                            searchid = values$random_number,
                            user = userid, rate = "bad")
    ss_random <- gs_add_row(ss_random,  ws = "stock", input = inputdata)
    
  })  
  
  
  ### random stock
  randomstock <- eventReactive(input$random_search, {
    

    stockname <- paste0(market_note(input$market_random),as.character(values$random_stock_name))
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "正在读取财务报表", value = 0)
    
    # Create a Progress object
    #progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    #on.exit(progress$close())
    
    #progress$set(message = "正在生成资产负债比率表", value = 0)
    
    values$stockinformation <- read.table(paste0("http://financials.morningstar.com/ajax/ReportProcess4CSV.html?t=",stockname,"&reportType=bs&period=12&dataType=A&order=asc&columnYear=5&number=3"),
                                          nrows=1)
    
    tempprice <- readLines(paste0("http://quotes.morningstar.com/stockq/c-header?&t=",stockname))
    
    values$price <-tempprice[12] %>%  gsub(" ", "", ., fixed = TRUE) %>% gsub(",", "", .) %>% as.numeric
    
    values$PETTM <- readHTMLTable(paste0("http://financials.morningstar.com/valuate/current-valuation-list.action?&t=",stockname))
    
    balance <- try(paste0("http://financials.morningstar.com/ajax/ReportProcess4CSV.html?t=",stockname,"&reportType=bs&period=12&dataType=A&order=asc&columnYear=5&number=3") %>% 
                     read.csv(skip=1) %>% trans_fina)
    
    counttest <- 0
    
    while ((class(balance) == "try-error") &&  (counttest < 10)) {
      balance <- try(paste0("http://financials.morningstar.com/ajax/ReportProcess4CSV.html?t=",stockname,"&reportType=bs&period=12&dataType=A&order=asc&columnYear=5&number=3") %>% 
                       read.csv(skip=1) %>% trans_fina)
      #print("retry")
      
      counttest <- counttest+1 
      Sys.sleep(0.2)
    }
    
    #balance <- try(trans_fina(balance))
    
    
    
    # Create a Progress object
    #progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    #on.exit(progress$close())
    
    #progress$set(message = "正在生成现金流量状况表", value = 0)
    
    
    # xianjin
    cash <- try(paste0("http://financials.morningstar.com/ajax/ReportProcess4CSV.html?t=",stockname,"&reportType=cf&period=12&dataType=A&order=asc&columnYear=5&number=3") %>%
                  read.csv(skip=1))
    
    
    counttest <- 0
    while ( (class(cash) == "try-error") &&  (counttest < 10) ) {
      cash <- try(paste0("http://financials.morningstar.com/ajax/ReportProcess4CSV.html?t=",stockname,"&reportType=cf&period=12&dataType=A&order=asc&columnYear=5&number=3") %>%
                    read.csv(skip=1))
      # print("retry")
      counttest <- counttest+1 
      Sys.sleep(0.2)
      
    }
    
    tempunit <- colnames(cash)[1] %>% strsplit("[.]")
    
    values$unit <- tempunit[[1]][7]
    
    
    cash <- trans_fina(cash)
    
    
    # Create a Progress object
    #progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    #on.exit(progress$close())
    
    #progress$set(message = "正在生成财务比率分析表", value = 0)
    
    
    keyratio <- try( paste0("http://financials.morningstar.com/ajax/exportKR2CSV.html?t=", stockname) %>% 
                       read.csv(skip = 2))
    
    counttest <- 0
    while ((class(balance) == "try-error") &&  (counttest < 10)) {
      keyratio <- try( paste0("http://financials.morningstar.com/ajax/exportKR2CSV.html?t=", stockname) %>% 
                         read.csv(skip = 2))
      #print("retry")
      
      counttest <- counttest+1 
      Sys.sleep(0.2)
    }
    
    keyratio <- keyratio[, c(1, 7:11) ]
    
    finance <- list(balance, cash, keyratio)
    finance
    
  })
  
  
  
  output$zichan_random <- renderTable({
    # zichan
    
    #allcol <- getcol(stockpage())
    
    #allcol_name <- getcolname(stockpage()) %>% substr(4,5) %>% paste0("20",.)
    
    balance <- randomstock()[[1]]
    
    if (class(balance) == "try-error") {
      
      zichan_table <- "读取数据错误，请重试"
      
    } else {
      
      zichan_table <- matrix( "-" ,11 ,nrow(balance))
      
      rownames(zichan_table) <- c("现金与约当现金","短期投资","应收账款", "存货", 
                                  "流动资产", "总资产",  "应付账款", 
                                  "流动负债", "长期负债", "股东权益", "总负债+股东权益")
      
      colnames(zichan_table) <- substr(rownames(balance), 2,5)
      
      
      for (i in 1:nrow(balance)){
        zichan_table[,i] <- get_zichan(balance,i ,values$weight) %>% round(.,1)
      }
      
      zichan_table
    }
    
  }, rownames = TRUE)
  
  output$cash_random <- renderTable({
    
    
    cash <- randomstock()[[2]]
    
    
    if (class(cash) == "try-error") {
      
      cash_table <- "读取数据错误，请重试"
      
    } else {
      
      cash_table <- matrix( "-" ,4 , nrow(cash)-1)
      
      colnames(cash_table) <- substr(rownames(cash)[1:(nrow(cash)-1)], 2,5)
      
      rownames(cash_table) <- c("营业活动现金流量(来自损益表)",
                                "投资活动现金流量(来自资产负债表左边)",
                                "理财活动现金流量(来自资产负债表右边)",
                                "自由现金流量")
      
      
      for (i in 1: (nrow(cash)-1) ){
        cash_table[,i] <- get_cash(cash, i  ) * values$weight %>% round(.,1)
      }
      
      cash_table    
      
    }
    
    
  }, rownames = TRUE)
  
  
  output$financial_random <- renderTable({
    
    
    #stockname <- paste0(market_note(input$market),as.character(input$stockname))
    # lirun 
    #income <- paste0("http://financials.morningstar.com/ajax/ReportProcess4CSV.html?t=",stockname,"&reportType=is&period=12&dataType=A&order=asc&columnYear=5&number=3") %>% 
    # read.csv(skip=1)
    
    #income <- trans_fina(income)
    
    balance <- randomstock()[[1]]
    
    keyratio <- randomstock()[[3]]
    
    
    if ( (class(balance) == "try-error") | (class(keyratio) == "try-error")  ) {
      
      fin_table <- "读取数据错误，请重试"
      
    } else {
      
      fin_table <- matrix( "-" ,20 ,ncol(keyratio))
      
      colnames(fin_table) <- c("财务比率",substr(colnames(keyratio)[2:(ncol(keyratio))], 2,5) )
      
      rownames(fin_table)<- c("财务结构", "","偿债能力","",
                              "经营能力","","","","","",
                              "获利能力","","","","","","",
                              "现金流量","","")
      
      fin_table[,1] <- c("负债占资产比率(%) = 资产负债率",
                         "长期资金占不动产/厂房及设备比率(%) = 长期资产适合率",
                         "流动比率(%)","速动比率(%)",
                         "应收款项周转率(次)","平均收现日数",
                         "存货周转率(次)","平均销货日数(平均在库天数)",
                         "不动产/厂房及设备周转率(次) = 固定资产周转率",
                         "总资产周转率(次)",
                         "资产报酬率(%) RoA = 总资产收益率",
                         "权益报酬率(%) RoE = 净资产收益率",
                         "税前纯益占实收资本比率(%)","营业毛利率(%)",
                         "营业利益率(%)","纯益率(%) = 净利率",
                         paste0( "每股盈余(元) = 每股收益（货币：", values$unit,"）"),
                         "现金流量比率(%)","现金流量允当比率(%)","现金再投资比率(%)")
      
      
      #balance <- stockpage()
      
      for (i in 1: (ncol(keyratio) - 1) ){
        fin_table[,i+1] <- get_finance(keyratio, balance ,i ) * values$weight %>% round(.,1)
      }
      
      values$eps <- as.numeric(fin_table[17,ncol(keyratio)])
      
      fin_table
      
    }
    
  }, rownames = TRUE)
  
  
  # extract data from Caibaoshuo
  
  output$test <- renderTable({
    dat <- readHTMLTable("http://caibaoshuo.com/companies/000001")
    kk <- dat$assets_ratio_table
    kk <- kk[,-1]
    
    rownames(kk) <- c("现金与约当现金","应收账款", "存货", 
                      "流动资产", "非流动资产","总资产",  "应付账款", 
                      "流动负债", "非流动负债", "股东权益", "总负债+股东权益")
    kk[-c(5,9),]
  }, rownames = TRUE)
  

  
})
