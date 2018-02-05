

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tags$head(HTML(
    "<script>
    (function(i,s,o,g,r,a,m){
    i['GoogleAnalyticsObject']=r;i[r]=i[r]||
    function(){
    (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();
    a=s.createElement(o), m=s.getElementsByTagName(o)[0];
    a.async=1;
    a.src=g;m.parentNode.insertBefore(a,m)
    })
    (window, document, 'script',
    '//www.google-analytics.com/analytics.js','ga');
    
    ga('create', 'UA-110380825-1', 'auto');
    ga('send', 'pageview');
    
    </script>"
  )),
  
  tags$script(HTML(
    "$(document).one('shiny:idle', 
    function() {
    ga('set','userId', Shiny.user);
    }
  );"
  )),

  navbarPage(
    
  # Application title
  title = "财报天下",
  theme = shinytheme("united"),
  
  tabPanel(
    "个股财报",
    
    fluidRow(
      column(
        4,      
        selectInput("market", label = "请选择股票市场：",
                    choices = c("美国", "香港", "台湾")),
        uiOutput("xiazai")

      ),
      column(
        4,      
        
        textInput("stockname", label="请输入股票代码："),
        uiOutput("example")
        
      ),
      column(
        4,
        #uiOutput("yanzheng"),
        h6("在线翻译报表，点击查询后请耐心等待，请勿重复点击，谢谢！"),
        actionButton("searchstock", "查询", width = "100%")
        
        
      )
      
      ),
    
    hr(),
    
    uiOutput("stockinfo"),
    hr(),
    
    fluidRow(
      column(
        4,
        
        h4("资产负债比率"),
        #h6("表内数据均为占总资产%，应收账款、长期负债统计口径可能有所出入"),
        tableOutput("zichan"),
        hr(),
        h6("财报天下：www.CaiBaoTianXia.com"),
        h6("作者：周珉纯"),
      # h6("数据源来自：http://www.morningstar.com/"),
        h6("有任何建议，请联系："),
        h6("微信：chocochun"),
        h6("邮箱：chocochun@gmail.com")
        
      ), 
      column(
        8,
        h4("财务比率分析"),
        tableOutput("financial"),
        
        hr(),
        h4("现金流量状况"),
        #h6("单位：百万元"),
        uiOutput("unit"),
        tableOutput("cash") 
      )
    ),
    
    hr(),
    
    h6(paste0("更新日期：",Sys.Date(),"，更新内容请查看“更新日志”")),
    h6("大陆股票财报，请阅读“财报说”：www.caibaoshuo.com， 或微信小程序“每天读财报”")
      ),
  
  
  tabPanel(
    "随机财报",
    
    sidebarLayout(
      sidebarPanel(      
        
        selectInput("market_random", label = "请选择股票市场：",
                    choices = c("美国", "香港", "台湾")),
        actionButton("random_search", "随机抽取财报", width = "100%")
        
      ),
      mainPanel( 
        
        fluidRow(
          column(
            6,
            uiOutput("reveal_info"),
            hr(),
            uiOutput("reveal_stock1"),
            uiOutput("reveal_stock2")
            ), 
          column(
            6,
            
            uiOutput("rate1"),
            hr(),
            uiOutput("rate2"),
            hr(),
            uiOutput("rate3")
            
            

            
            )
        
      )
    )),
    
    hr(),
    fluidRow(
      column(
        4,
        
        h4("资产负债比率"),
        #h6("表内数据均为占总资产%，应收账款、长期负债统计口径可能有所出入"),
        tableOutput("zichan_random"),
        hr(),
        h6("财报天下：www.CaiBaoTianXia.com"),
        h6("作者：周珉纯"),
        # h6("数据源来自：http://www.morningstar.com/"),
        h6("有任何建议，请联系："),
        h6("微信：chocochun"),
        h6("邮箱：chocochun@gmail.com")
        
      ), 
      column(
        8,
        h4("财务比率分析"),
        tableOutput("financial_random"),
        
        hr(),
        h4("现金流量状况"),
        #h6("单位：百万元"),
        uiOutput("unit_random"),
        tableOutput("cash_random") 
      )
    )
    
    

    
  ),
  tabPanel("美股分类",
           
           titlePanel("美股分类"),
           
           sidebarLayout(
             sidebarPanel(        
               #selectInput("country", label = "请选择国家：",
               #                                choices = country$Country_chn),
               
               selectInput("sector", label = "请选择股票分类 (Sector)：",
                           choices = unique(sector$Sectorall)),
               uiOutput("industry"),
               
               actionButton("searchus", "查询", width = "100%")
               
             ),
             mainPanel( 
               #DT::dataTableOutput("test"),
               DT::dataTableOutput("info"),
               hr(),
               uiOutput("explain")
               
             )
           )
  ),
  

  tabPanel(
    "更新日志",
    h6("2017年12月25日：添加“随机财报”功能，可随机抽取财报，判断股票好坏"),
    hr(),
    h6("2017年12月20日：调整打印页面格式，财报可打印在一页上；显示每股盈余单位；添加股票名称及价格"),
    hr(),
    h6("2017年12月18日：添加股票货币，添加短期投资，修复无法显示数据bug"),
    hr(),
    h6("2017年12月2日：添加美股行业分类数据。"),
    hr(),
    h6("2017年12月1日：由于爬虫流量使用过多，增加反爬虫机制。"),
    #h6("爬虫可获取单独链接，请联系微信：chocochun"),
    #h6("使用爬虫可登录：https://minchunstock.shinyapps.io/financemj_morningstar/"),
    hr(),
    h6("2017年11月26日：增加财报下载选项"),
    hr(),
    h6("2017年11月25日：“美股财报软件”更名为财报天下，更新如下："),
    h6("1. 使用晨星morningstar数据，财报长度变为5年"),
    h6("2. 增加香港、台湾股票财报"),
    hr(),
    h6("2017年6月：完成美股财报软件，使用guru数据")
    )
  
  )
))
