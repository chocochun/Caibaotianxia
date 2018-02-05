library("ggplot2")
library("stringr")
library(plyr)
library(knitr)
library(DT)
require(stringr)
require(rvest)
require(rjson)
library(shiny)
library(shinythemes)
library(googlesheets)
library(rmarkdown)
library(shinyjs)
library("sparkline")
library(XML)
library(RJSONIO)
library(RCurl)




### google token
googlesheets::gs_auth(token = "mjfinance_token.rds")
sheet_key <- "your key"
ss <- googlesheets::gs_key(sheet_key)

random_key <- "your key"
ss_random <- googlesheets::gs_key(random_key)


jscode <- "shinyjs.refresh = function() { history.go(0); }"

sector <- readRDS("alltranslate.rds")

for (i in 1:ncol(sector)){
  sector[,i] <- sector[,i] %>% as.character()
}

sector$Sectorall <- paste0(sector$Sector_chn," (",sector$Sector, ")")
sector$industryall <- paste0(sector$Industry_chn," (",sector$Industry, ")")

market_note <- function(x){
  
  result <- NULL
  
  if (x == "大陆") result <- ""
  if (x == "台湾") result <- "XTAI:"
  if (x == "美国") result <- ""
  if (x == "香港") result <- "XHKG:"
  
  return(result)
}


# function convert data to colname

trans_fina <- function(x){
  cbal <- as.character(x[,1])
  
  x <- as.data.frame(t(x[,-1]))
  #x <- x[-1,]
  
  colnames(x) <- cbal
  x
}


####### stock example


remove_comma <- function(k){
  as.numeric(sub(",", "", k, fixed = TRUE))
} 

ifnull <- function(x) ifelse(is.null(x), NA, x)

get_zichan <- function(x, therow, weight=1) {
  
  dat <- x[therow,] 
  
  cash_and_eq <- dat$`Cash and cash equivalents` %>% ifnull
  
  short_investment <- dat$`Short-term investments` %>% ifnull
  
  account_receivable <- dat$Receivables %>% ifnull
  
  current_asset <- dat$`Total current assets` %>% ifnull
  
  total_asset <- dat$`Total assets` %>% ifnull
  
  inventories <- dat$Inventories %>% ifnull
  
  account_payable <- dat$`Accounts payable` %>% ifnull
  
  current_liabilities <- dat$`Total current liabilities` %>% ifnull
  
  total_liabilities <- dat$`Total liabilities` %>% ifnull
  
  longterm_liabilities <- total_liabilities - current_liabilities %>% ifnull
  
  total_equity <- dat$`Total stockholders' equity` %>% ifnull
  
  outdata <- round(c(cash_and_eq,short_investment, account_receivable,inventories,current_asset,total_asset,
                     account_payable,current_liabilities,longterm_liabilities,total_equity,
                     total_liabilities+total_equity)/total_asset*100,digit=1)
  
  outdata[-c(5,10)] <- outdata[-c(5,10)]* weight
  outdata
}

get_cash <- function(x, therow) {
  
  dat <- x[therow,] 
  
  cash_operation <- dat$`Net cash provided by operating activities` %>% ifnull
  
  cash_invest <- dat$`Net cash used for investing activities` %>% ifnull
  
  cash_finance <- dat$`Net cash provided by (used for) financing activities` %>% ifnull
  
  cash_free <-  dat$`Free cash flow` %>% ifnull
  
  c(cash_operation,cash_invest,cash_finance ,cash_free)
}

factor2numeric <- function(x)  as.numeric(as.character(x))

get_finance <- function(keyratio, balance, therow) {
  
  dat <- keyratio[,c(1,therow+1)] 
  dat_orit<- dat
  dat[,2] <- as.numeric(gsub(",", "",dat[,2]))
  #dat[,2] <- as.numeric(as.character( dat[,2] ))
  zichan <- balance[therow,] 
  
  # deb_percent
  total_liabilities <- zichan$`Total liabilities`  %>% ifnull
  current_liabilities <- zichan$`Total current liabilities`  %>% ifnull
  total_asset <- zichan$`Total assets`  %>% ifnull
  
  deb_percent <- total_liabilities/total_asset * 100  %>% ifnull
  
  # 长期资金/固定资产
  total_equity <- zichan$`Total stockholders' equity`  %>% ifnull
  longterm_liabilities <- total_liabilities - current_liabilities  %>% ifnull
  net_property <- zichan$`Property and equipment, at cost`  %>% ifnull
  #longterm_percent <- (total_equity + longterm_liabilities)/net_property  %>% ifnull
  
  total_current_liabilities <- dat[81,2]
  fix_asset_turnover <- dat[100,2]
  asset_turnover <- dat[101,2]
  total_stockholders_equity <- dat[85,2]
  t_liabilities<- dat[84,2]
  longterm_percent <-  fix_asset_turnover / asset_turnover * 
    (total_stockholders_equity + t_liabilities - total_current_liabilities)  %>% ifnull
  
  
  # Fixed Assets Turnover / Asset Turnover x ( Total Stockholders' Equity + Total Liabilities - Total Current Liabilities )
  # (所有着权益合计+非流动负债合计)/(固定资产+在建工程+工程物资)
  
  # 流动比率
  #current_asset <- zichan$`Total current assets`
  #current_liabilities <- zichan$`Total current liabilities`
  #current_percent <- current_asset /current_liabilities*100
  
  current_percent <- (dat[88,2] %>% as.character() %>% as.numeric()) *100  %>% ifnull
  
  # 速动比率 = 流动资产-存货 - (预付费用) 
  ####### 需要寻找预付费用！！！！
  #inventories <- zichan$Inventories
  #speed_percent <- (current_asset - inventories) /current_liabilities*100
  speed_percent <- (dat[89,2] %>% as.character() %>% as.numeric()) *100  %>% ifnull
  
  # 应收款周转率
  receivable_turnover <- (dat[98,2] %>% as.character() %>% as.numeric())   %>% ifnull
  
  # 平均收现日数
  days_cash <- 360/receivable_turnover  %>% ifnull
  
  #cost_sale <- dat$`Cost of revenue`
  
  # 存货周转率
  inventory_turnover <-(dat[99,2] %>% as.character() %>% as.numeric())   %>% ifnull
  days_inventory <- 360/inventory_turnover  %>% ifnull
  
  
  # 固定资产周转率
  # revenue <- dat$Revenue
  property_turnover <- (dat[100,2] %>% as.character() %>% as.numeric())   %>% ifnull
  
  # 总资产周转率
  asset_turnover <- (dat[101,2] %>% as.character() %>% as.numeric())   %>% ifnull
  
  # 资产报酬率
  # 净利润/平均资产总额
  
  ROA <- (dat[31,2] %>% as.character() %>% as.numeric())   %>% ifnull
  
  # 权益报酬率
  ROE <- (dat[33,2] %>% as.character() %>% as.numeric())   %>% ifnull
  
  # 税前纯益占实收资本比率
  pretax_percent <- (dat[34,2] %>% as.character() %>% as.numeric())   %>% ifnull
  
  # 营业毛利率
  #gross_profit <- get_rownumber(x, 55, thecol)
  gross_percent <- (dat[2,2] %>% as.character() %>% as.numeric())  %>% ifnull
  
  # 营业利益率
  #operate_profit <- get_rownumber(x, 62, thecol)
  operate_percent <- (dat[4,2] %>% as.character() %>% as.numeric())   %>% ifnull
  
  # 净利率
  #net_profit <- get_rownumber(x, 75, thecol)
  net_percent <- (dat[29,2] %>% as.character() %>% as.numeric())   %>% ifnull
  
  # eps
  eps <- (dat[6,2] %>% as.character() %>% as.numeric())  %>% ifnull
  
  # 现金流量比率
  OCF <- as.numeric(gsub(",", "",dat[11,2])) %>% ifnull
  netincome <-  as.numeric(gsub(",", "",dat[5,2])) %>% ifnull
  cash_current_percent <- OCF/ (total_current_liabilities*  netincome/ ROA )  *100 %>% ifnull
  
  # 现金流量允当比率
  cash_current_yundang <- NA
  
  # 现金再投资比率
  shares_mil <- dat[9,2]
  cash_invest <-  (OCF - shares_mil) / ((netincome/ROA) * (1-total_current_liabilities/100) )
  
  c(deb_percent,longterm_percent,current_percent,speed_percent,receivable_turnover,
    days_cash,inventory_turnover, days_inventory, property_turnover,asset_turnover,
    ROA,ROE,pretax_percent,gross_percent,operate_percent,net_percent,eps,
    cash_current_percent,cash_current_yundang, cash_invest
  ) %>% round(.,1)
  
}


# function to select random stock

random_stock <- function(market){
  
  if (market == "美国"){
    
    stockfile <- read.csv("usstocklist.csv")
    select_stock <-   stockfile[sample(1:nrow(stockfile),1),]
    
  } else if (market ==  "台湾") {
    
    stockfile <- read.csv("taiwan.csv")
    # http://www.twse.com.tw/company/newlisting?response=csv&yy=&lang=en
    select_stock <-   stockfile[sample(1:nrow(stockfile),1),c(2,1)]
    
    
  } else if (market == "香港") {
    
    stockfile <- read.csv("hongkong.csv")
    select_stock <-   stockfile[sample(1:nrow(stockfile),1),]
    
  }
  
  colnames(select_stock) <- c("code","name")
  select_stock
}


# get history stock price year end

last <- function(x) {
  len <- length(x)
  x[[len]][1]
}

year_price <- function(stock, year){
  url <- paste0("http://globalquote.morningstar.com/globalcomponent/RealtimeHistoricalStockData.ashx?ticker=",stock,"&showVol=true&dtype=his&f=d&curry=USD&range=",year,"-12-25|",year,"-12-31&isD=true&isS=true&hasF=true&ProdCode=DIRECT")
  json_file = getURL(url)
  json_file2 = RJSONIO::fromJSON(json_file)
  
  last(json_file2$PriceDataList[[1]]$Datapoints)
  
}


