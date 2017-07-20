library(rvest)
library(dplyr)
library(shiny)
library(DT)

SRB <- '即期買入'
SRS <- '即期賣出'
CRB <- '現金買入'
CRS <- '現金賣出'

E.SUN_url <- read_html('https://www.esunbank.com.tw/bank/personal/deposit/rate/forex/foreign-exchange-rates')
CTBC_url <- read_html('https://www.ctbcbank.com/CTCBPortalWeb/appmanager/ebank/rb?_nfpb=true&_windowLabel=T20130328210000220011&_nffvid=%2FCTCBPortalWeb%2Fpages%2FexchangeRate%2FexchangeRate.faces&_pageLabel=TW_RB_CM_ebank_018001')
Taishin_url <- read_html('https://www.taishinbank.com.tw/TS/TS06/TS0605/TS060502/index.htm?urlPath1=TS02&urlPath2=TS0202')
Taiwan_url <- read_html('http://rate.bot.com.tw/xrt?Lang=zh-TW')
fubon_url <- read_html('https://ebank.taipeifubon.com.tw/B2C/cfhqu/cfhqu009/CFHQU009_Home.faces')
Cathay_url <- read_html('https://www.cathaybk.com.tw/cathaybk/exchange/currency-billboard.asp')


banks_ch <- c("台灣銀行", "玉山銀行", "中國信託", "台新銀行", "富邦銀行", "國泰世華")
currencies <- c('USD', 'JPY', 'KRW', 'CNY')


## 台灣銀行 ##
shinyServer(function(input, output) {
  
  Taiwan <- data.frame('kind' = c(CRB, CRS, SRB, SRS))
  for(c in currencies){
    .m <- Taiwan_url %>%
      html_nodes(paste0('.table-striped td:contains(', c, ')~td')) %>% 
      html_text()
    if(length(.m) == 0){
      .m <- c('--', '--', '--', '--')
    }
    .m <- .m[-c(5:length(.m))]
    .d <- matrix(.m, ncol = 1) %>% as.data.frame()
    Taiwan <- cbind(Taiwan, .d)
  }
  Taiwan <- Taiwan[order(Taiwan$kind),]
  
  E.SUN <- data.frame('kind' = c(SRB, SRS, CRB, CRS))
  for(c in currencies){
    .m <- E.SUN_url %>%
      html_nodes(paste0('.inteTable td:contains(', c, ')~td')) %>% 
      html_text()
    if(length(.m) == 0){
      .m <- c('--', '--', '--', '--')
    }
    .d <- matrix(.m, ncol = 1) %>% as.data.frame()
    E.SUN <- cbind(E.SUN, .d)
  }
  E.SUN <- E.SUN[order(E.SUN$kind),]
    
  ##  中國信託  ##
  CTBC <- data.frame('kind' = c(CRB, CRS, SRB, SRS))
  for(c in currencies){
    .m <- CTBC_url %>%
      html_nodes(paste0('.maintable td:contains(', c, ')~td')) %>% 
      html_text()
    if(length(.m) == 0){
      .m <- c('--', '--', '--', '--')
    }
    .d <- matrix(.m, ncol = 1) %>% as.data.frame()
    CTBC <- cbind(CTBC, .d)
  }
  CTBC <- CTBC[order(CTBC$kind),]
    
  ##  台新銀行  ##  
  Taishin <- data.frame('kind' = c(SRB, SRS, CRB, CRS))
  for(c in currencies){
    .m <- Taishin_url %>%
      html_nodes(paste0('.table01 td:contains(', c, ')~td')) %>% 
      html_text()
    if(length(.m) == 0){
      .m <- c('--', '--', '--', '--')
    }
    .d <- matrix(.m, ncol = 1) %>% as.data.frame()
    Taishin <- cbind(Taishin, .d)
  }
  Taishin <- Taishin[order(Taishin$kind),]
  
  ##  富邦銀行  ##  
  fubon <- data.frame('kind' = c(SRB, SRS, CRB, CRS))
  for(c in currencies){
    .m <- fubon_url %>%
      html_nodes(paste0('td:contains(', c, ')~td')) %>% 
      html_text()
    if(length(.m) == 0){
      .m <- c('--', '--', '--', '--')
    }
    .d <- matrix(.m, ncol = 1) %>% as.data.frame()
    fubon <- cbind(fubon, .d)
  }
  fubon <- fubon[order(fubon$kind),]
  
  ##  國泰世華  ## 
  Cathay <- data.frame('kind' = c(SRB, SRS, CRB, CRS))
  for(c in currencies){
    .m <- Cathay_url %>%
      html_nodes(paste0('.icon_currency_', tolower(c))) %>%
      xml_parent() %>% xml_parent()
    .m <- .m %>% html_nodes('.t_align_right') %>% html_text()
    if(length(.m) == 0){
      .m <- c('--', '--', '--', '--')
    }
    .d <- matrix(.m, ncol = 1) %>% as.data.frame()
    Cathay <- cbind(Cathay, .d)
  }
  Cathay <- Cathay[order(Cathay$kind),]
  
  
  banks_en <- list(Taiwan, E.SUN, CTBC, Taishin, fubon, Cathay)  
  
  USD <- sapply(banks_en, function(x) x[[2]]) %>% t() %>% as.data.frame(row.names = banks_ch) 
  JPY <- sapply(banks_en, function(x) x[[3]]) %>% t() %>% as.data.frame(row.names = banks_ch)
  KRW <- sapply(banks_en, function(x) x[[4]]) %>% t() %>% as.data.frame(row.names = banks_ch)
  CNY <- sapply(banks_en, function(x) x[[5]]) %>% t() %>% as.data.frame(row.names = banks_ch)
    
  
  names(USD) <- c(SRB, SRS, CRB, CRS)
  names(JPY) <- c(SRB, SRS, CRB, CRS)
  names(KRW) <- c(SRB, SRS, CRB, CRS)
  names(CNY) <- c(SRB, SRS, CRB, CRS)

  
  output$USD = DT::renderDataTable(USD[input$checkGroup, ])
  output$JPY = DT::renderDataTable(JPY[input$checkGroup, ])
  output$KRW = DT::renderDataTable(KRW[input$checkGroup, ])
  output$CNY = DT::renderDataTable(CNY[input$checkGroup, ])
})
