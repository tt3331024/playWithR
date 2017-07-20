library(shiny)

banks <- c("台灣銀行", "玉山銀行", "中國信託", "台新銀行", "富邦銀行")

shinyUI(fluidPage(
  title = 'Examples of DataTables',
#  fluidRow(
#    column(6, DT::dataTableOutput('USD'))
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        "input.dataset == TRUE",
        checkboxGroupInput('checkGroup', label = h3('choose the banks to compare:'),
          choices = banks, selected = banks)
      )
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel('美元(USD)', DT::dataTableOutput("USD")),
        tabPanel('日幣(JPY)', DT::dataTableOutput("JPY")),
        tabPanel('韓圜(KRW)', DT::dataTableOutput("KRW")),
        tabPanel('人民幣(CNY)', DT::dataTableOutput("CNY"))
      )
    )
  )
))
