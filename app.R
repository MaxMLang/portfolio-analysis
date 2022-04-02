# app.R
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(tidyquant)
library(tidyverse)

# Define UI for miles per gallon app ----
ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Stock Report Creator"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    pickerInput("ticker", "Select your ticker symbol", 
                multiple = FALSE,
                options = list( `live-search` = TRUE),
                choices = tickers[["Symbol"]]),
    numericInput("stock_amount", "Select the number of stocks in your portfolio",
                 value = 1,
                 min = 1,
                 step = 1),
  actionButton("add_ticker", label= "Add ticker to portfolio"),
  actionButton("remove_ticker", label = "Remove ticker from portfolio"),
  dateInput("start_date", label = "Select the start date of the analysis",
              min = lubridate::today() - lubridate::period(10, "years"), 
              max= lubridate::today() - lubridate::period(6, "months"), 
              value= lubridate::today()-lubridate::period(1, "year")),
  pickerInput("baseline", label = "Select the baseline index your portfolio should be compared to",
              choices = c("SP500", "NASDAQ", "Dow Jones", "DAX")),
  actionButton("create", "Create Analysis")),
  
  # Main panel for displaying outputs ----
  mainPanel(
    
    tableOutput("df_portfolio"),

    tabsetPanel(type = "tabs",
                tabPanel("Single Stocks", # dataTableOutput("stock_returns_monthly"),
                                          dataTableOutput("rarb_single_stock"),
                                          dataTableOutput("capm_single_stock")),
                tabPanel("Portfolio", # dataTableOutput("portfolio_returns_monthly"),
                                      dataTableOutput("rarb_single_portfolio"), 
                                      dataTableOutput("capm_single_portfolio"))
    )
  
    
  )
)


server <- function(input, output) {
  df_portfolio <- data.frame(ticker= character(), amount= numeric())
  
  observeEvent(input$add_ticker,{
    df_portfolio <<- df_portfolio %>% select(ticker, amount)
    row_list <- list(ticker= character(), amount= numeric())
    row_list$ticker <- c(row_list$ticker, input$ticker)
    row_list$amount <- c(row_list$amount, input$stock_amount)
    df_portfolio <<- rbind.data.frame(df_portfolio, row_list)
    df_portfolio <<- aggregate(x = df_portfolio$amount, by=list(ticker=df_portfolio$ticker), FUN=sum) %>% 
      rename("amount"= "x")
    
    df_portfolio$pct_of_portfolio <<- df_portfolio$amount / sum(df_portfolio$amount)
    
    output$df_portfolio <- renderTable(df_portfolio)
  
    
    
  })
  
  observeEvent(input$remove_ticker, {
    df_portfolio <<- df_portfolio %>% filter(ticker!= input$ticker)
    output$df_portfolio <- renderTable(df_portfolio)
  })
  
  observeEvent(input$create, {
 
    stock_returns_monthly <- df_portfolio$ticker %>%
      tq_get(get  = "stock.prices",
             from = input$start_date) %>%
      group_by(symbol) %>%
      tq_transmute(select     = adjusted,
                   mutate_fun = periodReturn,
                   period     = "monthly",
                   col_rename = "Ra") %>% 
      arrange(desc(date))
    
    # output$stock_returns_monthly <- renderDataTable(stock_returns_monthly)
    

    # baseline prices
    if(input$baseline %in% "SP500"){
      baseline_returns_monthly <- "^GSPC" %>%
      tq_get(get  = "stock.prices",
             from = input$start_date) %>%
      tq_transmute(select     = adjusted,
                   mutate_fun = periodReturn,
                   period     = "monthly",
                   col_rename = "Rb")

    }else if(input$baseline %in% "NASDAQ"){

    baseline_returns_monthly <- "NDAQ" %>%
      tq_get(get  = "stock.prices",
             from = input$start_date)%>%
      tq_transmute(select     = adjusted,
                   mutate_fun = periodReturn,
                   period     = "monthly",
                   col_rename = "Rb")

    }else if (input$baseline %in% "Dow Jones"){
      baseline_returns_monthly <- "^DJI" %>%
        tq_get(get  = "stock.prices",
               from = input$start_date) %>%
        tq_transmute(select     = adjusted,
                     mutate_fun = periodReturn,
                     period     = "monthly",
                     col_rename = "Rb")

    }else if (input$baseline %in% "DAX"){
      baseline_returns_monthly <- "^GDAXI" %>%
        tq_get(get  = "stock.prices",
               from = input$start_date) %>%
        tq_transmute(select     = adjusted,
                     mutate_fun = periodReturn,
                     period     = "monthly",
                     col_rename = "Rb")
    }
    
    rarb_single_stock <-left_join(stock_returns_monthly, baseline_returns_monthly, by= c("date"= "date"))
    output$rarb_single_stock <- renderDataTable(rarb_single_stock)
    capm_single_stock <- rarb_single_stock %>%
    tq_performance(Ra = Ra,
                        Rb = Rb,
                        performance_fun = table.CAPM)
     
    output$capm_single_stock <- renderDataTable(capm_single_stock)
  
    
    portfolio_returns_monthly <- stock_returns_monthly %>%
      tq_portfolio(assets_col  = symbol,
                   returns_col = Ra,
                   weights     = as.tibble(df_portfolio %>% select(ticker, pct_of_portfolio)),
                   col_rename  = "Ra")
    # output$portfolio_returns_monthly <- renderDataTable(portfolio_returns_monthly)
    
    RaRb_single_portfolio <- left_join(portfolio_returns_monthly,
                                       baseline_returns_monthly,
                                       by = "date")
    
    output$rarb_single_portfolio <- renderDataTable(RaRb_single_portfolio)
    
    capm_single_portfolio <- RaRb_single_portfolio %>%
      tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)
    
    output$capm_single_portfolio <- renderDataTable(capm_single_portfolio)
  
    
  })

}

shinyApp(ui, server)
