# app.R
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(tidyquant)
library(tidyverse)
library(lubridate)
library(shinythemes)
library(rsconnect)
library(treemapify)

tickers <- read_csv("ticker_data.csv") %>% 
  select(Symbol, Name, Country, Industry, `IPO Year`)

# Define UI for miles per gallon app ----
ui <- fluidPage(
  theme = shinytheme("superhero"), 
  # App title ----
  headerPanel("Portfolio Analysis"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    tabsetPanel(
      tabPanel("Manual Input",
    pickerInput("ticker", "Select your ticker symbol", 
                multiple = FALSE,
                options = list( `live-search` = TRUE),
                choices = tickers[["Symbol"]]),
    numericInput("stock_amount", "Select the number of stocks in your portfolio",
                 value = 1,
                 min = 1,
                 step = 1),
    tags$hr(),
    numericInput("invest_sum", "Insert the sum you invested in $",
                 value = 10000,
                 min = 1),
  actionButton("add_ticker", label= "Add ticker to portfolio"),
  actionButton("remove_ticker", label = "Remove ticker from portfolio"),
  tags$hr(),
  dateInput("start_date", label = "Select the start date of the analysis",
              min = lubridate::today() - lubridate::period(10, "years"), 
              max= lubridate::today() - lubridate::period(6, "months"), 
              value= lubridate::today()-lubridate::period(1, "year")),
  pickerInput("baseline", label = "Select the baseline index your portfolio should be compared to",
              choices = c("SP500", "NASDAQ", "Dow Jones", "DAX")),
  actionButton("create", "Create Analysis"),
  downloadButton("save_portfolio", "Save current portfolio")),
  tabPanel("Upload Portfolio",
           # Input: Select a file ----
           strong("Choose CSV File"), div(style="display: inline-block;vertical-align:top;", dropdownButton("I strongly recommend using a CSV file generated from this Shiny App. However, if you use another CSV file make sure that the column structures and at best even the headers are the same as this Shiny App.", status = 'success', icon = icon('info'), size= "xs")),
           tags$hr(),
           fileInput("file1", label= NULL,
                     multiple = FALSE,
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")),
           numericInput("invest_sum_file", "Insert the sum you invested in $",
                        value = 10000,
                        min = 1),
           dateInput("start_date_file", label = "Select the start date of the analysis",
                     min = lubridate::today() - lubridate::period(10, "years"), 
                     max= lubridate::today() - lubridate::period(6, "months"), 
                     value= lubridate::today()-lubridate::period(1, "year")),
           pickerInput("baseline_file", label = "Select the baseline index your portfolio should be compared to",
                       choices = c("SP500", "NASDAQ", "Dow Jones", "DAX")),
           
           # Horizontal line ----
           tags$hr(),
           
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
           actionButton("create_file", "Create Analysis")
           
          
            
  )
    )
       
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    
    h3("Portfolio overview"),
    tableOutput("df_portfolio"),

    tabsetPanel(type = "tabs",
                tabPanel("Overall",
                         h3("Industry Exposure"),
                         plotOutput("plot_industry_treemap"),
                         h3("Country Exposure"),
                         plotOutput("plot_country_treemap")),
                tabPanel("Portfolio Performance", # dataTableOutput("portfolio_returns_monthly"),
                         h3("Portfolio growth"),
                         plotOutput("plot_portfolio_growth"),
                         h3("Portfolio returns"),
                         plotOutput("plot_portfolio_returns"),
                         h3("CAPM model for your Portfolio"),
                         p(strong("Active Premium"),br(), "The investment's annualized return - benchmark's annualized return"),
                         p(strong("Alpha"), br(), "The number actually indicates the percentage above or below a benchmark index that the stock or fund price achieved.",
                           "Note, alpha is a historical number. It's useful to track a stock's alpha over time to see how it did, but it can't tell you how it will do tomorrow."),
                         p(strong("Beta", br(), "Beta is an indication of the volatility of a stock, a fund, or a stock portfolio in comparison with a benchmark index. The baseline number for beta is one, which indicates that the security's price moves exactly as the market moves. A beta of less than 1 means that the security is less volatile than the market, while a beta greater than 1 indicates that its price is more volatile than the market.")),
                         p(strong("Information Ratio"), br(), "The information ratio (IR) is a measurement of portfolio returns above the returns of a benchmark, usually an index such as the S&P 500, to the volatility of those returns.
The information ratio is used to evaluate the skill of a portfolio manager at generating returns in excess of a given benchmark."),
                         p(strong("R Squared"), br(), "R-squared measures how closely the performance of an asset can be attributed to the performance of a selected benchmark index.
R-squared is measured on a scale between 0 and 100; the higher the R-squared number, the more correlated the asset is to its benchmark."),
                         p(strong("Tracking Error"), br(), "Tracking error is the difference in actual performance between a position (usually an entire portfolio) and its corresponding benchmark.
The tracking error can be viewed as an indicator of how actively a fund is managed and its corresponding risk level.
Evaluating a past tracking error of a portfolio manager may provide insight into the level of benchmark risk control the manager may demonstrate in the future. "),
                         p(strong("Treynor Ratio"), br(), "The Treynor ratio is a risk/return measure that allows investors to adjust a portfolio's returns for systematic risk.
A higher Treynor ratio result means a portfolio is a more suitable investment.
The Treynor ratio is similar to the Sharpe ratio, although the Sharpe ratio uses a portfolio's standard deviation to adjust the portfolio returns."),
                         tableOutput("capm_single_portfolio"),
                         h3("Adjusted returns of portfolio and selected baseline index"),
                         dataTableOutput("rarb_single_portfolio")),
                tabPanel("Stock Performance", #dataTableOutput("stock_returns_monthly"),
                         h3("Single stock returns"),
                         h5("(Only displayed for a portfolio with less than 15 tickers)"),
                                          plotOutput("plot_stock_returns"),
                h3("Single Stock adjusted returns and selected baseline index"),
                dataTableOutput("rarb_single_stock"))
                
    ),
    code("Made by MaxMlang"),
    tags$a(href= "https://github.com/MaxMLang", icon("github", "fa-2x"))
  
    
  )
)


server <- function(input, output) {
  tickers <- read_csv("ticker_data.csv") %>% 
    select(Symbol, Name, Country, Industry, `IPO Year`)
  
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
    

    
    output$df_portfolio <- renderTable(df_portfolio %>% 
                                         rename("Ticker Symbol"= "ticker",
                                                "Amount" = "amount",
                                                "Percentage of Portfolio"= "pct_of_portfolio"))

    
  })
  
  observeEvent(input$remove_ticker, {
    df_portfolio <<- df_portfolio %>% filter(ticker!= input$ticker)
    output$df_portfolio <- renderTable(df_portfolio)
    
  })
  
  observeEvent(input$create, {
    if(nrow(df_portfolio)==0){
      showNotification("Your Portfolio is empty, add Stocks to perform an Analysis", type = "error")
    }else{
      showNotification("Your portfolio analysis is ongoing...", type= "message")
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
     
    
    
    wts <- as.tibble(df_portfolio %>% select(ticker, pct_of_portfolio))
    
    portfolio_returns_monthly <- stock_returns_monthly %>%
      tq_portfolio(assets_col  = symbol,
                   returns_col = Ra,
                   weights     = wts,
                   col_rename  = "Ra")
    # output$portfolio_returns_monthly <- renderDataTable(portfolio_returns_monthly)
    
    RaRb_single_portfolio <- left_join(portfolio_returns_monthly,
                                       baseline_returns_monthly,
                                       by = "date")
    
    output$rarb_single_portfolio <- renderDataTable(RaRb_single_portfolio)
    
    capm_single_portfolio <- RaRb_single_portfolio %>%
      tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)
    
    output$capm_single_portfolio <- renderTable(capm_single_portfolio)
    
    # Portfolio growth plot ----
    plot_portfolio_growth <-  stock_returns_monthly %>%
      tq_portfolio(assets_col   = symbol, 
                   returns_col  = Ra, 
                   weights      = wts, 
                   col_rename   = "investment.growth",
                   wealth.index = TRUE) %>%
      mutate(investment.growth = investment.growth*input$invest_sum)%>%
      ggplot(aes(x = date, y = investment.growth)) +
      geom_line(size = 1, color = palette_light()[[1]]) +
      labs(title = "Portfolio Growth",
           x = "", y = "Portfolio Value") +
      geom_smooth(method = "loess") +
      theme_tq() +
      scale_color_tq() +
      scale_y_continuous(labels = scales::dollar)
    
    output$plot_portfolio_growth <- renderPlot(plot_portfolio_growth)
    
    # Portfolio Returns ----
    
    plot_portfolio_returns <- stock_returns_monthly %>%
      tq_portfolio(assets_col  = symbol, 
                   returns_col = Ra, 
                   weights     = wts, 
                   col_rename  = "Ra") %>% 
      ggplot(aes(x = date, y= Ra)) +
      geom_bar(stat = "identity", fill = palette_light()[[1]])+
      labs(title = "Portfolio Returns",
           x = "", y = "Monthly Returns") +
      geom_smooth(method = "lm") +
      theme_tq() +
      scale_color_tq() +
      scale_y_continuous(labels = scales::percent)
    
    output$plot_portfolio_returns <- renderPlot(plot_portfolio_returns)
 
    # Stock returns facet plot----
    if(length(unique(df_portfolio$ticker)) <= 15){
    plot_stock_returns <- stock_returns_monthly %>% 
        ggplot(aes(x= date, y= Ra))+
        facet_wrap(~ symbol)+
        geom_bar(stat= "identity", fill= palette_light()[[1]])+
        labs(title = "Portfolio Returns",
             x = "", y = "Monthly Returns") +
        geom_smooth(method = "lm") +
        theme_tq() +
        scale_color_tq() +
        scale_y_continuous(labels = scales::percent)
    
    output$plot_stock_returns <- renderPlot(plot_stock_returns)
    
    
    }
    
    
    
   # Exposure Plot ----
    
    df_portfolio_ext <- left_join(df_portfolio, tickers, by= c("ticker"= "Symbol"))
    
    
    plot_industry_treemap <-  df_portfolio_ext %>%
      group_by(Industry) %>% 
      summarise(amount_industry= sum(amount)) %>% 
      mutate(pct_industry= amount_industry/sum(amount_industry)) %>% 
      ggplot(aes(area= pct_industry, fill= Industry, label= scales::percent(pct_industry)))+
      geom_treemap()+
      geom_treemap_text()+
      scale_fill_brewer(palette = "Set3")
    
    output$plot_industry_treemap <- renderPlot(plot_industry_treemap)
    
    
    plot_country_treemap <-  df_portfolio_ext %>%
      group_by(Country) %>% 
      summarise(amount_country= sum(amount)) %>% 
      mutate(pct_country= amount_country/sum(amount_country)) %>% 
      ggplot(aes(area= pct_country, fill= Country, label= scales::percent(pct_country)))+
      geom_treemap()+
      geom_treemap_text()+
      scale_fill_brewer(palette = "Set3")
    
    output$plot_country_treemap <- renderPlot(plot_country_treemap)
    
    
    showNotification("Done!", type= "message")
  
    }
  })
  
  observeEvent(input$create_file, {
      # Input file testing ----
      req(input$file1)
      tryCatch(
        {
          df_portfolio <<- read.csv(input$file1$datapath,
                         header = input$header,
                         sep = input$sep,
                         quote = input$quote)
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        })
    
    # Analysis start
    if(nrow(df_portfolio)==0){
      showNotification("Your Portfolio is empty, add Stocks to perform an Analysis", type = "error")
    }else{
      showNotification("Your portfolio analysis is ongoing...", type= "message")
      
      stock_returns_monthly <- df_portfolio$ticker %>%
        tq_get(get  = "stock.prices",
               from = input$start_date_file) %>%
        group_by(symbol) %>%
        tq_transmute(select     = adjusted,
                     mutate_fun = periodReturn,
                     period     = "monthly",
                     col_rename = "Ra") %>% 
        arrange(desc(date))
      
      # output$stock_returns_monthly <- renderDataTable(stock_returns_monthly)
      
      
      # baseline prices
      if(input$baseline_file %in% "SP500"){
        baseline_returns_monthly <- "^GSPC" %>%
          tq_get(get  = "stock.prices",
                 from = input$start_date_file) %>%
          tq_transmute(select     = adjusted,
                       mutate_fun = periodReturn,
                       period     = "monthly",
                       col_rename = "Rb")
        
      }else if(input$baseline_file %in% "NASDAQ"){
        
        baseline_returns_monthly <- "NDAQ" %>%
          tq_get(get  = "stock.prices",
                 from = input$start_date_file)%>%
          tq_transmute(select     = adjusted,
                       mutate_fun = periodReturn,
                       period     = "monthly",
                       col_rename = "Rb")
        
      }else if (input$baseline_file %in% "Dow Jones"){
        baseline_returns_monthly <- "^DJI" %>%
          tq_get(get  = "stock.prices",
                 from = input$start_date_file) %>%
          tq_transmute(select     = adjusted,
                       mutate_fun = periodReturn,
                       period     = "monthly",
                       col_rename = "Rb")
        
      }else if (input$baseline_file %in% "DAX"){
        baseline_returns_monthly <- "^GDAXI" %>%
          tq_get(get  = "stock.prices",
                 from = input$start_date_file) %>%
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
      
      
      
      wts <- as.tibble(df_portfolio %>% select(ticker, pct_of_portfolio))
      
      portfolio_returns_monthly <- stock_returns_monthly %>%
        tq_portfolio(assets_col  = symbol,
                     returns_col = Ra,
                     weights     = wts,
                     col_rename  = "Ra")
      # output$portfolio_returns_monthly <- renderDataTable(portfolio_returns_monthly)
      
      RaRb_single_portfolio <- left_join(portfolio_returns_monthly,
                                         baseline_returns_monthly,
                                         by = "date")
      
      output$rarb_single_portfolio <- renderDataTable(RaRb_single_portfolio)
      
      capm_single_portfolio <- RaRb_single_portfolio %>%
        tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)
      
      output$capm_single_portfolio <- tableOutput(capm_single_portfolio)
      
      # Portfolio growth plot
      plot_portfolio_growth <-  stock_returns_monthly %>%
        tq_portfolio(assets_col   = symbol, 
                     returns_col  = Ra, 
                     weights      = wts, 
                     col_rename   = "investment.growth",
                     wealth.index = TRUE) %>%
        mutate(investment.growth = investment.growth*input$invest_sum_file)%>%
        ggplot(aes(x = date, y = investment.growth)) +
        geom_line(size = 1, color = palette_light()[[1]]) +
        labs(title = "Portfolio Growth",
             x = "", y = "Portfolio Value") +
        geom_smooth(method = "loess") +
        theme_tq() +
        scale_color_tq() +
        scale_y_continuous(labels = scales::dollar)
      
      output$plot_portfolio_growth <- renderPlot(plot_portfolio_growth)
      
      # Portfolio Returns ----
      
      plot_portfolio_returns <- stock_returns_monthly %>%
        tq_portfolio(assets_col  = symbol, 
                     returns_col = Ra, 
                     weights     = wts, 
                     col_rename  = "Ra") %>% 
        ggplot(aes(x = date, y= Ra)) +
        geom_bar(stat = "identity", fill = palette_light()[[1]])+
        labs(title = "Portfolio Returns",
             x = "", y = "Monthly Returns") +
        geom_smooth(method = "lm") +
        theme_tq() +
        scale_color_tq() +
        scale_y_continuous(labels = scales::percent)
      
      output$plot_portfolio_returns <- renderPlot(plot_portfolio_returns)
      
      # Stock returns facet plot----
      if(length(unique(df_portfolio$ticker)) <= 15){
        plot_stock_returns <- stock_returns_monthly %>% 
          ggplot(aes(x= date, y= Ra))+
          facet_wrap(~ symbol)+
          geom_bar(stat= "identity", fill= palette_light()[[1]])+
          labs(title = "Portfolio Returns",
               x = "", y = "Monthly Returns") +
          geom_smooth(method = "lm") +
          theme_tq() +
          scale_color_tq() +
          scale_y_continuous(labels = scales::percent)
        
        output$plot_stock_returns <- renderPlot(plot_stock_returns)
      
      }
      
      
      
      # Exposure Plot ----
      
      df_portfolio_ext <- left_join(df_portfolio, tickers, by= c("ticker"= "Symbol"))
      
      
      plot_industry_treemap <-  df_portfolio_ext %>%
        group_by(Industry) %>% 
        summarise(amount_industry= sum(amount)) %>% 
        mutate(pct_industry= amount_industry/sum(amount_industry)) %>% 
        ggplot(aes(area= pct_industry, fill= Industry, label= scales::percent(pct_industry)))+
        geom_treemap()+
        geom_treemap_text()+
        scale_fill_brewer(palette = "Set3")
      
      output$plot_industry_treemap <- renderPlot(plot_industry_treemap)
      
      
      plot_country_treemap <-  df_portfolio_ext %>%
        group_by(Country) %>% 
        summarise(amount_country= sum(amount)) %>% 
        mutate(pct_country= amount_country/sum(amount_country)) %>% 
        ggplot(aes(area= pct_country, fill= Country, label= scales::percent(pct_country)))+
        geom_treemap()+
        geom_treemap_text()+
        scale_fill_brewer(palette = "Set3")
      
      output$plot_country_treemap <- renderPlot(plot_country_treemap)
      showNotification("Done!", type= "message")
    }
  })
    output$save_portfolio <- downloadHandler(
      filename = function() {
        paste("portfolio-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(df_portfolio, file, row.names = FALSE)
      }
    )
    
    
  
  

}

shinyApp(ui, server)
