tickers <- TTR::stockSymbols()

tickers <- tickers %>%
  select(Symbol, Name, Exchange, ETF)


df_portfolio <- data.frame(ticker= character(), amount= numeric())

row_list <- list(ticker= character(), amount= numeric())
row_list$ticker <- c("MSFT", "TSLA")
row_list$amount <- c(10, 4)
df_portfolio <- rbind.data.frame(df_portfolio, row_list) %>% 
  mutate(pct_of_portfolio= amount/sum(amount))

df_portfolio

# Single Stock Analysis-----
# return adjusted
Ra <- df_portfolio$ticker %>%
  tq_get(get  = "stock.prices",
         from = as.character(lubridate::today()- lubridate::period(1, "year")),
         to   = as.character(lubridate::today())) %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               col_rename = "Ra")

# baseline prices

Rb_sp <- "^GSPC" %>%
  tq_get(get  = "stock.prices",
         from = lubridate::today()- lubridate::period(1, "year"),
         to   = lubridate::today()) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               col_rename = "Rb")


RaRb <-left_join(Ra, Rb_sp, by= c("date"= "date"))

RaRb %>%
  tq_performance(Ra = Ra,
                 Rb = Rb,
                 performance_fun = table.CAPM)


# Portfolio Returns (Monthly) Analysis ----

tickers <- TTR::stockSymbols()

tickers <- tickers %>%
  select(Symbol, Name, Exchange, ETF)


df_portfolio <- data.frame(ticker= character(), amount= numeric())

row_list <- list(ticker= character(), amount= numeric())
row_list$ticker <- c("MSFT", "TSLA")
row_list$amount <- c(10, 4)
df_portfolio <- rbind.data.frame(df_portfolio, row_list)

df_portfolio
df_portfolio$pct_of_portfolio <- df_portfolio$amount / sum(df_portfolio$amount)
df_portfolio

# Start ----

stock_returns_monthly <- df_portfolio$ticker %>%
  tq_get(get  = "stock.prices",
         from = "2010-01-01",
         to   = "2015-12-31") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               col_rename = "Ra")
stock_returns_monthly


portfolio_returns_monthly <- stock_returns_monthly %>%
  tq_portfolio(assets_col  = symbol,
               returns_col = Ra,
               weights     = as.tibble(df_portfolio %>% select(ticker, pct_of_portfolio)),
               col_rename  = "Ra")
portfolio_returns_monthly

baseline_returns_monthly <- "^GDAXI" %>%
  tq_get(get  = "stock.prices",
         from = "2010-01-01",
         to   = "2015-12-31") %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               col_rename = "Rb")
baseline_returns_monthly

RaRb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   baseline_returns_monthly,
                                   by = "date")
RaRb_single_portfolio %>%
  tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)


