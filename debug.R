# tickers <- TTR::stockSymbols()
# 
# tickers <- tickers %>%
#   select(Symbol, Name, Exchange, ETF)

tickers <- read_csv("/Users/max/Desktop/nasdaq_screener_1648912388464.csv")
tickers <- tickers %>% 
  select(Symbol, Name, Country, Industry, `IPO Year`)


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

Rb <- "^GSPC" %>%
  tq_get(get  = "stock.prices",
         from = lubridate::today()- lubridate::period(1, "year"),
         to   = lubridate::today()) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               col_rename = "Rb")


RaRb <-left_join(Ra, Rb, by= c("date"= "date"))

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


# Portfolio Growth ----
stock_returns_monthly %>%
  tq_portfolio(assets_col   = symbol, 
               returns_col  = Ra, 
               weights      = wts, 
               col_rename   = "investment.growth",
               wealth.index = TRUE) %>%
  mutate(investment.growth = investment.growth*10000)%>%
  ggplot(aes(x = date, y = investment.growth)) +
  geom_line(size = 1, color = palette_light()[[1]]) +
  labs(title = "Portfolio Growth",
       x = "", y = "Portfolio Value") +
  geom_smooth(method = "loess") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

# Portfolio Returns ----
wts <- as.tibble(df_portfolio) %>% select()

stock_returns_monthly %>%
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

# Stock returns facet plot----
stock_returns_monthly %>% 
  ggplot(aes(x= date, y= Ra))+
  facet_wrap(~ symbol)+
  geom_bar(stat= "identity", fill= palette_light()[[1]])+
  labs(title = "Portfolio Returns",
       x = "", y = "Monthly Returns") +
  geom_smooth(method = "lm") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::percent)

# Stock returns to baseline


df_portfolio_ext <- left_join(df_portfolio, tickers, by= c("ticker"= "Symbol"))

tickers

# Industry treemap ----
df_portfolio_ext %>%
  group_by(Industry) %>% 
  summarise(amount_industry= sum(amount)) %>% 
  mutate(pct_industry= amount_industry/sum(amount_industry)) %>% 
  ggplot(aes(area= pct_industry, fill= Industry, label= Industry))+
  geom_treemap()+
  geom_treemap_text()+
  scale_fill_brewer(palette = "Set3")
  

df_portfolio_ext %>%
  group_by(Country) %>% 
  summarise(amount_country= sum(amount)) %>% 
  mutate(pct_country= amount_country/sum(amount_country)) %>% 
  ggplot(aes(area= pct_country, fill= Country, label= Country))+
  geom_treemap()+
  geom_treemap_text()+
  scale_fill_brewer(palette = "Set3")
