################################################################################
# SETUP
################################################################################

library(data.table)
library(ggplot2)
library(forcats)

rm(list = ls())
# setwd("C:/...")

################################################################################
# FUNCTIONS
################################################################################

calc_weighted_avg <- function(data, date_col, value_col, weight_col) {
  date_vals <- data[[date_col]]
  x_vals <- data[[value_col]]
  w_vals <- data[[weight_col]]
  
  n <- nrow(data)
  wavg <- numeric(n)
  wavg_min <- numeric(n)
  
  for (d in unique(date_vals)) {
    idx <- which(date_vals == d)
    x_day <- x_vals[idx]
    w_day <- w_vals[idx]
    
    wavg[idx] <- weighted.mean(x_day, w_day, na.rm = TRUE)
    
    for (j in seq_along(idx)) {
      x_minus <- x_day[-j]
      w_minus <- w_day[-j]
      
      if (sum(w_minus, na.rm = TRUE) > 0) {
        wavg_min[idx[j]] <- weighted.mean(x_minus, w_minus, na.rm = TRUE)
      } else {
        wavg_min[idx[j]] <- NA
      }
    }
  }
  
  col1 <- paste0("wavg_", value_col)
  col2 <- paste0("wavg_min_", value_col)
  
  data[[col1]] <- wavg
  data[[col2]] <- wavg_min
  
  return(data)
}

################################################################################
# TRADES
################################################################################

# # ---- trades setup ----
# trades = fread("TAS_2024_9.csv")
# trades = trades[, c(3:6,8)]
# setnames(trades,
#          c("EXCHTIME", "PRICE", "VOLUME", "MMT_CLASS", "RIC"),
#          c("exchange_time", "price", "size", "mmt", "ticker"))
# 
# # ---- remove trades with price == 0 ----
# trades <- trades[price != 0]
# 
# # ---- time setup ----
# trades$exchange_time = format(trades$exchange_time, usetz = TRUE, tz = "CET")
# trades$date <- as.Date(trades$exchange_time)
# trades <- trades[date != "2024-09-30"] # remove september 30th
# trades$clock <- format(as.POSIXct(trades$exchange_time), format = "%H:%M:%S")
# trades[, time := {
#   time_elements = strsplit(clock, split = ":")
#   time_elements = do.call(rbind, time_elements)
#   time = as.numeric(time_elements[, 1]) * 3600 + as.numeric(time_elements[, 2]) * 60
#   list(time)}]
# trades$clock = format(as.POSIXct(trades$time, tz = "UTC"), format = "%H:%M:%S")
# 
# # ---- sort variables ----
# trades <- trades[, .(ticker, date, clock, time, price, size, mmt)]
# 
# fwrite(trades, file = "time_cleaned_trades.csv")
trades <- fread("time_cleaned_trades.csv")

# ---- trades in trading-hours ----
open_time <- 9 * 3600
close_time <- 17.5 * 3600
intraday_auction_time <- 13 * 3600

trades <- trades[time >= (open_time + 300) & time < (close_time - 300) &
                   (time < (intraday_auction_time - 60) | time > (intraday_auction_time + 3 * 60))]

# ---- split ticker ----
trades$stock <- sub("\\..*", "", trades$ticker)
trades$venue <- sub(".*\\.", "", trades$ticker)
trades$stock <- as.factor(trades$stock)
trades$stock <- fct_recode(trades$stock,
                           "BASFn" = "BASd",
                           "IFXGn" = "IFXd",
                           "SIEGn" = "SIEd",
                           "EONGn" = "EOANd",
                           "DBKGn" = "DBKd",
                           "SATG_p" = "SRT3d",
                           "SAPG" = "SAPd",
                           "BAYGn" = "BAYNd",
                           "RWEG" = "RWEd",
                           "1COV" = "1COVd",
                           "HNKG_p" = "HEN3d",
                           "ZALG" = "ZALd",
                           "DTEGn" = "DTEd",
                           "MRCG" = "MRKd",
                           "PSHG_p" = "PAH3d",
                           "VOWG_p" = "VOW3d",
                           "BMWG" = "BMWd",
                           "BEIG" = "BEId",
                           "HEIG" = "HEId",
                           "RHMG" = "RHMd",
                           "MTXGn" = "MTXd",
                           "FREG" = "FREd",
                           "DHLn" = "DHLd",
                           "ALVG" = "ALVd",
                           "MUVGn" = "MUV2d",
                           "CONG" = "CONd",
                           "SHLG" = "SHLd",
                           "SY1G" = "SY1d",
                           "ADSGn" = "ADSd",
                           "HNRGn" = "HNR1d",
                           "DTGGe" = "DTGd",
                           "ENR1n" = "ENRd",
                           "MBGn" = "MBGd",
                           "BNRGn" = "BNRd",
                           "CBKG" = "CBKd",
                           "AIRG" = "AIRd",
                           "VNAn" = "VNAd",
                           "DB1Gn" = "DB1d",
                           "QIA" = "QIAd",
                           "P911_p" = "P911d")
trades <- trades[, .(ticker, stock, venue, date, clock, time, price, size, mmt)]

# ---- set class ----
trades$date <- as.Date(trades$date)
trades$stock <- as.factor(trades$stock)

# ---- different mmt classes ----
trades <- trades[substr(trades[, mmt], start = 1, stop = 2) == "12"|
                 substr(trades[, mmt], start = 1, stop = 2) == "82"]

table(substr(trades[, mmt], start = 1, stop = 2))
table(trades[, mmt])

# ---- trade size ----
trades[, trade_size := mean(size, na.rm = TRUE), by = .(stock, date)]
trades[, euro_trade_size := mean(size * price, na.rm = TRUE) / 1e3, by = .(stock, date)]

# ---- group trades of the same minute and mmt ----
trades <- trades[, .(price = sum(size * price) / sum(size), size = sum(size)), by = .(ticker, stock, venue, date, clock, time, mmt, trade_size, euro_trade_size)]

# ---- market share ----
trades[, Vol := sum(size * price)]
trades[, Vol_t := sum(size * price), by = .(date)]
trades[, Vol_i := sum(size * price), by = .(stock)]
trades[, Vol_i_t := sum(size * price), by = .(stock, date)]
trades[, MS_i_t := Vol_i_t / Vol_t]
trades[, MS_i := Vol_i / Vol]

MSR_i_t <- trades[, c("stock", "date", "Vol_i_t", "Vol_i", "Vol_t", "Vol", "MS_i_t", "MS_i")]
MSR_i_t <- unique(MSR_i_t)
fwrite(MSR_i_t, file = "Test/MSR_i_t.csv") 

MSR_i <- trades[, c("stock", "MS_i")]
MSR_i <- unique(MSR_i)
fwrite(MSR_i, file = "Test/MSR_i.csv")

# ---- LIT ----
test_LIT <- trades[, c("stock", "venue", "date", "MS_i_t", "price", "size")]
test_LIT[, LIT_venue_volume := sum(size * price , na.rm = TRUE), by = .(stock, date, venue)]
test_LIT[, LIT_volume := sum(size * price), by = .(stock, date)]
test_LIT <- unique(test_LIT[, c("stock", "date", "MS_i_t", "LIT_venue_volume", "LIT_volume")])
test_LIT[, market_share := LIT_venue_volume / LIT_volume]
test_LIT[, LIT := 1 - sum(market_share^2, na.rm = TRUE), by = .(stock, date)]
test_LIT <- unique(test_LIT[, c("stock", "date", "MS_i_t", "LIT")])

test_LIT[, lag_LIT := shift(LIT, n = 1, type = "lag"), by = stock]
test_LIT[, avg_LIT := mean(LIT, na.rm = TRUE), by = .(date)]
test_LIT[, avg_min_LIT := ((avg_LIT * length(unique(stock)) - LIT)) / (length(unique(stock)) - 1), by = date]
test_LIT <- calc_weighted_avg(data = test_LIT, date_col = "date", value_col = "LIT", weight_col = "MS_i_t")

test_LIT[, MS_i_t := NULL]
trades <- trades[test_LIT, on = c("stock", "date")]

# ---- ALGO ----
test_ALGO <- trades[, c("stock", "date", "MS_i_t", "price", "size", "mmt")]
test_ALGO[, ALGO := {
  algo_flags <- substr(mmt, 1, 2) == "12" & substr(mmt, 11, 11) == "H"
  total_flags <- substr(mmt, 1, 2) == "12"
  sum(algo_flags, na.rm = TRUE) / sum(total_flags, na.rm = TRUE)
}, by = .(stock, date)]
test_ALGO <- unique(test_ALGO[, c("stock", "date", "MS_i_t", "ALGO")])

test_ALGO[, lag_ALGO := shift(ALGO, n = 1, type = "lag"), by = stock]
test_ALGO[, avg_ALGO := mean(ALGO, na.rm = TRUE), by = date]
test_ALGO[, avg_min_ALGO := ((avg_ALGO * length(unique(stock)) - ALGO)) / (length(unique(stock)) - 1), by = date]  
test_ALGO <- calc_weighted_avg(data = test_ALGO, date_col = "date", value_col = "ALGO", weight_col = "MS_i_t")

test_ALGO[, MS_i_t := NULL]
trades <- trades[test_ALGO, on = c("stock", "date")]

# ---- DARK ----
test_DARK <- fread("Test/MarketShares_Dark.csv")
test_DARK[, date := as.Date(date)]
MSR_i_t[, date := as.Date(date)]
test_DARK <- test_DARK[MSR_i_t, on = c("stock", "date")]
test_DARK <- test_DARK[, .(stock, date, MS_i_t, DARK, PA, OTC, SI)]

test_DARK[, lag_DARK := shift(DARK, n = 1, type = "lag"), by = stock]
test_DARK[, avg_DARK := mean(DARK, na.rm = TRUE), by = date]
test_DARK[, avg_min_DARK := ((avg_DARK * length(unique(stock)) - DARK)) / (length(unique(stock)) - 1), by = date]
test_DARK <- calc_weighted_avg(data = test_DARK, date_col = "date", value_col = "DARK", weight_col = "MS_i_t")
test_DARK <- copy(test_DARK)

test_DARK[, lag_PA := shift(PA, n = 1, type = "lag"), by = stock]  
test_DARK[, avg_PA := mean(PA, na.rm = TRUE), by = date]
test_DARK[, avg_min_PA := ((avg_PA * length(unique(stock)) - PA)) / (length(unique(stock)) - 1), by = date]
test_DARK <- calc_weighted_avg(data = test_DARK, date_col = "date", value_col = "PA", weight_col = "MS_i_t")
test_DARK <- copy(test_DARK)

test_DARK[, lag_OTC := shift(OTC, n = 1, type = "lag"), by = stock]  
test_DARK[, avg_OTC := mean(OTC, na.rm = TRUE), by = date]
test_DARK[, avg_min_OTC := ((avg_OTC * length(unique(stock)) - OTC)) / (length(unique(stock)) - 1), by = date]
test_DARK <- calc_weighted_avg(data = test_DARK, date_col = "date", value_col = "OTC", weight_col = "MS_i_t")
test_DARK <- copy(test_DARK)

test_DARK[, lag_SI := shift(SI, n = 1, type = "lag"), by = stock] 
test_DARK[, avg_SI := mean(SI, na.rm = TRUE), by = date]
test_DARK[, avg_min_SI := ((avg_SI * length(unique(stock)) - SI)) / (length(unique(stock)) - 1), by = date]
test_DARK <- calc_weighted_avg(data = test_DARK, date_col = "date", value_col = "SI", weight_col = "MS_i_t")
test_DARK <- copy(test_DARK)

test_DARK[, DARK_OTC := DARK + PA + SI, by = stock]
test_DARK[, lag_DARK_OTC := shift(DARK_OTC, n = 1, type = "lag"), by = stock]
test_DARK[, avg_DARK_OTC := mean(DARK_OTC, na.rm = TRUE), by = date]
test_DARK[, avg_min_DARK_OTC := ((avg_DARK_OTC * length(unique(stock)) - DARK_OTC)) / (length(unique(stock)) - 1), by = date]
test_DARK <- calc_weighted_avg(data = test_DARK, date_col = "date", value_col = "DARK_OTC", weight_col = "MS_i_t")
test_DARK <- copy(test_DARK)

test_DARK <- test_DARK[, .(stock, date, 
                           DARK, lag_DARK, avg_DARK, avg_min_DARK, wavg_DARK, wavg_min_DARK, 
                           PA, lag_PA, avg_PA, avg_min_PA, wavg_PA, wavg_min_PA, 
                           OTC, lag_OTC, avg_OTC, avg_min_OTC, wavg_OTC, wavg_min_OTC, 
                           SI, lag_SI, avg_SI, avg_min_SI, wavg_SI, wavg_min_SI,
                           DARK_OTC, lag_DARK_OTC, avg_DARK_OTC, avg_min_DARK_OTC, wavg_DARK_OTC, wavg_min_DARK_OTC)]
trades <- trades[test_DARK, on = c("stock", "date")]

# ---- plot LIT, ALGO and DARK ----
table(trades$stock)
df_long <- melt(trades[stock == "1COV"],
                id.vars = "date",
                measure.vars = c("LIT", "LIT", "ALGO", "DARK", "PA", "OTC", "SI"),
                variable.name = "Metric",
                value.name = "Value")

ggplot(df_long, aes(x = date, y = Value, color = Metric)) +
  geom_line() +
  labs(title = "Fragmentation and Other Metrics Over Time",
       y = "Value",
       x = "Date",
       color = "Metric") +
  theme_minimal()

# ---- group trades of the same minute ----
trades[, `:=`(price = sum(size * price) / sum(size), size = sum(size)), by = .(stock, venue, date, clock, time)]
trades[, mmt := NULL]
trades <- unique(trades)

################################################################################
# QUOTES
################################################################################

# # ---- quotes setup ----
# quotes = fread("TAQ_2024_9.csv")
# quotes = quotes[, c(1, 16, 21, 26, 27)]
# setnames(quotes,
#          c("Timestamp", "BID", "ASK", "MID_PRICE", "RIC"),
#          c("date_time", "bid_price", "ask_price", "mid_price", "ticker"))
# 
# # ---- time setup ----
# quotes$date_time = format(quotes$date_time, usetz = TRUE, tz = "CET")
# quotes$date <- as.Date(quotes$date_time)
# quotes$clock <- format(as.POSIXct(quotes$date_time, tz = "CET"), format = "%H:%M:%S")
# quotes[, time := {
#   time_elements = strsplit(quotes$clock, split = ":")
#   time_elements = do.call(rbind, time_elements)
#   time = as.numeric(time_elements[, 1]) * 3600 + as.numeric(time_elements[, 2]) * 60
#   list(time)}]
# 
# # ---- sort variables ----
# quotes <- quotes[, .(ticker, date, clock, time, bid_price, ask_price, mid_price)]
# fwrite(quotes, file = "time_cleaned_quotes.csv")
quotes <- fread("time_cleaned_quotes.csv")

# ---- add missing minutes ----
time_seconds_seq <- seq(32400, 63000, by = 60)
all_seconds <- CJ(ticker = unique(quotes$ticker), date = unique(quotes$date), time = time_seconds_seq)
quotes <- merge(all_seconds, quotes, by = c("ticker", "date", "time"), all.x = TRUE)
quotes$clock <- format(as.POSIXct(quotes$time, tz = "UTC"), format = "%H:%M:%S")
rm(time_seconds_seq)
rm(all_seconds)

# ---- calculate mid_price ----
quotes$mid_price = round((quotes$ask_price + quotes$bid_price) / 2, 4)

# ---- NA's ----
quotes <- quotes[, .(ticker, date, clock, time, bid_price, ask_price, mid_price)]
table(is.na(quotes$ask_price),quotes$ticker)

# ---- forward-fill quoted prices ----
lob_variables <- c("bid_price", "ask_price", "mid_price")
quotes[, (lob_variables) := lapply(.SD, nafill, type = "locf"), .SDcols = (lob_variables), by = c("ticker", "date")]

# ---- remove quotes around auction times ----
open_time <- 9 * 3600
close_time <- 17.5 * 3600
intraday_auction_time <- 13 * 3600

quotes <- quotes[time >= (open_time + 300) & time < (close_time - 300)]
quotes[time > (intraday_auction_time - 60) & time < (intraday_auction_time + 3 * 60), (lob_variables)] <-  NA

# ---- calculate mid_price & spread ----
quotes$mid_price = round((quotes$ask_price + quotes$bid_price) / 2, 4)
quotes[, mid_price_next := shift(mid_price, n = -1, type = "lag"), by = .(date, ticker)]
quotes$spread_nom = (quotes$ask_price - quotes$bid_price)
quotes$spread_rel = (quotes$spread_nom / quotes$mid_price) * 1e4

# ---- NA's ----
table(is.na(quotes$ask_price),quotes$ticker)

# ---- split ticker ----
quotes$stock <- sub("\\..*", "", quotes$ticker)
quotes$venue <- sub(".*\\.", "", quotes$ticker)
quotes$stock <- as.factor(quotes$stock)
quotes$stock <- fct_recode(quotes$stock,
                            "BASFn" = "BASd",
                            "IFXGn" = "IFXd",
                            "SIEGn" = "SIEd",
                            "EONGn" = "EOANd",
                            "DBKGn" = "DBKd",
                            "SATG_p" = "SRT3d",
                            "SAPG" = "SAPd",
                            "BAYGn" = "BAYNd",
                            "RWEG" = "RWEd",
                            "1COV" = "1COVd",
                            "HNKG_p" = "HEN3d",
                            "ZALG" = "ZALd",
                            "DTEGn" = "DTEd",
                            "MRCG" = "MRKd",
                            "PSHG_p" = "PAH3d",
                            "VOWG_p" = "VOW3d",
                            "BMWG" = "BMWd",
                            "BEIG" = "BEId",
                            "HEIG" = "HEId",
                            "RHMG" = "RHMd",
                            "MTXGn" = "MTXd",
                            "FREG" = "FREd",
                            "DHLn" = "DHLd",
                            "ALVG" = "ALVd",
                            "MUVGn" = "MUV2d",
                            "CONG" = "CONd",
                            "SHLG" = "SHLd",
                            "SY1G" = "SY1d",
                            "ADSGn" = "ADSd",
                            "HNRGn" = "HNR1d",
                            "DTGGe" = "DTGd",
                            "ENR1n" = "ENRd",
                            "MBGn" = "MBGd",
                            "BNRGn" = "BNRd",
                            "CBKG" = "CBKd",
                            "AIRG" = "AIRd",
                            "VNAn" = "VNAd",
                            "DB1Gn" = "DB1d",
                            "QIA" = "QIAd",
                            "P911_p" = "P911d")
quotes <- quotes[, .(ticker, stock, venue, date, clock, time, bid_price, ask_price, mid_price, mid_price_next, spread_nom, spread_rel)]

# ---- venue data.frames ----
quotes_de <- quotes[venue == "DE", .SD, .SDcols = c("stock", "date", "clock", "time", "bid_price", "ask_price", "mid_price", "mid_price_next", "spread_nom", "spread_rel")]
quotes_tqe <- quotes[venue == "TQE", .SD, .SDcols = c("stock", "date", "clock", "time", "bid_price", "ask_price", "mid_price", "mid_price_next", "spread_nom", "spread_rel")]
quotes_aqe <- quotes[venue == "AQE", .SD, .SDcols = c("stock", "date", "clock", "time", "bid_price", "ask_price", "mid_price", "mid_price_next", "spread_nom", "spread_rel")]
quotes_dxe <- quotes[venue == "DXE", .SD, .SDcols = c("stock", "date", "clock", "time", "bid_price", "ask_price", "mid_price", "mid_price_next", "spread_nom", "spread_rel")]

quotes_ebbo1 <- merge(quotes_de, quotes_tqe, by = c("stock", "date", "clock", "time"), suffixes = c("_de", "_tqe"), all = TRUE, sort = TRUE)
quotes_ebbo2 <- merge(quotes_aqe, quotes_dxe, by = c("stock", "date", "clock", "time"), suffixes = c("_aqe", "_dxe"), all = TRUE, sort = TRUE)
quotes_ebbo <- merge(quotes_ebbo1, quotes_ebbo2, by = c("stock", "date", "clock", "time"), all = TRUE, sort = TRUE)

# ---- EBBO prices ----
quotes_ebbo[, best_bid_price := pmax(bid_price_de, bid_price_tqe, bid_price_aqe, bid_price_dxe, na.rm = TRUE)]
quotes_ebbo[, best_ask_price := pmin(ask_price_de, ask_price_tqe, ask_price_aqe, ask_price_dxe, na.rm = TRUE)]
quotes_ebbo[, midpoint := (best_bid_price + best_ask_price) / 2]
quotes_ebbo[, midpoint_next := shift(midpoint, n = -1, type = "lag"), by = .(date, stock)]

# ---- clean variables ----
rm(quotes_de, quotes_tqe, quotes_aqe, quotes_dxe, quotes_ebbo1, quotes_ebbo2)

# ---- problematic consolidated quotes ----
table(round(quotes_ebbo$best_ask_price - quotes_ebbo$best_bid_price, 3))
threshold <- 0.05

quotes_ebbo[, c("crossed", "locked", "large") := {
  quoted_spread = (best_ask_price - best_bid_price)
  list(quoted_spread < 0, quoted_spread == 0, quoted_spread / midpoint > threshold)}]

quotes_ebbo_filters  <- quotes_ebbo[, list(crossed = mean(crossed, na.rm = TRUE), locked = mean(locked, na.rm = TRUE), large = mean(large, na.rm = TRUE))]
quotes_ebbo_filters[, lapply(.SD * 100, round, digits = 2), .SDcols = c("crossed", "locked", "large")]

# ---- remove crossed consolidated quotes ----
quotes_ebbo <- quotes_ebbo[ask_price_de > bid_price_de]
quotes_ebbo <- quotes_ebbo[ask_price_aqe > bid_price_aqe]
quotes_ebbo <- quotes_ebbo[ask_price_tqe > bid_price_tqe]
quotes_ebbo <- quotes_ebbo[ask_price_dxe > bid_price_dxe]
quotes_ebbo <- quotes_ebbo[best_ask_price > best_bid_price]
quotes_ebbo <- quotes_ebbo[(ask_price_de - bid_price_de) / mid_price_de < threshold]
quotes_ebbo <- quotes_ebbo[(ask_price_aqe - bid_price_aqe) / mid_price_aqe < threshold]
quotes_ebbo <- quotes_ebbo[(ask_price_tqe - bid_price_tqe) / mid_price_tqe < threshold]
quotes_ebbo <- quotes_ebbo[(ask_price_dxe - bid_price_dxe) / mid_price_dxe < threshold]

# ---- problematic venue quotes ----
table(round(quotes_ebbo$ask_price_de - quotes_ebbo$bid_price_de, 3))

quotes_ebbo[, c("crossed", "locked", "large") := {
  quoted_spread = (ask_price_de - bid_price_de)
  list(quoted_spread < 0, quoted_spread == 0, quoted_spread / mid_price_de > threshold)}]

quotes_ebbo_filters  <- quotes_ebbo[, list(crossed = mean(crossed, na.rm = TRUE), locked = mean(locked, na.rm = TRUE), large = mean(large, na.rm = TRUE))]
quotes_ebbo_filters[, lapply(.SD * 100, round, digits = 2), .SDcols = c("crossed", "locked", "large")]

# ---- average midpoint  ----
quotes_ebbo[, avg_midpoint := mean(midpoint, na.rm = TRUE), by = .(stock, date)]

# ---- spreads ----
quotes_ebbo[, glo_rel_spread := ((best_ask_price - best_bid_price) / midpoint) * 1e4]
quotes_ebbo[, bm_rel_spread := pmin(spread_rel_de, spread_rel_tqe, spread_rel_aqe, spread_rel_dxe, na.rm=TRUE)]
quotes_ebbo[, loc_rel_spread := ((ask_price_de - bid_price_de) / mid_price_de) * 1e4]

# ---- MTFbbo ----
quotes_ebbo[, MTFbbo := mean(bm_rel_spread < loc_rel_spread, na.rm = TRUE), by = .(stock, date)]

# ---- set MS ----
trades_MS <- trades[, .(stock, date, MS_i_t)]
trades_MS <- unique(trades_MS)
quotes_ebbo <- quotes_ebbo[trades_MS, on = c("stock", "date")]

# ---- liq & resiliency calculations function ----
generate_liq_vars <- function(quotes_ebbo, spread_type) {
  
  spread_col <- switch(spread_type,
                       "glo" = "glo_rel_spread",
                       "loc" = "loc_rel_spread",
                       "bm" = "bm_rel_spread",
                       stop("Invalid spread type"))
  
  suffix <- paste0("_", spread_type)
  
  # ---- liq ----
  quotes_ebbo[, paste0("liq_i_t", suffix) := get(spread_col)]
  
  # ---- lag liq ----
  quotes_ebbo[, paste0("lag_liq", suffix) := shift(get(paste0("liq_i_t", suffix)), n = 1, type = "lag"), by = .(stock, date)]
  
  # ---- delta liq ----
  quotes_ebbo[, paste0("delta_liq", suffix) := get(paste0("liq_i_t", suffix)) - get(paste0("lag_liq", suffix)), by = .(stock, date)]
  
  # ---- lag deta liq ----
  for (j in 1:8) {
    quotes_ebbo[, paste0("lag_delta_liq_", j, suffix) := shift(get(paste0("delta_liq", suffix)), j, type = "lag"), by = .(stock, date)]
  }
  
  # ---- avg & avg min liq ----
  test_liq <- quotes_ebbo[, .(stock, date, liq_i_t = get(paste0("liq_i_t", suffix)), MS_i_t)]
  test_liq[, paste0("liq", suffix) := mean(liq_i_t, na.rm = TRUE), by = .(stock, date)]
  test_liq <- test_liq[!duplicated(test_liq, by = c("stock", "date", paste0("liq", suffix)))]
  test_liq[, paste0("avg_liq", suffix) := mean(get(paste0("liq", suffix)), na.rm = TRUE), by = .(date)]
  test_liq[, paste0("avg_min_liq", suffix) := (((get(paste0("avg_liq", suffix)) * length(unique(stock))) - get(paste0("liq", suffix))) / (length(unique(stock)) - 1)), by = date]
  test_liq <- calc_weighted_avg(data = test_liq, date_col = "date", value_col = paste0("liq", suffix), weight_col = "MS_i_t")
  test_liq[, liq_i_t := NULL]
  quotes_ebbo <- quotes_ebbo[test_liq, on = c("stock", "date")]
  
  # ---- resiliency regression model ----
  response_var <- paste0("delta_liq", suffix)
  predictor_vars <- c(paste0("liq_i_t", suffix), paste0("lag_delta_liq_", 1:8, suffix))
  formula <- reformulate(predictor_vars, response = response_var)
  
  resiliency_results <- quotes_ebbo[, {
    fit <- lm(formula, data = .SD)
    list(kappa = coef(fit)[paste0("liq_i_t", suffix)])
  }, by = .(stock, date)]
  
  # ---- merge ----
  quotes_ebbo <- merge(quotes_ebbo, resiliency_results, by = c("stock", "date"), all.x = TRUE)
  setnames(quotes_ebbo, "kappa", paste0("res", suffix))
  
  # ---- avg daily & min resiliency ----
  test_res <- quotes_ebbo[, .(stock, date, get(paste0("res", suffix)), MS_i_t)]
  test_res <- unique(test_res, by = c("stock", "date"))
  setnames(test_res, "V3", paste0("res", suffix))
  
  test_res[, paste0("avg_res", suffix) := mean(get(paste0("res", suffix)), na.rm = TRUE), by = .(date)]
  test_res[, paste0("avg_min_res", suffix) := ((get(paste0("avg_res", suffix)) * length(unique(stock)) - get(paste0("res", suffix)))) / (length(unique(stock)) - 1), by = date]
  test_res <- calc_weighted_avg(data = test_res, date_col = "date", value_col = paste0("res", suffix), weight_col = "MS_i_t")
  test_res <- test_res[, paste0("res", suffix) := NULL]
  quotes_ebbo <- quotes_ebbo[test_res, on = c("stock", "date")]
  
  return(quotes_ebbo)
}

# ---- execute functions ----
quotes_ebbo <- generate_liq_vars(quotes_ebbo, "glo")
quotes_ebbo <- generate_liq_vars(quotes_ebbo, "bm")
quotes_ebbo <- generate_liq_vars(quotes_ebbo, "loc")

################################################################################
# DAILY DATAFRAME
################################################################################

# ---- sort trades and quotes ----
setkeyv(trades, cols = c("stock", "date", "time"))
setkeyv(quotes_ebbo, cols = c("stock", "date", "time"))

# ---- match trades to quotes ----
df <- merge(quotes_ebbo, trades[, !c("time"), with = FALSE], by = c("stock", "date", "clock"), all = TRUE)

# ---- delete duped variables ----
setnames(df, "MS_i_t.y", "MS_i_t")
df[, MS_i_t.x := NULL]

# ---- flag trades that should be included ----
df[, include := !crossed & !locked & !large & !is.na(size) & size > 0 &
     !is.na(price) & price > 0 & !is.na(midpoint) & midpoint > 0]

# ---- report trade filtering stats ----
df_filters <- df[,
                 list(crossed = mean(crossed, na.rm = TRUE),
                      locked = mean(locked, na.rm = TRUE),
                      large = mean(large, na.rm = TRUE),
                      no_price = mean(is.na(price) | price == 0),
                      no_size = mean(is.na(size) | size == 0),
                      no_quotes = mean(is.na(midpoint) | midpoint <= 0),
                      included = mean(include)),
                 by = "ticker"]

# ---- volatility calculation ----
df[, volatility := (max(price, na.rm = TRUE) - min(price, na.rm = TRUE)) / avg_midpoint, by = .(stock, date)]

# ---- avg_price calculation ----
df[, avg_price := mean(midpoint, na.rm = TRUE), by = .(stock, date)]

# ---- volume calculation ----
df[, volume := sum(price * size, na.rm = TRUE) / 1e6, by = .(stock, date)]

# ---- forward-fill variables ----
numeric_cols <- names(df)[sapply(df, is.numeric)]
df[, (numeric_cols) := lapply(.SD, nafill, type = "nocb"), .SDcols = numeric_cols, by = c("stock", "date")]
df[, (numeric_cols) := lapply(.SD, nafill, type = "locf"), .SDcols = numeric_cols, by = c("stock", "date")]

# ---- direction of trade ----
df[, quote_diff :=
     fifelse(venue == "DE", sign(price - mid_price_de),
             fifelse(venue == "TQE", sign(price - mid_price_tqe),
                     fifelse(venue == "DXE", sign(price - mid_price_dxe),
                             fifelse(venue == "AQE", sign(price - mid_price_aqe), NA_real_))))]

price_tick <- data.table(stock = df$stock,
                         date = df$date,
                         time = df$time,
                         price_change = c(NA, sign(diff(df$price))))

price_tick <- price_tick[price_change != 0]

setkeyv(df, c("stock", "date", "time"))
setkeyv(price_tick, c("stock", "date", "time"))
df <- df[price_tick, roll = TRUE, mult = "last"]

df[, direction := {
  direction = quote_diff
  no_direction = is.na(direction) | direction == 0
  direction[no_direction] = price_change[no_direction]
  list(direction)}, by = "date"]

table(df$ticker, df$direction)

# ---- imbalance ----
df[, imbalance := abs(sum(price * size * direction)) / sum(price * size), by = .(stock, date)]

# ---- daily df ----
df <- df[, .(stock, date,  
                   
                   volatility, avg_price, volume, 
                   trade_size, MS_i_t, MTFbbo,
                   euro_trade_size, imbalance,
                   
                   liq_glo, avg_liq_glo, avg_min_liq_glo, wavg_liq_glo, wavg_min_liq_glo, 
                   liq_bm, avg_liq_bm, avg_min_liq_bm, wavg_liq_bm, wavg_min_liq_bm, 
                   liq_loc, avg_liq_loc, avg_min_liq_loc, wavg_liq_loc, wavg_min_liq_loc, 
                   
                   res_glo, avg_res_glo, avg_min_res_glo, wavg_res_glo, wavg_min_res_glo,
                   res_bm, avg_res_bm, avg_min_res_bm, wavg_res_bm, wavg_min_res_bm,
                   res_loc, avg_res_loc, avg_min_res_loc, wavg_res_loc, wavg_min_res_loc,
                   
                   LIT, lag_LIT, avg_LIT, avg_min_LIT, wavg_LIT, wavg_min_LIT,
                   ALGO, lag_ALGO, avg_ALGO, avg_min_ALGO, wavg_ALGO, wavg_min_ALGO,
                   
                   DARK, lag_DARK, avg_DARK, avg_min_DARK, wavg_DARK, wavg_min_DARK,
                   PA, lag_PA, avg_PA, avg_min_PA, wavg_PA, wavg_min_PA,  
                   OTC, lag_OTC, avg_OTC, avg_min_OTC, wavg_OTC, wavg_min_OTC, 
                   SI, lag_SI, avg_SI, avg_min_SI, wavg_SI, wavg_min_SI,
                   DARK_OTC, lag_DARK_OTC, avg_DARK_OTC, avg_min_DARK_OTC, wavg_DARK_OTC, wavg_min_DARK_OTC)]
df <- unique(df)
rm(list = setdiff(ls(), "df"))
