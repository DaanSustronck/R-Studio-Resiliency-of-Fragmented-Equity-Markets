################################################################################
# SETUP
################################################################################

library(data.table)
library(ggplot2)
library(fixest)

################################################################################
# FIRST STAGE - GLOBAL   
################################################################################

df$wavg_min_res <- df$wavg_min_res_glo
df$res <- df$res_glo

# ---- LIT ----
ggplot(df, aes(x = wavg_min_LIT, y = LIT)) +
  geom_point(alpha = 0.4) +  
  geom_smooth(aes(group = stock), method = "lm", color = "#116E8A", se = FALSE) +  
  geom_smooth(aes(group = 1), method = "lm", color = "red", size = 1.2, se = FALSE, linetype = "dashed") +  
  theme_minimal()

model <- feols(LIT ~ wavg_min_LIT + wavg_min_ALGO + wavg_min_DARK_OTC + wavg_min_OTC + wavg_min_res +
                 log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model)  
fitted_results <- fitted(model)
df$fit_LIT <- fitted_results

# ---- ALGO ----
ggplot(df, aes(x = wavg_min_res_glo, y = ALGO)) +
  geom_point(alpha = 0.4) +  
  geom_smooth(aes(group = stock), method = "lm", color = "#116E8A", se = FALSE) +  
  geom_smooth(aes(group = 1), method = "lm", color = "red", size = 1.2, se = FALSE, linetype = "dashed") +  
  theme_minimal()

model <- feols(ALGO ~ wavg_min_LIT + wavg_min_ALGO + wavg_min_DARK_OTC + wavg_min_OTC + wavg_min_res +
                 log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model)  
fitted_results <- fitted(model)
df$fit_ALGO <- fitted_results

# ---- DARK_OTC ----
ggplot(df, aes(x = wavg_min_DARK_OTC, y = DARK_OTC)) +
  geom_point(alpha = 0.4) +  
  geom_smooth(aes(group = stock), method = "lm", color = "#116E8A", se = FALSE) +  
  geom_smooth(aes(group = 1), method = "lm", color = "red", size = 1.2, se = FALSE, linetype = "dashed") +  
  theme_minimal()

model <- feols(DARK_OTC ~ wavg_min_LIT + wavg_min_ALGO + wavg_min_DARK_OTC + wavg_min_OTC + wavg_min_res +
                 log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model)  
fitted_results <- fitted(model)
df$fit_DARK_OTC <- fitted_results

# ---- OTC ----
ggplot(df, aes(x = wavg_min_OTC, y = OTC)) +
  geom_point(alpha = 0.4) +  
  geom_smooth(aes(group = stock), method = "lm", color = "#116E8A", se = FALSE) +  
  geom_smooth(aes(group = 1), method = "lm", color = "red", size = 1.2, se = FALSE, linetype = "dashed") +
  theme_minimal()

model <- feols(OTC ~ wavg_min_LIT + wavg_min_ALGO + wavg_min_DARK_OTC + wavg_min_OTC + wavg_min_res +
                 log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model)
fitted_results <- fitted(model)
df$fit_OTC <- fitted_results

################################################################################
# SECOND STAGE - GLOBAL  
################################################################################

model_glo <- feols(res ~ fit_LIT + fit_LIT^2 + fit_ALGO + fit_DARK_OTC + fit_OTC +
                     wavg_min_res + log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model_glo)

model_large_glo <- feols(res ~ fit_LIT * log(euro_trade_size) + fit_LIT^2 * log(euro_trade_size) + fit_ALGO + fit_DARK_OTC + fit_OTC +
                           wavg_min_res + log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model_large_glo)

model_imbalance_glo <- feols(res ~ fit_LIT * imbalance + fit_LIT^2 * imbalance + fit_ALGO + fit_DARK_OTC + fit_OTC +
                               wavg_min_res + log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model_imbalance_glo)

################################################################################
# FIRST STAGE - BEST-MARKET   
################################################################################

df$wavg_min_res <- df$wavg_min_res_bm
df$res <- df$res_bm

# ---- LIT ----
ggplot(df, aes(x = wavg_min_LIT, y = LIT)) +
  geom_point(alpha = 0.4) +  
  geom_smooth(aes(group = stock), method = "lm", color = "#116E8A", se = FALSE) +  
  geom_smooth(aes(group = 1), method = "lm", color = "red", size = 1.2, se = FALSE, linetype = "dashed") +  
  theme_minimal()

model <- feols(LIT ~ wavg_min_LIT + wavg_min_ALGO + wavg_min_DARK_OTC + wavg_min_OTC + wavg_min_res +
                 log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model)  
fitted_results <- fitted(model)
df$fit_LIT <- fitted_results

# ---- ALGO ----
ggplot(df, aes(x = wavg_min_res_bm, y = ALGO)) +
  geom_point(alpha = 0.4) +  
  geom_smooth(aes(group = stock), method = "lm", color = "#116E8A", se = FALSE) +  
  geom_smooth(aes(group = 1), method = "lm", color = "red", size = 1.2, se = FALSE, linetype = "dashed") +  
  theme_minimal()

model <- feols(ALGO ~ wavg_min_LIT + wavg_min_ALGO + wavg_min_DARK_OTC + wavg_min_OTC + wavg_min_res +
                 log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model)  
fitted_results <- fitted(model)
df$fit_ALGO <- fitted_results

# ---- DARK_OTC ----
ggplot(df, aes(x = wavg_min_DARK_OTC, y = DARK_OTC)) +
  geom_point(alpha = 0.4) +  
  geom_smooth(aes(group = stock), method = "lm", color = "#116E8A", se = FALSE) +  
  geom_smooth(aes(group = 1), method = "lm", color = "red", size = 1.2, se = FALSE, linetype = "dashed") +  
  theme_minimal()

model <- feols(DARK_OTC ~ wavg_min_LIT + wavg_min_ALGO + wavg_min_DARK_OTC + wavg_min_OTC + wavg_min_res +
                 log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model)  
fitted_results <- fitted(model)
df$fit_DARK_OTC <- fitted_results

# ---- OTC ----
ggplot(df, aes(x = wavg_min_OTC, y = OTC)) +
  geom_point(alpha = 0.4) +  
  geom_smooth(aes(group = stock), method = "lm", color = "#116E8A", se = FALSE) +  
  geom_smooth(aes(group = 1), method = "lm", color = "red", size = 1.2, se = FALSE, linetype = "dashed") +
  theme_minimal()

model <- feols(OTC ~ wavg_min_LIT + wavg_min_ALGO + wavg_min_DARK_OTC + wavg_min_OTC + wavg_min_res +
                 log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model)
fitted_results <- fitted(model)
df$fit_OTC <- fitted_results

################################################################################
# SECOND STAGE - BEST-MARKET   
################################################################################

model_bm <- feols(res ~ fit_LIT + fit_LIT^2 + fit_ALGO + fit_DARK_OTC + fit_OTC +
                    wavg_min_res + log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model_bm)

model_large_bm <- feols(res ~ fit_LIT * log(euro_trade_size) + fit_LIT^2 * log(euro_trade_size) + fit_ALGO + fit_DARK_OTC + fit_OTC +
                          wavg_min_res + log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model_large_bm)

model_imbalance_bm <- feols(res ~ fit_LIT * imbalance + fit_LIT^2 * imbalance + fit_ALGO + fit_DARK_OTC + fit_OTC +
                              wavg_min_res + log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model_imbalance_bm)

################################################################################
# FIRST STAGE - LOCAL  
################################################################################

df$wavg_min_res <- df$wavg_min_res_loc
df$res <- df$res_loc

# ---- LIT ----
ggplot(df, aes(x = wavg_min_LIT, y = LIT)) +
  geom_point(alpha = 0.4) +  
  geom_smooth(aes(group = stock), method = "lm", color = "#116E8A", se = FALSE) +  
  geom_smooth(aes(group = 1), method = "lm", color = "red", size = 1.2, se = FALSE, linetype = "dashed") +  
  theme_minimal()

model <- feols(LIT ~ wavg_min_LIT + wavg_min_ALGO + wavg_min_DARK_OTC + wavg_min_OTC + wavg_min_res +
                 log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model)  
fitted_results <- fitted(model)
df$fit_LIT <- fitted_results

# ---- ALGO ----
ggplot(df, aes(x = wavg_min_res_loc, y = ALGO)) +
  geom_point(alpha = 0.4) +  
  geom_smooth(aes(group = stock), method = "lm", color = "#116E8A", se = FALSE) +  
  geom_smooth(aes(group = 1), method = "lm", color = "red", size = 1.2, se = FALSE, linetype = "dashed") +  
  theme_minimal()

model <- feols(ALGO ~ wavg_min_LIT + wavg_min_ALGO + wavg_min_DARK_OTC + wavg_min_OTC + wavg_min_res +
                 log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model)  
fitted_results <- fitted(model)
df$fit_ALGO <- fitted_results

# ---- DARK_OTC ----
ggplot(df, aes(x = wavg_min_DARK_OTC, y = DARK_OTC)) +
  geom_point(alpha = 0.4) +  
  geom_smooth(aes(group = stock), method = "lm", color = "#116E8A", se = FALSE) +  
  geom_smooth(aes(group = 1), method = "lm", color = "red", size = 1.2, se = FALSE, linetype = "dashed") +  
  theme_minimal()

model <- feols(DARK_OTC ~ wavg_min_LIT + wavg_min_ALGO + wavg_min_DARK_OTC + wavg_min_OTC + wavg_min_res +
                 log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model)  
fitted_results <- fitted(model)
df$fit_DARK_OTC <- fitted_results

# ---- OTC ----
ggplot(df, aes(x = wavg_min_OTC, y = OTC)) +
  geom_point(alpha = 0.4) +  
  geom_smooth(aes(group = stock), method = "lm", color = "#116E8A", se = FALSE) +  
  geom_smooth(aes(group = 1), method = "lm", color = "red", size = 1.2, se = FALSE, linetype = "dashed") +
  theme_minimal()

model <- feols(OTC ~ wavg_min_LIT + wavg_min_ALGO + wavg_min_DARK_OTC + wavg_min_OTC + wavg_min_res +
                 log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model)
fitted_results <- fitted(model)
df$fit_OTC <- fitted_results

################################################################################
# SECOND STAGE - LOCAL    
################################################################################

model_loc <- feols(res ~ fit_LIT + fit_LIT^2 + fit_ALGO + fit_DARK_OTC + fit_OTC +
                     wavg_min_res + log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model_loc)

model_large_loc <- feols(res ~ fit_LIT * log(euro_trade_size) + fit_LIT^2 * log(euro_trade_size) + fit_ALGO + fit_DARK_OTC + fit_OTC +
                           wavg_min_res + log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model_large_loc)

model_imbalance_loc <- feols(res ~ fit_LIT * imbalance + fit_LIT^2 * imbalance + fit_ALGO + fit_DARK_OTC + fit_OTC +
                               wavg_min_res + log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model_imbalance_loc)

################################################################################
# ETABLES    
################################################################################

etable(model_glo, model_bm, model_loc, tex = TRUE)
etable(model_large_glo, model_large_bm, model_large_loc, tex = TRUE)
etable(model_imbalance_glo, model_imbalance_bm, model_imbalance_loc, tex = TRUE)
