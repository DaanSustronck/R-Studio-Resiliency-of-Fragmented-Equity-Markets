################################################################################
# SETUP
################################################################################

library(data.table)
library(ggplot2)
library(fixest)

################################################################################
# FIRST STAGE - GLOBAL
################################################################################

df$wavg_min_liq <- df$wavg_min_liq_glo
df$liq <- df$liq_glo

# ---- LIT ----
ggplot(df, aes(x = wavg_min_LIT, y = LIT)) +
  geom_point(alpha = 0.4) + 
  geom_smooth(aes(group = stock), method = "lm", color = "#116E8A", se = FALSE) +  
  geom_smooth(aes(group = 1), method = "lm", color = "red", size = 1.2, se = FALSE, linetype = "dashed") +  
  theme_minimal()

model <- feols(LIT ~ wavg_min_LIT + wavg_min_ALGO + wavg_min_DARK_OTC + wavg_min_OTC + wavg_min_liq +
                 log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model)  
fitted_results <- fitted(model)
df$fit_LIT <- fitted_results

# ---- ALGO ----
ggplot(df, aes(x = wavg_min_liq_glo, y = ALGO)) +
  geom_point(alpha = 0.4) + 
  geom_smooth(aes(group = stock), method = "lm", color = "#116E8A", se = FALSE) +  
  geom_smooth(aes(group = 1), method = "lm", color = "red", size = 1.2, se = FALSE, linetype = "dashed") +  
  theme_minimal()

model <- feols(ALGO ~ wavg_min_LIT + wavg_min_ALGO + wavg_min_DARK_OTC + wavg_min_OTC + wavg_min_liq +
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

model <- feols(DARK_OTC ~ wavg_min_LIT + wavg_min_ALGO + wavg_min_DARK_OTC + wavg_min_OTC + wavg_min_liq +
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

model <- feols(OTC ~ wavg_min_LIT + wavg_min_ALGO + wavg_min_DARK_OTC + wavg_min_OTC + wavg_min_liq +
                 log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model)
fitted_results <- fitted(model)
df$fit_OTC <- fitted_results

################################################################################
# SECOND STAGE - GLOBAL 
################################################################################

model_glo <- feols(liq ~ fit_LIT + fit_ALGO + fit_DARK_OTC + fit_OTC +
                     wavg_min_liq + log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model_glo)

################################################################################
# FIRST STAGE - BEST-MARKET
################################################################################

df$wavg_min_liq <- df$wavg_min_liq_bm
df$liq <- df$liq_bm

# ---- LIT ----
ggplot(df, aes(x = wavg_min_LIT, y = LIT)) +
  geom_point(alpha = 0.4) + 
  geom_smooth(aes(group = stock), method = "lm", color = "#116E8A", se = FALSE) +  
  geom_smooth(aes(group = 1), method = "lm", color = "red", size = 1.2, se = FALSE, linetype = "dashed") +  
  theme_minimal()

model <- feols(LIT ~ wavg_min_LIT + wavg_min_ALGO + wavg_min_DARK_OTC + wavg_min_OTC + wavg_min_liq +
                 log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model)  
fitted_results <- fitted(model)
df$fit_LIT <- fitted_results

# ---- ALGO ----
ggplot(df, aes(x = wavg_min_liq_bm, y = ALGO)) +
  geom_point(alpha = 0.4) + 
  geom_smooth(aes(group = stock), method = "lm", color = "#116E8A", se = FALSE) +  
  geom_smooth(aes(group = 1), method = "lm", color = "red", size = 1.2, se = FALSE, linetype = "dashed") +  
  theme_minimal()

model <- feols(ALGO ~ wavg_min_LIT + wavg_min_ALGO + wavg_min_DARK_OTC + wavg_min_OTC + wavg_min_liq +
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

model <- feols(DARK_OTC ~ wavg_min_LIT + wavg_min_ALGO + wavg_min_DARK_OTC + wavg_min_OTC + wavg_min_liq +
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

model <- feols(OTC ~ wavg_min_LIT + wavg_min_ALGO + wavg_min_DARK_OTC + wavg_min_OTC + wavg_min_liq +
                 log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model)
fitted_results <- fitted(model)
df$fit_OTC <- fitted_results

################################################################################
# SECOND STAGE - BEST-MARKET 
################################################################################

model_bm <- feols(liq ~ fit_LIT + fit_ALGO + fit_DARK_OTC + fit_OTC +
                    wavg_min_liq + log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model_bm)

################################################################################
# FIRST STAGE - LOCAL
################################################################################

df$wavg_min_liq <- df$wavg_min_liq_loc
df$liq <- df$liq_loc

# ---- LIT ----
ggplot(df, aes(x = wavg_min_LIT, y = LIT)) +
  geom_point(alpha = 0.4) + 
  geom_smooth(aes(group = stock), method = "lm", color = "#116E8A", se = FALSE) +  
  geom_smooth(aes(group = 1), method = "lm", color = "red", size = 1.2, se = FALSE, linetype = "dashed") +  
  theme_minimal()

model <- feols(LIT ~ wavg_min_LIT + wavg_min_ALGO + wavg_min_DARK_OTC + wavg_min_OTC + wavg_min_liq +
                 log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model)  
fitted_results <- fitted(model)
df$fit_LIT <- fitted_results

# ---- ALGO ----
ggplot(df, aes(x = wavg_min_liq_loc, y = ALGO)) +
  geom_point(alpha = 0.4) + 
  geom_smooth(aes(group = stock), method = "lm", color = "#116E8A", se = FALSE) +  
  geom_smooth(aes(group = 1), method = "lm", color = "red", size = 1.2, se = FALSE, linetype = "dashed") +  
  theme_minimal()

model <- feols(ALGO ~ wavg_min_LIT + wavg_min_ALGO + wavg_min_DARK_OTC + wavg_min_OTC + wavg_min_liq +
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

model <- feols(DARK_OTC ~ wavg_min_LIT + wavg_min_ALGO + wavg_min_DARK_OTC + wavg_min_OTC + wavg_min_liq +
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

model <- feols(OTC ~ wavg_min_LIT + wavg_min_ALGO + wavg_min_DARK_OTC + wavg_min_OTC + wavg_min_liq +
                 log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model)
fitted_results <- fitted(model)
df$fit_OTC <- fitted_results

################################################################################
# SECOND STAGE - LOCAL 
################################################################################

model_loc <- feols(liq ~ fit_LIT + fit_ALGO + fit_DARK_OTC + fit_OTC +
                     wavg_min_liq + log(volatility) + log(avg_price) + log(volume) + MTFbbo | stock, data = df)
summary(model_loc)

################################################################################
# ETABLES
################################################################################

etable(model_glo, model_bm, model_loc, tex = TRUE)

