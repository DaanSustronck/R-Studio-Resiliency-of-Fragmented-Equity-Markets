################################################################################
# SETUP
################################################################################

library(data.table)
library(ggplot2)
library(fixest)
library(dplyr)
library(tidyr)
library(glue)

################################################################################
# SUMMARY
################################################################################
  
summary(df)

vars <- c("avg_price", "trade_size", "euro_trade_size", "volume",  
          "volatility", "imbalance",
          "LIT", "MTFbbo", "MS_i_t",
          "ALGO",
          "DARK", "PA", "OTC", "SI", 
          "liq_glo", "liq_bm", "liq_loc", 
          "res_glo", "res_bm", "res_loc")

summary_table <- df %>%
  select(all_of(vars)) %>%
  summarise(across(
    everything(),
    list(
      Mean = ~mean(.x, na.rm = TRUE),
      Stdev = ~sd(.x, na.rm = TRUE),
      `25th` = ~quantile(.x, 0.25, na.rm = TRUE),
      `50th` = ~quantile(.x, 0.50, na.rm = TRUE),
      `75th` = ~quantile(.x, 0.75, na.rm = TRUE)
    )
  )) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Stat"),
    names_pattern = "^(.*)_(Mean|Stdev|25th|50th|75th)$"
  ) %>%
  pivot_wider(names_from = Stat, values_from = value) %>%
  select(Variable, Mean, Stdev, `25th`, `50th`, `75th`)

summary_table <- summary_table %>%
  mutate(add_space = Variable %in% c())

latex_rows <- summary_table %>%
  mutate(latex_row = glue("{Variable} & {round(Mean, 2)} & {round(Stdev, 2)} & {round(`25th`, 2)} & {round(`50th`, 2)} & {round(`75th`, 2)} \\\\{ifelse(add_space, '\n\\addlinespace', '')}")) %>%
  pull(latex_row)

latex_table <- c(
  "\\begin{table}[h!]",
  "\\centering",
  "\\caption{Summary Statistics for Key Variables}",
  "\\begin{tabular}[t]{lrrrrr}",
  "\\toprule",
  "Variable & Mean & Stdev & 25th & 50th & 75th\\\\",
  "\\midrule",
  latex_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)

cat(paste(latex_table, collapse = "\n"))
  
################################################################################
# GRAPHS
################################################################################

# ---- LIT per stock ----
df_long <- df %>%
  group_by(stock) %>%
  summarise(
    min_LIT = min(LIT, na.rm = TRUE),
    mean_LIT = mean(LIT, na.rm = TRUE),
    max_LIT = max(LIT, na.rm = TRUE),
    mean_MS = mean(MS_i_t, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(overall_mean_LIT = mean(mean_LIT, na.rm = TRUE)) %>%
  arrange(desc(mean_MS)) %>%
  select(stock, mean_MS, overall_mean_LIT, min_LIT, mean_LIT, max_LIT) %>%
  pivot_longer(cols = c(min_LIT, mean_LIT, max_LIT), names_to = "Type", values_to = "LIT") %>%
  mutate(
    Type = factor(Type, levels = c("max_LIT", "mean_LIT", "min_LIT"), labels = c("Max", "Mean", "Min"))
  )

ggplot(df_long, aes(x = reorder(stock, -mean_MS), y = LIT, alpha = Type)) +
  geom_bar(stat = "identity", position = "identity", fill = "#116E8A") +
  geom_hline(aes(yintercept = overall_mean_LIT[1]), color = "red", linetype = "dashed", size = 0.8) +
  scale_alpha_manual(values = c("Max" = 0.3, "Mean" = 0.6, "Min" = 1.0), name = "") +
  labs(x = "Stock", y = "Lit Fragmentation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), panel.grid.major.y = element_line(color = "grey80"), legend.position = "top") +
  scale_y_continuous(breaks = seq(0, 1, 0.05), labels = scales::percent_format(accuracy = 1))

# ---- Time Series – Average Liquidity ----
df_time <- df %>%
  group_by(date) %>%
  summarise(Global = mean(liq_glo, na.rm = TRUE),
            Best_Market = mean(liq_bm, na.rm = TRUE),
            Local = mean(liq_loc, na.rm = TRUE)) %>%
  pivot_longer(cols = -date, names_to = "Type", values_to = "Liquidity")

df_time$Type <- factor(df_time$Type, levels = c("Global", "Best_Market", "Local"))

ggplot(df_time, aes(x = as.Date(date), y = Liquidity, color = Type)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = c(
      "Global" = "#00BA38",
      "Best_Market" = "#619CFF",
      "Local" = "#F8766D"
    )
  ) +
  labs(x = "Date", y = "Avg. Liquidity") +
  theme_minimal()

# ---- Time Series – Average Resiliency ----
df_time <- df %>%
  group_by(date) %>%
  summarise(Global = mean(res_glo, na.rm = TRUE),
            Best_Market = mean(res_bm, na.rm = TRUE),
            Local = mean(res_loc, na.rm = TRUE)) %>%
  pivot_longer(cols = -date, names_to = "Type", values_to = "Resiliency")

df_time$Type <- factor(df_time$Type, levels = c("Global", "Best_Market", "Local"))

ggplot(df_time, aes(x = as.Date(date), y = Resiliency, color = Type)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = c(
      "Global" = "#00BA38",
      "Best_Market" = "#619CFF",
      "Local" = "#F8766D"
    )
  ) +
  labs(x = "Date", y = "Avg. Resiliency") +
  theme_minimal()

table(df$stock)

ggplot(df[df$stock == "SATG_p"], aes(x = as.Date(date), y = res_glo)) +
  geom_line(size = 1) +
  labs(x = "Date", y = "Avg. Resiliency") +
  theme_minimal()
