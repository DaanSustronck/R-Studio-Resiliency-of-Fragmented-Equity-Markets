# R-Studio-Resiliency-of-Fragmented-Equity-Markets

This repository contains the full R-based pipeline for analyzing equity market fragmentation and its impact on liquidity and spread resiliency. The methodology is applied to DAX 40 stocks and structured around a two-stage regression framework.

⚠️ **Note**: This repository does **not** include raw data. Users must collect their own data using the companion repository: [Refinitiv Eikon Data Collection](https://github.com/DaanSustronck/Refinitiv-Eikon-Data-Retrieval-for-DAX-40).

---

## Workflow Overview

Execute the scripts in the following order:

### 1. `Tidy_Market_Microstructure.R`
Prepares the dataset for analysis by:
- Cleaning trade and quote data
- Filtering by trading hours and removing auction periods
- Computing fragmentation measures: `LIT`, `ALGO`, `DARK`, `PA`, `OTC`, `SI`
- Constructing liquidity and resiliency metrics (global, best-market, local)
- Creating the final daily panel dataset `df`

### 2. `Summary_Statistics_Variables.R`
Explores the structure and distribution of key variables:
- Summary statistics table with mean, standard deviation, and percentiles
- Visualizations of fragmentation per stock and time series plots of liquidity and resiliency

### 3. `Regression_Simple_Models_Liquidity.R`
Estimates how market fragmentation affects relative bid-ask spreads:
- Two-stage regression:
  - Stage 1: Fragmentation instruments using cross-sectional averages
  - Stage 2: Impact of fitted fragmentation on liquidity
- Models are run for global, best-market, and local spread definitions

### 4. `Regression_Simple_Models_Resiliency.R`
Analyzes the link between fragmentation and market resiliency:
- Two-stage regression as above
- Includes extensions with interactions:
  - Fragmentation × trade size
  - Fragmentation × order imbalance

---

## Data Requirements

This repository assumes you have access to:
- Trade and quote data for European stocks (e.g., from Refinitiv Eikon or LSEG Workspace)

If not: Data can be collected using the companion repository:

🔗 **[Refinitiv Eikon Data Collection](https://github.com/DaanSustronck/Refinitiv-Eikon-Data-Retrieval-for-DAX-40)**

That repository includes:
- Raw data extraction workflows
- Timestamps, bid/ask quotes, market venues, and trade sizes
- Format suitable for use with this analysis

---

## Dependencies

Make sure the following R packages are installed:

```r
install.packages(c("data.table", "dplyr", "ggplot2", "fixest", "tidyr", "glue", "forcats"))

