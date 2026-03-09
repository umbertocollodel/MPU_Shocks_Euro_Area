# Monetary Policy Uncertainty (MPU) Index — Research Pipeline Documentation

**Research question:** Does ECB Governing Council communication generate measurable uncertainty surprises in OIS markets, and are those surprises distinct from conventional monetary policy direction surprises?

**Last verified:** 2026-03-09

---

## Table of Contents

1. [Project Overview](#1-project-overview)
2. [Directory Structure](#2-directory-structure)
3. [Prerequisites](#3-prerequisites)
4. [Pipeline Overview](#4-pipeline-overview)
5. [Script-by-Script Reference](#5-script-by-script-reference)
6. [Data Dictionary — `range_difference_df.rds`](#6-data-dictionary--range_difference_dfrds)
7. [Raw Data Files](#7-raw-data-files)
8. [Output Files](#8-output-files)
9. [Known Limitations and TODOs](#9-known-limitations-and-todos)

---

## 1. Project Overview

This project constructs a **Monetary Policy Uncertainty (MPU) index** for the ECB using intraday high–low ranges of Overnight Index Swap (OIS) rates around ECB Governing Council (GovC) meetings. The core idea is that if a GovC meeting generates more uncertainty about the future rate path, the intraday trading range of OIS rates will be wider in the days after the meeting than before.

The MPU index is defined as:

```
MPU = avg(high–low range, N days post-GovC) − avg(high–low range, N days pre-GovC)
```

expressed in basis points. The baseline uses a symmetric 3-day pre/post window (`diff_3`). Six OIS tenors are covered: 3-month, 6-month, 1-year, 2-year, 5-year, and 10-year.

---

## 2. Directory Structure

```
Uncertainty_surprises/
│
├── code/                          # All R scripts (run from here as working directory)
│   ├── 01create_MPU.R
│   ├── 02plot_MPU_and_compare_with_MP_surprises.R
│   ├── 03appendix_run_exogeneity_tests_MPU.R
│   ├── 03.1appendix_create_mp_daily.R
│   ├── 03.2relationship_liquidity_mpu_testing.R
│   ├── 03.3check_swaption_correlation.R
│   ├── 03.3running_placebo.R
│   ├── 20temperature_robustness.R
│   ├── docs/
│   │   └── methodology.md         # This file
│   └── segoeui.ttf                # Font file (optional, see §3)
│
├── raw_data/                      # Input data — do not modify
│   ├── daily_OIS_updated15Sept_2025..xls
│   ├── dates_govc.xlsx
│   ├── 00EA_MPD_update_june2025.xlsx
│   ├── information_shock_merge.xlsx
│   └── ask_quotes_daily.xlsx
│
├── intermediate_data/             # Created automatically by script 01
│   ├── range_difference_df.rds    # Main MPU dataset (see §6)
│   └── mp_daily_df.rds
│
├── output/
│   ├── figures/                   # PDF and PNG charts
│   └── tables/                    # LaTeX (.tex) tables
│
└── docs/
    └── methodology.md             # This file
```

---

## 3. Prerequisites

### R packages

The pipeline uses `pacman` to install and load all packages automatically. Each script now loads its own dependencies and can be run standalone.

| Package    | Used in              | Purpose                            |
|------------|----------------------|------------------------------------|
| padr       | 01                   | Pad time series to daily frequency |
| DescTools  | 01                   | Winsorization                      |
| readxl     | all                  | Read Excel files                   |
| tidyverse  | all                  | Data manipulation and plotting     |
| zoo        | 01                   | Rolling window utilities           |
| corrr      | 02                   | Tidy correlation matrices          |
| stargazer  | 02                   | LaTeX table export                 |
| xtable     | 02                   | LaTeX correlation matrix table     |
| Hmisc      | 01                   | Miscellaneous statistics           |
| broom      | 03                   | Tidy regression output             |
| lubridate  | 02, 03.1, 03.2, 03.3 | Date parsing                       |
| showtext   | all                  | Custom font rendering              |
| sysfonts   | all                  | Font loading                       |
| patchwork  | 03.3running_placebo  | Multi-panel figure composition     |

### Font

Scripts use **Segoe UI Light** for publication-quality figures. Place `segoeui.ttf` in the `code/` directory. If absent, a warning is issued and the system default font is used.

### Working directory

All scripts use relative paths (`../raw_data/`, `../output/`, etc.) and **must be sourced with `code/` as the working directory**. In RStudio: use an RStudio Project rooted at `code/`, or call `setwd("path/to/code")` before sourcing.

### API keys (scripts 02 and 20 only)

| Key                  | Script | How to set                                                        |
|----------------------|--------|-------------------------------------------------------------------|
| `REFINITIV_API_KEY`  | 02     | `Sys.setenv(REFINITIV_API_KEY = "key")` in `.Renviron`. LSEG Workspace desktop app must be running. Section can be commented out if unavailable. |
| `OPENROUTER_API_KEY` | 20     | `Sys.setenv(OPENROUTER_API_KEY = "key")` in `.Renviron`.         |

---

## 4. Pipeline Overview

Script `01` must run first — it creates `range_difference_df.rds` which all downstream scripts consume. Scripts `02` onward can run in any order and are now standalone (each loads its own packages and data).

```
RAW DATA
    │
    ▼
[01] create_MPU.R
     Exports: intermediate_data/range_difference_df.rds
    │
    ├──► [02] plot_MPU_and_compare_with_MP_surprises.R
    │         Descriptive figures and tables
    │
    ├──► [03] appendix_run_exogeneity_tests_MPU.R
    │         AR(3) serial correlation test
    │
    ├──► [03.1] appendix_create_mp_daily.R
    │           Daily-window MP surprise; exports mp_daily_df.rds
    │
    ├──► [03.2] relationship_liquidity_mpu_testing.R
    │           Ask-bid spread change vs MPU
    │
    └──► [03.3] running_placebo.R
                Placebo test — 1000 draws of non-announcement days
```

---

## 5. Script-by-Script Reference

### 5.1 `01create_MPU.R` — Construct the MPU Index

**Purpose:** Build the MPU index from daily OIS high–low ranges. Entry point for the pipeline.

**Inputs:**

| File | Description |
|------|-------------|
| `raw_data/daily_OIS_updated15Sept_2025..xls` | One sheet per tenor (6 sheets). Columns: `Timestamp`, `high`, `low`, `first`, `last`. Coverage: 1999–Sept 2025. |
| `raw_data/dates_govc.xlsx` | ECB GovC announcement dates. Columns: `day`, `month`, `year`. Format: `%d-%m-%Y`. |

**Methodology:**

1. Load and pad OIS data to a continuous daily calendar using `padr::pad()`.
2. Mark GovC days; assign a unique `govc_id` to each meeting via cumulative sums spread across ±7 trading-day rows.
3. Compute `gap = high − low` (intraday volatility proxy, in pp of OIS rate).
4. For 8 window specifications (symmetric ±1, ±2, **±3 baseline**, ±5, ±7; asymmetric 1_5, 2_4, 4_2): compute mean pre/post gap, align to GovC day, take difference × 100 (bps).
5. Keep one row per GovC meeting; winsorize all `diff_*` columns at 1st/99th percentile; flag spikes (±1.5 SD).

**Outputs:**

| File | Description |
|------|-------------|
| `intermediate_data/range_difference_df.rds` | Main MPU dataset (see §6) |
| `output/figures/correlation_alternative_windows.pdf` | Correlation of each window's MPU with the 3-day baseline, by tenor |

---

### 5.2 `02plot_MPU_and_compare_with_MP_surprises.R` — Descriptive Analysis

**Purpose:** Publication-ready figures and tables. Compares MPU with ECB MPD surprises, CESIUSD, and pure MP/CBI shocks.

**Inputs:**

| File | Description |
|------|-------------|
| `intermediate_data/range_difference_df.rds` | Loaded automatically if not in session |
| `raw_data/00EA_MPD_update_june2025.xlsx` | Sheet 4: ECB MPD surprises by tenor. Dates may be in mixed formats — script handles all three (`DD/MM/YYYY`, Excel serial, `YYYY-MM-DD`). |
| `raw_data/information_shock_merge.xlsx` | Pure MP surprises (`MP_median`), CBI shocks, MPU spread columns (`uncert_spread_*`). |
| LSEG Workspace (live API, optional) | Citi ESI (`.CESIUSD`). Requires `REFINITIV_API_KEY`. Comment out this block if unavailable. |

**Key fix:** Significance stars in the MP/MPU correlation figure are now computed dynamically from `cor.test()` p-values, not hardcoded.

**Outputs (figures):**

| File | Description |
|------|-------------|
| `comparison_surprises.pdf` | MPU and MPD surprise over time, by tenor |
| `correlation_surprises.pdf` | Correlation of MPU with MP direction/size surprise |
| `mpu.pdf` | 10Y MPU over time |
| `mpu_all_tenors.pdf` | MPU over time, 6-panel grid |
| `mpu_all.png` | Post-GovC OIS volatility (3M, 2Y, 10Y) |
| `CESIUSD_vs_mpu_scatter.pdf` | Citi ESI vs MPU scatter (requires Refinitiv) |
| `correlation_uncertainty_cleaned_mp_surprises.pdf` | Pure MP surprise vs MPU, by maturity |

**Outputs (tables):**

| File | Description |
|------|-------------|
| `mpu_correlation.tex` | 6×6 Pearson correlation matrix of MPU across tenors |
| `top_5_mpu.tex` / `bottom_5_mpu.tex` | Top/bottom 5 MPU events at 10Y |
| `share_increases.tex` | Share of positive vs negative MPU by tenor |
| `summary_statistics_mpu.tex` | N, mean, median, SD by tenor |

---

### 5.3 `03appendix_run_exogeneity_tests_MPU.R` — Serial Correlation Test

**Purpose:** AR(3) test for serial correlation in MPU surprises. Unpredictability supports treating MPU as an exogenous shock.

**Model:** `diff_3(t) = β₀ + β₁·diff_3(t−1) + β₂·diff_3(t−2) + β₃·diff_3(t−3) + ε`, estimated separately per tenor.

**Outputs:**

| File | Description |
|------|-------------|
| `output/figures/autocorrelation_surprises.pdf` | AR(3) coefficient plot ± 95% CI, all tenors |

---

### 5.4 `03.1appendix_create_mp_daily.R` — Daily-Window MP Surprise

**Purpose:** Construct an MP direction surprise using the same 3-day window as MPU (using the closing bid rate), and validate against the ECB MPD 1-day surprise.

**Formula:**
```
mp_daily = avg(OIS close, 3 trading days post) − avg(OIS close, 3 trading days pre)  [× 100, bps]
```
Windows use the 3 nearest **trading days** before/after (not calendar days).

**Outputs:**

| File | Description |
|------|-------------|
| `intermediate_data/mp_daily_df.rds` | Long-format: `date`, `mp_daily`, `tenor` |
| `output/figures/mp_daily_vs_ecb_scatter.pdf` | Daily-window vs ECB MPD scatter, by tenor |

---

### 5.5 `03.2relationship_liquidity_mpu_testing.R` — Liquidity vs MPU

**Purpose:** Test whether MPU reflects bid-ask spread widening (reduced liquidity) rather than genuine uncertainty.

**Formula:**
```
spread        = ask_close − bid_close
liq_measure   = avg(spread, 3 trading days post) − avg(spread, 3 trading days pre)
```

**Inputs:**

| File | Description |
|------|-------------|
| `raw_data/daily_OIS_updated15Sept_2025..xls` | Bid close (`last` column) |
| `raw_data/ask_quotes_daily.xlsx` | Ask quotes. Paired columns (date, ask_close) × 6 tenors; skip first row (title). |

**Outputs:**

| File | Description |
|------|-------------|
| `output/figures/liquidity_vs_mpu_scatter.pdf` | Bid-ask spread change vs MPU, by tenor |

---

### 5.6 `03.3running_placebo.R` — Placebo Test

**Purpose:** Show MPU is specific to ECB announcement days. 1000 draws of non-announcement weekdays; tests both mean and % positive of pooled MPU.

**Design:**
- Exclusion window: ±10 trading days around each GovC
- Pooled MPU: average across all 6 tenors per date
- Two output figures: faceted by tenor + two-panel pooled (mean | % positive)

**Outputs:**

| File | Description |
|------|-------------|
| `output/figures/placebo_1000_draws_by_tenor.pdf` | Histogram of 1000 placebo means, faceted by tenor |
| `output/figures/placebo_1000_draws_pooled.pdf` | Pooled mean and % positive distributions |

---

## 6. Data Dictionary — `range_difference_df.rds`

**Format:** R data frame, long format | **Unit of observation:** GovC meeting × OIS tenor

| Column | Type | Unit | Description |
|--------|------|------|-------------|
| `tenor` | character | — | OIS tenor: `"3mnt"`, `"6mnt"`, `"1Y"`, `"2Y"`, `"5Y"`, `"10Y"` |
| `date` | Date | — | ECB GovC announcement date |
| `correct_pre_mean_3` | numeric | pp | Mean daily high–low range, 3 trading days before GovC |
| `correct_post_mean_3` | numeric | pp | Mean daily high–low range, 3 trading days after GovC |
| `diff_3` | numeric | bps | **Baseline MPU.** `(post − pre) × 100`. Winsorized at 1st/99th pct. |
| `diff_1` | numeric | bps | MPU with 1-day symmetric window. Winsorized. |
| `diff_2` | numeric | bps | MPU with 2-day symmetric window. Winsorized. |
| `correct_post_mean_1` | numeric | pp | Post-meeting range, 1-day window |
| `correct_post_mean_2` | numeric | pp | Post-meeting range, 2-day window |
| `spike` | integer | — | 1 if `diff_3` > ±1.5 SD from the full-sample mean (after winsorizing) |

---

## 7. Raw Data Files

| File | Description |
|------|-------------|
| `daily_OIS_updated15Sept_2025..xls` | Daily OHLC for EUR OIS, 6 tenors. One sheet per tenor. Coverage: 1999–Sept 2025. |
| `dates_govc.xlsx` | ECB GovC announcement dates. Coverage confirmed through Sept 2024. |
| `00EA_MPD_update_june2025.xlsx` | ECB Monetary Policy Database — OIS-based MP surprises, sheet 4. |
| `information_shock_merge.xlsx` | Pure MP surprises, CBI shocks, MPU spread columns. |
| `ask_quotes_daily.xlsx` | Ask-side daily closing quotes, 6 tenors. Paired columns per tenor; first row is title. |

---

## 8. Output Files

### Figures (`output/figures/`)

| File | Script | Description |
|------|--------|-------------|
| `correlation_alternative_windows.pdf` | 01 | Alternative-window MPU vs 3-day baseline correlation |
| `comparison_surprises.pdf` | 02 | MPU and MP direction surprise over time |
| `correlation_surprises.pdf` | 02 | MPU vs MP surprise correlation (dynamic p-values) |
| `mpu.pdf` | 02 | 10Y MPU over time |
| `mpu_all_tenors.pdf` | 02 | MPU over time, all tenors |
| `mpu_all.png` | 02 | Post-GovC OIS volatility, 3M/2Y/10Y |
| `CESIUSD_vs_mpu_scatter.pdf` | 02 | Citi ESI vs MPU (requires Refinitiv) |
| `correlation_uncertainty_cleaned_mp_surprises.pdf` | 02 | Pure MP vs MPU scatter |
| `autocorrelation_surprises.pdf` | 03 | AR(3) exogeneity test |
| `mp_daily_vs_ecb_scatter.pdf` | 03.1 | Daily-window MP vs ECB MPD |
| `liquidity_vs_mpu_scatter.pdf` | 03.2 | Bid-ask spread change vs MPU |
| `placebo_1000_draws_by_tenor.pdf` | 03.3 | Placebo test, by tenor |
| `placebo_1000_draws_pooled.pdf` | 03.3 | Placebo test, pooled |

### Tables (`output/tables/`)

| File | Script | Description |
|------|--------|-------------|
| `mpu_correlation.tex` | 02 | 6×6 MPU correlation matrix |
| `top_5_mpu.tex` | 02 | Top-5 positive MPU events |
| `bottom_5_mpu.tex` | 02 | Top-5 negative MPU events |
| `share_increases.tex` | 02 | Share MPU > 0 by tenor |
| `summary_statistics_mpu.tex` | 02 | Summary statistics by tenor |

All `.tex` files use `booktabs` style and are ready for `\input{}`.

---

## 9. Known Limitations and TODOs

- **`dates_govc.xlsx` coverage gap:** GovC dates confirmed through Sept 2024; OIS data runs through Sept 2025. Meetings after the date file's end are silently excluded. **TODO:** update `dates_govc.xlsx`.
- **`govc_id` overlap:** If two meetings fall within 14 trading days of each other, the ±7-row lag/lead windows overlap and `govc_id` may conflate observations from adjacent meetings.
- **Lead/lag alignment fragility (`01`):** The shift-by-1 that places pre/post means on the GovC day breaks silently if the immediately adjacent trading day is missing from the data.
- **Refinitiv block in `02`:** Will error without active LSEG Workspace session. Wrap in `tryCatch()` or comment out if unavailable.
- **Package version pinning:** Managed via `renv`. Run `renv::restore()` in the `code/` directory to install the exact package versions recorded in `renv.lock`.
