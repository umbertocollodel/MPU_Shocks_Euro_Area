# Replication Code: ECB Monetary Policy Uncertainty

**Author:** Umberto Collodel (Central Bank of Malta), Vanessa Kunzmann (Deutsche Bundesbank)

This repository contains replication code for two related papers. **Paper 1** constructs a novel market-based measure of monetary policy uncertainty (MPU) and studies its transmission to financial markets. **Paper 2** uses that MPU measure as a benchmark to evaluate LLM-agent simulations of trader disagreement following ECB press conferences. Scripts are numbered sequentially; 01–06 belong to Paper 1, 07–18 to Paper 2.

---

## Paper 1: Market-based Monetary Policy Uncertainty Shocks in the Euro Area

> This paper investigates the transmission of monetary policy to financial markets within the Euro area, focusing on the role of uncertainty. We introduce a novel market-based measure of uncertainty regarding future interest rates, calculated as the difference in the standard deviation of OIS rates in a three-day window around ECB policy announcements. ECB announcements generally increase market uncertainty about future interest rates, regardless of the sign of the policy surprise, leading to higher nominal yields, lower stock market returns, and Euro appreciation against safe-haven currencies.

### Scripts (01–06)

| Script | Description |
|--------|-------------|
| `01create_MPU.R` | Constructs the MPU index from daily OIS rates across six tenors (3M, 6M, 1Y, 2Y, 5Y, 10Y) |
| `02plot_MPU_and_compare_with_MP_surprises.R` | Figures and tables comparing MPU with MP surprises; CESIUSD confound check |
| `03relationship_liquidity_mpu_testing.R` | Correlation between MPU and bid-ask spread liquidity changes |
| `04appendix_exogeneity_tests_MPU.R` | AR(3) exogeneity/serial-correlation check (appendix) |
| `05mpu_all_days_vs_govc.R` | Distributional comparison: MPU on GovC days vs. all other trading days |
| `06get_calendar_us_releases.R` | US economic release calendar — checks for systematic confounding by US data surprises |

### Quick Start — Paper 1

```r
source("01create_MPU.R")
source("02plot_MPU_and_compare_with_MP_surprises.R")
source("03relationship_liquidity_mpu_testing.R")
source("04appendix_exogeneity_tests_MPU.R")
source("05mpu_all_days_vs_govc.R")
source("06get_calendar_us_releases.R")
```

**Runtime:** ~10–15 minutes (no API calls required).

### Data Requirements — Paper 1

All files go in `../raw_data/`:

| File | Description | Source | Access |
|------|-------------|--------|--------|
| `daily_OIS_updated15Sept_2025..xls` | Daily Euro area OIS rates, tenors 3M–10Y, 1998–2025 | Refinitiv Eikon / Bloomberg | Proprietary |
| `dates_govc.xlsx` | ECB Governing Council meeting dates | ECB website | Free |
| `00EA_MPD_update_june2025.xlsx` | ECB staff macroeconomic projections | ECB Monetary Policy Database | Free |
| `information_shock_merge.xlsx` | Cleaned MP surprises (Jarociński & Karadi decomposition) | Authors | On request |

**Note on the double dot in `daily_OIS_updated15Sept_2025..xls`:** this is the actual filename — include both dots.

---

## Paper 2: Interpreting the Interpreter: Can We Model post-ECB Conferences Volatility with LLM Agents?

> This paper develops a novel method to simulate financial market reactions to ECB press conferences using a Large Language Model (LLM). We create a behavioral, agent-based simulation of 30 synthetic traders, each with distinct risk preferences, cognitive biases, and interpretive styles. These agents forecast Euro interest rate swap levels at 3-month, 2-year, and 10-year maturities, with the variation across forecasts serving as a measure of market uncertainty or disagreement. Even the naive approach generates a strong correlation (roughly 0.5) between synthetic disagreement and actual market outcomes. These results demonstrate that LLM-driven simulations can capture interpretive uncertainty beyond traditional measures.

### Scripts (07–18)

#### Data preparation

| Script | Description |
|--------|-------------|
| `07scraping_ecb_pressconf.R` | Scrapes and stores all ECB press conference transcripts |
| `08calculate_complexity_documents.R` | Computes readability/complexity metrics for each transcript |

#### LLM ensemble — main results

| Script | Description |
|--------|-------------|
| `09run_full_ensemble_p1.R` | Full ensemble: 283 conferences × R=10 seeds = 2,830 API calls (parallelised, resumable) |
| `10post_zero_shot_p1.R` | Main figures (Figures 3–5), reliability G(R), stabilisation diagnostics |
| `18regression_ois_on_synthetic_measures.R` | Regression of actual OIS volatility on synthetic disagreement (Table 3) |

#### Robustness

| Script | Description |
|--------|-------------|
| `11run_prompt_stability_robustness.R` | Sensitivity to prompt wording (10 minor + 5 medium variations) |
| `12run_model_stability_robustness.R` | Cross-model robustness (Gemini, Claude, OpenAI) |
| `13run_real_oos_test.R` | Out-of-sample validation using precomputed R=10 ensemble |
| `14run_endogeneity_p1.R` | Endogeneity test: 2-stage panel generation vs forecasting |
| `15post_endogeneity_p1.R` | Plots and tables from endogeneity test |
| `16run_fewshot_ensemble_p1.R` | Few-shot ensemble: 90 stratified conferences × R=10 seeds |
| `17post_fewshot_ensemble_p1.R` | Few-shot vs naive comparison (correlation bars, calibration table) |

#### Infrastructure

| Script | Description |
|--------|-------------|
| `src/run_model.R` | Entry point for a single Gemini-based model run |
| `src/run_model_openrouter.R` | Entry point for OpenRouter (alternative LLM provider) |
| `src/llm_api/gemini_api.R` | Gemini API wrappers |
| `src/llm_api/openrouter_api.R` | OpenRouter API wrappers |
| `config/prompts.R` | All prompt templates |
| `config/model_config.yaml` | Parameters for all models (edit here, not in code) |

### Quick Start — Paper 2

#### Setup

```bash
cp .Renviron.example .Renviron
# Edit .Renviron: GEMINI_API_KEY=your_key_here
pip install -r requirements.txt   # Python deps for llm_as_judge only
```

#### Run

```r
# Full pipeline (15–25 hours)
source("00run_complete_pipeline.R")
```

Or stage by stage:

```r
# 1. Data preparation (30–60 min)
source("07scraping_ecb_pressconf.R")
source("08calculate_complexity_documents.R")

# 2. LLM ensemble — main results (10–20 hours)
source("09run_full_ensemble_p1.R")

# 3. Figures and regression (10 min)
source("10post_zero_shot_p1.R")
source("18regression_ois_on_synthetic_measures.R")

# 4. Robustness (1–3 hours)
source("11run_prompt_stability_robustness.R")
source("12run_model_stability_robustness.R")
source("13run_real_oos_test.R")
source("14run_endogeneity_p1.R")
source("15post_endogeneity_p1.R")
source("16run_fewshot_ensemble_p1.R")
source("17post_fewshot_ensemble_p1.R")
```

### Configuration

Edit `config/model_config.yaml` — no code changes needed:

```yaml
active_model: "naive"   # or "historical_surprise" or "llm_as_judge"

models:
  naive:
    temperature: 1
    parallel_workers: 5
    seed: 120
  historical_surprise:
    history_window: 3
  llm_as_judge:
    max_optimization_iterations: 10
    analyst_model: "gemini/gemini-2.5-flash"
    judge_model: "gemini/gemini-2.5-pro"
```

### Data Requirements — Paper 2

The MPU index from Paper 1 (`../intermediate_data/range_difference_df.rds`) is a direct input. Additional requirements:

| Item | Description | Source |
|------|-------------|--------|
| ECB press conference transcripts | Automatically downloaded by `07scraping_ecb_pressconf.R` | ECB website (free) |
| Gemini API key | Required for all Gemini model runs | Google AI Studio (free tier available) |
| OpenRouter API key | Required for non-Gemini model runs | OpenRouter.ai |

### Software Requirements

- **R** ≥ 4.2.0 (packages managed via `renv` — run `renv::restore()` on first use)
- **Python** ≥ 3.8 (only for `llm_as_judge` model): `numpy`, `pandas`, `scipy`, `pyreadr`, `litellm`, `tqdm`

### Troubleshooting

**"GEMINI_API_KEY not found"**
```r
file.exists(".Renviron")  # Should be TRUE
# If not: file.copy(".Renviron.example", ".Renviron"), then restart R
```

**Rate limit errors (429)**
```yaml
# In config/model_config.yaml:
parallel_workers: 3  # Reduce from 5
```

**Test on a small subset first**
```r
# In src/run_model.R, around line ~133:
dates_ecb_presconf <- dates_ecb_presconf[1:3]  # 3 conferences ≈ 10 min, ~$1–2
```

**Font warnings about "Segoe UI":** harmless on Windows; on Mac/Linux place `segoeui.ttf` in `code/` or ignore.

---

## Repository Structure

```
code/
├── 00run_complete_pipeline.R         # Runs full pipeline end-to-end
│
├── Paper 1 — MPU construction & analysis (01–06)
│   ├── 01create_MPU.R
│   ├── 02plot_MPU_and_compare_with_MP_surprises.R
│   ├── 03relationship_liquidity_mpu_testing.R
│   ├── 04appendix_exogeneity_tests_MPU.R
│   ├── 05mpu_all_days_vs_govc.R
│   └── 06get_calendar_us_releases.R
│
├── Paper 2 — LLM agent simulation (07–18)
│   ├── 07scraping_ecb_pressconf.R
│   ├── 08calculate_complexity_documents.R
│   ├── 09run_full_ensemble_p1.R      # Main LLM run
│   ├── 10post_zero_shot_p1.R         # Main figures
│   ├── 11run_prompt_stability_robustness.R
│   ├── 12run_model_stability_robustness.R
│   ├── 13run_real_oos_test.R
│   ├── 14run_endogeneity_p1.R
│   ├── 15post_endogeneity_p1.R
│   ├── 16run_fewshot_ensemble_p1.R
│   ├── 17post_fewshot_ensemble_p1.R
│   └── 18regression_ois_on_synthetic_measures.R
│
├── poster/                            # Academic conference poster assets
│   ├── poster_fig_sd_timeseries.R
│   ├── poster_fig_GR.R
│   └── poster_qr_code.R
│
├── src/                               # Core LLM infrastructure
│   ├── run_model.R
│   ├── run_model_openrouter.R
│   └── llm_api/
│       ├── gemini_api.R
│       └── openrouter_api.R
│
├── config/
│   ├── model_config.yaml
│   └── prompts.R
│
└── archive/                           # Deprecated scripts (kept for reference)
```

---

## Output

```
../output/
├── figures/
│   ├── (Paper 1 figures: MPU series, correlations, distributions)
│   ├── full_ensemble_p1/    # Paper 2 main results
│   └── oos/                 # Out-of-sample validation
└── tables/
    └── (LaTeX tables for both papers)

../intermediate_data/
├── range_difference_df.rds        # MPU index (Paper 1 → Paper 2 input)
├── full_ensemble_p1/              # Ensemble API results (per-call RDS cache)
├── p1/                            # Post-processed ensemble outputs
└── texts/                         # ECB press conference transcripts
```

---

## Data Availability Statement

ECB press conference transcripts and meeting dates are freely available from the ECB website. OIS rates (Refinitiv Eikon) are proprietary. Researchers seeking to replicate Paper 1 can:

1. Access OIS data via institutional subscriptions to Refinitiv Eikon or Bloomberg Terminal
2. Contact the author for data access for replication purposes (subject to data provider terms)
3. Use ECB Statistical Data Warehouse data as an alternative (results may differ slightly)

---

## Citation

```bibtex
@article{collodel2025mpu,
  title={Market-based Monetary Policy Uncertainty Shocks in the Euro Area},
  author={Collodel, Umberto and Kunzmann, Vanessa},
  institution={Central Bank of Malta; Deutsche Bundesbank},
  year={2025}
}

@article{collodel2025interpreting,
  title={Interpreting the Interpreter: Can We Model post-ECB Conferences Volatility with LLM Agents?},
  author={Collodel, Umberto},
  institution={Central Bank of Malta},
  year={2025}
}
```

---

**Contact:** Umberto Collodel — Central Bank of Malta
**Last Updated:** June 2026
