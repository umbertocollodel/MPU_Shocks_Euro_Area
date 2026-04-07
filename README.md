# Replication Code: ECB Monetary Policy Uncertainty

**Author:** Umberto Collodel (Central Bank of Malta), Vanessa Kunzmann (Deutsche Bundesbank)

This repository contains replication code for two related papers. **Paper 1** constructs a novel market-based measure of monetary policy uncertainty (MPU) and studies its transmission to financial markets. **Paper 2** uses that MPU measure as a benchmark to evaluate LLM-agent simulations of trader disagreement following ECB press conferences. The scripts are numbered sequentially; 01–03.4 belong to Paper 1, 04 onwards to Paper 2.

---

## Paper 1: Market-based Monetary Policy Uncertainty Shocks in the Euro Area

> This paper investigates the transmission of monetary policy to financial markets within the Euro area, focusing on the role of uncertainty. While previous research has extensively examined the effects of changes in expected policy rates through event studies of ECB announcements, the impact of second moments and uncertainty has been far less explored. We address this gap by introducing a novel market-based measure of uncertainty regarding future interest rates, calculated as the difference in the standard deviation of Overnight Index Swap (OIS) rates in a three-day window around ECB policy announcements. Our findings reveal that ECB announcements generally increase market uncertainty about future interest rates, regardless of the sign of the policy surprise. This increased uncertainty significantly impacts asset prices, leading to higher nominal yields, lower stock market returns, and Euro appreciation against safe-haven currencies.

### Scripts (01–03.4)

| Script | Description |
|--------|-------------|
| `01create_MPU.R` | Constructs the MPU index from daily OIS rates across six tenors (3M, 6M, 1Y, 2Y, 5Y, 10Y) |
| `02plot_MPU_and_compare_with_MP_surprises.R` | Figures and tables comparing MPU with MP surprises; CESIUSD confound check |
| `03.1relationship_liquidity_mpu_testing.R` | Correlation between MPU and bid-ask spread liquidity changes |
| `03.2appendix_run_exogeneity_tests_MPU.R` | AR(3) exogeneity/serial-correlation check (appendix) |
| `03.3mpu_all_days_vs_govc.R` | Distributional comparison: MPU on GovC days vs. all other trading days |
| `03.4get_calendar_us_releases.R` | US economic release calendar — checks for systematic confounding of MPU by US data surprises |

### Quick Start — Paper 1

```r
source("01create_MPU.R")
source("02plot_MPU_and_compare_with_MP_surprises.R")
source("03.1relationship_liquidity_mpu_testing.R")
source("03.2appendix_run_exogeneity_tests_MPU.R")
source("03.3mpu_all_days_vs_govc.R")
source("03.4get_calendar_us_releases.R")
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

For OIS data alternatives, see the Data Availability Statement below.

---

## Paper 2: Interpreting the Interpreter: Can We Model post-ECB Conferences Volatility with LLM Agents?

> This paper develops a novel method to simulate financial market reactions to ECB press conferences using a Large Language Model (LLM). We create a behavioral, agent-based simulation of 30 synthetic traders, each with distinct risk preferences, cognitive biases, and interpretive styles. These agents forecast Euro interest rate swap levels at 3-month, 2-year, and 10-year maturities, with the variation across forecasts serving as a measure of market uncertainty or disagreement. We evaluate three prompting strategies — naive, few-shot (enriched with historical data), and an advanced iterative 'LLM-as-a-Judge' framework — to assess the effect of prompt design on predictive performance. Even the naive approach generates a strong correlation (roughly 0.5) between synthetic disagreement and actual market outcomes, particularly for longer-term maturities. The LLM-as-a-Judge framework further improves accuracy at the first iteration. These results demonstrate that LLM-driven simulations can capture interpretive uncertainty beyond traditional measures, providing central banks with a practical tool to anticipate market reactions, refine communication strategies, and enhance financial stability.

### Scripts (04 onwards)

#### Data preparation
| Script | Description |
|--------|-------------|
| `04scraping_ecb_pressconf.R` | Scrapes and stores all ECB press conference transcripts |
| `05calculate_complexity_documents.R` | Computes readability/complexity metrics for each transcript |

#### LLM simulation
| Script | Description |
|--------|-------------|
| `07run_all_models_sequential.R` | Runs all three model variants back-to-back |
| `src/run_model.R` | Entry point for a single Gemini-based model run |
| `src/run_model_openrouter.R` | Entry point for OpenRouter (alternative LLM provider) |
| `src/llm_api/gemini_api.R` | Gemini API wrappers |
| `src/llm_api/openrouter_api.R` | OpenRouter API wrappers |
| `config/prompts.R` | All prompt templates |
| `config/model_config.yaml` | Parameters for all models (edit here, not in code) |

#### Analysis and results
| Script | Description |
|--------|-------------|
| `08clean_llm_result.R` | Parses raw Gemini LLM outputs |
| `09clean_openrouter_result.R` | Parses raw OpenRouter outputs |
| `09plot_llm_results.R` | Main figures and validation — **primary results** |
| `18regression_ois_on_synthetic_measures.R` | Regression of actual OIS on synthetic disagreement |
| `19plot_qwen3_results.R` | Results for Qwen3 model variant |

#### Robustness
| Script | Description |
|--------|-------------|
| `12run_bootstrap_robustness.R` | Bootstrap confidence intervals |
| `14run_prompt_stability_test_robustness.R` | Sensitivity to prompt wording |
| `15run_model_stability_test_robustness.R` | Sensitivity to model choice |
| `16run_counterfactual_exercise.R` | Counterfactual simulation |
| `17run_real_oos_test.R` | Out-of-sample validation |
| `20temperature_robustness.R` | Sensitivity to LLM temperature parameter |

### Quick Start — Paper 2

#### Setup

```bash
# Add your API key
cp .Renviron.example .Renviron
# Edit .Renviron: GEMINI_API_KEY=your_key_here

# Python dependencies (llm_as_judge model only)
pip install -r requirements.txt
```

#### Run

```r
# Full pipeline (15–25 hours)
source("00run_complete_pipeline.R")
```

Or stage by stage:

```r
# 1. Data preparation (30–60 min)
source("04scraping_ecb_pressconf.R")
source("05calculate_complexity_documents.R")

# 2. LLM models (10–20 hours)
source("07run_all_models_sequential.R")
# OR a single model:
source("src/run_model.R")
run_model(model_name = "naive")               # 2–4 hours
run_model(model_name = "historical_surprise") # 2–4 hours
run_model(model_name = "llm_as_judge")       # 6–12 hours

# 3. Parse and plot (10 min)
source("08clean_llm_result.R")
source("09plot_llm_results.R")   # Main results

# 4. Robustness (1–3 hours)
source("12run_bootstrap_robustness.R")
source("14run_prompt_stability_test_robustness.R")
source("15run_model_stability_test_robustness.R")
source("16run_counterfactual_exercise.R")
source("17run_real_oos_test.R")
```

### Three LLM Models

| Model | Description | Runtime | Cost |
|-------|-------------|---------|------|
| **Naive** | Basic prompt, no context | 2–4 hours | ~$8–12 |
| **Historical Surprise** | Includes past volatility as context | 2–4 hours | ~$10–15 |
| **LLM-as-Judge** | Iterative meta-learning prompt tuning | 6–12 hours | ~$15–25 |

All models simulate 30 heterogeneous synthetic traders forecasting OIS rates at 3M, 2Y, and 10Y maturities.

**Total cost for full replication (all models + robustness): ~$40–65.**

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
| ECB press conference transcripts | Automatically downloaded by `04scraping_ecb_pressconf.R` | ECB website (free) |
| Gemini API key | Required for Gemini model runs | Google AI Studio (free tier available) |
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
├── 00run_complete_pipeline.R        # Runs Paper 2 pipeline end-to-end
│
├── Paper 1 — MPU construction & analysis
│   ├── 01create_MPU.R
│   ├── 02plot_MPU_and_compare_with_MP_surprises.R
│   ├── 03.1relationship_liquidity_mpu_testing.R
│   ├── 03.2appendix_run_exogeneity_tests_MPU.R
│   ├── 03.3mpu_all_days_vs_govc.R
│   └── 03.4get_calendar_us_releases.R
│
├── Paper 2 — LLM agent simulation
│   ├── 04scraping_ecb_pressconf.R
│   ├── 05calculate_complexity_documents.R
│   ├── 07run_all_models_sequential.R
│   ├── 08clean_llm_result.R
│   ├── 09clean_openrouter_result.R
│   ├── 09plot_llm_results.R          # Main results
│   ├── 18regression_ois_on_synthetic_measures.R
│   ├── 19plot_qwen3_results.R
│   └── 20temperature_robustness.R
│
├── Robustness (Paper 2)
│   ├── 12run_bootstrap_robustness.R
│   ├── 14run_prompt_stability_test_robustness.R
│   ├── 15run_model_stability_test_robustness.R
│   ├── 16run_counterfactual_exercise.R
│   └── 17run_real_oos_test.R
│
├── src/                              # Core LLM infrastructure
│   ├── run_model.R
│   ├── run_model_openrouter.R
│   └── llm_api/
│       ├── gemini_api.R
│       └── openrouter_api.R
│
└── config/
    ├── model_config.yaml
    └── prompts.R
```

**Note on script numbering:** gaps (06, 10, 11, 13) reflect merged or deprecated scripts from earlier development stages.

---

## Output

```
../output/
├── figures/
│   ├── (Paper 1 figures: MPU series, correlations, distributions)
│   ├── prompt_naive/        # Paper 2 main results
│   └── oos_jan2025/         # Out-of-sample validation
└── tables/
    └── (LaTeX tables for both papers)

../intermediate_data/
├── range_difference_df.rds        # MPU index (Paper 1 → Paper 2 input)
├── top_bottom_5_descriptions.csv  # Cached narrative descriptions (Paper 1)
├── gemini_result/                 # Raw LLM responses
├── aggregate_gemini_result/       # Cleaned LLM data
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
  author={Collodel, Umberto; Kunzmann, Vanessa},
  institution={Central Bank of Malta ; Deutsche BUndesbank},
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
**Last Updated:** April 2026
