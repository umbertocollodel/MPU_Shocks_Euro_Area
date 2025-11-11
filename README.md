# Interpreting the Interpreter: LLM-Based Uncertainty Measurement

**Author:** Umberto Collodel (Central Bank of Malta)

Replication code for analyzing ECB communication using Large Language Models to measure monetary policy uncertainty.

---

## Quick Start

### 1. Setup (5 minutes)
```bash
# Copy API key template
cp .Renviron.example .Renviron

# Edit and add your Gemini API key
# GEMINI_API_KEY=your_key_here

# Install Python dependencies (only for llm_as_judge model)
pip install -r requirements.txt
```

### 2. Run Complete Pipeline
```r
# Run entire analysis pipeline (15-25 hours)
source("00run_complete_pipeline.R")
```

**OR** run individual stages:
```r
# Run all three LLM model variants sequentially (10-20 hours)
source("07run_all_models_sequential.R")

# OR run individual models
source("src/run_model.R")
run_model(model_name = "naive")                # 2-4 hours
run_model(model_name = "historical_surprise")  # 2-4 hours
run_model(model_name = "llm_as_judge")        # 6-12 hours
```

### 3. Analyze Results
```r
source("08clean_llm_result.R")    # Parse LLM outputs
source("09plot_llm_results.R")    # Generate visualizations
```

**Done!** See `REPLICATION_GUIDE.md` for detailed instructions.

---

## What This Code Does

### Three LLM Models for Uncertainty Measurement

**The Research Question:** Can Large Language Models simulate how traders interpret ECB press conferences, and does their disagreement predict market volatility?

**The Approach:** Three progressively sophisticated models:

| Model | Description | Key Feature | Runtime |
|-------|-------------|-------------|---------|
| **Naive** | Basic prompt, no context | Baseline comparison | 2-4 hours |
| **Historical Surprise** | Includes past volatility | Contextual learning | 2-4 hours |
| **LLM-as-Judge** | Meta-learning optimization | Automatic prompt tuning | 6-12 hours |

All models simulate 30 heterogeneous traders reacting to each ECB press conference, producing predictions for 3 interest rate tenors (3M, 2Y, 10Y).

---

## Repository Structure

```
code/
├── 00run_complete_pipeline.R    # Master script - runs everything
│
├── config/                       # Model configurations
│   ├── model_config.yaml         # Parameters for all models
│   └── prompts.R                 # All prompt templates
│
├── src/                          # Core unified system
│   ├── run_model.R               # Main entry point
│   └── llm_api/
│       ├── gemini_api.R          # R API functions
│       └── llm_optimizer.py      # Python meta-learning
│
├── 07run_all_models_sequential.R # Run all models sequentially
│
├── Data preparation:
│   ├── 01create_MPU.R            # Monetary Policy Uncertainty index
│   ├── 02plot_MPU_and_compare_with_MP_surprises.R
│   ├── 03appendix_run_exogeneity_tests_MPU.R
│   ├── 04scraping_ecb_pressconf.R
│   ├── 05calculate_complexity_documents.R
│   └── calendar_us_releases.R    # US economic releases analysis
│
├── Analysis pipeline:
│   ├── 08clean_llm_result.R      # Parse LLM outputs
│   └── 09plot_llm_results.R      # Visualizations & main validation
│
├── Robustness tests:
│   ├── 12run_bootstrap_robustness.R
│   ├── 14run_prompt_stability_test_robustness.R
│   ├── 15run_model_stability_test_robustness.R
│   ├── 16run_counterfactual_exercise.R
│   └── 17run_real_oos_test.R
│
└── Documentation:
    ├── README.md                 # This file
    ├── REPLICATION_GUIDE.md      # Full instructions
    └── VALIDATION_CHECKLIST.md   # Testing protocol
```

**Note on script numbering:** Gaps in numbering (06, 10, 11, 13) reflect merged or deprecated scripts from earlier development stages. Current numbering is intentional and all necessary scripts are present.

---

## Requirements

### Software
- **R** ≥ 4.2.0
- **Python** ≥ 3.8 (only for llm_as_judge model)
- **Google Gemini API** key ([get one free](https://makersuite.google.com/app/apikey))

### R Packages (auto-installed via pacman)
The following packages will be automatically installed when running scripts:
```r
# Core packages
tidyverse, yaml, gemini.R, httr2, readtext, crayon, stringr, purrr,
readxl, writexl, showtext, lubridate, zoo, patchwork

# Statistical packages
boot, broom, corrr, DescTools, Hmisc, stargazer, xtable, padr

# Additional
future, furrr, RColorBrewer, ggplot2, scales
```

### Python Packages
```bash
pip install -r requirements.txt
```

Contents: `numpy`, `pandas`, `scipy`, `pyreadr`, `litellm`, `tqdm`

---

## Data Requirements

### Required Data Files

Place all data files in `../raw_data/` (one level above the code directory):

#### 1. **OIS Swap Rates** (REQUIRED)
- **Filename:** `daily_OIS_updated15Sept_2025..xls`
- **Description:** Daily Euro area overnight indexed swap (OIS) rates for tenors 3M, 2Y, 10Y
- **Source:** Refinitiv Eikon / Bloomberg Terminal (proprietary)
- **Time period:** 1998-2025
- **Format:** Excel file with separate sheets per tenor
- **Used by:** Scripts 01, 02, 03, 09

**Alternative:** If you don't have access to Refinitiv/Bloomberg, you can:
- Use ECB Statistical Data Warehouse for Euro Short-Term Rate (€STR)
- Contact the author for data access for replication purposes
- Note: Results may differ slightly with alternative data sources

#### 2. **ECB Governing Council Meeting Dates** (REQUIRED)
- **Filename:** `dates_govc.xlsx`
- **Description:** Dates of all ECB Governing Council meetings with press conferences
- **Source:** ECB website - [Monetary Policy Decisions](https://www.ecb.europa.eu/press/calendars/mgcgc/html/index.en.html)
- **Access:** Freely available, manually compiled
- **Format:** Excel file with columns: `year`, `month`, `day`
- **Used by:** Scripts 01, 02, 04, calendar_us_releases.R

**To replicate:** Download meeting dates from ECB website and format as XLSX with columns: year, month, day

#### 3. **ECB Macroeconomic Projections Database** (OPTIONAL)
- **Filename:** `00EA_MPD_update_june2025.xlsx`
- **Description:** ECB staff macroeconomic projections
- **Source:** ECB Monetary Policy Database
- **Access:** Freely available
- **URL:** https://www.ecb.europa.eu/stats/ecb_surveys/survey_of_professional_forecasters/html/index.en.html
- **Used by:** Script 01 (for additional controls)

#### 4. **US Economic Release Dates** (OPTIONAL - for calendar analysis)
- **Directory:** `us_releases/`
- **Files needed:**
  - `release_dates_50.xlsx` - Employment Situation
  - `release_dates_10.xlsx` - CPI
  - `release_dates_53.xlsx` - GDP
  - `release_dates_9.xlsx` - Retail Sales
- **Source:** US Bureau of Labor Statistics / Census Bureau / BEA
- **Access:** Freely available via FRED (Federal Reserve Economic Data)
- **URL:** https://fred.stlouisfed.org/
- **Used by:** `calendar_us_releases.R` (optional analysis)

**To download:** Visit FRED website and download release dates for each indicator

#### 5. **Refinitiv Tick Data** (OPTIONAL)
- **Filename:** `Refinitiv_Tick_Data.xlsx`
- **Description:** High-frequency tick data for additional analysis
- **Source:** Refinitiv (proprietary)
- **Used by:** Optional analysis scripts
- **Note:** Not required for main results

### ECB Press Conference Transcripts

**Automatic Download:** Script `04scraping_ecb_pressconf.R` automatically downloads all ECB press conference transcripts from the ECB website.

- **Source:** ECB website (freely available)
- **Storage:** `../intermediate_data/texts/`
- **No manual download required** - script handles this automatically

---

## Cost Estimate

**API Costs (Google Gemini):**
- Naive model: ~$8-12
- Historical Surprise model: ~$10-15
- LLM-as-Judge model: ~$15-25
- Robustness tests: ~$10-15

**Total: ~$40-65** for complete replication with all robustness tests

**To reduce costs:**
- Test on subset first (see Troubleshooting section)
- Run only Naive model (~$10)
- Reduce parallel workers in config

---

## Configuration

All parameters in `config/model_config.yaml`:

```yaml
# Select which model to run
active_model: "naive"

models:
  naive:
    temperature: 1        # Randomness (0-2)
    parallel_workers: 5   # API calls in parallel
    seed: 120            # Reproducibility

  historical_surprise:
    history_window: 3    # Previous conferences to include

  llm_as_judge:
    max_optimization_iterations: 10
    analyst_model: "gemini/gemini-2.5-flash"
    judge_model: "gemini/gemini-2.5-pro"
```

**No code changes needed** - just edit YAML!

---

## Complete Analysis Pipeline

### Option 1: Full Automated Pipeline (EASIEST)

```r
# Run everything with one command (15-25 hours total)
source("00run_complete_pipeline.R")
```

This executes all 6 stages automatically:
1. ✓ Baseline MPU index (5 min)
2. ✓ Data preparation (30-60 min)
3. ✓ LLM models (10-20 hours)
4. ✓ Main analysis (10 min)
5. ✓ Robustness tests (1-2 hours)
6. ✓ Out-of-sample validation (1-2 hours)

### Option 2: Stage-by-Stage Execution

```r
# Stage 1: Baseline MPU index (5 minutes)
source("01create_MPU.R")
source("02plot_MPU_and_compare_with_MP_surprises.R")
source("03appendix_run_exogeneity_tests_MPU.R")

# Stage 2: Data preparation (30-60 minutes)
source("04scraping_ecb_pressconf.R")         # Downloads ECB transcripts
source("05calculate_complexity_documents.R")  # Document complexity metrics
source("calendar_us_releases.R")             # Optional: US release calendar

# Stage 3: Run all LLM models (10-20 hours) ⏰
source("07run_all_models_sequential.R")

# Stage 4: Main analysis (10 minutes)
source("08clean_llm_result.R")               # Parse LLM outputs
source("09plot_llm_results.R")               # ⭐ MAIN RESULTS & VALIDATION

# Stage 5: Robustness tests (1-2 hours)
source("12run_bootstrap_robustness.R")
source("14run_prompt_stability_test_robustness.R")
source("15run_model_stability_test_robustness.R")
source("16run_counterfactual_exercise.R")

# Stage 6: Out-of-sample validation (1-2 hours)
source("17run_real_oos_test.R")
```

**Total runtime:** ~15-25 hours
**Total cost:** ~$40-65

### Option 3: Minimal Replication (Core Results Only)

```r
source("01create_MPU.R")                     # 3 min
source("07run_all_models_sequential.R")      # 10-20 hours ⏰
source("08clean_llm_result.R")               # 3 min
source("09plot_llm_results.R")               # 5 min ⭐ MAIN RESULT
```

**Total runtime:** ~10-20 hours
**Total cost:** ~$30-40

---

## Key Results

**Main Finding:** LLM-generated disagreement correlates significantly with market-based monetary policy uncertainty.

**Main validation:** Script `09plot_llm_results.R` produces:
- Spearman correlation: LLM disagreement vs OIS volatility
- Time series comparison plots (overall and rolling correlations)
- Robustness across tenors (3M, 2Y, 10Y)
- Forecast error analysis

**Output location:** `../output/figures/prompt_naive/`

---

## Troubleshooting

### "GEMINI_API_KEY not found"
```r
file.exists(".Renviron")  # Should be TRUE
# If FALSE, create it:
file.copy(".Renviron.example", ".Renviron")
# Then edit .Renviron and add your key
# Restart R session
```

### Rate limit errors (429)
Edit `config/model_config.yaml`:
```yaml
parallel_workers: 3  # Reduce from 5
```

Or add delays between requests in the code.

### Test on subset first
To test the pipeline on a small subset before full run:

Edit `src/run_model.R` around line ~133:
```r
dates_ecb_presconf <- dates_ecb_presconf[1:3]  # Test with 3 conferences
```

This reduces runtime to ~10 minutes and cost to ~$1-2.

### Data file not found errors
1. Verify all files are in `../raw_data/` directory
2. Check file names match exactly (case-sensitive on Linux/Mac)
3. See "Data Requirements" section above for download sources

### Font warnings
If you see warnings about "Segoe UI" font:
- **Windows:** Font is pre-installed, warnings are harmless
- **Mac/Linux:** Place `segoeui.ttf` in `code/` directory, or ignore (will use default font)

**See `VALIDATION_CHECKLIST.md` for complete testing protocol.**

---

## Key Innovation: Unified Model System

**Before:** Three separate branches, manual switching, scattered configuration

**After:** One codebase, YAML-driven configuration, seamless model switching

```r
# Change one line in config/model_config.yaml:
active_model: "historical_surprise"

# Then run:
source("src/run_model.R")
run_model()  # Uses the model from config
```

All three models share:
- Same data pipeline
- Same API infrastructure
- Same output format
- Easy comparison

**Perfect for replication and publication.**

---

## Output Structure

All results are saved to:

```
../output/
├── figures/               # All visualizations
│   ├── prompt_naive/      # Main model results
│   ├── oos_jan2025/       # Out-of-sample validation
│   └── ...
└── tables/                # LaTeX/Excel tables

../intermediate_data/
├── gemini_result/         # Raw LLM responses
├── aggregate_gemini_result/ # Cleaned LLM data
├── texts/                 # ECB transcripts
└── range_difference_df.rds # Market volatility data
```

---

## Citation

```bibtex
@article{collodel2025interpreting,
  title={Interpreting the Interpreter: Can We Model Post-ECB Conferences Volatility with LLM Agents?},
  author={Collodel, Umberto},
  institution={Central Bank of Malta},
  year={2025}
}
```

---

## Documentation

- **`README.md`** - This file (overview and quick start)
- **`REPLICATION_GUIDE.md`** - Complete step-by-step instructions
- **`VALIDATION_CHECKLIST.md`** - Pre-run testing protocol

---

## Data Availability Statement

The code and methodology are freely available in this repository. Proprietary data sources (OIS rates from Refinitiv) are subject to licensing restrictions. Researchers seeking to replicate this study can:

1. **Access via institutional subscriptions** to Refinitiv Eikon or Bloomberg Terminal
2. **Contact the author** for data access for replication purposes (subject to data provider terms)
3. **Use alternative data sources** such as ECB Statistical Data Warehouse (results may differ slightly)

All other data (ECB press conference transcripts, meeting dates, macroeconomic projections) are freely available from public sources as documented above.

---

## Support

**Issues:** Please report issues or questions via the GitHub repository
**Contact:** Umberto Collodel - Central Bank of Malta
**Email:** [Include your email if appropriate]

---

## License

This code is provided for academic research and replication purposes. Please cite the paper if you use this code.

---

**Last Updated:** November 2025 | **Version:** 2.0 (Unified System)
