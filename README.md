# Monetary Policy Uncertainty and LLM-Based Validation

This repository contains replication code for **two connected research papers**:

1. **"MPU Shocks in the Euro Area"** - Central Bank of Malta Working Paper WP-01-2025 [(PDF)](https://www.centralbankmalta.org/site/Publications/Economic%20Research/2025/WP-01-2025.pdf?revcount=6302)
   - Constructs the Monetary Policy Uncertainty (MPU) index from OIS data
   - Analyzes uncertainty shocks around ECB Governing Council meetings

2. **"Interpreting the Interpreter: Can We Model post-ECB Conferences Volatility with LLM Agents?"** - arXiv preprint
   - Validates the MPU index using Large Language Model simulations
   - Simulates 30 heterogeneous trader interpretations of ECB press conferences
   - Tests whether LLM-derived disagreement correlates with market-based uncertainty

## Table of Contents

- [Overview](#overview)
- [Project Structure](#project-structure)
- [Setup Instructions](#setup-instructions)
- [Scripts Overview by Paper](#scripts-overview-by-paper)
- [Data Requirements](#data-requirements)
- [Execution Order](#execution-order)
- [Computational Requirements](#computational-requirements)
- [Citation](#citation)

## Overview

### Connection Between Papers

**Paper 1 (MPU Index)** develops a novel measure of monetary policy uncertainty based on high-low ranges of Euro area OIS rates around ECB meetings. **Paper 2 (LLM Validation)** uses Large Language Models to simulate how heterogeneous traders interpret ECB press conferences, providing an independent validation of the uncertainty measure through textual analysis rather than just market prices.

**Shared Components**:
- Both papers use the same MPU index (scripts 01-03)
- Both analyze ECB press conference transcripts (scripts 04-05)
- The LLM paper extends the MPU paper's methodology with AI-based validation

## Project Structure

```
Uncertainty_surprises/
├── code/                          # Scripts (current directory)
├── raw_data/                      # Original data files (user-provided)
│   ├── daily_OIS_updated15Sept_2025..xls
│   └── dates_govc.xlsx
├── intermediate_data/             # Processing outputs
│   ├── texts/                     # ECB press conference transcripts
│   ├── gemini_result/             # Raw LLM outputs by prompt type
│   ├── aggregate_gemini_result/   # Processed LLM results
│   ├── counterfactual_cross_llm/  # Counterfactual experiments
│   ├── cross_llm_analysis/        # Cross-LLM validation
│   └── robustness_analysis/       # Robustness test outputs
└── output/                        # Final figures and tables
```

## Setup Instructions

### 1. R Environment

**Required R Version**: 4.0.0 or higher

**Install Required Packages**:
```r
# The scripts use pacman for automatic package management
if (!require("pacman")) install.packages("pacman")

# Core packages (automatically installed by scripts):
# tidyverse, dplyr, readxl, writexl, zoo, ggplot2, purrr, stringr,
# gemini.R, httr2, readtext, cli, crayon, showtext, future, furrr,
# rvest, RSelenium, chromote, quanteda, boot, Hmisc, DescTools, broom
```

**Recommended**: Use `renv` for dependency management:
```r
install.packages("renv")
renv::init()
```

### 2. Python Environment (LLM Paper Only)

**Required Python Version**: 3.8 or higher

**Install Required Packages**:
```bash
pip install pandas numpy
```

### 3. API Access (LLM Paper Only)

**Google Gemini API**: Required for LLM simulation (scripts 07-17)

1. Obtain API key from [Google AI Studio](https://makersuite.google.com/app/apikey)
2. Create a `.Renviron` file in the `code/` directory (see `.Renviron.example`)
3. Add your key:
   ```
   GEMINI_API_KEY=your_actual_api_key_here
   ```

**Cost Estimate**: Approximately $20-50 for a full LLM pipeline run (depends on API pricing)

### 4. Data Files

Place the following files in `../raw_data/`:
- `daily_OIS_updated15Sept_2025..xls` - Daily OIS rates for multiple tenors
- `dates_govc.xlsx` - ECB Governing Council meeting dates

**Note**: Contact the authors if you need access to the raw data files.

## Scripts Overview by Paper

### 📊 Paper 1: MPU Shocks in the Euro Area (CBM WP-01-2025)

These scripts construct and validate the market-based MPU index:

#### **01create_MPU.R** ⭐ CORE
- **Purpose**: Calculates the Monetary Policy Uncertainty (MPU) index from raw daily OIS data
- **Inputs**: `../raw_data/daily_OIS_updated15Sept_2025..xls`, `dates_govc.xlsx`
- **Outputs**: MPU time series for multiple yield curve tenors (3M, 2Y, 10Y, etc.)
- **Runtime**: ~2-3 minutes
- **Key Operations**:
  - Loads daily OIS data for multiple maturities
  - Identifies 2-day windows around ECB Governing Council meetings
  - Calculates high-low range before/after meetings as uncertainty measure
  - Computes difference in ranges as MPU shock

#### **02plot_MPU_and_compare_with_MP_surprises.R**
- **Purpose**: Generates visualizations comparing MPU with monetary policy surprises
- **Inputs**: Output from 01create_MPU.R
- **Outputs**: Figures saved to `../output/`
- **Runtime**: ~1 minute
- **Key Visualizations**: Time series plots, correlation heatmaps, event studies

#### **03appendix_run_exogeneity_tests_MPU.R**
- **Purpose**: Tests exogeneity of MPU measure (checks for autocorrelation issues)
- **Inputs**: MPU data from script 01
- **Outputs**: Statistical test results and diagnostic plots
- **Runtime**: ~2 minutes
- **Tests**: Ljung-Box tests, ACF/PACF analysis

---

### 🤖 Paper 2: Interpreting the Interpreter (arXiv)

These scripts use LLMs to validate the MPU index through textual analysis:

#### **Shared Data Collection Scripts** (Used by Both Papers)

##### **04scraping_ecb_pressconf.R**
- **Purpose**: Web scraping of ECB press conference transcripts from 1998-2025
- **Inputs**: ECB website URLs (automated)
- **Outputs**: `../intermediate_data/texts/*.txt` (one file per conference)
- **Runtime**: ~30-60 minutes (depends on internet speed)
- **Dependencies**: Chrome browser, RSelenium or chromote
- **Note**: Can be skipped if transcript files already exist

##### **05calculate_complexity_documents.R**
- **Purpose**: Calculates text complexity metrics for each press conference
- **Inputs**: `../intermediate_data/texts/*.txt`
- **Outputs**: Readability scores, lexical diversity metrics
- **Runtime**: ~5 minutes
- **Metrics**: Flesch reading ease, average sentence length, vocabulary complexity

#### **LLM Prompt Development**

##### **create_prompts.R**
- **Purpose**: Defines multiple LLM prompt variations for trader simulation
- **Inputs**: None (defines prompt templates)
- **Outputs**: Prompt objects loaded in memory for other scripts
- **Prompt Types**:
  - `prompt_naive`: Basic trader simulation without additional context
  - `prompt_anchor`: Includes pre-meeting rate anchoring
  - `prompt_microstructure`: Emphasizes market microstructure considerations
  - `prompt_history_surprises`: Incorporates historical surprise patterns

#### **LLM Simulation Pipeline**

##### **07running_llm_docs.R** ⚡ CORE SCRIPT
- **Purpose**: Runs Gemini LLM to simulate 30 heterogeneous trader reactions per conference
- **Inputs**:
  - `../intermediate_data/texts/*.txt` (press conferences)
  - `create_prompts.R` (prompt definitions)
  - `GEMINI_API_KEY` environment variable
- **Outputs**: `../intermediate_data/gemini_result/[prompt_name]/[date].rds`
- **Runtime**: ~2-4 hours (5 parallel workers)
- **Cost**: ~$20-30 per full run
- **Key Features**:
  - Parallel processing with 5 workers
  - Exponential backoff for failed requests
  - Generates predictions for 3 tenors (3M, 2Y, 10Y)
  - Each trader assigned unique risk profile and behavioral biases
- **Parameters**: Temperature=1, seed=1234 for reproducibility

##### **08clean_llm_result.R**
- **Purpose**: Post-processes raw LLM outputs into structured data
- **Inputs**: `../intermediate_data/gemini_result/*/` (RDS files)
- **Outputs**: Cleaned data frames with parsed trader predictions
- **Runtime**: ~3-5 minutes
- **Operations**: Parse markdown tables, extract numerical predictions, validate data

##### **run_last_prompt.py** (Python)
- **Purpose**: Executes best-performing prompt on training/test splits
- **Inputs**: `optimization_history_in_sample_training_20250731_124608.json`
- **Outputs**: JSON files with in-sample and out-of-sample LLM responses
- **Runtime**: ~1-2 hours
- **Note**: Requires LLM API wrapper functions (currently has placeholder imports)

#### **Analysis and Validation**

##### **09plot_llm_results.R**
- **Purpose**: Comprehensive visualizations of LLM-derived uncertainty measures
- **Inputs**: Cleaned LLM results from script 08
- **Outputs**: Distribution plots, time series, disagreement metrics
- **Runtime**: ~2-3 minutes

##### **010compare_llm_ois.R** ⭐ MAIN VALIDATION
- **Purpose**: Compares LLM-generated uncertainty with actual OIS-based MPU
- **Inputs**: MPU data (script 01) + LLM results (script 08)
- **Outputs**: Correlation analysis, regression tables, validation plots
- **Runtime**: ~3-5 minutes
- **Key Analysis**: Pearson/Spearman correlations, lagged effects, tenor-specific patterns
- **Paper Result**: Tests central hypothesis that textual disagreement predicts market uncertainty

##### **11compare_prompts.R**
- **Purpose**: Comparative analysis of different prompt specifications
- **Inputs**: LLM results from multiple prompt types
- **Outputs**: Prompt performance metrics, stability comparisons
- **Runtime**: ~2 minutes

#### **Robustness Testing**

##### **12run_bootstrap_robustness.R**
- **Purpose**: Bootstrap resampling for statistical robustness of correlations
- **Inputs**: MPU + LLM results
- **Outputs**: `../intermediate_data/robustness_analysis/bootstrap_results.rds`
- **Runtime**: ~10-15 minutes
- **Parameters**: 1000 bootstrap iterations

##### **13run_recalling_robustness.R**
- **Purpose**: Tests whether LLM "recalls" previously processed conferences (contamination test)
- **Inputs**: LLM results from multiple runs
- **Outputs**: Memory contamination test statistics
- **Runtime**: ~5 minutes

##### **14run_prompt_stability_test_robustness.R**
- **Purpose**: Evaluates stability of predictions across prompt variations
- **Inputs**: Results from all prompt types
- **Outputs**: Stability metrics, variance decomposition
- **Runtime**: ~5 minutes

##### **15run_model_stability_test_robustness.R**
- **Purpose**: Tests stability across different LLM models (Gemini, GPT, etc.)
- **Inputs**: Cross-model LLM results
- **Outputs**: Model agreement statistics
- **Runtime**: ~5 minutes (if cross-model data exists)

#### **Extensions and Out-of-Sample Testing**

##### **16run_counterfactual_exercise.R**
- **Purpose**: Counterfactual analysis - ChatGPT edits conferences, Gemini evaluates
- **Inputs**: Original press conferences + ChatGPT-modified versions
- **Outputs**: `../intermediate_data/counterfactual_cross_llm/`
- **Runtime**: ~1-2 hours (requires ChatGPT API)
- **Goal**: Assess whether clearer communication reduces disagreement

##### **17run_real_oos_test.R** 🔬 VALIDATION
- **Purpose**: True out-of-sample test on post-January 2025 ECB conferences
- **Inputs**: Post-cutoff press conferences
- **Outputs**: Out-of-sample correlation results
- **Runtime**: ~30 minutes
- **Importance**: Tests whether LLM-MPU relationship holds for unseen future data

#### **Auxiliary Scripts**

##### **calendar_us_releases.R**
- **Purpose**: Checks timing of US economic releases vs. ECB meetings
- **Inputs**: US release calendar data
- **Outputs**: Overlap analysis to control for confounding events
- **Runtime**: ~2 minutes

## Data Requirements

### Required Input Files (User Must Provide)

1. **`../raw_data/daily_OIS_updated15Sept_2025..xls`**
   - Daily OIS rates for multiple tenors (3M, 6M, 1Y, 2Y, 5Y, 10Y)
   - Format: Excel workbook with sheets for each tenor
   - Columns: `Timestamp`, `High`, `Low`, `First`, `Last`
   - Date Range: 1999-01-03 to 2025-09-15

2. **`../raw_data/dates_govc.xlsx`**
   - ECB Governing Council meeting dates
   - Format: Excel file with columns `day`, `month`, `year`
   - Date Range: 1998 to present

### Generated Intermediate Files

3. **`../intermediate_data/texts/*.txt`**
   - ECB press conference transcripts (generated by script 04)
   - One text file per conference date (e.g., `2024-01-25.txt`)
   - Can be pre-provided to skip scraping step

### API-Generated Files

4. **`../intermediate_data/gemini_result/[prompt_type]/[date].rds`**
   - Raw LLM outputs (generated by script 07)
   - RDS format containing structured trader predictions

## Execution Order

### Paper 1: MPU Shocks (CBM Working Paper)

To replicate **only** the MPU index construction and validation:

```r
# Calculate and visualize MPU index
source("01create_MPU.R")
source("02plot_MPU_and_compare_with_MP_surprises.R")

# Appendix: Exogeneity tests
source("03appendix_run_exogeneity_tests_MPU.R")
```

**Runtime**: ~5 minutes total
**Requirements**: R only, no API access needed

---

### Paper 2: Interpreting the Interpreter (arXiv)

To replicate the **full LLM validation pipeline**:

#### Minimal Pipeline (Core Results)

```r
# Stage 1: Calculate MPU index (from Paper 1)
source("01create_MPU.R")

# Stage 2: Get ECB transcripts (or use pre-scraped files)
source("04scraping_ecb_pressconf.R")  # Can skip if texts/ already populated

# Stage 3: Run LLM simulation
source("create_prompts.R")  # Load prompt definitions
source("07running_llm_docs.R")  # ~2-4 hours, requires Gemini API
source("08clean_llm_result.R")

# Stage 4: Main validation analysis
source("09plot_llm_results.R")
source("010compare_llm_ois.R")  # ⭐ MAIN VALIDATION RESULTS
```

**Runtime**: ~3-5 hours
**Requirements**: R + Gemini API access (~$20-30)

#### Full Pipeline (All Robustness Tests)

```r
# Minimal pipeline (above) +

# Additional analysis
source("05calculate_complexity_documents.R")
source("11compare_prompts.R")

# Robustness tests
source("12run_bootstrap_robustness.R")
source("13run_recalling_robustness.R")
source("14run_prompt_stability_test_robustness.R")
source("15run_model_stability_test_robustness.R")

# Extensions
source("16run_counterfactual_exercise.R")  # Requires ChatGPT API
source("17run_real_oos_test.R")
source("calendar_us_releases.R")
```

**Runtime**: ~6-10 hours
**Requirements**: R + Gemini API + optional ChatGPT API for script 16

---

### Script Dependencies (Visual)

```
Paper 1 (MPU Index):
01create_MPU.R → 02plot_MPU_and_compare_with_MP_surprises.R
               → 03appendix_run_exogeneity_tests_MPU.R

Paper 2 (LLM Validation):
01create_MPU.R ────────┐
                       ├→ 010compare_llm_ois.R (Main result)
04scraping → 05calc   │
          ↓            │
07running_llm ────────┘
          ↓
08clean_llm → 09plot_llm
           ↓
           └→ 11compare_prompts
           └→ 12-15 (robustness tests)
           └→ 16-17 (extensions)
```

## Computational Requirements

### Hardware Recommendations
- **RAM**: Minimum 8GB, recommended 16GB
- **CPU**: Multi-core processor (5+ cores recommended for parallel LLM processing)
- **Storage**: ~2GB for intermediate data, ~500MB for outputs
- **Internet**: Stable connection required for web scraping (script 04) and API calls (scripts 07, 16-17)

### Runtime Summary by Paper

**Paper 1 (MPU Index)**:
| Scripts | Total Time | Requirements |
|---------|------------|--------------|
| 01-03   | ~5 minutes | R only, local computation |

**Paper 2 (LLM Validation)**:
| Stage | Scripts | Total Time | Notes |
|-------|---------|------------|-------|
| Data Collection | 04-05 | ~35-65 minutes | One-time, can use cached files |
| LLM Simulation | 07-08 | ~2.5-4.5 hours | Depends on API speed, parallel workers |
| Analysis | 09-11 | ~7-10 minutes | Visualization and comparison |
| Robustness | 12-15 | ~25-30 minutes | Bootstrap is slowest component |
| Extensions | 16-17 | ~1.5-2.5 hours | Optional, requires additional API access |

**Total LLM Paper Runtime**: ~4-8 hours (excluding one-time web scraping)

### API Usage and Costs

**Script 07 (Main LLM simulation)**:
- ~300-500 API calls per prompt type (30 traders × 3 tenors × ~3-5 conferences per batch)
- Estimated cost: $20-30 per full run

**Script 16 (Counterfactual experiments)**:
- ~100-200 API calls (requires both ChatGPT and Gemini)
- Estimated cost: $10-20

**Total estimated cost for full replication**: $30-60

## Important Notes

### Running from Correct Directory
⚠️ **All scripts should be run from the `code/` directory** as they use relative paths (`../raw_data/`, `../intermediate_data/`, `../output/`)

```r
# Set working directory before running scripts
setwd("path/to/Uncertainty_surprises/code/")
```

### API Key Security
- Never commit `.Renviron` file to git (already in `.gitignore`)
- Revoke and regenerate API keys if accidentally exposed
- Monitor API usage to avoid unexpected charges

### Font Requirements
Scripts 01 and 09 use the "Segoe UI" font for visualizations. If this font is not available on your system:
- Windows: Usually pre-installed
- Mac/Linux: Download from Microsoft or use alternative font (modify scripts)

### Reproducibility Notes
- **LLM calls use `seed=1234`** for reproducibility, but results may still vary slightly due to API-side randomness and model updates
- Temperature parameter set to 1.0 to simulate diverse trader behavior (intentionally stochastic)
- Bootstrap results use `set.seed()` for deterministic resampling

### Known Issues
- `run_last_prompt.py` contains placeholder imports and requires additional function definitions to run independently
- Some robustness scripts (12, 14-17) contain `setwd()` calls that may need adjustment for your environment

## Citation

If you use this code, please cite the relevant paper(s):

### MPU Index (Paper 1):
```bibtex
@techreport{collodel2025mpu,
  title={MPU Shocks in the Euro Area},
  author={Collodel, Umberto},
  institution={Central Bank of Malta},
  type={Working Paper},
  number={WP-01-2025},
  year={2025},
  url={https://www.centralbankmalta.org/site/Publications/Economic%20Research/2025/WP-01-2025.pdf}
}
```

### LLM Validation (Paper 2):
```bibtex
@article{collodel2025interpreting,
  title={Interpreting the Interpreter: Can We Model post-ECB Conferences Volatility with LLM Agents?},
  author={Collodel, Umberto},
  journal={arXiv preprint arXiv:XXXX.XXXXX},
  year={2025}
}
```

## Contact

For questions, data access requests, or collaboration inquiries:
- **Umberto Collodel** - Central Bank of Malta
- GitHub Issues: Please report bugs or suggest improvements via GitHub issues

## License

This code is provided for academic research purposes. Please refer to the respective working papers for terms of use.
