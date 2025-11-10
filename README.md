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

### 2. Run Models (10-20 hours total)
```r
# Run all three model variants sequentially
source("run_all_models_sequential.R")
```

**OR** run individual models:
```r
source("src/run_model.R")
run_model(model_name = "naive")                # 2-4 hours
run_model(model_name = "historical_surprise")  # 2-4 hours
run_model(model_name = "llm_as_judge")        # 6-12 hours
```

### 3. Analyze Results
```r
source("08clean_llm_result.R")    # Parse outputs
source("09plot_llm_results.R")    # Visualizations
source("010compare_llm_ois.R")    # Main validation
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
├── config/                      # Model configurations
│   ├── model_config.yaml        # Parameters for all models
│   └── prompts.R                # All prompt templates
│
├── src/                         # Core unified system
│   ├── run_model.R              # Main entry point
│   └── llm_api/
│       ├── gemini_api.R         # R API functions
│       └── llm_optimizer.py     # Python meta-learning
│
├── run_all_models_sequential.R # Run all models (easiest)
│
├── Analysis pipeline:
│   ├── 01create_MPU.R           # Monetary Policy Uncertainty index
│   ├── 08clean_llm_result.R     # Parse LLM outputs
│   ├── 09plot_llm_results.R     # Visualizations
│   └── 010compare_llm_ois.R     # Main validation (LLM vs market)
│
├── Robustness tests:
│   ├── 12run_bootstrap_robustness.R
│   ├── 13run_recalling_robustness.R
│   ├── 14run_prompt_stability_test_robustness.R
│   ├── 15run_model_stability_test_robustness.R
│   ├── 16run_counterfactual_exercise.R
│   └── 17run_real_oos_test.R
│
└── Documentation:
    ├── REPLICATION_GUIDE.md     # Full instructions
    ├── VALIDATION_CHECKLIST.md  # Testing protocol
    └── SETUP.md                 # Detailed setup guide
```

---

## Requirements

### Software
- **R** ≥ 4.2.0
- **Python** ≥ 3.8 (only for llm_as_judge model)
- **Google Gemini API** key ([get one free](https://makersuite.google.com/app/apikey))

### R Packages (auto-installed)
```r
tidyverse, yaml, gemini.R, httr2, readtext, crayon,
stringr, purrr, future, furrr, ggplot2, boot
```

### Python Packages
```bash
pip install -r requirements.txt
```

### Data Files
Place in `../raw_data/`:
- `daily_OIS_updated15Sept_2025..xls` - Daily OIS rates
- `dates_govc.xlsx` - ECB meeting dates

### Cost Estimate
~$30-50 for complete replication (all three models + robustness tests)

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

### Full Replication (All Models + Robustness)

```r
# Stage 1: Baseline MPU index (5 minutes)
source("01create_MPU.R")
source("02plot_MPU_and_compare_with_MP_surprises.R")
source("03appendix_run_exogeneity_tests_MPU.R")

# Stage 2: Data preparation (skip if you have transcript files)
source("04scraping_ecb_pressconf.R")  # 30-60 min
source("05calculate_complexity_documents.R")  # 5 min

# Stage 3: Run all LLM models (10-20 hours)
source("run_all_models_sequential.R")

# Stage 4: Main analysis (10 minutes)
source("08clean_llm_result.R")
source("09plot_llm_results.R")
source("010compare_llm_ois.R")  # ⭐ MAIN VALIDATION RESULT

# Stage 5: Robustness tests (30-60 minutes)
source("12run_bootstrap_robustness.R")
source("13run_recalling_robustness.R")
source("14run_prompt_stability_test_robustness.R")
source("15run_model_stability_test_robustness.R")

# Stage 6: Extensions (2-3 hours)
source("16run_counterfactual_exercise.R")
source("17run_real_oos_test.R")
```

**Total runtime:** ~15-25 hours
**Total cost:** ~$40-60

### Minimal Replication (Core Results Only)

```r
source("01create_MPU.R")           # 3 min
source("run_all_models_sequential.R")  # 10-20 hours
source("08clean_llm_result.R")     # 3 min
source("010compare_llm_ois.R")     # 5 min ⭐ MAIN RESULT
```

**Total runtime:** ~10-20 hours
**Total cost:** ~$30-40

---

## Key Results

**Main Finding:** LLM-generated disagreement correlates significantly with market-based monetary policy uncertainty.

**Validation:** Script `010compare_llm_ois.R` produces:
- Spearman correlation: LLM disagreement vs OIS volatility
- Time series comparison plots
- Robustness across tenors (3M, 2Y, 10Y)

---

## Troubleshooting

### "GEMINI_API_KEY not found"
```r
file.exists(".Renviron")  # Should be TRUE
# If FALSE, create it:
writeLines("GEMINI_API_KEY=your_key", ".Renviron")
# Restart R
```

### Rate limit errors (429)
Edit `config/model_config.yaml`:
```yaml
parallel_workers: 3  # Reduce from 5
```

### Test on subset first
Edit `src/run_model.R` line ~133:
```r
dates_ecb_presconf <- dates_ecb_presconf[1:3]  # Test with 3 conferences
```

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

- **`REPLICATION_GUIDE.md`** - Complete step-by-step instructions
- **`VALIDATION_CHECKLIST.md`** - Pre-run testing protocol
- **`SETUP.md`** - Detailed environment setup
- **`MERGE_COMPLETE.md`** - Technical details of unified system

---

## Support

**Issues:** [GitHub Issues](https://github.com/yourrepo/issues)
**Contact:** Umberto Collodel - Central Bank of Malta

---

## License

Academic research use only. See paper for terms.

---

**Last Updated:** November 2025 | **Version:** 2.0 (Unified System)
