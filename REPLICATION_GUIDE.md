# Replication Guide: Interpreting the Interpreter
## ECB Communication Analysis with Large Language Models

**Author:** Umberto Collodel
**Institution:** Central Bank of Malta
**Last Updated:** November 2025

---

## Table of Contents

1. [Overview](#overview)
2. [System Requirements](#system-requirements)
3. [Installation](#installation)
4. [Configuration](#configuration)
5. [Running Models](#running-models)
6. [Model Descriptions](#model-descriptions)
7. [Output Structure](#output-structure)
8. [Troubleshooting](#troubleshooting)
9. [Citation](#citation)

---

## Overview

This repository contains code to replicate three LLM-based models for analyzing ECB press conference communications:

1. **Naive Model** - Baseline prompt without historical context
2. **Historical Surprise Model** - Includes recent market volatility context
3. **LLM-as-Judge Model** - Meta-learning with iterative prompt optimization

All models are unified under a single configuration system for easy replication and comparison.

---

## System Requirements

### Required Software

- **R** (≥ 4.2.0)
- **Python** (≥ 3.8) - Only required for LLM-as-Judge model
- **Git** (for cloning the repository)

### R Packages

The following R packages will be automatically installed when you run the models:

```r
pacman, yaml, gemini.R, httr2, readtext, crayon, stringr,
purrr, readr, writexl, scales, showtext, readxl, tidyverse,
future, furrr, cli
```

### Python Packages (LLM-as-Judge only)

```bash
pip install -r requirements.txt
```

Or manually:
```bash
pip install numpy pandas scipy pyreadr litellm tqdm
```

### API Requirements

- **Google Gemini API Key** - Required for all models
  - Sign up at: https://makersuite.google.com/app/apikey
  - Free tier available with generous quotas
  - Estimated cost: ~$5-20 per full dataset run (depending on model)

---

## Installation

### 1. Clone the Repository

```bash
git clone <repository-url>
cd code
```

### 2. Set Up Environment Variables

Create a `.Renviron` file in the `code/` directory:

```bash
# Windows
echo GEMINI_API_KEY=your_api_key_here > .Renviron

# Linux/Mac
echo "GEMINI_API_KEY=your_api_key_here" > .Renviron
```

**Security Note:** Never commit `.Renviron` to version control. It's included in `.gitignore`.

### 3. Install Python Dependencies (Optional - LLM-as-Judge only)

```bash
pip install -r requirements.txt
```

### 4. Verify Data Structure

Ensure your directory structure looks like this:

```
code/
├── config/
│   ├── model_config.yaml      # Model configuration
│   └── prompts.R              # All prompt templates
├── src/
│   ├── llm_api/
│   │   ├── gemini_api.R       # R API functions
│   │   └── llm_optimizer.py   # Python meta-learning
│   └── run_model.R            # Main entry point
├── 07running_llm_docs.R       # Legacy entry point (backward compatible)
├── requirements.txt           # Python dependencies
└── .Renviron                  # API keys (create this!)
```

---

## Configuration

### Editing Model Parameters

All model parameters are defined in `config/model_config.yaml`:

```yaml
# Set which model to run by default
active_model: "naive"  # Options: naive, historical_surprise, llm_as_judge

models:
  naive:
    gemini_model: "2.5-flash"
    temperature: 1
    parallel_workers: 5
    # ... other parameters
```

**Common parameters to adjust:**

- `temperature` (0-2): Controls randomness. Higher = more diverse trader behavior
- `parallel_workers` (1-10): Number of parallel API calls. Adjust based on your API rate limits
- `seed`: For reproducibility
- `output_dir`: Where results are saved

---

## Running Models

### Method 1: Using the Legacy Script (Easiest)

1. Open `07running_llm_docs.R`
2. Edit line 34 to select your model:

```r
MODEL_TO_RUN <- "naive"  # Or "historical_surprise" or "llm_as_judge"
```

3. Run in R:

```r
source("07running_llm_docs.R")
```

### Method 2: Using the Unified Runner

From R console:

```r
source("src/run_model.R")
run_model(model_name = "naive")
```

From command line:

```bash
Rscript src/run_model.R --model naive
Rscript src/run_model.R --model historical_surprise
```

### Method 3: Python Model (LLM-as-Judge)

From R:
```r
source("src/run_model.R")
run_model(model_name = "llm_as_judge")
```

Or directly in Python:
```bash
python src/llm_api/llm_optimizer.py
```

---

## Model Descriptions

### 1. Naive Model (Baseline)

**Description:**
Simulates 30 heterogeneous traders reacting to ECB press conferences without any historical context.

**Use case:** Baseline comparison
**Runtime:** ~2-4 hours for full dataset (parallel)
**Output:** Individual trader predictions per conference

**Key parameters:**
```yaml
use_history: false
temperature: 1
parallel_workers: 5
```

---

### 2. Historical Surprise Model

**Description:**
Same as naive, but includes standard deviations from the previous 3 ECB press conferences to provide market context.

**Use case:** Test if historical volatility improves predictions
**Runtime:** ~2-4 hours for full dataset (parallel)
**Output:** Individual trader predictions with historical context

**Key parameters:**
```yaml
use_history: true
history_window: 3
history_data_path: "../intermediate_data/range_difference_df.rds"
```

**Requirements:**
- Must have `range_difference_df.rds` file containing historical volatility data
- Generated by earlier preprocessing scripts

---

### 3. LLM-as-Judge Model (Meta-Learning)

**Description:**
Two-LLM system where:
- **Analyst LLM** (Gemini Flash) generates predictions
- **Judge LLM** (Gemini Pro) critiques and refines the prompt iteratively

Optimizes for correlation between predicted and actual market volatility.

**Use case:** State-of-the-art performance with automated prompt engineering
**Runtime:** ~6-12 hours for optimization + validation
**Output:** Optimized prompt + final predictions

**Key parameters:**
```yaml
analyst_model: "gemini/gemini-2.5-flash"
judge_model: "gemini/gemini-2.5-pro"
max_optimization_iterations: 10
training_percentage: 0.75
```

**Requirements:**
- Python 3.8+
- Additional packages (litellm, pyreadr)
- More API calls = higher cost

---

## Output Structure

### R Models (Naive & Historical Surprise)

Results saved to: `../intermediate_data/gemini_result/<model_name>/`

```
├── 2024-01-15.rds       # Raw LLM output for each date
├── 2024-03-12.rds
├── ...
└── failed_requests.log  # Error log (if any failures)
```

Each `.rds` file contains the raw markdown table returned by the LLM.

### Python Model (LLM-as-Judge)

Results saved to: `../intermediate_data/aggregate_gemini_result/judge_llm/`

```
├── optimized_prompt.txt              # Final optimized prompt
├── optimization_history.json         # Full optimization trajectory
├── final_predictions.csv             # Aggregated predictions
└── performance_metrics.json          # Correlation scores
```

---

## Troubleshooting

### Issue: "GEMINI_API_KEY not found"

**Solution:**
Ensure `.Renviron` file exists in the `code/` directory with your API key:

```bash
GEMINI_API_KEY=your_actual_key_here
```

Then restart R to reload environment variables.

---

### Issue: "API Error: Status code 429"

**Cause:** Rate limit exceeded

**Solution:**
Reduce `parallel_workers` in `config/model_config.yaml`:

```yaml
parallel_workers: 3  # Lower value = fewer parallel requests
```

---

### Issue: Python script fails with "ModuleNotFoundError"

**Solution:**

```bash
pip install -r requirements.txt
```

Or install missing package individually:

```bash
pip install litellm
```

---

### Issue: "Historical data file not found"

**Cause:** Missing `range_difference_df.rds`

**Solution:**
Run the data preprocessing scripts first to generate historical volatility data:

```r
source("01create_MPU.R")
source("05processing_documents.R")
```

---

### Issue: Models take too long to run

**Solution:**
Test on a subset first by filtering `dates_ecb_presconf` in `src/run_model.R`:

```r
# Add after loading dates
dates_ecb_presconf <- dates_ecb_presconf[1:10]  # Test on first 10 conferences
```

---

## Sequential Replication (All Models)

To replicate all three models sequentially:

```r
# Set working directory
setwd("path/to/code/")

# Run Model 1: Naive
source("src/run_model.R")
run_model(model_name = "naive")

# Run Model 2: Historical Surprise
run_model(model_name = "historical_surprise")

# Run Model 3: LLM-as-Judge
run_model(model_name = "llm_as_judge")
```

**Total estimated time:** 10-20 hours (depending on dataset size and API speed)

---

## Comparing Models

After running all models, use the analysis scripts:

```r
source("11compare_prompts.R")    # Compare naive vs historical_surprise
source("09plot_llm_results.R")   # Visualize results
```

---

## Advanced Configuration

### Custom Experiment

Add to `config/model_config.yaml`:

```yaml
experiments:
  my_custom_experiment:
    base_model: "naive"
    modifications:
      temperature: 0.5
      parallel_workers: 10
      seed: 42
```

Then run:

```r
# Edit src/run_model.R to support custom experiments
# Or duplicate and modify a model configuration
```

---

## File Manifest

### Core Files
- `config/model_config.yaml` - All model configurations
- `config/prompts.R` - All prompt templates
- `src/run_model.R` - Main unified runner
- `src/llm_api/gemini_api.R` - R API functions
- `src/llm_api/llm_optimizer.py` - Python meta-learning

### Legacy Files (Backward Compatible)
- `07running_llm_docs.R` - Original entry point (now a wrapper)
- `create_prompts.R` - Old prompt file (superseded by config/prompts.R)

### Analysis Scripts
- `08clean_llm_result.R` - Parse LLM outputs
- `09plot_llm_results.R` - Visualize results
- `11compare_prompts.R` - Compare models

---

## Citation

If you use this code in your research, please cite:

```bibtex
@article{collodel2025interpreting,
  title={Interpreting the Interpreter: Analyzing Central Bank Communication with Large Language Models},
  author={Collodel, Umberto},
  journal={Journal Name},
  year={2025},
  institution={Central Bank of Malta}
}
```

---

## Support

For issues or questions:
1. Check the [Troubleshooting](#troubleshooting) section above
2. Review the code comments in `src/run_model.R`
3. Examine `config/model_config.yaml` for parameter details

---

## License

[Add your license information here]

---

**Last Updated:** November 2025
**Version:** 2.0 (Unified System)
