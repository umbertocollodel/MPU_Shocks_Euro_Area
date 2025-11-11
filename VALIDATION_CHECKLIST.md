# Validation Checklist for Unified Model System

## Pre-Run Checklist

### ✅ Environment Setup
- [ ] `.Renviron` file created with `GEMINI_API_KEY`
- [ ] R version >= 4.2.0
- [ ] Python >= 3.8 (if running llm_as_judge)
- [ ] Required R packages will auto-install on first run
- [ ] Python packages installed: `pip install -r requirements.txt` (if needed)

### ✅ Data Requirements
- [ ] `../intermediate_data/texts/` directory exists with ECB transcripts
- [ ] `../intermediate_data/range_difference_df.rds` exists (for historical_surprise)
- [ ] Output directories will be created automatically

### ✅ File Structure Verification

```
code/
├── config/
│   ├── model_config.yaml ✓
│   └── prompts.R ✓
├── src/
│   ├── llm_api/
│   │   ├── gemini_api.R ✓
│   │   └── llm_optimizer.py ✓
│   └── run_model.R ✓
├── 07run_all_models_sequential.R ✓
├── requirements.txt ✓
├── REPLICATION_GUIDE.md ✓
└── .Renviron (YOU MUST CREATE THIS!)
```

---

## Model Testing Protocol

### Test 1: Naive Model (Shortest runtime)

**Recommended: Test on subset first**

```r
# In R console
source("src/run_model.R")

# Modify for subset testing (optional):
# Edit src/run_model.R line ~133 to add:
# dates_ecb_presconf <- dates_ecb_presconf[1:3]  # Test with 3 conferences

run_model(model_name = "naive")
```

**Expected output:**
- Directory created: `../intermediate_data/gemini_result/prompt_naive/`
- Files created: One `.rds` file per conference
- Console: Green ✅ messages for each successful conference
- Summary at end showing success rate

**Time estimate:** 3 conferences = ~2-5 minutes | Full dataset = 2-4 hours

**Success criteria:**
- [ ] No API key errors
- [ ] At least 1 conference processed successfully
- [ ] Output files created in correct directory
- [ ] No syntax errors

---

### Test 2: Historical Surprise Model

**Prerequisites:**
- [ ] `range_difference_df.rds` exists
- [ ] Test 1 (Naive) completed successfully

```r
run_model(model_name = "historical_surprise")
```

**Expected output:**
- Directory: `../intermediate_data/gemini_result/prompt_history_surprises/`
- Files: One `.rds` file per conference
- Console: Messages show "with historical context"

**Time estimate:** Similar to Naive model

**Success criteria:**
- [ ] Historical data loaded without errors
- [ ] Output includes historical context in prompts (check one .rds file)
- [ ] Results differ from naive model (as expected)

---

### Test 3: LLM-as-Judge Model (Most complex)

**Prerequisites:**
- [ ] Python packages installed: `pip install -r requirements.txt`
- [ ] Tests 1 & 2 completed successfully
- [ ] More API budget available (uses more calls)

```r
run_model(model_name = "llm_as_judge")
```

**Expected output:**
- Python script launched from R
- Progress bars in console
- Multiple iterations of optimization
- Final output in: `../intermediate_data/aggregate_gemini_result/judge_llm/`

**Time estimate:** Full run = 6-12 hours (can reduce iterations in config)

**Success criteria:**
- [ ] Python script launches without errors
- [ ] Iterative optimization progresses
- [ ] Final optimized prompt saved
- [ ] Performance metrics generated

---

## Common Issues & Quick Fixes

### Issue: "GEMINI_API_KEY not found"
```r
# Check if .Renviron exists
file.exists(".Renviron")

# If FALSE, create it:
writeLines("GEMINI_API_KEY=your_key_here", ".Renviron")

# Then restart R
```

### Issue: "File not found: config/model_config.yaml"
```bash
# Ensure you're in the code/ directory
getwd()  # Should end in .../code

# If not, set it:
setwd("path/to/code/")
```

### Issue: Rate limit errors (429)
```yaml
# Edit config/model_config.yaml
parallel_workers: 3  # Reduce from 5 to 3
```

### Issue: Python not found
```bash
# Windows
where python

# Linux/Mac
which python3

# If not found, install Python 3.8+
```

---

## Validation Tests Passed

### Minimal Validation (Required)
- [ ] Naive model runs on 3 conferences without errors
- [ ] Output files created in correct location
- [ ] No API authentication errors

### Full Validation (Recommended)
- [ ] Naive model completes full dataset
- [ ] Historical surprise model runs successfully
- [ ] Both models produce different outputs (as expected)

### Complete Validation (Optional)
- [ ] All three models run successfully
- [ ] LLM-as-judge optimization completes
- [ ] Results can be compared using analysis scripts

---

## Quick Start Command Summary

```r
# OPTION 1: Run all models sequentially (easiest)
source("07run_all_models_sequential.R")  # Runs all three models

# OPTION 2: Direct command (run individual model)
source("src/run_model.R")
run_model(model_name = "naive")

# OPTION 3: Command line
# From terminal:
Rscript src/run_model.R --model naive
```

---

## Post-Run Verification

### Check Outputs

```r
# List output files
list.files("../intermediate_data/gemini_result/prompt_naive/")

# Load one result
sample_result <- readRDS("../intermediate_data/gemini_result/prompt_naive/2024-01-15.rds")
cat(sample_result)  # Should show markdown table
```

### Compare Models

```r
# Run comparison scripts
source("11compare_prompts.R")
source("09plot_llm_results.R")
```

---

## Support

If validation fails:
1. Check error message carefully
2. Review [REPLICATION_GUIDE.md](REPLICATION_GUIDE.md) Troubleshooting section
3. Verify all prerequisites met
4. Try with subset of data first (3 conferences)

---

## Sign-off

Validated by: _________________
Date: _________________
Models tested: [ ] Naive [ ] Historical Surprise [ ] LLM-as-Judge
Issues encountered: _________________
