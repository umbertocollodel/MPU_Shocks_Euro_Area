# Setup Instructions

This document provides step-by-step instructions for setting up the environment to replicate the analyses in both papers.

## Table of Contents
- [Quick Start](#quick-start)
- [Detailed Setup](#detailed-setup)
- [Troubleshooting](#troubleshooting)

## Quick Start

### For Paper 1 Only (MPU Index - R only, no API)

```r
# 1. Install R (4.0.0 or higher)
# 2. Open R/RStudio and run:
if (!require("pacman")) install.packages("pacman")

# 3. Navigate to code/ directory and run:
source("01create_MPU.R")  # Packages will auto-install via pacman
source("02plot_MPU_and_compare_with_MP_surprises.R")
```

### For Paper 2 (LLM Validation - R + Python + API)

```bash
# 1. Set up R environment (see detailed instructions below)
# 2. Set up Python environment:
pip install -r requirements.txt

# 3. Set up API keys:
cp .Renviron.example .Renviron
# Edit .Renviron and add your Gemini API key

# 4. Run the pipeline (from code/ directory)
Rscript 01create_MPU.R
Rscript 07running_llm_docs.R
# ... etc (see README.md for full pipeline)
```

---

## Detailed Setup

### 1. System Requirements

#### Minimum Requirements:
- **OS**: Windows 10/11, macOS 10.14+, or Linux (Ubuntu 20.04+)
- **RAM**: 8GB (16GB recommended)
- **Storage**: 5GB free space
- **Internet**: Stable connection for API calls and package downloads

#### Software Requirements:
- **R**: Version 4.0.0 or higher
  - Download from: https://cran.r-project.org/
- **RStudio** (Optional but recommended):
  - Download from: https://posit.co/downloads/
- **Python**: Version 3.8 or higher (only for Paper 2)
  - Download from: https://www.python.org/downloads/

---

### 2. R Environment Setup

#### Option A: Automatic Package Management (Recommended)

All R scripts use `pacman` for automatic package installation. Simply run any script and packages will be installed automatically on first use.

```r
# Packages will be installed automatically when you run:
source("01create_MPU.R")
```

#### Option B: Using renv for Reproducible Environments (Advanced)

For exact package version control:

```r
# Install renv
install.packages("renv")

# Initialize renv in the project
setwd("path/to/Uncertainty_surprises/code/")
renv::init()

# Install all required packages
renv::restore()  # If renv.lock exists
# OR run a script to let pacman install packages, then:
renv::snapshot()  # Save the current package versions
```

**Benefits of renv**:
- Locks package versions for reproducibility
- Isolates project dependencies from your global R library
- Creates `renv.lock` file that can be shared with others

#### Manual Package Installation (If Needed)

```r
# Core packages for Paper 1
install.packages(c(
  "tidyverse", "dplyr", "readxl", "writexl", "zoo", "ggplot2",
  "purrr", "stringr", "padr", "DescTools", "corrr", "stargazer",
  "Hmisc", "broom", "RColorBrewer", "showtext", "patchwork"
))

# Additional packages for Paper 2
install.packages(c(
  "gemini.R", "httr2", "readtext", "cli", "crayon", "future", "furrr",
  "rvest", "RSelenium", "chromote", "quanteda", "boot", "lubridate",
  "openai"  # Optional, for script 16
))
```

---

### 3. Python Environment Setup (Paper 2 Only)

#### Option A: Using pip (Simple)

```bash
# Navigate to code/ directory
cd path/to/Uncertainty_surprises/code/

# Install dependencies
pip install -r requirements.txt
```

#### Option B: Using Virtual Environment (Recommended)

```bash
# Create virtual environment
python -m venv venv

# Activate virtual environment
# On Windows:
venv\Scripts\activate
# On macOS/Linux:
source venv/bin/activate

# Install dependencies
pip install -r requirements.txt

# When done, deactivate:
deactivate
```

#### Option C: Using Conda

```bash
# Create conda environment
conda create -n uncertainty python=3.10

# Activate environment
conda activate uncertainty

# Install dependencies
pip install -r requirements.txt
```

---

### 4. API Key Configuration

#### Required API Keys

**For Paper 2 (LLM Validation)**:
- **Google Gemini API** (Required for scripts 07-17)
  - Sign up: https://makersuite.google.com/app/apikey
  - Free tier available, but paid tier recommended for full replication
  - Cost estimate: $20-30 for full pipeline

**Optional API Keys**:
- **OpenAI API** (Optional, only for script 16 - counterfactual experiments)
  - Sign up: https://platform.openai.com/api-keys
  - Cost estimate: $10-20 for counterfactuals
- **Anthropic API** (Optional, only for script 15 - cross-model validation)
  - Sign up: https://console.anthropic.com/

#### Setting Up API Keys

1. **Copy the template file**:
```bash
cp .Renviron.example .Renviron
```

2. **Edit `.Renviron` and add your keys**:
```bash
# On Windows (using notepad):
notepad .Renviron

# On macOS/Linux (using nano):
nano .Renviron
```

3. **Add your actual API key**:
```
GEMINI_API_KEY=AIzaSyXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
```

4. **Restart R/RStudio** for changes to take effect

5. **Verify API key is loaded**:
```r
Sys.getenv("GEMINI_API_KEY")  # Should print your key (not empty)
```

**Security Reminder**:
- `.Renviron` is in `.gitignore` and will NOT be committed to git
- NEVER share your API keys publicly
- NEVER commit `.Renviron` to version control

---

### 5. Data Setup

#### Required Data Files

Place these files in `../raw_data/` (one level up from `code/`):

1. **daily_OIS_updated15Sept_2025..xls**
   - Daily OIS rates for multiple tenors
   - Source: [Contact authors or obtain from data provider]

2. **dates_govc.xlsx**
   - ECB Governing Council meeting dates
   - Source: [ECB website or contact authors]

#### Optional Data Files

For script `calendar_us_releases.R`, place US economic release files in `../raw_data/us_releases/`:
- `release_dates_50.xlsx` (Employment Situation)
- `release_dates_10.xlsx` (CPI)
- `release_dates_53.xlsx` (GDP)
- `release_dates_9.xlsx` (Retail Sales)

#### Directory Structure Verification

Your directory structure should look like:
```
Uncertainty_surprises/
├── code/                      # Scripts (current directory)
│   ├── .Renviron              # Your API keys (created from .Renviron.example)
│   ├── .gitignore             # Git ignore file
│   ├── README.md              # Main documentation
│   ├── SETUP.md               # This file
│   ├── 01create_MPU.R         # Scripts...
│   └── ...
├── raw_data/                  # Place input data here
│   ├── daily_OIS_updated15Sept_2025..xls
│   ├── dates_govc.xlsx
│   └── us_releases/           # Optional
├── intermediate_data/         # Auto-created by scripts
└── output/                    # Auto-created by scripts
```

---

### 6. Font Setup (Optional)

Scripts `01create_MPU.R` and `09plot_llm_results.R` use the "Segoe UI" font for visualizations.

#### Windows
- Usually pre-installed, no action needed

#### macOS/Linux
1. Download Segoe UI font from Microsoft or use alternative
2. Place `segoeui.ttf` in the `code/` directory
3. OR modify scripts to use a different font (e.g., "Arial", "Helvetica")

If font is not available, scripts will print a warning and use default font.

---

### 7. Web Scraping Setup (Optional)

Script `04scraping_ecb_pressconf.R` requires a web browser driver.

#### Option A: Use Pre-Scraped Data (Recommended)
If transcript files already exist in `../intermediate_data/texts/`, skip web scraping.

#### Option B: Run Web Scraping
```r
# Install Chrome or Chromium browser
# Install chromedriver (https://chromedriver.chromium.org/)

# Or use RSelenium approach (see script 04 comments)
```

---

## Verification Checklist

Before running the full pipeline, verify:

- [ ] R version 4.0.0 or higher installed
- [ ] Python 3.8+ installed (for Paper 2)
- [ ] All required packages installed (or pacman configured)
- [ ] API keys added to `.Renviron` (for Paper 2)
- [ ] Data files placed in `../raw_data/`
- [ ] Currently in `code/` directory when running scripts
- [ ] At least 5GB free disk space
- [ ] Stable internet connection (for API calls)

---

## Quick Test

Run this to test your setup:

```r
# Test R environment
setwd("path/to/Uncertainty_surprises/code/")

# Test 1: Check packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl)
cat("✓ Basic R packages working\n")

# Test 2: Check data files
if (file.exists("../raw_data/dates_govc.xlsx")) {
  cat("✓ Data directory accessible\n")
} else {
  cat("✗ Data files not found\n")
}

# Test 3: Check API key (for Paper 2)
if (Sys.getenv("GEMINI_API_KEY") != "") {
  cat("✓ API key loaded\n")
} else {
  cat("! No API key (OK if only running Paper 1)\n")
}
```

---

## Troubleshooting

### Common Issues

#### "Cannot find package X"
```r
# Solution: Install manually
install.packages("package_name")
```

#### "Cannot find data file"
```
Error: '../raw_data/dates_govc.xlsx' not found
```
**Solution**:
- Verify you're running scripts from `code/` directory
- Check that data files are in `../raw_data/`

#### "GEMINI_API_KEY not found"
```r
# Solution 1: Verify .Renviron file exists and contains key
file.exists(".Renviron")
readLines(".Renviron")

# Solution 2: Restart R/RStudio after editing .Renviron

# Solution 3: Set key manually in R session (temporary)
Sys.setenv(GEMINI_API_KEY = "your_key_here")
```

#### "API quota exceeded" or "Rate limit"
**Solution**:
- Wait a few minutes and retry
- Check your API usage dashboard
- Consider upgrading API plan
- Reduce parallel workers in script 07 (from 5 to 2-3)

#### "Font not found" warning
**Solution**:
- Place `segoeui.ttf` in `code/` directory
- OR ignore warning (will use default font)
- OR modify scripts to use different font

#### Scripts running slowly
**Solution**:
- Reduce parallel workers in script 07:
  ```r
  plan(multisession, workers = 2)  # Instead of 5
  ```
- Close other applications to free RAM
- Check internet connection for API calls

---

## Getting Help

If you encounter issues:

1. Check the main [README.md](README.md) for detailed script documentation
2. Review script headers for specific requirements
3. Check git issues: https://github.com/[your-repo]/issues (if applicable)
4. Contact authors (see README.md)

---

## Next Steps

Once setup is complete:
1. See [README.md](README.md) for execution order
2. Start with Paper 1 scripts (01-03) to verify R environment
3. Then proceed to Paper 2 pipeline if needed

Good luck with your replication!
