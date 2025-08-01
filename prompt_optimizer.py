import subprocess
import sys
import os
import json
import numpy as np
import pandas as pd
from scipy.stats import pearsonr, spearmanr
from datetime import datetime
import time
from typing import List, Dict, Any, Tuple, Optional
import re
import pyreadr
from concurrent.futures import ThreadPoolExecutor, as_completed

# ----- Set environment libraries: ----
def install(package):
    """Installs a pip package if not already installed."""
    try:
        subprocess.check_call([sys.executable, "-m", "pip", "install", package])
    except subprocess.CalledProcessError as e:
        print(f"Error installing {package}: {e}")
        print(f"Please try running 'pip install {package}' manually.")
        sys.exit(1) # Exit if essential package installation fails

# List of required packages
required_packages = {
    "numpy": "numpy",
    "pandas": "pandas",
    "scipy": "scipy",
    "pyreadr":"pyreadr",
    "litellm": "litellm"
}

# Install only the ones that are not built-in (os, json, datetime, time, typing, re are built-in)
for module, package in required_packages.items():
    if package:
        try:
            __import__(module)
        except ImportError:
            print(f"Installing {package}...")
            install(package)
            print(f"{package} installed.")

# --- Configuration ---
# Ensure you have your API keys set as environment variables, e.g.:
# export OPENAI_API_KEY="sk-..."
# export ANTHROPIC_API_KEY="sk-ant-..."
# export GOOGLE_API_KEY="AIza..."

# LLM Models
ANALYST_LLM_MODEL = "gemini/gemini-2.5-flash" # Example: "gpt-4o", "claude-3-5-sonnet", "gemini-1.5-pro"
JUDGE_LLM_MODEL = "gemini/gemini-2.5-pro" # Example: "gpt-4o", "claude-3-5-sonnet", "gemini-1.5-pro"

# Maximum iterations for prompt optimization
MAX_OPTIMIZATION_ITERATIONS = 10

# Directory for transcripts and target tenors
TRANSCRIPT_DIR = "../intermediate_data/texts"
TARGET_TENORS = ['3MNT', '2Y', '10Y'] # This should match the 'tenor' values in your RDS file (after uppercasing)

# Directory to save the final simulated DataFrame
SAVE_FINAL_DF_DIR = "../intermediate_data/aggregate_gemini_result/judge_llm"

# Percentage of transcripts to use for optimization (training)
TRAINING_PERCENTAGE = 0.75 # Use 75% of the conferences for optimization


# LLM API Call Function
try:
    from litellm import completion
    print("Using litellm for LLM API calls.")
    def call_llm(model: str, system_prompt: str, user_message: str, temperature: float = 0.7, json_mode: bool = False) -> str:
        messages = [
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": user_message}
        ]
        
        response_format_arg = {"type": "json_object"} if json_mode else {"type": "text"}

        response = completion(
            model=model,
            messages=messages,
            temperature=temperature,
            response_format=response_format_arg
        )
        return response.choices[0].message.content
except ImportError:
    print("litellm not found. Please install it (`pip install litellm`) or implement direct API calls.")
    print("Proceeding with placeholder call_llm function (will raise NotImplementedError).")
    def call_llm(model: str, system_prompt: str, user_message: str, temperature: float = 0.7, json_mode: bool = False) -> str:
        """Placeholder for LLM API call."""
        raise NotImplementedError(f"LLM API call for model {model} not implemented. Install litellm or integrate specific SDK.")

# --- Prompts ---

INITIAL_ANALYST_PROMPT = """
Context:
You are simulating the Euro area interest rate swap market, composed of 30 individual traders.
These traders interpret the ECB Governing Council press conference, which communicates monetary policy decisions, economic assessments, and includes a Q&A session with journalists.
Each trader then makes a trading decision to maximize profit based on their interpretation of the conference and their unique characteristics.

Trader Characteristics:
Each trader has the following attributes:
- Risk Aversion: High / Medium / Low — determines sensitivity to uncertainty and preference for stability.
- Behavioral Biases (1–2 per trader): e.g., Confirmation Bias, Overconfidence, Anchoring, Herding, Loss Aversion, Recency Bias.
- Interpretation Style (1 per trader): e.g., Fundamentalist, Sentiment Reader, Quantitative, Skeptic, Narrative-Driven.

Task:
You are given a certain number of distinct ECB press conferences.
For each of the 30 traders, simulate their individual trading action in the interest rate swap market across three tenors (3 months, 2 years, 10 years).
For each tenor, the trader must:
    - Provide an expected rate direction: Up / Down / Unchanged
    - Provide a new expected swap rate (in percent, to two decimal places)
    - Provide a confidence level (0-100%) in their decision

Output:
Provide a table with the following structure for each press conference, trader, and interest rate tenor:

| Date      | Trader ID | Tenor   | Expected Direction | New Expected Rate (%)  |
|-----------|-----------|---------|--------------------|------------------------|
| YYYY-MM-DD | T001      | 3MNT    | Up                 | 3.15                   |
| YYYY-MM-DD | T001      | 2Y      | Down               | 2.85                   |
| ...       | ...       | ...     | ...                | ...                    |

Guidelines:
- Use only the information available as of [date].
- Do not aggregate or summarize responses.
- Reflect diversity in interpretation, risk tolerance, and horizon. Rationale must be unique for each trader and can vary across tenors.
- Output only a markdown table with the specified columns, no additional text. Do not use JSON or any other data serialization format.

"""

JUDGE_SYSTEM_PROMPT = """
You are an expert AI system designed to optimize prompts for another AI (the "Analyst LLM").

Your ultimate goal is to refine the Analyst LLM's prompt to improve its ability to replicate the market volatility of OIS rates based on ECB press conference transcripts.
Specifically, you must ensure that the standard deviation of the Analyst LLM's predictions correlates highly with the actual, observed market volatility of the 3-month, 2-years and 10-year OIS rates.
This means a higher standard deviation in the Analyst's predictions should correspond to higher actual market volatility, and vice-versa.

You will be provided with:
- The current Analyst LLM prompt.
- The most recent performance (Spearman correlation coefficient between the Analyst LLM's predicted standard deviations and actual market volatility).
- The historical performance trend, including past critiques and proposed prompt summaries.

Your task is to:
1.  Critique the current prompt: Identify specific weaknesses or areas of ambiguity that might directly hinder achieving a high positive correlation. Consider:
    -Clarity and Specificity: Is the Analyst LLM's task unambiguous?
    -Emphasis on Uncertainty: Does the prompt adequately guide the Analyst to reflect internal uncertainty in its prediction spread?
    -Guidance on Nuance: Does it encourage consideration of subtle market signals from the text?
2.  Suggest a Revised Prompt:Propose a new version of the Analyst LLM's prompt that directly addresses the identified weaknesses and aims to increase the correlation. Be precise with your suggested changes.
3.  Explain your reasoning: Articulate why your proposed revisions are expected to improve the correlation, linking specific prompt changes to anticipated improvements in the Analyst LLM's behavior regarding uncertainty quantification.

Your output must be in JSON format. Do not include any other text outside the JSON.

Example JSON output:
```json
{
  "critique": "The previous prompt was too general regarding how to express uncertainty. It didn't explicitly ask the Analyst LLM to consider multiple viewpoints, which is key for its standard deviation to accurately reflect market volatility. It also lacked emphasis on how ambiguity in the transcript should translate to higher spread.",
  "revised_prompt": "You are a highly analytical financial expert specializing in macroeconomic analysis, with a focus on central bank communication. Your task is to analyze excerpts from ECB press conferences and predict the immediate percentage change in the 10-year Overnight Index Swap (OIS) rate (in basis points). When analyzing each excerpt, **explicitly consider and internalize the potential range of market interpretations**. If the language is ambiguous, vague, or contains conflicting signals, your internal simulation of potential outcomes should broaden. Conversely, clear and unambiguous guidance should lead to a narrower range. Your final prediction should be a single numerical value (e.g., +5, -2, 0) reflecting your best estimate. The *variability* across multiple independent predictions you generate for the same transcript is expected to directly reflect the market's anticipated uncertainty. Example Format: +5",
  "reasoning": "The revised prompt adds explicit instructions to consider the 'range of market interpretations' and directly links 'ambiguous language' to a 'broadened' internal simulation. This should encourage the Analyst LLM to generate a higher standard deviation in its outputs for uncertain transcripts and a lower standard deviation for clear ones, thereby directly improving its correlation with actual market volatility. The emphasis on 'multiple independent predictions' also reinforces the mechanism by which uncertainty is quantified."
}
"""

# --- Helper Function for Summarizing Prompts ---
def summarize_prompt(prompt: str, max_length: int = 200) -> str:
    """Summarizes a prompt for logging purposes."""
    return (prompt[:max_length] + '...') if len(prompt) > max_length else prompt

# --- Core Parsing Functions ---

def parse_markdown_table(markdown_string: str) -> Optional[pd.DataFrame]:
    """
    Parses a markdown table string into a pandas DataFrame.
    Assumes the table has a header and a separator line.
    """
    lines = markdown_string.strip().split('\n')
    if len(lines) < 2:
        return None

    header_line = None
    separator_line_index = -1
    for i, line in enumerate(lines):
        if re.match(r'^\|\s*[-:]+\s*\|', line):
            separator_line_index = i
            header_line = lines[i-1]
            break

    if separator_line_index == -1 or header_line is None:
        return None

    headers = [h.strip() for h in header_line.split('|') if h.strip()]
    
    data_rows = []
    for line in lines[separator_line_index + 1:]:
        if not line.strip():
            continue
        cells = [c.strip() for c in line.split('|') if c.strip()]
        if len(cells) == len(headers):
            data_rows.append(cells)
        else:
            print(f"Warning: Row has inconsistent number of columns: {line}")

    if not data_rows:
        return None

    try:
        df = pd.DataFrame(data_rows, columns=headers)
        df.columns = [col.strip() for col in df.columns]  # Strip whitespace from column names
        df['New Expected Rate (%)'] = pd.to_numeric(
            df['New Expected Rate (%)'], errors='coerce'
        )
        df.dropna(subset=['New Expected Rate (%)'], inplace=True)
        return df
    except Exception as e:
        print(f"Error creating DataFrame from parsed markdown: {e}")
        return None


def parse_comma_separated_list(list_string: str) -> Optional[List[float]]:
    """
    Parses a comma-separated string of numbers into a list of floats.
    Handles potential whitespace and non-numeric values gracefully.
    """
    try:
        # Use regex to find all numbers, including negative and decimals
        numbers_str = re.findall(r'[\d\.-]+', list_string)
        if not numbers_str:
            return None
        
        # Convert to float
        predictions = [float(n) for n in numbers_str]
        
        return predictions
    except ValueError:
        print(f"Error converting values in the list to float: {list_string}")
        return None
    except Exception as e:
        print(f"General error parsing comma-separated list: {e}")
        return None

# --- Data Loading Function ---
def get_transcript_data(transcript_dir: str) -> List[Dict[str, str]]:
    """
    Reads all .txt files from the specified directory and returns a list of dictionaries,
    each containing 'date' and 'text'.
    Assumes filenames are in 'YYYY-MM-DD_GovernorName.txt' format.
    """
    transcript_data = []
    print(f"Reading transcripts from: {transcript_dir}")
    if not os.path.exists(transcript_dir):
        print(f"Error: Transcript directory '{transcript_dir}' not found. Please create it and place .txt files inside.")
        return []

    for filename in os.listdir(transcript_dir):
        if filename.endswith(".txt"):
            try:
                date_part = filename.split('_')[0]
                datetime.strptime(date_part, "%Y-%m-%d")
                
                filepath = os.path.join(transcript_dir, filename)
                with open(filepath, 'r', encoding='utf-8') as f:
                    transcript_text = f.read()
                
                transcript_data.append({'date': date_part, 'text': transcript_text})
                # print(f"  Loaded transcript for {date_part}") # Suppressed for cleaner output
            except ValueError:
                print(f"Warning: Filename '{filename}' does not match YYYY-MM-DD_Name.txt format (date part invalid). Skipping.")
            except Exception as e:
                print(f"Error reading transcript file {filename}: {e}")
    if not transcript_data:
        print("No valid transcript files found.")
    else:
        print(f"Successfully loaded {len(transcript_data)} transcripts.")
    return transcript_data

# --- Analyst LLM Execution Functions ---


def run_analyst_llm_for_transcript_markdown(transcript_info: Dict[str, str], analyst_prompt: str) -> Optional[pd.DataFrame]:
    """
    Runs the Analyst LLM once for a single transcript, expecting a markdown table output
    for all tenors in a single call.
    """
    conference_date = transcript_info['date']
    transcript_text = transcript_info['text']
    
    formatted_prompt = analyst_prompt.replace("[date]", conference_date)

    user_message = f"Here is the ECB press conference excerpt for the date {conference_date}: \n\n{transcript_text}\n\n" \
                   f"Please simulate the trading actions for the 30 traders as per the instructions in your system prompt. " \
                   f"Ensure the output is strictly a markdown table."
    
    try:
        response_content = call_llm(
            model=ANALYST_LLM_MODEL,
            system_prompt=formatted_prompt,
            user_message=user_message,
            temperature=1 
        )
        simulated_df = parse_markdown_table(response_content)

        if simulated_df is not None and not simulated_df.empty:
            # Handle column renaming carefully to avoid duplicates
            if 'Date' in simulated_df.columns:
                simulated_df.rename(columns={'Date': 'Conference_Date'}, inplace=True)
            else:
                simulated_df['Conference_Date'] = conference_date
            
            # Standardize other column names
            simulated_df.rename(columns={'New Expected Rate (%)': 'New Expected Rate'}, inplace=True)
            
            expected_min_rows = 30 * len(TARGET_TENORS)
            if len(simulated_df) < expected_min_rows:
                 print(f"Warning: Analyst LLM output for {conference_date} has fewer rows than expected. "
                       f"Expected ~{expected_min_rows}, got {len(simulated_df)}. This may affect evaluation.")
            return simulated_df
        else:
            print(f"Warning: Analyst LLM returned unparseable, empty, or incomplete table for transcript on {conference_date}.")
            return None
    except Exception as e:
        print(f"Error during Analyst LLM simulation for {conference_date}: {e}")
        return None

def run_analyst_llm_for_transcript_and_tenor_list(transcript_info: Dict[str, str], analyst_prompt: str, tenor: str) -> Optional[pd.DataFrame]:
    """
    Runs the Analyst LLM once for a single transcript and a single tenor,
    expecting a comma-separated list.
    """
    conference_date = transcript_info['date']
    transcript_text = transcript_info['text']
    
    formatted_prompt = analyst_prompt.replace("{TENOR}", tenor)
    
    user_message = (
        f"Here is the ECB press conference excerpt for the date {conference_date}: \n\n"
        f"{transcript_text}\n\n"
        f"Provide your 30 predictions for the {tenor} OIS rate change in basis points. "
        f"Ensure the output is strictly a single, comma-separated list of 30 numerical values."
    )
    
    try:
        response_content = call_llm(
            model=ANALYST_LLM_MODEL,
            system_prompt=formatted_prompt,
            user_message=user_message,
            temperature=1
        )
        
        predictions = parse_comma_separated_list(response_content)

        if predictions and len(predictions) == 30:
            simulated_data = {
                'Conference_Date': [conference_date] * 30,
                'Trader ID': [f'T{i+1:03d}' for i in range(30)],
                'Tenor': [tenor] * 30,
                'New Expected Rate': predictions
            }
            return pd.DataFrame(simulated_data)
        else:
            print(f"Warning: Analyst LLM returned an unparseable or incomplete list for {conference_date} and {tenor}. "
                  f"Got {len(predictions) if predictions else 0} predictions.")
            return None

    except Exception as e:
        print(f"Error during Analyst LLM simulation for {conference_date} and {tenor}: {e}")
        return None

def run_analyst_llm_for_all_transcripts_dispatcher(
    transcripts: List[Dict[str, str]],
    analyst_prompt: str
) -> pd.DataFrame:
    """
    Dynamically dispatches to the correct Analyst LLM execution function based on the prompt content.
    """
    all_simulated_dfs = []
    max_workers = 4

    if "{TENOR}" in analyst_prompt:
        print("\n--- Running Analyst LLM (List Format) for all transcripts and tenors (in parallel) ---")
        tasks = [(t_info, tenor) for t_info in transcripts for tenor in TARGET_TENORS]
        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            future_to_task = {
                executor.submit(run_analyst_llm_for_transcript_and_tenor_list, t_info, analyst_prompt, tenor): (t_info['date'], tenor)
                for t_info, tenor in tasks
            }
            for future in as_completed(future_to_task):
                conference_date, tenor = future_to_task[future]
                try:
                    simulated_df = future.result()
                    if simulated_df is not None and not simulated_df.empty:
                        all_simulated_dfs.append(simulated_df)
                        # print(f"  Successfully processed transcript for {conference_date} ({tenor})") # Suppressed for cleaner output
                except Exception as exc:
                    print(f"  Transcript for {conference_date} ({tenor}) generated an exception: {exc}")
    else:
        print("\n--- Running Analyst LLM (Markdown Table Format) for all transcripts (in parallel) ---")
        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            future_to_transcript = {
                executor.submit(run_analyst_llm_for_transcript_markdown, t_info, analyst_prompt): t_info['date']
                for t_info in transcripts
            }
            for future in as_completed(future_to_transcript):
                conference_date = future_to_transcript[future]
                try:
                    simulated_df = future.result()
                    if simulated_df is not None and not simulated_df.empty:
                        all_simulated_dfs.append(simulated_df)
                        # print(f"  Successfully processed transcript for {conference_date}") # Suppressed for cleaner output
                except Exception as exc:
                    print(f"  Transcript for {conference_date} generated an exception: {exc}")
    
    if not all_simulated_dfs:
        print("No simulated data collected from any transcript.")
        return pd.DataFrame()
    
    combined_simulated_df = pd.concat(all_simulated_dfs, ignore_index=True)
    return combined_simulated_df

# --- Evaluation Function ---
def evaluate_analyst_performance(
    simulated_df: pd.DataFrame,
    historical_data_df: pd.DataFrame
) -> Tuple[float, pd.DataFrame]:
    """
    Evaluates the performance by calculating the standard deviation of 'New Expected Rate'
    from the simulated data and correlating it with actual market volatilities.
    """
    if simulated_df.empty:
        print("Simulated DataFrame is empty. Cannot evaluate performance.")
        return 0.0, pd.DataFrame()
    if historical_data_df.empty:
        print("Historical data DataFrame is empty. Cannot evaluate performance.")
        return 0.0, pd.DataFrame()
    
    # Standardize column names for merging
    simulated_df.rename(columns={'Date': 'Conference_Date', 'New Expected Rate (%)': 'New Expected Rate'}, inplace=True)

    # --- Data Preparation for Merging ---
    if 'Conference_Date' not in simulated_df.columns:
        print("Error: 'Conference_Date' column not found in simulated_df. Cannot evaluate.")
        return 0.0, pd.DataFrame()
    simulated_df['Date_dt'] = pd.to_datetime(simulated_df['Conference_Date']).dt.date

    if 'date' not in historical_data_df.columns:
        print("Error: 'date' column not found in historical_data_df. Cannot evaluate.")
        return 0.0, pd.DataFrame()
    try:
        historical_data_df['date_dt'] = pd.to_datetime(historical_data_df['date']).dt.date
    except Exception as e:
        print(f"Error converting 'date' column in historical_data_df: {e}. Cannot evaluate.")
        return 0.0, pd.DataFrame()

    if 'tenor' not in historical_data_df.columns:
        print("Error: 'tenor' column not found in historical_data_df. Cannot evaluate.")
        return 0.0, pd.DataFrame()
    historical_data_df['Tenor'] = historical_data_df['tenor'].astype(str).str.upper()

    # --- Calculate predicted standard deviations ---
    predicted_sds = simulated_df.groupby(['Date_dt', 'Tenor'])['New Expected Rate'].std().reset_index()
    predicted_sds.rename(columns={'New Expected Rate': 'predicted_sd'}, inplace=True)
    predicted_sds.dropna(subset=['predicted_sd'], inplace=True)

    if 'correct_post_mean' not in historical_data_df.columns:
        print("Error: 'correct_post_mean' column not found in historical_data_df. Cannot evaluate.")
        return 0.0, pd.DataFrame()

    # --- Prepare actual volatilities for merging ---
    actual_volatilities = historical_data_df[['date_dt', 'Tenor', 'correct_post_mean']].copy()
    actual_volatilities.rename(columns={'correct_post_mean': 'actual_volatility'}, inplace=True)
    actual_volatilities.dropna(subset=['actual_volatility'], inplace=True)

    # --- Merge and calculate correlation ---
    merged_data = pd.merge(
        predicted_sds,
        actual_volatilities,
        on=['date_dt', 'Tenor'],
        how='inner'
    )
    
    if merged_data.empty:
        print("No common dates or tenors between simulated and historical data for correlation. Returning 0.0.")
        return 0.0, pd.DataFrame()

    tenor_correlations = []
    detailed_results_list = []

    for tenor in TARGET_TENORS:
        tenor_data = merged_data[merged_data['Tenor'] == tenor].copy()
        
        if len(tenor_data['predicted_sd'].dropna()) >= 2 and \
           len(tenor_data['actual_volatility'].dropna()) >= 2:
            
            correlation, p_value = spearmanr(tenor_data['predicted_sd'], tenor_data['actual_volatility'])
            tenor_correlations.append(correlation)
            print(f"  Spearman Correlation for {tenor}: {correlation:.4f} (p-value: {p_value:.4f})")
            
            for _, row in tenor_data.iterrows():
                detailed_results_list.append({
                    "transcript_date": row['date_dt'].strftime('%Y-%m-%d'),
                    "tenor": tenor,
                    "actual_volatility": row['actual_volatility'],
                    "predicted_sd": row['predicted_sd']
                })
        else:
            print(f"  Not enough valid data points ({len(tenor_data)}) to calculate correlation for tenor {tenor}. Skipping.")

    if not tenor_correlations:
        print("No valid correlations calculated for any tenor. Returning 0.0 average correlation.")
        return 0.0, pd.DataFrame(detailed_results_list)

    average_correlation = np.mean(tenor_correlations)
    print(f"Overall Average Spearman Correlation (across tenors): {average_correlation:.4f}")

    return average_correlation, pd.DataFrame(detailed_results_list)

# --- Judge LLM Function ---
def run_judge_llm(
    current_analyst_prompt: str,
    current_correlation: float,
    optimization_history: List[Dict[str, Any]]
) -> Tuple[str, str, str]:
    """
    Runs the Judge LLM to critique the current prompt and suggest a revision.
    """
    history_summary = "\n".join([
        f"Iteration {i+1}: Correlation={entry['correlation_training']:.4f}, Critique='{entry['critique']}', Proposed Prompt='{entry['proposed_prompt_summary']}'"
        for i, entry in enumerate(optimization_history)
    ])

    user_message = f"""
Current Analyst LLM Prompt:

{current_analyst_prompt}

Current Performance (Spearman Correlation): {current_correlation:.4f}

Optimization History:
{history_summary if history_summary else "No previous optimization history."}

Based on this, critique the current prompt and suggest a revised prompt to improve the correlation between the Analyst LLM's predicted standard deviations and actual market volatility. Explain your reasoning.
"""
    print("\n--- Running Judge LLM ---")
    try:
        response_content = call_llm(
            model=JUDGE_LLM_MODEL,
            system_prompt=JUDGE_SYSTEM_PROMPT,
            user_message=user_message,
            temperature=1,
            json_mode=True
        )
        judge_response = json.loads(response_content)
        critique = judge_response.get("critique", "No critique provided.")
        revised_prompt = judge_response.get("revised_prompt", current_analyst_prompt)
        reasoning = judge_response.get("reasoning", "No reasoning provided.")
        return critique, revised_prompt, reasoning
    except json.JSONDecodeError as e:
        print(f"Error parsing Judge LLM JSON response: {e}")
        print(f"Raw response: {response_content}")
        return "JSON parsing error.", current_analyst_prompt, "Judge LLM returned malformed JSON."
    except Exception as e:
        print(f"Error running Judge LLM: {e}")
        return "LLM call error.", current_analyst_prompt, "Failed to call Judge LLM."

# --- Main Optimization Loop ---
def run_optimization():
    """
    Executes the full LLM-as-Judge prompt optimization process.
    """
    current_analyst_prompt = INITIAL_ANALYST_PROMPT
    optimization_history = []
    best_correlation = -1.0
    best_prompt = INITIAL_ANALYST_PROMPT

    print("Starting LLM-as-Judge Prompt Optimization (In-sample using training set)...")
    print(f"Initial Analyst Prompt (Summary): {summarize_prompt(current_analyst_prompt)}")

    # Step 0: Load all transcripts once
    all_transcripts_data = get_transcript_data(TRANSCRIPT_DIR)
    if not all_transcripts_data:
        print(f"No transcript files found in {TRANSCRIPT_DIR}. Exiting.")
        return None, None, None

    # --- Split transcripts for training only ---
    num_transcripts = len(all_transcripts_data)
    num_training_transcripts = int(num_transcripts * TRAINING_PERCENTAGE)
    
    all_transcripts_data.sort(key=lambda x: x['date'])

    training_transcripts = all_transcripts_data[:num_training_transcripts]

    print(f"Total transcripts found: {num_transcripts}")
    print(f"Using {len(training_transcripts)} transcripts for optimization (in-sample training).")

    # Step 0: Load historical OIS volatility data once
    try:
        print(f"Loading historical data from ../intermediate_data/range_difference_df.rds...")
        historical_data_df_dict = pyreadr.read_r("../intermediate_data/range_difference_df.rds")
        historical_data_df = historical_data_df_dict[None]

        if 'date' not in historical_data_df.columns:
            print("Error: 'date' column not found in historical_data_df. This is crucial for merging. Exiting.")
            return None, None, None
        try:
            historical_data_df['date'] = pd.to_datetime(historical_data_df['date']).dt.date
        except Exception as e:
            print(f"Error converting 'date' column in historical_data_df: {e}. Exiting.")
            return None, None, None

    except Exception as e:
        print(f"Error loading historical OIS volatility data from RDS: {e}. Exiting optimization.")
        return None, None, None

    training_dates_for_filter = {datetime.strptime(t['date'], "%Y-%m-%d").date() for t in training_transcripts}
    historical_data_df_training = historical_data_df[historical_data_df['date'].isin(training_dates_for_filter)].copy()

    if historical_data_df_training.empty:
        print("No historical data available for the selected training transcripts. Exiting.")
        return None, None, None
    print(f"Found {len(historical_data_df_training['date'].unique())} historical data points for training dates.")

    current_simulated_df_training = pd.DataFrame()

    for i in range(MAX_OPTIMIZATION_ITERATIONS):
        print(f"\n===== Iteration {i+1}/{MAX_OPTIMIZATION_ITERATIONS} (In-sample Optimization) =====")
        
        # Dispatch to the appropriate Analyst LLM execution function
        current_simulated_df_training = run_analyst_llm_for_all_transcripts_dispatcher(
            training_transcripts,
            current_analyst_prompt
        )

        if current_simulated_df_training.empty:
            print(f"Iteration {i+1}: No simulated data generated. Cannot evaluate. Assigning 0.0 correlation.")
            correlation = 0.0
            detailed_results_df = pd.DataFrame()
        else:
            correlation, detailed_results_df = evaluate_analyst_performance(
                current_simulated_df_training,
                historical_data_df_training.copy()
            )

        print(f"Iteration {i+1} Result (In-sample Training): Correlation = {correlation:.4f}")

        if correlation > best_correlation:
            print(f"New best correlation found: {correlation:.4f} (Previous best: {best_correlation:.4f})")
            best_correlation = correlation
            best_prompt = current_analyst_prompt
            os.makedirs(os.path.dirname("../intermediate_data/"), exist_ok=True)
            with open("../intermediate_data/best_analyst_prompt.txt", "w") as f:
                f.write(best_prompt)
            print("Best prompt saved to best_analyst_prompt.txt")
        else:
            print(f"No improvement in training correlation. Best correlation remains {best_correlation:.4f}")

        optimization_history.append({
            "iteration": i + 1,
            "prompt_before_judge": current_analyst_prompt,
            "correlation_training": correlation,
            "detailed_results_training": detailed_results_df.to_dict('records') if not detailed_results_df.empty else [],
            "critique": "",
            "proposed_prompt": "",
            "reasoning": "",
            "proposed_prompt_summary": ""
        })

        if i == MAX_OPTIMIZATION_ITERATIONS - 1:
            print("Reached maximum optimization iterations. Finalizing in-sample optimization.")
            break

        critique, revised_prompt, reasoning = run_judge_llm(
            current_analyst_prompt,
            correlation,
            [{k: v for k, v in entry.items() if k not in ["detailed_results_training", "prompt_before_judge"]} for entry in optimization_history]
        )

        optimization_history[-1]["critique"] = critique
        optimization_history[-1]["proposed_prompt"] = revised_prompt
        optimization_history[-1]["reasoning"] = reasoning
        optimization_history[-1]["proposed_prompt_summary"] = summarize_prompt(revised_prompt)

        print(f"Judge's Critique: {critique}")
        print(f"Judge's Reasoning: {reasoning}")
        print(f"Revised Prompt (Summary): {summarize_prompt(revised_prompt)}")

        current_analyst_prompt = revised_prompt
        time.sleep(2)

    print("\n===== In-sample Optimization Complete =====")
    print(f"Final Best In-sample Training Correlation: {best_correlation:.4f}")
    print(f"Optimized Analyst Prompt:\n{best_prompt}")

    df_to_save = current_simulated_df_training
    os.makedirs(SAVE_FINAL_DF_DIR, exist_ok=True)
    final_df_path = os.path.join(SAVE_FINAL_DF_DIR, "final_optimized_simulated_df.csv")
    print(f"Saving final simulated DataFrame from the last training run to {final_df_path}")
    if not df_to_save.empty:
        df_to_save.to_csv(final_df_path, index=False)
    else:
        print("Final simulated DataFrame is empty, no file saved.")

    return best_prompt, best_correlation, optimization_history

if __name__ == "__main__":
    best_prompt, best_correlation, history = run_optimization()

    if best_prompt:
        print("\n--- Final Out-of-Sample Test ---")
        # Implement the out-of-sample test here using the remaining transcripts
        print("Final out-of-sample test would be implemented here using the remaining transcripts.")