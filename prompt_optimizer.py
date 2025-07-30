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
from concurrent.futures import ThreadPoolExecutor, as_completed # For parallelization

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
    "litellm": "litellm" # Add litellm here for explicit installation check
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
# You'll need to choose which LLM provider to use and configure API keys.
# Using litellm is recommended for multi-provider support and fallbacks.

# Ensure you have your API keys set as environment variables, e.g.:
# export OPENAI_API_KEY="sk-..."
# export ANTHROPIC_API_KEY="sk-ant-..."
# export GOOGLE_API_KEY="AIza..."

# LLM Models
ANALYST_LLM_MODEL = "gemini/gemini-2.5-flash" # Example: "gpt-4o", "claude-3-5-sonnet", "gemini-1.5-pro"
JUDGE_LLM_MODEL = "gemini/gemini-2.5-pro" # Example: "gpt-4o", "claude-3-5-sonnet", "gemini-1.5-pro"


# Maximum iterations for prompt optimization
MAX_OPTIMIZATION_ITERATIONS = 6 

# Threshold for improvement to continue optimization (e.g., 0.001 increase in average correlation)
# MIN_CORRELATION_IMPROVEMENT = 0.02 # <-- COMMENTED OUT AS REQUESTED

# Directory for transcripts and target tenors
TRANSCRIPT_DIR = "../intermediate_data/texts"
TARGET_TENORS = ['3MNT', '2Y', '10Y'] # This should match the 'tenor' values in your RDS file (after uppercasing)

# Directory to save the final simulated DataFrame
SAVE_FINAL_DF_DIR = "../intermediate_data/aggregate_gemini_result/judge_llm"


# LLM API Call Function
try:
    from litellm import completion
    print("Using litellm for LLM API calls.")
    # Ensure litellm version is recent enough for response_format
    # if not hasattr(completion, 'response_format'):
    #     print("Warning: litellm version might be old. 'response_format' might not be supported.")
    #     print("Please consider upgrading: pip install --upgrade litellm")

    def call_llm(model: str, system_prompt: str, user_message: str, temperature: float = 0.7, json_mode: bool = False) -> str:
        messages = [
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": user_message}
        ]
        
        # litellm expects response_format to be a dict, not a string
        # For text mode, litellm typically handles it without explicit response_format,
        # but including it for clarity if the API supports it
        response_format_arg = {"type": "json_object"} if json_mode else {"type": "text"}

        response = completion(
            model=model,
            messages=messages,
            temperature=temperature,
            response_format=response_format_arg # Pass the dictionary directly
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

| Date       | Trader ID | Tenor   | Expected Direction | New Expected Rate (%)  |
|------------|-----------|---------|--------------------|------------------------|
| YYYY-MM-DD | T001      | 3MNT    | Up                 | 3.15                   |
| YYYY-MM-DD | T001      | 2Y      | Down               | 2.85                   |
| ...        | ...       | ...     | ...                | ...                    |

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
- The most recent performance (Pearson correlation coefficient between the Analyst LLM's predicted standard deviations and actual market volatility).
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

# --- Core Functions ---

def parse_markdown_table(markdown_string: str) -> Optional[pd.DataFrame]:
    """
    Parses a markdown table string into a pandas DataFrame.
    Assumes the table has a header and a separator line.
    """
    lines = markdown_string.strip().split('\n')
    if len(lines) < 2: # Need at least header and separator
        # print("Warning: Markdown table too short to parse.")
        return None

    # Find the separator line (e.g., |---|---|---|)
    header_line = None
    separator_line_index = -1
    for i, line in enumerate(lines):
        if re.match(r'^\|[- ]*\|[- ]*\|', line): # Check for lines like |---| or | --- |
            separator_line_index = i
            header_line = lines[i-1] # Header is the line before the separator
            break

    if separator_line_index == -1 or header_line is None:
        # print("Warning: Could not find valid markdown table separator or header.")
        return None

    # Parse headers (strip whitespace and |)
    headers = [h.strip() for h in header_line.split('|') if h.strip()]
    
    # Process data rows
    data_rows = []
    for line in lines[separator_line_index + 1:]:
        if not line.strip(): # Skip empty lines
            continue
        # Split by '|' and strip whitespace from each cell, filter out empty strings
        cells = [c.strip() for c in line.split('|') if c.strip()]
        if len(cells) == len(headers):
            data_rows.append(cells)
        else:
            print(f"Warning: Row has inconsistent number of columns: {line}")

    if not data_rows:
        # print("Warning: No data rows parsed from markdown table.")
        return None

    try:
        df = pd.DataFrame(data_rows, columns=headers)
        # Convert 'New Expected Rate (%)' to numeric
        # Handle cases where LLM might output non-numeric or malformed rates
        df['New Expected Rate (%)'] = pd.to_numeric(
            df['New Expected Rate (%)'], errors='coerce'
        )
        # Drop rows where 'New Expected Rate (%)' could not be converted
        df.dropna(subset=['New Expected Rate (%)'], inplace=True)
        return df
    except Exception as e:
        print(f"Error creating DataFrame from parsed markdown: {e}")
        return None

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
                # Extract the date part (YYYY-MM-DD) from the filename
                # It's the part before the first underscore
                date_part = filename.split('_')[0]
                
                # Validate date format
                datetime.strptime(date_part, "%Y-%m-%d")
                
                filepath = os.path.join(transcript_dir, filename)
                with open(filepath, 'r', encoding='utf-8') as f:
                    transcript_text = f.read()
                
                transcript_data.append({'date': date_part, 'text': transcript_text})
                print(f"  Loaded transcript for {date_part}")
            except ValueError:
                print(f"Warning: Filename '{filename}' does not match YYYY-MM-DD_Name.txt format (date part invalid). Skipping.")
            except Exception as e:
                print(f"Error reading transcript file {filename}: {e}")
    if not transcript_data:
        print("No valid transcript files found.")
    return transcript_data


def run_analyst_llm_for_transcript(transcript_info: Dict[str, str], analyst_prompt: str) -> Optional[pd.DataFrame]:
    """
    Runs the Analyst LLM once for a single transcript,
    simulating the 30 traders and collecting the resulting DataFrame.
    Returns None if parsing fails or data is incomplete.
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
            # Add Conference_Date column, useful for debugging and if LLM's 'Date' column is problematic
            simulated_df['Conference_Date'] = conference_date 
            # Check for expected number of rows, 30 traders * number of tenors
            expected_min_rows = 30 * len(TARGET_TENORS) 
            if len(simulated_df) < expected_min_rows:
                 print(f"Warning: Analyst LLM output for {conference_date} has fewer rows than expected. "
                       f"Expected ~{expected_min_rows}, got {len(simulated_df)}.")
            return simulated_df
        else:
            print(f"Warning: Analyst LLM returned unparseable, empty, or incomplete table for transcript on {conference_date}.")
            # print(f"Raw response for {conference_date} was:\n{response_content[:1000]}...") # Uncomment for debugging
            return None
    except Exception as e:
        print(f"Error during Analyst LLM simulation for {conference_date}: {e}")
        return None


def run_analyst_llm_for_all_transcripts(
    transcripts: List[Dict[str, str]],
    analyst_prompt: str
) -> pd.DataFrame:
    """
    Runs the Analyst LLM for all provided transcripts in parallel and collects all simulated data
    into a single pandas DataFrame.
    """
    all_simulated_dfs = []
    print("\n--- Running Analyst LLM for all transcripts (in parallel) ---")

    # Parallelize LLM calls using ThreadPoolExecutor
    # Max workers can be adjusted based on your API rate limits and CPU cores
    max_workers = 5 # Example: Run 5 calls concurrently
    
    with ThreadPoolExecutor(max_workers=max_workers) as executor:
        # Submit tasks for each transcript
        future_to_transcript = {
            executor.submit(run_analyst_llm_for_transcript, t_info, analyst_prompt): t_info['date']
            for t_info in transcripts
        }

        for future in as_completed(future_to_transcript):
            conference_date = future_to_transcript[future]
            try:
                simulated_df = future.result()
                if simulated_df is not None and not simulated_df.empty:
                    all_simulated_dfs.append(simulated_df)
                    print(f"  Successfully processed transcript for {conference_date}")
                else:
                    print(f"  No valid simulation output collected for {conference_date}.")
            except Exception as exc:
                print(f"  Transcript for {conference_date} generated an exception: {exc}")

    if not all_simulated_dfs:
        print("No simulated data collected from any transcript.")
        return pd.DataFrame()
    
    combined_simulated_df = pd.concat(all_simulated_dfs, ignore_index=True)
    return combined_simulated_df


def evaluate_analyst_performance(
    simulated_df: pd.DataFrame,
    historical_data_df: pd.DataFrame # This is your actual OIS volatility data
) -> Tuple[float, pd.DataFrame]:
    """
    Evaluates the performance by calculating the standard deviation of 'New Expected Rate (%)'
    from the simulated data for each tenor and date, and then correlating these
    with actual market volatilities from historical_data_df (using 'correct_post_mean').
    Returns the average correlation across tenors and detailed results.
    """
    if simulated_df.empty:
        print("Simulated DataFrame is empty. Cannot evaluate performance.")
        return 0.0, pd.DataFrame()

    if historical_data_df.empty:
        print("Historical data DataFrame is empty. Cannot evaluate performance.")
        return 0.0, pd.DataFrame()
    
    # --- Data Preparation for Merging ---
    # Ensure 'Date' column in simulated_df is converted to datetime.date object
    # This 'Date' column is typically derived from the LLM's markdown table output.
    if 'Date' in simulated_df.columns:
        try:
            simulated_df['Date_dt'] = pd.to_datetime(simulated_df['Date']).dt.date
        except Exception as e:
            print(f"Error converting 'Date' column in simulated_df to datetime.date: {e}")
            # Fallback to Conference_Date if 'Date' from LLM output is problematic
            if 'Conference_Date' in simulated_df.columns:
                print("Attempting to use 'Conference_Date' for merging as a fallback.")
                simulated_df['Date_dt'] = pd.to_datetime(simulated_df['Conference_Date']).dt.date
            else:
                print("No suitable date column found in simulated_df for merging. Cannot evaluate.")
                return 0.0, pd.DataFrame()
    elif 'Conference_Date' in simulated_df.columns:
        # If 'Date' from LLM output is missing, use 'Conference_Date' (derived from filename)
        print("Warning: 'Date' column not found in simulated_df. Using 'Conference_Date' for merging.")
        simulated_df['Date_dt'] = pd.to_datetime(simulated_df['Conference_Date']).dt.date
    else:
        print("Error: Neither 'Date' nor 'Conference_Date' found in simulated_df. Cannot evaluate.")
        return 0.0, pd.DataFrame()

    # Ensure 'date' column in historical_data_df is converted to datetime.date object
    if 'date' in historical_data_df.columns:
        if pd.api.types.is_datetime64_any_dtype(historical_data_df['date']):
            historical_data_df['date_dt'] = historical_data_df['date'].dt.date
        else:
            try:
                historical_data_df['date_dt'] = pd.to_datetime(historical_data_df['date']).dt.date
            except Exception as e:
                print(f"Error converting 'date' column in historical_data_df to datetime.date: {e}")
                return 0.0, pd.DataFrame()
    else:
        print("Error: 'date' column not found in historical_data_df. Cannot evaluate.")
        return 0.0, pd.DataFrame()
    
    # Ensure 'tenor' column in historical_data_df is correctly typed and UPPERCASED
    if 'tenor' in historical_data_df.columns:
        historical_data_df['Tenor'] = historical_data_df['tenor'].astype(str).str.upper()
    else:
        print("Error: 'tenor' column not found in historical_data_df. Cannot evaluate.")
        return 0.0, pd.DataFrame()

    # Calculate predicted standard deviations from the simulated data
    # Group by BOTH Date_dt and Tenor to get per-conference, per-tenor standard deviations
    predicted_sds = simulated_df.groupby(['Date_dt', 'Tenor'])['New Expected Rate (%)'].std().reset_index()
    predicted_sds.rename(columns={'Date_dt': 'date_dt', 'New Expected Rate (%)': 'predicted_sd'}, inplace=True)

    # Prepare actual volatility data for merging directly using 'correct_post_mean'
    # Ensure 'correct_post_mean' column exists in the historical data
    if 'correct_post_mean' not in historical_data_df.columns:
        print("Error: 'correct_post_mean' column not found in historical_data_df. Cannot evaluate.")
        return 0.0, pd.DataFrame()

    # Select relevant columns from historical_data_df for actual volatility
    actual_volatilities = historical_data_df[['date_dt', 'Tenor', 'correct_post_mean']].copy()
    actual_volatilities.rename(columns={'correct_post_mean': 'actual_volatility'}, inplace=True)
    actual_volatilities.dropna(subset=['actual_volatility'], inplace=True) # Drop rows with missing actual volatility

    # Merge predicted and actual data
    # Merge on both date and tenor to align per-conference, per-tenor values
    merged_data = pd.merge(
        predicted_sds,
        actual_volatilities,
        on=['date_dt', 'Tenor'],
        how='inner' # Only keep rows where both predicted and actual data exist for a date/tenor
    )
    
    if merged_data.empty:
        print("No common dates or tenors between simulated and historical data for correlation. Returning 0.0.")
        return 0.0, pd.DataFrame()

    # Calculate correlations for each tenor individually
    tenor_correlations = []
    detailed_results_list = []

    for tenor in TARGET_TENORS:
        tenor_data = merged_data[merged_data['Tenor'] == tenor].copy()
        
        # Ensure enough non-NaN data points for correlation calculation (min 2 points)
        if len(tenor_data['predicted_sd'].dropna()) >= 2 and \
           len(tenor_data['actual_volatility'].dropna()) >= 2:
            
            # --- Using Spearman correlation as requested ---
            correlation, p_value = spearmanr(tenor_data['predicted_sd'], tenor_data['actual_volatility'])
            
            tenor_correlations.append(correlation)
            print(f"  Spearman Correlation for {tenor}: {correlation:.4f} (p-value: {p_value:.4f})")
            
            # Add detailed results for this tenor and date
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


def run_judge_llm(
    current_analyst_prompt: str,
    current_correlation: float,
    optimization_history: List[Dict[str, Any]]
) -> Tuple[str, str, str]:
    """
    Runs the Judge LLM to critique the current prompt and suggest a revision.
    """
    history_summary = "\n".join([
        f"Iteration {i+1}: Correlation={entry['correlation']:.4f}, Critique='{entry['critique']}', Proposed Prompt='{entry['proposed_prompt_summary']}'"
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


def run_optimization():
    current_analyst_prompt = INITIAL_ANALYST_PROMPT
    optimization_history = []
    best_correlation = -1.0 # Initialize with a value lower than any possible correlation
    best_prompt = INITIAL_ANALYST_PROMPT

    print("Starting LLM-as-Judge Prompt Optimization...")
    print(f"Initial Analyst Prompt (Summary): {summarize_prompt(current_analyst_prompt)}")

    # Step 0: Load all transcripts once
    all_transcripts_data = get_transcript_data(TRANSCRIPT_DIR)
    if not all_transcripts_data:
        print(f"No transcript files found in {TRANSCRIPT_DIR}. Exiting.")
        return None, None, None

    # Step 0: Load historical OIS volatility data once
    try:
        print(f"Loading historical data from ../intermediate_data/range_difference_df.rds...")
        historical_data_df_dict = pyreadr.read_r("../intermediate_data/range_difference_df.rds")
        historical_data_df = historical_data_df_dict[None] 
        
        # Ensure 'date' column is datetime.date objects for consistent merging
        if 'date' in historical_data_df.columns:
            if pd.api.types.is_datetime64_any_dtype(historical_data_df['date']):
                historical_data_df['date'] = historical_data_df['date'].dt.date
            else:
                try:
                    historical_data_df['date'] = pd.to_datetime(historical_data_df['date']).dt.date
                except Exception as e:
                    print(f"Warning: Could not convert 'date' column in historical_data_df to datetime.date: {e}")
        else:
            print("Warning: 'date' column not found in historical_data_df. This is crucial for merging.")

    except Exception as e:
        print(f"Error loading historical OIS volatility data from RDS: {e}. Exiting optimization.")
        return None, None, None

    # Initialize current_simulated_df outside the loop to store the last one
    current_simulated_df = pd.DataFrame() 

    for i in range(MAX_OPTIMIZATION_ITERATIONS):
        print(f"\n===== Iteration {i+1}/{MAX_OPTIMIZATION_ITERATIONS} =====")

        # Step 1: Run Analyst LLM for ALL transcripts to get the complete simulated_df
        current_simulated_df = run_analyst_llm_for_all_transcripts(
            all_transcripts_data,
            current_analyst_prompt
        )

        if current_simulated_df.empty:
            print(f"Iteration {i+1}: No simulated data generated. Cannot evaluate. Continuing to next iteration or breaking.")
            # If no data is generated at all, we might want to break or assign a very low correlation
            correlation = 0.0 # Assign a zero correlation for no data
            detailed_results_df = pd.DataFrame()
        else:
            # Step 2: Evaluate Analyst LLM with the complete simulated_df and historical_data_df
            correlation, detailed_results_df = evaluate_analyst_performance(
                current_simulated_df,
                historical_data_df.copy() # Pass a copy to ensure original isn't modified
            )

        print(f"Iteration {i+1} Result: Correlation = {correlation:.4f}")

        if correlation > best_correlation:
            print(f"New best correlation found: {correlation:.4f} (Previous best: {best_correlation:.4f})")
            best_correlation = correlation
            best_prompt = current_analyst_prompt
            # Save the best prompt
            with open("../intermediate_data/best_analyst_prompt.txt", "w") as f:
                f.write(best_prompt)
            print("Best prompt saved to best_analyst_prompt.txt")
        else:
            print(f"No improvement. Best correlation remains {best_correlation:.4f}")


        optimization_history.append({
            "iteration": i + 1,
            "prompt_before_judge": current_analyst_prompt,
            "correlation": correlation,
            "detailed_results": detailed_results_df.to_dict('records') if not detailed_results_df.empty else [],
            "critique": "",
            "proposed_prompt": "",
            "reasoning": "",
            "proposed_prompt_summary": ""
        })

        # --- Removed stopping criteria based on MIN_CORRELATION_IMPROVEMENT as requested ---
        # if i > 0 and (correlation - optimization_history[-2]['correlation']) < MIN_CORRELATION_IMPROVEMENT and correlation < best_correlation:
        #     print(f"Stopping early: Improvement ({correlation:.4f} - {optimization_history[-2]['correlation']:.4f}) below threshold {MIN_CORRELATION_IMPROVEMENT} and not the best prompt.")
        #     break

        # If this is the last iteration, no need to run the Judge LLM
        if i == MAX_OPTIMIZATION_ITERATIONS - 1:
            print("Reached maximum optimization iterations. Finalizing.")
            break

        critique, revised_prompt, reasoning = run_judge_llm(
            current_analyst_prompt,
            correlation,
            [{k: v for k, v in entry.items() if k not in ["detailed_results", "prompt_before_judge"]} for entry in optimization_history]
        )

        optimization_history[-1]["critique"] = critique
        optimization_history[-1]["proposed_prompt"] = revised_prompt
        optimization_history[-1]["reasoning"] = reasoning
        optimization_history[-1]["proposed_prompt_summary"] = summarize_prompt(revised_prompt)

        print(f"Judge's Critique: {critique}")
        print(f"Judge's Reasoning: {reasoning}")
        print(f"Revised Prompt (Summary): {summarize_prompt(revised_prompt)}")

        current_analyst_prompt = revised_prompt
        time.sleep(2) # Small delay to respect API rate limits

    print("\n===== Optimization Complete =====")
    print(f"Final Best Correlation: {best_correlation:.4f}")
    print(f"Optimized Analyst Prompt:\n{best_prompt}")

    # --- NEW: Save the final simulated_df ---
    if not current_simulated_df.empty:
        # Ensure the directory exists
        os.makedirs(SAVE_FINAL_DF_DIR, exist_ok=True)
        # Create a timestamped filename to avoid overwriting
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        final_simulated_df_filename = f"final_analyst_simulated_df_{timestamp}.csv"
        final_simulated_df_path = os.path.join(SAVE_FINAL_DF_DIR, final_simulated_df_filename)
        
        current_simulated_df.to_csv(final_simulated_df_path, index=False)
        print(f"Final analyst simulated DataFrame saved to {final_simulated_df_path}")
    else:
        print("No final simulated DataFrame to save as it was empty.")


    history_filename = f"optimization_history_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
    with open(history_filename, "w") as f:
        json.dump(optimization_history, f, indent=2)
    print(f"Full optimization history saved to {history_filename}")

    return best_prompt, best_correlation, optimization_history


if __name__ == "__main__":
    # Define TARGET_TENORS here as well if you run this script directly and not as part of a larger module
    # or ensure it's imported correctly if it's in a config file.
    # For now, it's defined at the top of this script.

    optimized_prompt, final_correlation, full_history = run_optimization()