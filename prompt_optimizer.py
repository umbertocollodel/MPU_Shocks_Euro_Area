# ----- Set environment libraries: ----

import subprocess
import sys

def install(package):
    subprocess.check_call([sys.executable, "-m", "pip", "install", package])

# List of required packages
required_packages = {
    "os": None,  # built-in
    "json": None,  # built-in
    "numpy": "numpy",
    "pandas": "pandas",
    "scipy": "scipy",
    "pyreadr":"pyreadr",
    "datetime": None,  # built-in
    "time": None,  # built-in
    "typing": None,  # built-in
    "re": None  # built-in
}

# Install only the ones that are not built-in
for module, package in required_packages.items():
    if package:
        try:
            __import__(module)
        except ImportError:
            install(package)

# Now you can safely import everything
import os
import json
import numpy as np
import pandas as pd
from scipy.stats import pearsonr
from datetime import datetime
import time
from typing import List, Dict, Any, Tuple, Optional
import re
import pyreadr




# --- Configuration ---
# You'll need to choose which LLM provider to use and configure API keys.
# Using litellm is recommended for multi-provider support and fallbacks.
# For simplicity, this example will show placeholders for direct API calls,
# but litellm can wrap these seamlessly.

# Ensure you have your API keys set as environment variables, e'g':
# export OPENAI_API_KEY="sk-..."
# export ANTHROPIC_API_KEY="sk-ant-..."
# export GOOGLE_API_KEY="AIza..."

# LLM Models
ANALYST_LLM_MODEL = "gemini-2.5-flash" # Example: "gpt-4o", "claude-3-5-sonnet", "gemini-1.5-pro"
JUDGE_LLM_MODEL = "gemini-2.5-pro"   # Example: "gpt-4o", "claude-3-5-sonnet", "gemini-1.5-pro"


# Maximum iterations for prompt optimization
MAX_OPTIMIZATION_ITERATIONS = 8

# Threshold for improvement to continue optimization (e.g., 0.001 increase in average correlation)
MIN_CORRELATION_IMPROVEMENT = 0.02


# LLM API Call Function

try:
    from litellm import completion
    print("Using litellm for LLM API calls.")
    def call_llm(model: str, system_prompt: str, user_message: str, temperature: float = 0.7, json_mode: bool = False) -> str:
        messages = [
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": user_message}
        ]
        response_format = {"type": "json_object"} if json_mode else {"type": "text"}

        response = completion(
            model=model,
            messages=messages,
            temperature=temperature,
            response_format=response_format
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

| Date       | Trader ID | Tenor   | Expected Direction | New Expected Rate (%)  | Confidence Level (%) |
|------------|-----------|---------|--------------------|------------------------|----------------------|
| YYYY-MM-DD | T001      | 3M      | Up                 | 3.15                   |                      |
| YYYY-MM-DD | T001      | 2Y      | Down               | 2.85                   |                      |
| ...        | ...       | ...     | ...                | ...                    |                      | 

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
1.  **Critique the current prompt:** Identify specific weaknesses or areas of ambiguity that might directly hinder achieving a high positive correlation. Consider:
    * **Clarity and Specificity:** Is the Analyst LLM's task unambiguous?
    * **Emphasis on Uncertainty:** Does the prompt adequately guide the Analyst to reflect *internal uncertainty* in its prediction spread?
    * **Guidance on Nuance:** Does it encourage consideration of subtle market signals from the text?
2.  **Suggest a Revised Prompt:** Propose a new version of the Analyst LLM's prompt that directly addresses the identified weaknesses and aims to increase the correlation. Be precise with your suggested changes.
3.  **Explain your reasoning:** Articulate *why* your proposed revisions are expected to improve the correlation, linking specific prompt changes to anticipated improvements in the Analyst LLM's behavior regarding uncertainty quantification.

Your output must be in JSON format. Do not include any other text outside the JSON.

Example JSON output:
```json
{
  "critique": "The previous prompt was too general regarding how to express uncertainty. It didn't explicitly ask the Analyst LLM to consider multiple viewpoints, which is key for its standard deviation to accurately reflect market volatility. It also lacked emphasis on how ambiguity in the transcript should translate to higher spread.",
  "revised_prompt": "You are a highly analytical financial expert specializing in macroeconomic analysis, with a focus on central bank communication. Your task is to analyze excerpts from ECB press conferences and predict the immediate percentage change in the 10-year Overnight Index Swap (OIS) rate (in basis points). When analyzing each excerpt, **explicitly consider and internalize the potential range of market interpretations**. If the language is ambiguous, vague, or contains conflicting signals, your internal simulation of potential outcomes should broaden. Conversely, clear and unambiguous guidance should lead to a narrower range. Your final prediction should be a single numerical value (e.g., +5, -2, 0) reflecting your best estimate. The *variability* across multiple independent predictions you generate for the same transcript is expected to directly reflect the market's anticipated uncertainty. Example Format: +5",
  "reasoning": "The revised prompt adds explicit instructions to consider the 'range of market interpretations' and directly links 'ambiguous language' to a 'broadened' internal simulation. This should encourage the Analyst LLM to generate a higher standard deviation in its outputs for uncertain transcripts and a lower standard deviation for clear ones, thereby directly improving its correlation with actual market volatility. The emphasis on 'multiple independent predictions' also reinforces the mechanism by which uncertainty is quantified."
}

"""

  # --- Core Functions ---

def parse_markdown_table(markdown_string: str) -> Optional[pd.DataFrame]:
    """
    Parses a markdown table string into a pandas DataFrame.
    Assumes the table has a header and a separator line.
    """
    lines = markdown_string.strip().split('\n')
    if len(lines) < 2: # Need at least header and separator
        print("Warning: Markdown table too short to parse.")
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
        print("Warning: Could not find valid markdown table separator or header.")
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
        print("Warning: No data rows parsed from markdown table.")
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


def run_analyst_llm_for_transcript(transcript_text: str, analyst_prompt: str, conference_date: str) -> List[pd.DataFrame]:
    """
    Runs the Analyst LLM once for a single transcript,
    simulating the 30 traders and collecting the resulting DataFrame.
    The `num_simulations` parameter is no longer used, as only one simulation is performed.
    """
    all_simulated_market_data = []
    # Replace the [date] placeholder in the prompt
    formatted_prompt = analyst_prompt.replace("[date]", conference_date)

    # Perform only one simulation
    print(f"  Running Analyst LLM simulation for transcript on {conference_date}...")
    user_message = f"Here is the ECB press conference excerpt for the date {conference_date}: \n\n{transcript_text}\n\n" \
                   f"Please simulate the trading actions for the 30 traders as per the instructions in your system prompt. " \
                   f"Ensure the output is strictly a markdown table."
    try:
        response_content = call_llm(
            model=ANALYST_LLM_MODEL,
            system_prompt=formatted_prompt,
            user_message=user_message,
            temperature=1 # Temperature set to 1, as requested
        )
        # Parse the markdown table from the response
        simulated_df = parse_markdown_table(response_content)

        # Ensure the parsed DataFrame has expected number of rows for 30 traders * 3 tenors = 90 rows
        # This is a heuristic check; LLMs might not always perfectly comply.
        if simulated_df is not None and not simulated_df.empty and len(simulated_df) >= 30 * len(TARGET_TENORS):
            # Add a simulation ID (always 1 for single simulation) to track which run this came from
            simulated_df['Simulation_ID'] = 1
            all_simulated_market_data.append(simulated_df)
        else:
            print(f"Warning: Analyst LLM returned unparseable, empty, or incomplete table for transcript on {conference_date}. Expected ~{30 * len(TARGET_TENORS)} rows, got {len(simulated_df) if simulated_df is not None else 'None'}.")
            # print(f"Raw response was:\n{response_content[:1000]}...") # Print first 1000 chars for debugging

    except Exception as e:
        print(f"Error during Analyst LLM simulation for {conference_date}: {e}")
    return all_simulated_market_data



# Load your actual historical data here
historical_data_df = pyreadr.read_r("../intermediate_data/range_difference_df.rds")




def evaluate_analyst_performance(
    current_analyst_prompt: str,
    historical_data: pd.DataFrame
) -> Tuple[float, pd.DataFrame]:
    """
    Evaluates the current Analyst LLM prompt by running it on historical data,
    calculating the standard deviation of 'New Expected Rate (%)' across simulated traders for each tenor,
    and then correlating these with actual market volatilities.
    Returns the average correlation across tenors.
    """
    all_transcripts_sim_sds = {} # { (transcript_id, tenor): [sd_sim1, sd_sim2, ...] }
    actual_volatilities_per_tenor = {} # { (transcript_id, tenor): actual_volatility }

    # Initialize actual_volatilities_per_tenor based on historical_data_df
    # Assuming historical_data_df has columns like 'actual_ois_volatility_3M_bps', 'actual_ois_volatility_2Y_bps', etc.
    # We need to map actual data to specific tenors.
    # For simplicity of this example, let's assume actual_ois_volatility_bps is for 10Y and replicate it for others.
    # YOU MUST ADAPT THIS PART TO YOUR ACTUAL HISTORICAL DATA STRUCTURE!
    tenors = ['3M', '2Y', '10Y'] # Define your tenors
    for index, row in historical_data.iterrows():
        transcript_id = row['transcript_id']
        conference_date = row['date'] # Assuming 'date' column exists for [date] in prompt

        for tenor in tenors:
            # Assuming actual volatility is in columns like 'actual_ois_volatility_3M', 'actual_ois_volatility_2Y', 'actual_ois_volatility_10Y'
            # If your mock data only has 'actual_ois_volatility_bps', you need to decide how to use it
            # For demonstration, I'll use a placeholder or assume it maps to 10Y and then generate for others.
            col_name = f'actual_ois_volatility_{tenor}_bps'
            actual_vol_val = row.get(col_name, row['actual_ois_volatility_bps'] if tenor == '10Y' else np.random.uniform(0.5, 10.0)) # Placeholder if not specific
            actual_volatilities_per_tenor[(transcript_id, tenor)] = actual_vol_val

    print(f"\n--- Evaluating Analyst LLM with current prompt (on {len(historical_data)} transcripts) ---")
    for index, row in historical_data.iterrows():
        transcript_id = row['transcript_id']
        transcript_text = row['transcript_text']
        conference_date = row['date'] # Use the actual date for the prompt

        simulated_dfs = run_analyst_llm_for_transcript(transcript_text, current_analyst_prompt, conference_date)

        if not simulated_dfs:
            print(f"Skipping {transcript_id}: No valid simulations generated.")
            continue

        # Aggregate results from multiple simulations for this transcript
        combined_sim_df = pd.concat(simulated_dfs, ignore_index=True)

        for tenor in tenors:
            # Filter for the current tenor
            tenor_data = combined_sim_df[combined_sim_df['Tenor'] == tenor]

            if not tenor_data.empty and len(tenor_data['New Expected Rate (%)'].dropna()) >= 2:
                # Calculate SD of 'New Expected Rate (%)' across *all traders in all simulations* for this tenor/transcript
                # This is the standard deviation of the "market view" for this specific tenor on this date.
                predicted_sd = np.std(tenor_data['New Expected Rate (%)'].dropna())
                
                # Store it
                if (transcript_id, tenor) not in all_transcripts_sim_sds:
                    all_transcripts_sim_sds[(transcript_id, tenor)] = []
                all_transcripts_sim_sds[(transcript_id, tenor)].append(predicted_sd)
            else:
                print(f"  Warning: Not enough valid rates for {transcript_id}, {tenor} to calculate SD.")

    # Now, calculate correlations per tenor and then average them
    tenor_correlations = []
    detailed_results_for_all_tenors = []

    for tenor in tenors:
        analyst_sds_for_tenor = []
        actual_volatilities_for_tenor = []
        
        for transcript_id in historical_data['transcript_id'].unique():
            key = (transcript_id, tenor)
            if key in all_transcripts_sim_sds and len(all_transcripts_sim_sds[key]) > 0 and key in actual_volatilities_per_tenor:
                # For each (transcript, tenor), we need ONE predicted SD.
                # If NUM_ANALYST_LLM_SIMULATIONS_PER_TRANSCRIPT > 1, you might average these SDs or pick one.
                # For simplicity here, let's take the mean of the SDs calculated across simulations for the same transcript.
                avg_predicted_sd_for_transcript_tenor = np.mean(all_transcripts_sim_sds[key])
                analyst_sds_for_tenor.append(avg_predicted_sd_for_transcript_tenor)
                actual_volatilities_for_tenor.append(actual_volatilities_per_tenor[key])
                
                detailed_results_for_all_tenors.append({
                    "transcript_id": transcript_id,
                    "tenor": tenor,
                    "actual_volatility": actual_volatilities_per_tenor[key],
                    "predicted_sd": avg_predicted_sd_for_transcript_tenor
                })


        if len(analyst_sds_for_tenor) >= 2 and len(actual_volatilities_for_tenor) >= 2:
            correlation, p_value = pearsonr(analyst_sds_for_tenor, actual_volatilities_for_tenor)
            tenor_correlations.append(correlation)
            print(f"  Correlation for {tenor}: {correlation:.4f} (p-value: {p_value:.4f})")
        else:
            print(f"  Not enough valid data points to calculate correlation for tenor {tenor}. Skipping.")

    if not tenor_correlations:
        print("No valid correlations calculated for any tenor. Returning 0.0 average correlation.")
        return 0.0, pd.DataFrame(detailed_results_for_all_tenors)

    average_correlation = np.mean(tenor_correlations)
    print(f"Overall Average Pearson Correlation (across tenors): {average_correlation:.4f}")

    return average_correlation, pd.DataFrame(detailed_results_for_all_tenors)

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

Current Performance (Pearson Correlation): {current_correlation:.4f}

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
            json_mode=True # Expect JSON output
        )
        judge_response = json.loads(response_content) # Parse JSON response
        critique = judge_response.get("critique", "No critique provided.") # .get() safely retrieves value or default
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

def summarize_prompt(prompt: str, max_len: int = 100) -> str:
    """Summarizes a long prompt for logging."""
    return (prompt[:max_len] + "...") if len(prompt) > max_len else prompt


# --- Main Optimization Loop ---

def run_optimization():
    current_analyst_prompt = INITIAL_ANALYST_PROMPT
    optimization_history = []
    best_correlation = 0.1 # Initialize with a low value
    best_prompt = INITIAL_ANALYST_PROMPT

    print("Starting LLM-as-Judge Prompt Optimization...")
    print(f"Initial Analyst Prompt: {summarize_prompt(current_analyst_prompt)}")

    for i in range(MAX_OPTIMIZATION_ITERATIONS):
        print(f"\n===== Iteration {i+1}/{MAX_OPTIMIZATION_ITERATIONS} =====")

        # Step 1: Evaluate Analyst LLM with current prompt
        correlation, detailed_results_df = evaluate_analyst_performance(current_analyst_prompt, historical_data_df)

        print(f"Iteration {i+1} Result: Correlation = {correlation:.4f}")

        # Update best prompt if current is better
        if correlation > best_correlation:
            print(f"New best correlation found: {correlation:.4f} (Previous best: {best_correlation:.4f})")
            best_correlation = correlation
            best_prompt = current_analyst_prompt
            # Save the best prompt to a file for easy access
            with open("../intermediate_data/best_analyst_prompt.txt", "w") as f: # 'w' for write mode
                f.write(best_prompt)
            print("Best prompt saved to best_analyst_prompt.txt")

        # Record this iteration's performance
        optimization_history.append({
            "iteration": i + 1,
            "prompt_before_judge": current_analyst_prompt,
            "correlation": correlation,
            "detailed_results": detailed_results_df.to_dict('records'), # Store detailed results
            "critique": "", # Placeholder to be filled by judge later
            "proposed_prompt": "", # Placeholder to be filled by judge later
            "reasoning": "", # Placeholder to be filled by judge later
            "proposed_prompt_summary": "" # Summarized for history passed to judge
        })

        # Step 2: Check stopping criteria (no significant improvement)
        # This checks if the current correlation is not significantly better than the previous one AND not the best overall.
        if i > 0 and (correlation - optimization_history[-2]['correlation']) < MIN_CORRELATION_IMPROVEMENT and correlation < best_correlation:
            print(f"Stopping early: Improvement ({correlation:.4f} - {optimization_history[-2]['correlation']:.4f}) below threshold {MIN_CORRELATION_IMPROVEMENT} and not the best prompt.")
            break

        # Step 3: Run Judge LLM to get new prompt
        # Passes a copy of history with sensitive/large parts removed for cleaner input
        critique, revised_prompt, reasoning = run_judge_llm(
            current_analyst_prompt,
            correlation,
            [{k: v for k, v in entry.items() if k not in ["detailed_results", "prompt_before_judge"]} for entry in optimization_history] # Pass summarized history
        )

        # Update history with Judge's output
        optimization_history[-1]["critique"] = critique
        optimization_history[-1]["proposed_prompt"] = revised_prompt
        optimization_history[-1]["reasoning"] = reasoning
        optimization_history[-1]["proposed_prompt_summary"] = summarize_prompt(revised_prompt)

        print(f"Judge's Critique: {critique}")
        print(f"Judge's Reasoning: {reasoning}")
        print(f"Revised Prompt: {summarize_prompt(revised_prompt)}")

        # Update the prompt for the next iteration
        current_analyst_prompt = revised_prompt

        # Optional: Add a small delay to respect API rate limits
        time.sleep(2)

    print("\n===== Optimization Complete =====")
    print(f"Final Best Correlation: {best_correlation:.4f}")
    print(f"Optimized Analyst Prompt:\n{best_prompt}")

    # Save full optimization history
    history_filename = f"optimization_history_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
    with open(history_filename, "w") as f:
        json.dump(optimization_history, f, indent=2) # json.dump writes a dictionary to a file as JSON
    print(f"Full optimization history saved to {history_filename}")

    return best_prompt, best_correlation, optimization_history


    # --- Run the optimization ---
if __name__ == "__main__":
    optimized_prompt, final_correlation, full_history = run_optimization()