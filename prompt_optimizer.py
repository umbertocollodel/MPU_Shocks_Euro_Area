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
MAX_OPTIMIZATION_ITERATIONS = 10

# Threshold for improvement to continue optimization (e.g., 0.001 increase in correlation)
MIN_CORRELATION_IMPROVEMENT = 0.05


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
You are a highly analytical financial expert specializing in macroeconomic analysis. Your task is to analyze excerpts from central bank press conferences. For each excerpt, you need to predict the percentage change in the 10-year Overnight Index Swap (OIS) rate (in basis points) that you expect the market to react with *immediately after* the conference.

Consider all factors: explicit forward guidance, nuances in language, tone, implied policy shifts, and any ambiguity that might lead to diverse market interpretations.

Provide your prediction as a single numerical value (e.g., +5, -2, 0). If you believe the statement is highly ambiguous and could lead to widely diverging market interpretations, this should be reflected in the variability of your predictions across multiple mental simulations (which will be captured by the standard deviation of your responses).

Example Format for your response (just the number):
+5
"""

JUDGE_SYSTEM_PROMPT = """
You are an expert AI system designed to optimize prompts for another AI (the "Analyst LLM"). Your goal is to refine the Analyst LLM's prompt to improve its ability to:
1.  Predict the immediate percentage change in the 10-year OIS rate following central bank press conferences.
2.  Crucially, ensure the *standard deviation* of the Analyst LLM's predictions for a given transcript *correlates highly* with actual market volatility (represented by the real-world 10Y OIS rate standard deviation over the next 24 hours).

You will be provided with:
- The current Analyst LLM prompt.
- The most recent performance (Pearson correlation coefficient between Analyst LLM's predicted standard deviations and actual market volatility).
- The historical performance trend and critiques.

Your task is to:
1.  **Critique** the current prompt, identifying specific weaknesses that might hinder high correlation. Consider clarity, specificity, emphasis on uncertainty, and guidance on numerical output.
2.  **Suggest a Revised Prompt** that addresses these weaknesses.
3.  **Explain your reasoning** for the revisions.

Your output must be in JSON format. Do not include any other text outside the JSON.

Example JSON output:
```json
{
  "critique": "The previous prompt was too general regarding how to express uncertainty. It didn't explicitly ask the Analyst LLM to consider multiple viewpoints.",
  "revised_prompt": "You are a highly analytical financial expert... When analyzing the excerpt, explicitly consider potential bull and bear cases for the OIS rate, and let your internal uncertainty about these cases be reflected in your final numerical prediction range across multiple runs.",
  "reasoning": "Adding explicit instruction to consider bull/bear cases helps the Analyst LLM generate a more meaningful spread of predictions, directly improving its ability to capture uncertainty for correlation.
  }
  """

  # --- Core Functions ---

def run_analyst_llm_for_transcript(transcript_text: str, analyst_prompt: str, num_runs: int = NUM_ANALYST_LLM_RUNS) -> List[float]:
    """
    Runs the Analyst LLM multiple times for a single transcript
    and collects its numerical predictions.
    """
    predictions = []
    for _ in range(num_runs): # _ is a common Python convention for a variable you don't use
        user_message = f"Analyze the following central bank press conference excerpt and predict the percentage change in the 10-year OIS rate in basis points: \n\n{transcript_text}\n\nYour prediction (just the number):"
        try:
            response_content = call_llm(
                model=ANALYST_LLM_MODEL,
                system_prompt=analyst_prompt,
                user_message=user_message,
                temperature=0.7 # Keep temperature moderate for consistent but varied responses
            )
            # Attempt to parse the prediction, handle non-numeric responses
            prediction = float(response_content.strip().replace('+', '')) # Remove '+' if present
            predictions.append(prediction)
        except (ValueError, AttributeError) as e:
            print(f"Warning: Analyst LLM returned non-numeric prediction: '{response_content}'. Error: {e}")
            continue # Skip this non-numeric prediction
    return predictions

def evaluate_analyst_performance(
    current_analyst_prompt: str,
    historical_data: pd.DataFrame
) -> Tuple[float, pd.DataFrame]:
    """
    Evaluates the current Analyst LLM prompt by running it on historical data
    and calculating the correlation between predicted SD and actual volatility.
    """
    analyst_sds = []
    actual_volatilities = []
    results_detail = []

    print(f"\n--- Evaluating Analyst LLM with current prompt (on {len(historical_data)} transcripts) ---")
    for index, row in historical_data.iterrows(): # Iterating through DataFrame rows
        transcript_id = row['transcript_id']
        transcript_text = row['transcript_text']
        actual_volatility = row['actual_ois_volatility_bps']

        predictions = run_analyst_llm_for_transcript(transcript_text, current_analyst_prompt)

        if len(predictions) >= 2: # Need at least two predictions to calculate SD
            predicted_sd = np.std(predictions) # Calculate standard deviation using numpy
            analyst_sds.append(predicted_sd)
            actual_volatilities.append(actual_volatility)
            results_detail.append({
                "transcript_id": transcript_id,
                "actual_volatility": actual_volatility,
                "analyst_predictions": predictions,
                "predicted_sd": predicted_sd
            })
        else:
            print(f"Skipping {transcript_id}: Not enough valid predictions ({len(predictions)}) to calculate SD.")

    if len(analyst_sds) < 2 or len(actual_volatilities) < 2:
        print("Not enough data points with valid SDs to calculate correlation. Returning 0.0 correlation.")
        return 0.0, pd.DataFrame(results_detail)

    correlation, p_value = pearsonr(analyst_sds, actual_volatilities) # Pearson correlation using scipy
    print(f"Calculated Pearson Correlation: {correlation:.4f} (p-value: {p_value:.4f})")

    return correlation, pd.DataFrame(results_detail)

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
            temperature=0.8, # Higher temp for creativity in critiques/revisions
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
    best_correlation = -1.0 # Initialize with a low value
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
            with open("best_analyst_prompt.txt", "w") as f: # 'w' for write mode
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