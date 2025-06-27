# Multiple prompts for LLM request: -----

prompt <- c("
Context:
You are simulating the Euro area interest rate swap market, composed of 20 individual traders.
These traders interpret the ECB Governing Council press conference, which communicates monetary policy decisions, economic assessments, and includes a Q&A session with journalists.
Each trader then makes a trading decision to maximize profit based on their interpretation of the conference and their unique characteristics.

Trader Characteristics:
Each trader has the following attributes:
- Risk Aversion: High / Medium / Low — determines sensitivity to uncertainty and preference for stability.
- Behavioral Biases (1–2 per trader): e.g., Confirmation Bias, Overconfidence, Anchoring, Herding, Loss Aversion, Recency Bias.
- Interpretation Style (1 per trader): e.g., Fundamentalist, Sentiment Reader, Quantitative, Skeptic, Narrative-Driven.

Task:
You are given a certain number of distinct ECB press conferences.
For each of the 20 traders, simulate their individual trading action in the interest rate swap market across three tenors (3 months, 2 years, 10 years).
For each tenor, the trader must:
   - Provide an expected rate direction: Up / Down / Unchanged
   - Provide a new expected swap rate (in percent, to two decimal places)
   - Provide a one-sentence rationale for the trading decision

Output:
Provide a table with the following structure for each press conference, trader, and interest rate tenor:

| Date       | Trader ID | Tenor   | Expected Direction | New Expected Rate (%) | Rationale (1 sentence)          |
|------------|-----------|---------|--------------------|------------------------|--------------------------------|
| YYYY-MM-DD | T001      | 3M      | Up                 | 3.15                   | [Trader's rationale]           |
| YYYY-MM-DD | T001      | 2Y      | Down               | 2.85                   | [Trader's rationale]           |
| ...        | ...       | ...     | ...                | ...                    | ...                            |

Guidelines:
- Use only the information available as of [date].
- Do not aggregate or summarize responses.
- Reflect diversity in interpretation, risk tolerance, and horizon. Rationale must be unique for each trader and can vary across tenors.
- Output only a markdown table with the specified columns, no additional text. Do not use JSON or any other data serialization format.
- If multiple press conferences are included, clearly distinguish between them using the 'Date' field.
")


prompt_anchor_values <- c("
Context:
You are simulating the Euro area interest rate swap market, composed of 20 individual traders.
These traders interpret the ECB Governing Council press conference, which communicates monetary policy decisions, economic assessments, and includes a Q&A session with journalists.
Each trader then makes a trading decision to maximize profit based on their interpretation of the conference and their unique characteristics.

Trader Characteristics:
Each trader has the following attributes:
- Risk Aversion: High / Medium / Low — determines sensitivity to uncertainty and preference for stability.
- Behavioral Biases (1–2 per trader): e.g., Confirmation Bias, Overconfidence, Anchoring, Herding, Loss Aversion, Recency Bias.
- Interpretation Style (1 per trader): e.g., Fundamentalist, Sentiment Reader, Quantitative, Skeptic, Narrative-Driven.

Task:
You are given a certain number of distinct ECB press conferences.
For each of the 20 traders, simulate their individual trading action in the interest rate swap market across three tenors (3 months, 2 years, 10 years).
For each tenor, the trader must:
   - Provide an expected rate direction: Up / Down / Unchanged
   - Provide a new expected swap rate (in percent, to two decimal places)

Output:
Provide a table with the following structure for each press conference, trader, and interest rate tenor:

| Date       | Trader ID | Tenor   | Expected Direction | New Expected Rate (%)  |
|------------|-----------|---------|--------------------|------------------------|
| YYYY-MM-DD | T001      | 3M      | Up                 | 3.15                   |
| YYYY-MM-DD | T001      | 2Y      | Down               | 2.85                   | 
| ...        | ...       | ...     | ...                | ...                    |

Guidelines:
- At the beginning of each press conference text, we provide the value of OIS rates just before the conference. If not available is an NA.
- Use only the information available as of [date].
- Do not aggregate or summarize responses.
- Reflect diversity in interpretation, risk tolerance, and horizon. Rationale must be unique for each trader and can vary across tenors.
- Output only a markdown table with the specified columns, no additional text. Do not use JSON or any other data serialization format.
- If multiple press conferences are included, clearly distinguish between them using the 'Date' field.
")