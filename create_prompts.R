# Multiple prompts for LLM request: -----

prompt_naive <- c("
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



prompt_microstructure <- c("
Context:
You are simulating the Euro area interest rate swap market, composed of 20 individual traders.
These traders interpret the ECB Governing Council press conference, which communicates monetary policy decisions, economic assessments, and includes a Q&A session with journalists.
Each trader then makes a trading decision to maximize profit based on their interpretation of the conference, their unique characteristics, and their institutional role in the market.

Trader Characteristics:
Each trader has the following attributes:
- Institutional Role: One of [Bank, Hedge Fund, Pension Fund, Insurance Firm, Central Bank, Retail Trader]
- Risk Aversion: High / Medium / Low — determines sensitivity to uncertainty and preference for stability.
- Behavioral Biases (1–2 per trader): e.g., Confirmation Bias, Overconfidence, Anchoring, Herding, Loss Aversion, Recency Bias.
- Interpretation Style (1 per trader): e.g., Fundamentalist, Sentiment Reader, Quantitative, Skeptic, Narrative-Driven.

Market Structure:
The composition of the euro OIS market evolves over time. Based on the date of the ECB press conference, use the following market composition to assign institutional roles to the 20 traders:

- 1998–2002: Banks 80%, Hedge Funds 5%, Pension Funds 5%, Insurance Firms 5%, Central Banks 4%, Others 1%
- 2003–2007: Banks 70%, Hedge Funds 10%, Pension Funds 7%, Insurance Firms 7%, Central Banks 5%, Others 1%
- 2008–2012: Banks 60%, Hedge Funds 15%, Pension Funds 8%, Insurance Firms 8%, Central Banks 7%, Others 2%
- 2013–2015: Banks 55%, Hedge Funds 18%, Pension Funds 10%, Insurance Firms 10%, Central Banks 5%, Others 2%
- 2016–2020: Banks 50%, Hedge Funds 20%, Pension Funds 12%, Insurance Firms 12%, Central Banks 4%, Others 2%
- 2021–2025: Banks 40%, Hedge Funds 25%, Pension Funds 10%, Insurance Firms 10%, Central Banks 5%, Others 10%

Assign each trader one of these institutional roles based on the date of the press conference. Their role affects:
- Tenor focus (e.g., Pension Funds = 10Y, Hedge Funds = 2Y)
- Trading horizon and frequency
- Reaction function to ECB communication
- Exposure to regulatory or liquidity constraints

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
- At the beginning of each press conference text, we provide the value of OIS rates just before the conference. If not available, it is marked as NA.
- Use only the information available as of [date].
- Do not aggregate or summarize responses.
- Reflect diversity in interpretation, institutional role, and behavioral profile. Rationale must be unique for each trader and can vary across tenors.
- Output only a markdown table with the specified columns, no additional text. Do not use JSON or any other data serialization format.
- If multiple press conferences are included, clearly distinguish between them using the 'Date' field.
")

  
prompt_history_surprises_mean <- c("
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
- Use only the information available as of [date].
- Do not aggregate or summarize responses.
- Reflect diversity in interpretation, risk tolerance, and horizon. Rationale must be unique for each trader and can vary across tenors.
- Output only a markdown table with the specified columns, no additional text. Do not use JSON or any other data serialization format.
- If multiple press conferences are included, clearly distinguish between them using the 'Date' field.
")
  
  
  