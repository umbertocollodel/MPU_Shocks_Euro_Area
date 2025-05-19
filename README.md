# MPU Shocks in the Euro Area

This repository contains replication code for the first part of the Central Bank of Malta Working Paper ["MPU Shocks in the Euro Area"](https://www.centralbankmalta.org/site/Publications/Economic%20Research/2025/WP-01-2025.pdf?revcount=6302) published in 2025, as well as extensions in the pipeline.

The code constructs the MPU index and performs validity tests and graphical inspections. It also derives different factors for different portions of the yield curve similar to MP surprises. Finally, it scrapes ECB press conferences and calculates an individual complexity score for each one of them to be correlated with the change in uncertainty.

## Scripts overview

- **uncertainty.R**: Calculates monetary policy uncertainty by processing raw daily OIS data.
- **1create_factors.R**: Conducts factor decomposition of OIS yield ranges around GovC meetings.
- **02plot_factors.R**: Generates and saves plots for factor decomposition and monetary surprises.
- **03prelim_notebook.R**: Provides an R Notebook for executing and visualizing code.
- **04scraping_ecb_pressconf.R**: Scrapes ECB press conference texts, dates, and governor names.
- **05processing_documents.R**: Processes ECB press conference texts for readability analysis.
- **06appendix_run_exogeneity_tests.R**: Performs exogeneity tests on OIS yield range differences to check for autocorrelation.

