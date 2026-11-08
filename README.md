# Ohio Revenue Framework

An early prototype of a framework that forecasts how interest rates and inflation affect state tax revenue, starting with Ohio.

**Author:** Nicholas Nyonyoh
**Status:** Work in progress, started November 2026

## What this project does

State budgets depend on revenue forecasts, and those forecasts miss most when the economy turns. This project builds open, tested models that show how Ohio's tax revenue is likely to respond to changes in interest rates, inflation, unemployment and economic growth. It produces scenario estimates rather than a single forecast. All data are public, and all code is published here so anyone can check and reuse it.

## Data

All series are downloaded automatically from FRED (Federal Reserve Bank of St. Louis). The untouched downloads are saved in `data/raw/`.

| Series | Description | Frequency | Original source |
|---|---|---|---|
| OHTOTLTAX | Ohio total state tax collections (thousands of dollars) | Annual (fiscal year) | U.S. Census Bureau |
| OHINCTAX | Ohio individual income tax collections | Annual (fiscal year) | U.S. Census Bureau |
| OHSALESTAX | Ohio general sales tax collections | Annual (fiscal year) | U.S. Census Bureau |
| FEDFUNDS | Federal funds effective rate | Monthly | Federal Reserve |
| CPIAUCSL | Consumer price index, all urban consumers | Monthly | Bureau of Labor Statistics |
| OHUR | Ohio unemployment rate | Monthly | Bureau of Labor Statistics |
| GDPC1 | U.S. real GDP | Quarterly | Bureau of Economic Analysis |

**Fiscal year:** Ohio's fiscal year runs July 1 to June 30 and is named for the year it ends (fiscal 2009 = July 2008 to June 2009). Monthly and quarterly data are averaged over each fiscal year so they line up with the tax data.

## How to run it

1. Install [R](https://cran.r-project.org/) and [RStudio](https://posit.co/download/rstudio-desktop/).
2. Open this folder as an RStudio project.
3. Install the packages once:
   ```r
   install.packages(c("tidyverse", "sandwich", "lmtest", "forecast"))
   ```
4. Run the scripts in order:

| Script | What it does | Output |
|---|---|---|
| `R/01_load_revenue.R` | Downloads Ohio tax data and computes annual growth | `data/ohio_revenue.csv` |
| `R/02_load_macro.R` | Downloads economic data and converts it to Ohio fiscal years | `data/panel.csv` |
| `R/03_model.R` | Forecast model, out-of-sample test (2000–2025), scenario model | `output/` |

## Results so far

[Fill in after running Part 3, in your own words: two or three sentences on whether the forecast model beat the simple benchmarks, and what the scenario model shows. Link to output/accuracy.csv and output/scenarios.csv.]

## Next steps

- Adjust for Ohio tax-law changes (for example, the 2005 tax reform and income tax rate cuts)
- Separate models for income and sales tax
- Machine learning methods compared with the regression models
- Extend to other states

## Disclosure

The code was written and run in R by the author, with coding help and explanations from an AI assistant (Claude). The author designed the research, ran and checked all results against the source data, and wrote the interpretation.

## Contact

Nicholas Nyonyoh · nnyonyoh@gmail.com
