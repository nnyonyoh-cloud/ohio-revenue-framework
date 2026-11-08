# =============================================================================
# 02_load_macro.R
# Ohio revenue framework, Part 2: economic variables, on Ohio's fiscal year
#
# Author: Nicholas Nyonyoh
# Written and run in R by the author, with coding help and explanations from
# an AI assistant (Claude). The author ran, checked and interpreted all results.
#
# What this script does:
#   1. Downloads monthly and quarterly U.S. and Ohio economic data from FRED.
#   2. Converts each to Ohio FISCAL years (July to June), so it lines up with
#      the tax data from Part 1.
#   3. Builds the variables the model will use: inflation, interest rates,
#      unemployment and real GDP growth.
#   4. Joins everything with the revenue data and saves data/panel.csv.
# =============================================================================

library(tidyverse)

# Reuse the download helper from Part 1. source() runs another script; here we
# just copy the function so this script can run on its own.
get_fred <- function(series_id) {
  url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", series_id)
  raw <- read_csv(url, show_col_types = FALSE)
  write_csv(raw, file.path("data/raw", paste0(series_id, ".csv")))
  names(raw) <- c("date", "value")
  raw |> mutate(date = as.Date(date), value = suppressWarnings(as.numeric(value)))
}


# --- 1. Download the economic series -----------------------------------------
#   FEDFUNDS : federal funds effective rate, percent, monthly
#   CPIAUCSL : consumer price index for all urban consumers, monthly index
#   OHUR     : Ohio unemployment rate, percent, monthly
#   GDPC1    : U.S. real GDP, billions of chained dollars, quarterly

fedfunds <- get_fred("FEDFUNDS")
cpi      <- get_fred("CPIAUCSL")
ohur     <- get_fred("OHUR")
gdp      <- get_fred("GDPC1")


# --- 2. Convert dates to Ohio fiscal years -----------------------------------
# Ohio's fiscal year runs July 1 to June 30 and is named for the year it ends.
# So July 2008 to June 2009 is fiscal 2009.
# Rule: a month in July or later belongs to the NEXT fiscal year.
#   fy = calendar year + 1 if month is July to December
#   fy = calendar year     if month is January to June

to_fy <- function(date) {
  year(date) + if_else(month(date) >= 7, 1, 0)
}

# For each series, average all observations within each fiscal year.
# We keep only complete fiscal years (12 months, or 4 quarters for GDP), so a
# half-finished year does not distort the average.

fy_average <- function(df, name, periods_per_year) {
  df |>
    mutate(fy = to_fy(date)) |>
    group_by(fy) |>
    summarise(value = mean(value), n = n(), .groups = "drop") |>
    filter(n == periods_per_year) |>
    select(fy, value) |>
    rename(!!name := value)   # rename the column to the name we choose
}

macro <- fy_average(fedfunds, "fedfunds", 12) |>
  full_join(fy_average(cpi,  "cpi",  12), by = "fy") |>
  full_join(fy_average(ohur, "unemp", 12), by = "fy") |>
  full_join(fy_average(gdp,  "gdp",   4), by = "fy") |>
  arrange(fy)


# --- 3. Build the model variables ---------------------------------------------
#   inflation     : growth in the fiscal-year average CPI, percent
#   d_fedfunds    : change in the average federal funds rate, percentage points
#   d_unemp       : change in Ohio's average unemployment rate, points
#   gdp_growth    : growth in fiscal-year average real GDP, percent

macro <- macro |>
  mutate(
    inflation  = 100 * (log(cpi) - log(dplyr::lag(cpi))),
    d_fedfunds = fedfunds - dplyr::lag(fedfunds),
    d_unemp    = unemp - dplyr::lag(unemp),
    gdp_growth = 100 * (log(gdp) - log(dplyr::lag(gdp)))
  )


# --- 4. Join with the revenue data from Part 1 -------------------------------
revenue <- read_csv("data/ohio_revenue.csv", show_col_types = FALSE)

panel <- revenue |>
  left_join(macro, by = "fy") |>
  arrange(fy)


# --- 5. Check the results -----------------------------------------------------
# Plausibility checks you can make yourself:
#   - Inflation should be highest in recent decades around fiscal 2022-2023.
#   - The federal funds rate should average above 5 percent in fiscal 2024.
#   - Ohio unemployment should jump in fiscal 2010 and fiscal 2021.
#   - GDP growth should be negative in fiscal 2009 and fiscal 2020.
panel |>
  filter(fy %in% c(2008, 2009, 2010, 2020, 2021, 2022, 2023, 2024, 2025)) |>
  select(fy, g_total, inflation, fedfunds, d_fedfunds, unemp, gdp_growth) |>
  print()


# --- 6. Save -------------------------------------------------------------------
write_csv(panel, "data/panel.csv")
cat("Saved data/panel.csv with", nrow(panel), "rows.\n")
