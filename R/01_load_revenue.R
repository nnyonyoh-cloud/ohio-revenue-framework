# =============================================================================
# 01_load_revenue.R
# Ohio revenue framework, Part 1: download and prepare Ohio tax revenue data
#
# Author: Nicholas Nyonyoh
# Written and run in R by the author, with coding help and explanations from
# an AI assistant (Claude). The author ran, checked and interpreted all results.
#
# What this script does:
#   1. Downloads three Ohio tax series from FRED (St. Louis Fed), which
#      republishes the U.S. Census Bureau's Annual Survey of State Tax
#      Collections.
#   2. Saves an untouched copy of each download (so anyone can check our data).
#   3. Combines them into one table, one row per Ohio fiscal year.
#   4. Computes annual growth rates.
#   5. Saves the result to data/ohio_revenue.csv for the next scripts.
# =============================================================================


# --- 0. One-time setup --------------------------------------------------------
# Run this line ONCE (remove the # in front, run it, then put the # back).
# It installs the "tidyverse", a standard set of packages for data work.
# install.packages("tidyverse")

library(tidyverse)   # loads readr (reading files), dplyr (data steps), etc.

# Make the folders we save into. "showWarnings = FALSE" means: don't complain
# if the folder already exists.
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)


# --- 1. A small helper function to download one FRED series -----------------
# Every FRED series can be downloaded as a CSV from a URL of this form:
#   https://fred.stlouisfed.org/graph/fredgraph.csv?id=SERIES_ID
# The file has two columns: a date, and the value.
#
# This function takes a series ID (for example "OHTOTLTAX"), downloads it,
# saves the raw file, and returns a clean table with two columns:
#   date  : the date FRED stamps on the value (January 1 of each year)
#   value : the number (FRED writes "." for missing values; we turn those
#           into NA, R's code for "missing")

get_fred <- function(series_id) {
  url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", series_id)

  raw <- read_csv(url, show_col_types = FALSE)

  # Save the untouched download, e.g. data/raw/OHTOTLTAX.csv
  write_csv(raw, file.path("data/raw", paste0(series_id, ".csv")))

  # FRED's column names vary ("DATE" or "observation_date"), so we rename
  # by position: column 1 is the date, column 2 is the value.
  names(raw) <- c("date", "value")

  raw |>
    mutate(
      date  = as.Date(date),
      value = suppressWarnings(as.numeric(value))   # "." becomes NA
    )
}


# --- 2. Download the three Ohio tax series -----------------------------------
# All three are in THOUSANDS of dollars.
#   OHTOTLTAX  : total state tax collections
#   OHINCTAX   : individual income tax collections
#   OHSALESTAX : general sales and gross receipts tax collections

total  <- get_fred("OHTOTLTAX")
income <- get_fred("OHINCTAX")
sales  <- get_fred("OHSALESTAX")


# --- 3. Combine into one table, one row per fiscal year ----------------------
# FRED stamps each annual value as January 1, but the Census reports state
# taxes by each state's FISCAL year. Ohio's fiscal year runs July 1 to June 30
# and is named after the year it ends in. So "2009-01-01" means fiscal 2009,
# July 2008 to June 2009. We keep just the year and call it fy.

ohio <- total |>
  transmute(fy = year(date), total = value) |>
  full_join(transmute(income, fy = year(date), income = value), by = "fy") |>
  full_join(transmute(sales,  fy = year(date), sales  = value), by = "fy") |>
  arrange(fy)


# --- 4. Annual growth rates ---------------------------------------------------
# Growth = 100 * (log of this year - log of last year).
# Log differences are close to percent changes and are what the models use.
# dplyr::lag() gives the previous row's value. (We write dplyr::lag to avoid
# confusion with a different function, stats::lag.)

ohio <- ohio |>
  mutate(
    g_total  = 100 * (log(total)  - log(dplyr::lag(total))),
    g_income = 100 * (log(income) - log(dplyr::lag(income))),
    g_sales  = 100 * (log(sales)  - log(dplyr::lag(sales)))
  )

# Tax-law note (from my own knowledge of Ohio taxes):
# Ohio's personal income tax began in 1972, so the first years of income tax
# growth reflect the tax being phased in, not the economy. Later scripts will
# start the income tax analysis after those start-up years.


# --- 5. Check the results -----------------------------------------------------
# Fiscal 2009 (the Great Recession) is a good check. Expected log changes:
#   total about -8.62, income about -16.82, sales about -7.08
ohio |>
  filter(fy %in% c(2008, 2009, 2010, 2024, 2025)) |>
  print()

# A quick picture of total revenue growth over time
ggplot(ohio, aes(x = fy, y = g_total)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  labs(
    title = "Ohio total state tax collections, annual growth",
    x = "Fiscal year",
    y = "Log change, percent",
    caption = "Source: U.S. Census Bureau via FRED (OHTOTLTAX)"
  )


# --- 6. Save for the next script ---------------------------------------------
write_csv(ohio, "data/ohio_revenue.csv")
cat("Saved data/ohio_revenue.csv with", nrow(ohio), "rows.\n")
