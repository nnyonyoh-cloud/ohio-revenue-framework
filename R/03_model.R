# =============================================================================
# 03_model.R
# Ohio revenue framework, Part 3: first models, out-of-sample test, scenarios
#
# Author: Nicholas Nyonyoh
# Written and run in R by the author, with coding help and explanations from
# an AI assistant (Claude). The author ran, checked and interpreted all results.
#
# Two models, for two different questions:
#
#   A. FORECAST model: "Standing at budget time, what will revenue growth be
#      next fiscal year?" It may only use information known at that time, so
#      every predictor is LAGGED one fiscal year.
#
#   B. SCENARIO model: "IF inflation, interest rates, unemployment and GDP
#      move a certain way during a fiscal year, how is revenue likely to
#      respond?" It uses same-year values, because the user supplies the
#      scenario.
#
# Model A is tested honestly: for each year 2000-2025 it is estimated using
# only earlier years, then compared with what actually happened and with two
# simple benchmarks.
# =============================================================================


# --- 0. One-time setup --------------------------------------------------------
# Run ONCE (remove the #, run, put the # back):
# install.packages(c("sandwich", "lmtest", "forecast"))

library(tidyverse)
dir.create("output", showWarnings = FALSE)


# --- 1. Load the panel and create lagged variables ---------------------------
panel <- read_csv("data/panel.csv", show_col_types = FALSE) |>
  arrange(fy)

# For each variable, make a copy shifted down one year, named "..._l1".
# Example: in the fiscal 2009 row, inflation_l1 holds fiscal 2008 inflation.
panel <- panel |>
  mutate(across(
    c(g_total, inflation, d_fedfunds, d_unemp, gdp_growth),
    ~ dplyr::lag(.x),
    .names = "{.col}_l1"
  ))

# Ohio's monthly unemployment rate starts in 1976, so the first complete
# fiscal year is 1977, the first CHANGE is 1978 and the first LAGGED change is
# 1979. The model sample therefore starts in fiscal 1979.
model_data <- panel |>
  filter(fy >= 1979) |>
  drop_na(g_total, g_total_l1, inflation_l1, d_fedfunds_l1, d_unemp_l1,
          gdp_growth_l1, inflation, d_fedfunds, d_unemp, gdp_growth)

cat("Model sample: fiscal", min(model_data$fy), "to", max(model_data$fy),
    "-", nrow(model_data), "years\n")


# --- 2. Model A: forecast model (lagged predictors) --------------------------
# Revenue growth this year explained by last year's revenue growth, inflation,
# change in interest rates, change in unemployment and GDP growth.
f_forecast <- g_total ~ g_total_l1 + inflation_l1 + d_fedfunds_l1 +
  d_unemp_l1 + gdp_growth_l1


# --- 3. Out-of-sample test, fiscal 2000 to 2025 -------------------------------
# For each test year t:
#   - estimate Model A on years BEFORE t only
#   - forecast year t
#   - also record two simple benchmarks:
#       naive   : this year's growth = last year's growth
#       drift10 : this year's growth = average growth over the last 10 years
# map_dfr() runs the function for each year and stacks the results in a table.

test_years <- 2000:2025

results <- map_dfr(test_years, function(t) {
  train <- model_data |> filter(fy < t)
  test  <- model_data |> filter(fy == t)
  if (nrow(test) == 0) return(NULL)

  fit <- lm(f_forecast, data = train)

  tibble(
    fy      = t,
    actual  = test$g_total,
    model   = as.numeric(predict(fit, newdata = test)),
    naive   = test$g_total_l1,
    drift10 = mean(tail(train$g_total, 10))
  )
})

# Accuracy: RMSE (penalizes big misses), MAE (average miss) and bias
# (positive = forecasts too low on average, negative = too high).
accuracy <- results |>
  pivot_longer(c(model, naive, drift10), names_to = "method",
               values_to = "forecast") |>
  mutate(error = actual - forecast) |>
  group_by(method) |>
  summarise(
    rmse = sqrt(mean(error^2)),
    mae  = mean(abs(error)),
    bias = mean(error),
    n    = n(),
    .groups = "drop"
  ) |>
  mutate(relative_rmse = rmse / rmse[method == "drift10"]) |>
  arrange(rmse)

print(accuracy)
# relative_rmse below 1 means the method beat the 10-year average benchmark.

# Is the model's accuracy significantly different from the benchmark?
# Diebold-Mariano test (with the Harvey-Leybourne-Newbold correction).
if (requireNamespace("forecast", quietly = TRUE)) {
  e_model   <- results$actual - results$model
  e_drift10 <- results$actual - results$drift10
  print(forecast::dm.test(e_model, e_drift10, h = 1))
}

# Picture: actual growth against the forecasts
results |>
  pivot_longer(c(actual, model, drift10), names_to = "series",
               values_to = "growth") |>
  ggplot(aes(x = fy, y = growth, colour = series)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, colour = "grey50") +
  labs(
    title = "Ohio total tax revenue growth: actual vs out-of-sample forecasts",
    x = "Fiscal year", y = "Log change, percent", colour = NULL
  )


# --- 4. Model B: scenario model (same-year conditions) -----------------------
f_scenario <- g_total ~ inflation + d_fedfunds + d_unemp + gdp_growth
fit_sc <- lm(f_scenario, data = model_data)
summary(fit_sc)

# Standard errors that allow for correlation over time (Newey-West).
if (requireNamespace("sandwich", quietly = TRUE) &&
    requireNamespace("lmtest", quietly = TRUE)) {
  print(lmtest::coeftest(fit_sc, vcov = sandwich::NeweyWest(fit_sc, lag = 1)))
}


# --- 5. Scenarios --------------------------------------------------------------
# A baseline and four alternatives. YOU can change these numbers.
#   inflation  : percent
#   d_fedfunds : change in the federal funds rate, points
#   d_unemp    : change in Ohio unemployment, points
#   gdp_growth : percent
scenarios <- tribble(
  ~scenario,                    ~inflation, ~d_fedfunds, ~d_unemp, ~gdp_growth,
  "Baseline",                          2.5,         0.0,      0.0,         2.0,
  "Higher inflation (+2 pts)",         4.5,         0.0,      0.0,         2.0,
  "Rate increase (+1 pt)",             2.5,         1.0,      0.0,         2.0,
  "Mild recession",                    2.0,        -1.0,      1.5,        -1.0,
  "Stagflation",                       5.0,         1.0,      1.0,         0.0
)

# Predicted revenue growth with an 80 percent prediction interval
pred <- predict(fit_sc, newdata = scenarios, interval = "prediction",
                level = 0.80)

scenario_table <- bind_cols(scenarios, as_tibble(pred)) |>
  rename(predicted_growth = fit, low_80 = lwr, high_80 = upr)

print(scenario_table)


# --- 6. Save results -----------------------------------------------------------
write_csv(results,        "output/forecast_results.csv")
write_csv(accuracy,       "output/accuracy.csv")
write_csv(scenario_table, "output/scenarios.csv")
cat("Saved results to the output folder.\n")


# --- 7. Your next step (tax-law adjustment) ----------------------------------
# Some years moved because of tax-law changes, not the economy (for example,
# the start of the income tax in 1972, the 2005 tax reform, income tax rate
# cuts). Next, we will add a variable flagging those years, using YOUR list,
# and see whether the models improve. Write your list of years and changes
# here as comments:
#
#
