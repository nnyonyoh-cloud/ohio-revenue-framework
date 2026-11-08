# =============================================================================
# 04_tax_law.R
# Ohio revenue framework, Part 4: adjusting for tax-law changes
#
# Author: Nicholas Nyonyoh
# Written and run in R by the author, with coding help and explanations from
# an AI assistant (Claude). The author compiled the list of Ohio tax-law
# changes from his own tax-practice knowledge, ran and checked all results,
# and wrote the interpretation.
#
# Why this matters: tax collections change not only with the economy but also
# when lawmakers change rates or bases. A model that ignores this can mistake
# a tax cut for an economic slowdown. Here we add two flags:
#   increase = 1 in a fiscal year when a tax increase or base expansion took
#              effect
#   cut      = 1 in a fiscal year when a tax cut took effect
# and test whether they improve the forecast and scenario models.
#
# Note on fairness of the forecast test: enacted tax-law changes are known
# before the fiscal year starts (budget offices score them when bills pass),
# so using them in a forecast does not use future information.
# =============================================================================

library(tidyverse)

# --- 1. Load the panel and the tax-law table --------------------------------
panel <- read_csv("data/panel.csv", show_col_types = FALSE) |> arrange(fy)

# data/tax_law_changes.csv is the author's own table of Ohio tax-law changes,
# mapped to fiscal years. Years not listed have no change (0).
laws <- read_csv("data/tax_law_changes.csv", show_col_types = FALSE) |>
  select(fy, increase, cut) |>
  # Some fiscal years have more than one row (for example, separate income
  # tax and CAT changes in fiscal 2024). Combine them into one row per year:
  # a flag is 1 if ANY change that year was an increase (or a cut).
  group_by(fy) |>
  summarise(increase = max(increase), cut = max(cut), .groups = "drop")

panel <- panel |>
  left_join(laws, by = "fy") |>
  mutate(
    increase = replace_na(increase, 0),
    cut      = replace_na(cut, 0)
  ) |>
  mutate(across(
    c(g_total, inflation, d_fedfunds, d_unemp, gdp_growth),
    ~ dplyr::lag(.x),
    .names = "{.col}_l1"
  ))

model_data <- panel |>
  filter(fy >= 1979) |>
  drop_na(g_total, g_total_l1, inflation_l1, d_fedfunds_l1, d_unemp_l1,
          gdp_growth_l1, inflation, d_fedfunds, d_unemp, gdp_growth)

cat("Years flagged in the model sample: increases =", sum(model_data$increase),
    ", cuts =", sum(model_data$cut), "\n")


# --- 2. Forecast models: without and with tax-law flags ----------------------
f_base <- g_total ~ g_total_l1 + inflation_l1 + d_fedfunds_l1 +
  d_unemp_l1 + gdp_growth_l1
f_law  <- update(f_base, . ~ . + increase + cut)   # same model plus the flags

test_years <- 2000:2025

results <- map_dfr(test_years, function(t) {
  train <- model_data |> filter(fy < t)
  test  <- model_data |> filter(fy == t)
  if (nrow(test) == 0) return(NULL)
  tibble(
    fy        = t,
    actual    = test$g_total,
    model     = as.numeric(predict(lm(f_base, data = train), newdata = test)),
    model_law = as.numeric(predict(lm(f_law,  data = train), newdata = test)),
    drift10   = mean(tail(train$g_total, 10))
  )
})

accuracy <- results |>
  pivot_longer(c(model, model_law, drift10), names_to = "method",
               values_to = "forecast") |>
  mutate(error = actual - forecast) |>
  group_by(method) |>
  summarise(rmse = sqrt(mean(error^2)), mae = mean(abs(error)),
            bias = mean(error), n = n(), .groups = "drop") |>
  mutate(relative_rmse = rmse / rmse[method == "drift10"]) |>
  arrange(rmse)

print(accuracy)

if (requireNamespace("forecast", quietly = TRUE)) {
  cat("\nModel with tax-law flags vs model without:\n")
  print(forecast::dm.test(results$actual - results$model_law,
                          results$actual - results$model, h = 1))
  cat("\nModel with tax-law flags vs 10-year average:\n")
  print(forecast::dm.test(results$actual - results$model_law,
                          results$actual - results$drift10, h = 1))
}

# Which years did the flags help most? (positive = smaller miss with flags)
results |>
  mutate(improvement = abs(actual - model) - abs(actual - model_law)) |>
  arrange(desc(abs(improvement))) |>
  select(fy, actual, model, model_law, improvement) |>
  head(8) |>
  print()


# --- 3. Scenario model: without and with tax-law flags -----------------------
fit_sc     <- lm(g_total ~ inflation + d_fedfunds + d_unemp + gdp_growth,
                 data = model_data)
fit_sc_law <- lm(g_total ~ inflation + d_fedfunds + d_unemp + gdp_growth +
                   increase + cut, data = model_data)

summary(fit_sc_law)

if (requireNamespace("sandwich", quietly = TRUE) &&
    requireNamespace("lmtest", quietly = TRUE)) {
  print(lmtest::coeftest(fit_sc_law,
                         vcov = sandwich::NeweyWest(fit_sc_law, lag = 1)))
}

# Does adding the flags explain significantly more? (F-test)
print(anova(fit_sc, fit_sc_law))

cat("\nAdjusted R-squared without flags:", round(summary(fit_sc)$adj.r.squared, 3),
    " with flags:", round(summary(fit_sc_law)$adj.r.squared, 3), "\n")


# --- 4. Save --------------------------------------------------------------------
write_csv(results,  "output/forecast_results_taxlaw.csv")
write_csv(accuracy, "output/accuracy_taxlaw.csv")
cat("Saved results to the output folder.\n")
