#### XGBoost fDOM Forecasting — 30-day ahead predictions for 2024
# Focal variable: fDOM_1_QSU_daily (1m depth, temperature-corrected)
# Mirrors the GLM script structure for direct comparison (Option A: fit once).
#
# Three feature sets:
#   XGB_M1: fDOM ~ lag1
#   XGB_M2: fDOM ~ lag1 + Temp + DO + SW
#   XGB_M4: fDOM ~ lag1 + Temp + DO + SW + RH_Q + RH_DOC
#
# No feature scaling needed — XGBoost tree splits are scale-invariant.
# Lag is updated iteratively during forecasting (same mechanic as GLM script).
#
# Two paths to fitted models (choose one, both produce identical output objects):
#   PATH 1 — Hyperparameter tuning via time-series CV (recommended, slower)
#   PATH 2 — Out-of-box defaults (quick, good starting point)

library(tidyverse)
library(tidymodels)  # parsnip, recipes, rsample, tune, workflows, yardstick, dials
library(xgboost)
library(vip)

tidymodels_prefer()
set.seed(42)


#### 1. Load data ----
ar_df <- read_csv("./CCR_AR_Modeling/Data/Daily_catwalk_met_RH_2021_2025_lagZS.csv") |>
  mutate(Date = as.Date(Date))


#### 2. Training split ----
train_df <- ar_df |> filter(Date < ymd("2024-01-01"))
test_df  <- ar_df |> filter(Date >= ymd("2024-01-01"), Date < ymd("2025-01-01"))


#### 3. Feature sets ----
# Matching M1, M2, M4 from the GLM script (M4 uses RH_Q + RH_DOC instead of just RH_DOC_gm2day)
features_M1 <- c("fDOM_1m_lag1")

features_M2 <- c("fDOM_1m_lag1",
                  "Temp_1_C_daily", "DOsat_1_pct_daily", "SW_Wm2_daily")

features_M4 <- c("fDOM_1m_lag1",
                  "Temp_1_C_daily", "DOsat_1_pct_daily", "SW_Wm2_daily",
                  "RH_Q_m3day", "RH_DOC_mgL")

target <- "fDOM_1_QSU_daily"


#### 4. Helper: prepare clean training data for a feature set ----
# XGBoost requires no missing values in the training matrix.
# drop_na() removes rows with NAs in either the target or any feature.
# Each model may have a slightly different n due to different feature coverage.

make_train_data <- function(df, features, target = "fDOM_1_QSU_daily") {
  df |>
    select(all_of(c(target, features))) |>
    drop_na()
}

train_M1 <- make_train_data(train_df, features_M1)
train_M2 <- make_train_data(train_df, features_M2)
train_M4 <- make_train_data(train_df, features_M4)

cat("Training n — M1:", nrow(train_M1), " M2:", nrow(train_M2), " M4:", nrow(train_M4), "\n")


#### 5. Hyperparameter tuning (PATH 1) ----
# Uses time-series appropriate cross-validation via rolling_origin().
# - cumulative = TRUE: expanding window (each fold adds more training data)
# - initial = 365: minimum of 1 year in the first training fold
# - assess = 30: each fold evaluated on 30-day window (matches forecast horizon)
# - skip = 29: folds advance by 30 days at a time
#
# This produces ~18–20 CV folds from ~1000 training days.
# Latin hypercube grid samples 30 combinations across 4 hyperparameters.
# Total model fits ≈ 30 × 20 = ~600 per model. Expect ~5–15 min per model.
#
# To skip tuning entirely, jump to PATH 2 at the bottom of this section.


# 5a. Tunable model specification
xgb_spec_tune <- boost_tree(
  trees      = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  min_n      = tune()
) |>
  set_engine("xgboost", nthread = parallel::detectCores() - 1) |>
  set_mode("regression")


# 5b. Tuning function
# Returns a list with tune results, the unfinalised workflow, and the clean
# training data — everything needed for finalize_workflow() + fit() downstream.

tune_xgb_model <- function(train_clean, features, target = "fDOM_1_QSU_daily") {

  rec <- recipe(as.formula(paste(target, "~ .")), data = train_clean)

  wf <- workflow() |>
    add_recipe(rec) |>
    add_model(xgb_spec_tune)

  # Time-series CV splits (row-indexed, data must be sorted by date)
  cv_splits <- rolling_origin(
    train_clean,
    initial    = 365,
    assess     = 30,
    skip       = 29,
    cumulative = TRUE
  )

  # Latin hypercube grid — space-filling across reasonable ecological ranges
  xgb_grid <- grid_latin_hypercube(
    trees(range      = c(100L, 750L)),
    tree_depth(range = c(2L, 6L)),
    learn_rate(range = c(-2, -1)),   # log10 scale: 0.01 to 0.1
    min_n(range      = c(5L, 25L)),
    size = 30
  )

  tune_res <- tune_grid(
    wf,
    resamples = cv_splits,
    grid      = xgb_grid,
    metrics   = metric_set(rmse, rsq),
    control   = control_grid(verbose = TRUE, save_pred = FALSE)
  )

  list(tune_res = tune_res, workflow = wf, train_clean = train_clean)
}


# Run tuning (comment out if using PATH 2)
tune_M1 <- tune_xgb_model(train_M1, features_M1)
#### RUN TUNE FOR 2 and 4
tune_M2 <- tune_xgb_model(train_M2, features_M2)
tune_M4 <- tune_xgb_model(train_M4, features_M4)

# Inspect results
autoplot(tune_M1$tune_res)  # hyperparameter performance landscape
autoplot(tune_M2$tune_res)
autoplot(tune_M4$tune_res)

show_best(tune_M1$tune_res, metric = "rmse", n = 5)
show_best(tune_M2$tune_res, metric = "rmse", n = 5)
show_best(tune_M4$tune_res, metric = "rmse", n = 5)

# Select best hyperparameters by CV RMSE
best_M1 <- select_best(tune_M1$tune_res, metric = "rmse")
best_M2 <- select_best(tune_M2$tune_res, metric = "rmse")
best_M4 <- select_best(tune_M4$tune_res, metric = "rmse")

print(best_M1)
print(best_M2)
print(best_M4)

# Finalize and fit (PATH 1)
fit_M1 <- tune_M1$workflow |> finalize_workflow(best_M1) |> fit(data = train_M1)
fit_M2 <- tune_M2$workflow |> finalize_workflow(best_M2) |> fit(data = train_M2)
fit_M4 <- tune_M4$workflow |> finalize_workflow(best_M4) |> fit(data = train_M4)


# ---- PATH 2: Out-of-box defaults (skip tuning, run in seconds) ----
# These defaults are reasonable for daily ecological time series with ~1000 obs:
#   trees = 500    — enough rounds to converge at learn_rate = 0.05
#   tree_depth = 4 — moderate depth; shallower trees reduce overfitting on small n
#   learn_rate = 0.05 — conservative; slower to converge but generalises better
#   min_n = 10     — at least 10 observations per leaf node
#
# After running the full tuning above once, replace these with the printed
# best_M1 / best_M2 / best_M4 values for the best of both worlds.
#
# To use defaults: comment out the PATH 1 fit lines above and
# uncomment the three fit lines below.

fit_xgb_defaults <- function(train_clean, target = "fDOM_1_QSU_daily") {

  spec <- boost_tree(
    trees      = 500,
    tree_depth = 4,
    learn_rate = 0.05,
    min_n      = 10
  ) |>
    set_engine("xgboost", nthread = parallel::detectCores() - 1) |>
    set_mode("regression")

  rec <- recipe(as.formula(paste(target, "~ .")), data = train_clean)

  workflow() |>
    add_recipe(rec) |>
    add_model(spec) |>
    fit(data = train_clean)
}

# fit_M1 <- fit_xgb_defaults(train_M1)   # uncomment for PATH 2
# fit_M2 <- fit_xgb_defaults(train_M2)
# fit_M4 <- fit_xgb_defaults(train_M4)
# ---- end PATH 2 ----


#### 6. Diagnostics ----

# 6a. Variable importance (gain = improvement in loss from splits on this feature)
vip::vip(extract_fit_parsnip(fit_M2), num_features = length(features_M2)) +
  labs(title = "XGB_M2 variable importance") + theme_bw()

vip::vip(extract_fit_parsnip(fit_M4), num_features = length(features_M4)) +
  labs(title = "XGB_M4 variable importance") + theme_bw()


# 6b. Extrapolation audit
# XGBoost cannot extrapolate beyond the training range — it flatlines at the
# boundary of seen values. Any feature where 2024 values exceed the training
# range will produce clipped predictions. Pay special attention to pct_outside
# for the lag feature and RH_Q during storm events (e.g., Helene, Sept 2024).

audit_extrapolation <- function(train_df, test_df, features) {
  map_dfr(features, function(col) {
    tr <- range(train_df[[col]], na.rm = TRUE)
    te <- test_df[[col]]
    tibble(
      feature     = col,
      train_min   = tr[1],
      train_max   = tr[2],
      test_min    = min(te, na.rm = TRUE),
      test_max    = max(te, na.rm = TRUE),
      pct_outside = round(mean(te < tr[1] | te > tr[2], na.rm = TRUE) * 100, 1)
    )
  })
}

audit_M4 <- audit_extrapolation(train_df, test_df, features_M4)
print(audit_M4)

# Plot training vs 2024 distribution for each M4 feature
audit_M4 |>
  pivot_longer(c(train_min, train_max, test_min, test_max)) |>
  ggplot(aes(x = feature, y = value, color = name)) +
  geom_point(size = 3) +
  coord_flip() +
  labs(title = "Training vs 2024 feature ranges (extrapolation audit)",
       x = NULL, y = "Value", color = NULL) +
  theme_bw() + theme(legend.position = "top")


#### 7. Forecast function ----
#' Iterative 1-30 day ahead XGBoost forecasts for a single reference date.
#'
#' @param ref_date      Date. Observed fDOM at this date seeds lag for h = 1.
#' @param workflow_fit  Fitted tidymodels workflow (output of fit()).
#' @param model_name    Character label for model_id column.
#' @param horizon       Max forecast horizon in days (default 30).
#' @param all_data      Full dataset; observed covariates pulled from here.
#' @param feature_cols  Character vector of feature column names for this model.
#' @return Tibble: model_id, reference_date, forecast_date, horizon, prediction.

forecast_fdom_xgb <- function(ref_date, workflow_fit, model_name, horizon = 30,
                               all_data, feature_cols) {

  obs_ref <- all_data |> filter(Date == ref_date) |> pull(fDOM_1_QSU_daily)
  if (length(obs_ref) == 0 || is.na(obs_ref)) {
    message("No observed fDOM at ", ref_date, " — skipping.")
    return(NULL)
  }

  prev_pred <- obs_ref
  results   <- vector("list", horizon)

  for (h in seq_len(horizon)) {
    forecast_date <- ref_date + h
    fc_row <- all_data |> filter(Date == forecast_date)
    if (nrow(fc_row) == 0) next

    # Build newdata tibble: iterated lag + observed covariates at forecast_date.
    # tibble (not data.frame) required by tidymodels predict().
    newdata <- tibble(fDOM_1m_lag1 = prev_pred)

    for (cv in setdiff(feature_cols, "fDOM_1m_lag1")) {
      newdata[[cv]] <- fc_row[[cv]]
    }

    pred_val <- as.numeric(predict(workflow_fit, new_data = newdata)$.pred)

    results[[h]] <- tibble(
      model_id       = model_name,
      reference_date = ref_date,
      forecast_date  = forecast_date,
      horizon        = h,
      prediction     = pred_val
    )

    prev_pred <- pred_val  # update lag for next step
  }

  bind_rows(results)
}


#### 8. Single-date test ----
test_ref_date <- ymd("2024-09-15")  # matches GLM script for direct comparison

test_xgb_M1 <- forecast_fdom_xgb(test_ref_date, fit_M1, "XGB_M1", 30, ar_df, features_M1)
test_xgb_M2 <- forecast_fdom_xgb(test_ref_date, fit_M2, "XGB_M2", 30, ar_df, features_M2)
test_xgb_M4 <- forecast_fdom_xgb(test_ref_date, fit_M4, "XGB_M4", 30, ar_df, features_M4)

test_all_xgb <- bind_rows(test_xgb_M1, test_xgb_M2, test_xgb_M4) |>
  left_join(ar_df |> select(Date, fDOM_1_QSU_daily), by = c("forecast_date" = "Date"))

ggplot(test_all_xgb, aes(x = forecast_date, y = prediction, col = model_id)) +
  geom_line() +
  geom_point(aes(y = fDOM_1_QSU_daily), col = "black", size = 1.5, alpha = 0.7) +
  geom_vline(xintercept = as.numeric(test_ref_date), linetype = "dashed", color = "gray40") +
  labs(title = paste("XGBoost 30-day forecast from", test_ref_date),
       x = "Date", y = "fDOM (QSU)", color = "Model") +
  theme_bw() + theme(legend.position = "top")


#### 9. Full 2024 run ----
pred_dates_2024 <- ar_df |>
  filter(Date >= ymd("2024-01-01"),
         Date <  ymd("2025-01-01"),
         !is.na(fDOM_1_QSU_daily)) |>
  pull(Date)

message("Running XGBoost forecasts for ", length(pred_dates_2024), " reference dates...")

forecasts_xgb_M1 <- map_dfr(pred_dates_2024, \(d) forecast_fdom_xgb(
  d, fit_M1, "XGB_M1", 30, ar_df, features_M1))

forecasts_xgb_M2 <- map_dfr(pred_dates_2024, \(d) forecast_fdom_xgb(
  d, fit_M2, "XGB_M2", 30, ar_df, features_M2))

forecasts_xgb_M4 <- map_dfr(pred_dates_2024, \(d) forecast_fdom_xgb(
  d, fit_M4, "XGB_M4", 30, ar_df, features_M4))

all_forecasts_xgb <- bind_rows(forecasts_xgb_M1, forecasts_xgb_M2, forecasts_xgb_M4)

# write_csv(all_forecasts_xgb, "./CCR_AR_Modeling/Data/fDOM_1m_XGBfit_forecasts_2024.csv")


#### 10. RMSE by horizon ----
all_forecasts_xgb_obs <- all_forecasts_xgb |>
  left_join(ar_df |> select(Date, fDOM_obs = fDOM_1_QSU_daily),
            by = c("forecast_date" = "Date"))

rmse_xgb <- all_forecasts_xgb_obs |>
  filter(!is.na(prediction), !is.na(fDOM_obs)) |>
  group_by(horizon, model_id) |>
  summarise(RMSE = sqrt(mean((prediction - fDOM_obs)^2)), .groups = "drop")

ggplot(rmse_xgb, aes(x = horizon, y = RMSE, col = model_id)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.4) +
  labs(title = "XGBoost forecast skill by horizon",
       x = "Forecast horizon (days ahead)", y = "RMSE (QSU)") +
  theme_bw() + theme(legend.position = "top", text = element_text(size = 14))
