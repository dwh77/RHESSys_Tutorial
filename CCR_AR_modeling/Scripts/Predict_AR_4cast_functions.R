#### AR Forecasting — Generalized Function-Based Workflow
# Collapses model fitting, iterative forecasting, and plotting into a single
# function call. Supports any target variable, lag column, and covariate set,
# making it easy to run and compare a wide array of model configurations.
#
# Core workflow per model:
#   fit_ar_model() — fits GLM, runs 30-day iterative forecasts, returns a list
#   plot_ar_forecast() — train/test timeseries plot from that list
#   calc_rmse_by_horizon() — RMSE vs horizon from that list
#
# Example call:
#   m <- fit_ar_model(ar_df, target = "fDOM_1_QSU_daily",
#                     lag_col = "fDOM_1m_lag1",
#                     covariates = c("Temp_1_C_daily", "DOsat_1_pct_daily"),
#                     model_name = "my_model")
#   plot_ar_forecast(m, horizon = 7)

library(tidyverse)
library(lubridate)


#### Load data ----
ar_df <- read_csv("./CCR_AR_Modeling/Data/Daily_catwalk_met_RH_2021_2025_lagZS.csv") |>
  mutate(Date = as.Date(Date),
         RH_DOCload_10day = slider::slide_dbl(RH_streamflow_DOC_gm2day, mean,
                                              .before = 10, .after = 0, .complete = F),
         RH_Q_7daymean = slider::slide_dbl(RH_Q_m3day , mean,
                                              .before = 7, .after = 0, .complete = F),
         RH_DOC_7daymean = slider::slide_dbl(RH_DOC_mgL , mean,
                                           .before = 7, .after = 0, .complete = F),
         Chla_7daymean = slider::slide_dbl(Chla_1_ugL_daily , mean,
                                           .before = 7, .after = 0, .complete = F)
         )

# All 2024 calendar dates — fit_ar_model filters to those with observed target
pred_dates_2024 <- ar_df |>
  filter(Date >= ymd("2024-01-01"), Date < ymd("2025-01-01")) |>
  pull(Date)


#### 1. Generalized iterative forecast function ----
# (called internally by fit_ar_model; not usually called directly)

forecast_ar_gen <- function(ref_date, model, model_name, horizon = 30,
                             all_data, target, lag_col, covar_cols = NULL) {

  obs_ref <- all_data |> filter(Date == ref_date) |> pull(!!sym(target))
  if (length(obs_ref) == 0 || is.na(obs_ref)) return(NULL)

  prev_pred <- obs_ref
  results   <- vector("list", horizon)

  for (h in seq_len(horizon)) {
    forecast_date <- ref_date + h
    fc_row <- all_data |> filter(Date == forecast_date)
    if (nrow(fc_row) == 0) next

    # Build newdata with the correct lag column name
    newdata <- setNames(data.frame(prev_pred), lag_col)

    if (!is.null(covar_cols)) {
      for (cv in covar_cols) newdata[[cv]] <- fc_row[[cv]]
    }

    pred_val <- as.numeric(predict(model, newdata = newdata))

    results[[h]] <- tibble(
      model_id       = model_name,
      reference_date = ref_date,
      forecast_date  = forecast_date,
      horizon        = h,
      prediction     = pred_val
    )

    prev_pred <- pred_val
  }

  bind_rows(results)
}


#### 2. Main wrapper: fit model + run forecasts ----
#'
#' @param all_data      Full dataset (ar_df). Training split done internally.
#' @param train_cutoff  Date. Data before this date used for training.
#'                      Default: 2024-01-01.
#' @param pred_dates    Vector of dates to use as forecast reference dates.
#'                      Rows where target is NA are silently dropped.
#' @param target        Column name of the response variable (string).
#'                      e.g. "fDOM_1_QSU_daily"
#' @param lag_col       Column name of the lag predictor (string).
#'                      e.g. "fDOM_1m_lag1"
#' @param covariates    Character vector of additional covariate column names.
#'                      NULL for a lag-only model.
#' @param model_name    Label written to the model_id output column.
#' @param horizon       Max forecast horizon in days. Default 30.
#'
#' @return Named list:
#'   $model       — fitted glm object
#'   $model_name  — character label
#'   $target      — target column name (used by plot/rmse helpers)
#'   $lag_col     — lag column name
#'   $covariates  — covariate column names
#'   $train_data  — training rows with an added 'fitted' column
#'   $forecasts   — forecast tibble with 'fDOM_obs' column joined

fit_ar_model <- function(all_data,
                          train_cutoff = ymd("2024-01-01"),
                          pred_dates,
                          target,
                          lag_col,
                          covariates = NULL,
                          model_name,
                          horizon = 30) {

  # --- 1. Build training data: filter to pre-cutoff, drop NAs ---
  train_df <- all_data |>
    filter(Date < train_cutoff,
           !is.na(!!sym(target)),
           !is.na(!!sym(lag_col)))

  if (!is.null(covariates)) {
    for (cv in covariates) {
      train_df <- train_df |> filter(!is.na(!!sym(cv)))
    }
  }

  # --- 2. Build formula and fit GLM ---
  rhs     <- paste(c(lag_col, covariates), collapse = " + ")
  formula <- as.formula(paste(target, "~", rhs))

  mod <- glm(formula, data = train_df, family = gaussian, na.action = na.fail)
  print(summary(mod))

  # --- 3. Add in-sample fitted values to training data ---
  train_fitted <- train_df |>
    mutate(fitted = predict(mod, newdata = train_df))

  # --- 4. Filter pred_dates to rows with observed target (seeds the lag) ---
  valid_pred_dates <- all_data |>
    filter(Date %in% pred_dates, !is.na(!!sym(target))) |>
    pull(Date)

  message(model_name, " | n_train = ", nrow(train_df),
          " | n_ref_dates = ", length(valid_pred_dates))

  # --- 5. Run iterative forecasts across all valid reference dates ---
  forecasts <- map_dfr(valid_pred_dates, \(d) forecast_ar_gen(
    ref_date    = d,
    model       = mod,
    model_name  = model_name,
    horizon     = horizon,
    all_data    = all_data,
    target      = target,
    lag_col     = lag_col,
    covar_cols  = covariates
  ))

  # --- 6. Join observed target values onto forecasts by forecast_date ---
  forecasts_obs <- forecasts |>
    left_join(
      all_data |> select(Date, fDOM_obs = !!sym(target)),
      by = c("forecast_date" = "Date")
    )

  list(
    model       = mod,
    model_name  = model_name,
    target      = target,
    lag_col     = lag_col,
    covariates  = covariates,
    train_data  = train_fitted,
    forecasts   = forecasts_obs
  )
}


#### 3. Plot function ----
#' Train vs test timeseries for a fitted model object.
#'
#' @param model_obj  List returned by fit_ar_model().
#' @param horizon    Integer. Which forecast horizon to display in the test period.
#' @param train_cutoff  Date used to draw the vertical train/test divider.

plot_ar_forecast <- function(model_obj, horizon,
                              train_cutoff = as.Date("2024-01-01")) {

  target     <- model_obj$target
  model_name <- model_obj$model_name
  df_train   <- model_obj$train_data
  df_test    <- model_obj$forecasts |> filter(horizon == !!horizon)

  obs_data <- bind_rows(
    df_train |> select(Date, fDOM = !!sym(target)),
    df_test  |> select(Date = forecast_date, fDOM = fDOM_obs)
  ) |>
    filter(!is.na(fDOM)) |>
    mutate(series = "Observed")

  train_fit <- df_train |>
    filter(!is.na(fitted)) |>
    select(Date, fDOM = fitted) |>
    mutate(series = "Training fit")

  test_pred <- df_test |>
    filter(!is.na(prediction)) |>
    select(Date = forecast_date, fDOM = prediction) |>
    mutate(series = "Test prediction")

  plot_df <- bind_rows(obs_data, train_fit, test_pred)

  ggplot(plot_df, aes(x = Date, y = fDOM, color = series)) +
    geom_point(size = 1.2, alpha = 0.8) +
    geom_vline(xintercept = train_cutoff, linetype = "dashed", color = "gray40") +
    scale_color_manual(
      values = c("Observed"        = "black",
                 "Training fit"    = "orange",
                 "Test prediction" = "dodgerblue"),
      breaks = c("Observed", "Training fit", "Test prediction")
    ) +
    labs(
      title = paste0(model_name, "  |  horizon = ", horizon, " day(s)"),
      x = "Date", y = "fDOM (QSU)", color = NULL
    ) +
    theme_bw() +
    theme(legend.position = "top")
}


#### 4. RMSE helper ----
#' RMSE by forecast horizon for a fitted model object.
#'
#' @param model_obj  List returned by fit_ar_model().
#' @return Tibble with columns: horizon, model_id, RMSE.

calc_rmse_by_horizon <- function(model_obj, by_season = FALSE) {

  df <- model_obj$forecasts |>
    filter(!is.na(prediction), !is.na(fDOM_obs))

  if (by_season) {
    df <- df |>
      mutate(
        month  = month(forecast_date),
        Season = case_when(
          month %in% c(12, 1, 2)  ~ "Winter",
          month %in% c(3,  4, 5)  ~ "Spring",
          month %in% c(6,  7, 8)  ~ "Summer",
          month %in% c(9, 10, 11) ~ "Fall"
        )
      )
    df |>
      group_by(model_id, horizon, Season) |>
      summarise(RMSE = sqrt(mean((prediction - fDOM_obs)^2)), .groups = "drop")
  } else {
    df |>
      group_by(model_id, horizon) |>
      summarise(RMSE = sqrt(mean((prediction - fDOM_obs)^2)), .groups = "drop")
  }
}




#### 5. Example usage ----

# --- 1m fDOM models ---
## Lag1 only
m1_1m <- fit_ar_model(
  all_data   = ar_df,
  pred_dates = pred_dates_2024,
  target     = "fDOM_1_QSU_daily",
  lag_col    = "fDOM_1m_lag1",
  covariates = NULL,
  model_name = "M1_1m_lag"
)

head(m1_1m$forecasts)
head(m1_1m$train_data)
m1_1m$model


## Lake - Temp, DO, SW
m2_1m <- fit_ar_model(
  all_data   = ar_df,
  pred_dates = pred_dates_2024,
  target     = "fDOM_1_QSU_daily",
  lag_col    = "fDOM_1m_lag1",
  covariates = c("Temp_1_C_daily", "DOsat_1_pct_daily", "SW_Wm2_daily"),
  model_name = "M2_1m_lag_LAKEa"
)

m2_1m$model

## Lake - Temp, DO
m2b_1m <- fit_ar_model(
  all_data   = ar_df,
  pred_dates = pred_dates_2024,
  target     = "fDOM_1_QSU_daily",
  lag_col    = "fDOM_1m_lag1",
  covariates = c("Temp_1_C_daily", "DOsat_1_pct_daily"),
  model_name = "M2_1m_lag_LAKEb"
)

m2b_1m$model

## Lake - Temp, DO, Chla7day
m2c_1m <- fit_ar_model(
  all_data   = ar_df,
  pred_dates = pred_dates_2024,
  target     = "fDOM_1_QSU_daily",
  lag_col    = "fDOM_1m_lag1",
  covariates = c("Temp_1_C_daily", "DOsat_1_pct_daily", "Chla_7daymean"),
  model_name = "M2_1m_lag_LAKEc"
)


## Lake (Temp, DO, SW) + Catchment (Q, DOC)
m4_1m <- fit_ar_model(
  all_data   = ar_df,
  pred_dates = pred_dates_2024,
  target     = "fDOM_1_QSU_daily",
  lag_col    = "fDOM_1m_lag1",
  covariates = c("Temp_1_C_daily", "DOsat_1_pct_daily", "SW_Wm2_daily",
                 "RH_Q_m3day", "RH_DOC_mgL"),
  model_name = "M4_1m_lag_LAKEa_RH"
)

## Lake (Temp, DO) + Catchment (Q, DOC)
m4b_1m <- fit_ar_model(
  all_data   = ar_df,
  pred_dates = pred_dates_2024,
  target     = "fDOM_1_QSU_daily",
  lag_col    = "fDOM_1m_lag1",
  covariates = c("Temp_1_C_daily", "DOsat_1_pct_daily",
                 "RH_Q_m3day", "RH_DOC_mgL"),
  model_name = "M4_1m_lag_LAKEb_RH"
)

##  Catchment (Q, DOC)
m5_1m <- fit_ar_model(
  all_data   = ar_df,
  pred_dates = pred_dates_2024,
  target     = "fDOM_1_QSU_daily",
  lag_col    = "fDOM_1m_lag1",
  covariates = c("RH_Q_m3day", "RH_DOC_mgL"),
  model_name = "M5_1m_lag_RH"
)

##  Catchment (Q, DOC) 7 day means
m5b_1m <- fit_ar_model(
  all_data   = ar_df,
  pred_dates = pred_dates_2024,
  target     = "fDOM_1_QSU_daily",
  lag_col    = "fDOM_1m_lag1",
  covariates = c("RH_Q_7daymean", "RH_DOC_7daymean"),
  model_name = "M5_1m_lag_RHlag"
)

## Lake (Temp, DO, SW) + Rain
m6_1m <- fit_ar_model(
  all_data   = ar_df,
  pred_dates = pred_dates_2024,
  target     = "fDOM_1_QSU_daily",
  lag_col    = "fDOM_1m_lag1",
  covariates = c("Temp_1_C_daily", "DOsat_1_pct_daily", "SW_Wm2_daily",
                 "Rain_mm_lag1"),
  model_name = "M6_1m_lag_Lakea_Rain"
)

## Lake (Temp, DO) + Rain
m6b_1m <- fit_ar_model(
  all_data   = ar_df,
  pred_dates = pred_dates_2024,
  target     = "fDOM_1_QSU_daily",
  lag_col    = "fDOM_1m_lag1",
  covariates = c("Temp_1_C_daily", "DOsat_1_pct_daily",
                 "Rain_mm_lag1"),
  model_name = "M6_1m_lag_Lakeb_Rain"
)


## + Rain
m7_1m <- fit_ar_model(
  all_data   = ar_df,
  pred_dates = pred_dates_2024,
  target     = "fDOM_1_QSU_daily",
  lag_col    = "fDOM_1m_lag1",
  covariates = c("Rain_mm_lag1"),
  model_name = "M7_1m_lag_Rain"
)



# --- 9m fDOM models ---
# m1_9m <- fit_ar_model(
#   all_data   = ar_df,
#   pred_dates = pred_dates_2024,
#   target     = "fDOM_9_QSU_daily",
#   lag_col    = "fDOM_9m_lag1",
#   covariates = NULL,
#   model_name = "M1_9m_lag"
# )
#
# m2_9m <- fit_ar_model(
#   all_data   = ar_df,
#   pred_dates = pred_dates_2024,
#   target     = "fDOM_9_QSU_daily",
#   lag_col    = "fDOM_9m_lag1",
#   covariates = c("Temp_9_C_daily", "DOsat_9_pct_daily", "SW_Wm2_daily"),
#   model_name = "M2_9m_lag_env"
# )
#
# m4_9m <- fit_ar_model(
#   all_data   = ar_df,
#   pred_dates = pred_dates_2024,
#   target     = "fDOM_9_QSU_daily",
#   lag_col    = "fDOM_9m_lag1",
#   covariates = c("Temp_9_C_daily", "DOsat_9_pct_daily", "SW_Wm2_daily",
#                  "RH_Q_m3day", "RH_DOC_mgL"),
#   model_name = "M4_9m_lag_env_RH"
# )


# --- Training vs Testing Plots ---
plot_ar_forecast(m1_1m, horizon = 1)
plot_ar_forecast(m1_1m, horizon = 30)
plot_ar_forecast(m2_1m, horizon = 7)
plot_ar_forecast(m4_1m, horizon = 7)


# --- RMSE by horizon: single model ---
calc_rmse_by_horizon(m2_1m)                  # original behaviour
calc_rmse_by_horizon(m2_1m, by_season = TRUE) # seasonal breakdown


# --- RMSE by horizon: compare multiple models ---
# 4 models for comm meeting
rmse_all <- bind_rows(
  calc_rmse_by_horizon(m1_1m),
  calc_rmse_by_horizon(m2_1m),
  calc_rmse_by_horizon(m4_1m),
  calc_rmse_by_horizon(m6_1m)
)

# new models
rmse_all <- bind_rows(
  calc_rmse_by_horizon(m1_1m),
  # # calc_rmse_by_horizon(m2_1m),
  # calc_rmse_by_horizon(m2b_1m),
  # calc_rmse_by_horizon(m2c_1m),
  # #calc_rmse_by_horizon(m4_1m),
  # calc_rmse_by_horizon(m4b_1m),
  # calc_rmse_by_horizon(m5_1m),
  # calc_rmse_by_horizon(m5b_1m),
  # #calc_rmse_by_horizon(m6_1m),
  calc_rmse_by_horizon(m6b_1m),
  calc_rmse_by_horizon(m7_1m)
)

# new models
rmse_all_season <- bind_rows(
  calc_rmse_by_horizon(m1_1m, by_season = TRUE),
  calc_rmse_by_horizon(m2_1m, by_season = TRUE),
  calc_rmse_by_horizon(m2b_1m, by_season = TRUE),
  calc_rmse_by_horizon(m2c_1m, by_season = TRUE),
  calc_rmse_by_horizon(m4_1m, by_season = TRUE),
  calc_rmse_by_horizon(m4b_1m, by_season = TRUE),
  calc_rmse_by_horizon(m5_1m, by_season = TRUE),
  calc_rmse_by_horizon(m5b_1m, by_season = TRUE),
  calc_rmse_by_horizon(m6_1m, by_season = TRUE),
  calc_rmse_by_horizon(m6b_1m, by_season = TRUE),
  calc_rmse_by_horizon(m7_1m, by_season = TRUE)
)

### RMSE plot across full sampling
ggplot(rmse_all, aes(x = horizon, y = RMSE, col = model_id)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.4) +
  labs(title = "Prediction skill by horizon",
       x = "Forecast horizon (days ahead)", y = "RMSE (QSU)") +
  theme_bw() + theme(legend.position = "top", text = element_text(size = 14))

### RMSE plot across full sampling by season
ggplot(rmse_all_season, aes(x = horizon, y = RMSE, col = model_id)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.4) +
  facet_wrap(~Season, nrow = 1)+
  labs(title = "Prediction skill by horizon",
       x = "Forecast horizon (days ahead)", y = "RMSE (QSU)") +
  theme_bw() + theme(legend.position = "top", text = element_text(size = 14))



#### Bring results together and make plots across year
# Collect all fitted model objects into a named list
model_list <- list(m1_1m, m2_1m, m2b_1m, m2c_1m,
                   m4_1m, m4b_1m, m5_1m, m5b_1m,
                   m6_1m, m6b_1m, m7_1m)

# Bind all forecast outputs into one data frame
all_forecasts_obs <- map_dfr(model_list, \(m) m$forecasts)

unique(all_forecasts_obs$model_id)

# Now works with existing plots, e.g.:
all_forecasts_obs |>
  filter(horizon %in% c(1, 7, 15, 30),
         model_id == "M5_1m_lag_RH") |>
  ggplot() +
  geom_line(aes(x = forecast_date, y = prediction, col = as.factor(horizon)), size = 1.1) +
  geom_point(aes(x = forecast_date, y = fDOM_obs), size = 0.8) +
  labs(x = "Date", y = "fDOM (QSU)", col = "Model Horizon") +
  theme_bw() + theme(legend.position = "top")
