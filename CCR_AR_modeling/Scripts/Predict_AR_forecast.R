#### AR Model Forecasting — 30-day ahead predictions for 2024
# Focal variable: fDOM_1_QSU_daily (1m depth, temperature-corrected fDOM)
#
# Models trained on Date < 2024-01-01, evaluated on all 2024 reference dates.
#
# Three models:
#   M1: fDOM ~ fDOM_lag1
#   M2: fDOM ~ fDOM_lag1 + Temp + DO + SW
#   M3: fDOM ~ fDOM_lag1 + Temp + DO + SW + RH_DOC_streamflow
#
# Forecasting approach (following Lofton et al.):
#   - Covariates use observed values at the forecast timestep
#   - Lag term is updated iteratively from the prior day's prediction

library(tidyverse)
library(lubridate)


#### Load data ----
ar_df <- read_csv("./CCR_AR_Modeling/Data/Daily_catwalk_met_RH_2021_2025_lagZS.csv") |>
  mutate(Date = as.Date(Date),
         RH_DOCload_10day = slider::slide_dbl(RH_streamflow_DOC_gm2day, mean,  #RH_streamflow_DOC_gm2day
                                              .before = 10, .after = 0, .complete = F)
  )



#### Training data (pre-2024) ----
train_df <- ar_df |> filter(Date < ymd("2024-01-01"))


#### Fit models ----

# M1: lag only
train_m1 <- train_df |>
  filter(!is.na(fDOM_1_QSU_daily), !is.na(fDOM_1m_lag1))

mod_m1 <- glm(fDOM_1_QSU_daily ~ fDOM_1m_lag1,
              data = train_m1, family = gaussian, na.action = na.fail)
summary(mod_m1)


# M2: lag + environmental covariates
train_m2 <- train_df |>
  filter(!is.na(fDOM_1_QSU_daily), !is.na(fDOM_1m_lag1),
         !is.na(Temp_1_C_daily), !is.na(DOsat_1_pct_daily), !is.na(SW_Wm2_daily))

mod_m2 <- glm(fDOM_1_QSU_daily ~ fDOM_1m_lag1 +
                Temp_1_C_daily + DOsat_1_pct_daily + SW_Wm2_daily,
              data = train_m2, family = gaussian, na.action = na.fail)
summary(mod_m2)


# M3: lag + environmental covariates + RHESSys DOC streamflow
train_m3 <- train_df |>
  filter(!is.na(fDOM_1_QSU_daily), !is.na(fDOM_1m_lag1),
         !is.na(Temp_1_C_daily), !is.na(DOsat_1_pct_daily), !is.na(SW_Wm2_daily),
         !is.na(RH_streamflow_DOC_gm2day))

mod_m3 <- glm(fDOM_1_QSU_daily ~ fDOM_1m_lag1 +
                Temp_1_C_daily + DOsat_1_pct_daily + SW_Wm2_daily +
                RH_streamflow_DOC_gm2day,
              data = train_m3, family = gaussian, na.action = na.fail)
summary(mod_m3)


# M4: lag + environmental covariates + RHESSys DOC streamflow 10 day mean
train_m4 <- train_df |>
  filter(!is.na(fDOM_1_QSU_daily), !is.na(fDOM_1m_lag1),
         !is.na(Temp_1_C_daily), !is.na(DOsat_1_pct_daily), !is.na(SW_Wm2_daily),
         !is.na(RH_DOCload_10day))

mod_m4 <- glm(fDOM_1_QSU_daily ~ fDOM_1m_lag1 +
                Temp_1_C_daily + DOsat_1_pct_daily + SW_Wm2_daily +
                RH_DOCload_10day,
              data = train_m4, family = gaussian, na.action = na.fail)
summary(mod_m4)

# M4: lag + environmental covariates + RHESSys Q
train_m5 <- train_df |>
  filter(!is.na(fDOM_1_QSU_daily), !is.na(fDOM_1m_lag1),
         !is.na(Temp_1_C_daily), !is.na(DOsat_1_pct_daily), !is.na(SW_Wm2_daily),
         !is.na(RH_Q_m3day))

mod_m5 <- glm(fDOM_1_QSU_daily ~ fDOM_1m_lag1 +
                Temp_1_C_daily + DOsat_1_pct_daily + SW_Wm2_daily +
                RH_Q_m3day,
              data = train_m5, family = gaussian, na.action = na.fail)
summary(mod_m5)


#### Forecast function ----
#' Generate iterative 1–30 day ahead forecasts for a single reference date.
#'
#' @param ref_date   Date. Observed fDOM here seeds the lag for step h = 1.
#' @param model      Fitted GLM.
#' @param model_name Character label written to the output column model_id.
#' @param horizon    Max days ahead (default 30).
#' @param all_data   Full data frame; observed covariate columns are pulled
#'                   from here for each forecast date.
#' @param covar_cols Character vector of raw covariate column names to take
#'                   from all_data at each forecast date. NULL for M1.
#' @return Tibble: model_id, reference_date, forecast_date, horizon, prediction.

forecast_fdom_ar <- function(ref_date, model, model_name, horizon = 30,
                              all_data, covar_cols = NULL) {

  # Seed the iterated lag with the observed fDOM at the reference date
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

    # The iterated lag is the previous day's prediction (raw QSU)
    newdata <- data.frame(fDOM_1m_lag1 = prev_pred)

    # Pull observed covariates for this forecast date
    if (!is.null(covar_cols)) {
      for (cv in covar_cols) {
        newdata[[cv]] <- fc_row[[cv]]
      }
    }

    pred_val <- as.numeric(predict(model, newdata = newdata))

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


#### Test with a single reference date ----
test_ref_date <- ymd("2024-09-25")

test_m1 <- forecast_fdom_ar(
  ref_date   = test_ref_date,
  model      = mod_m1,
  model_name = "M1_lag1only",
  horizon    = 30,
  all_data   = ar_df,
  covar_cols = NULL
)

test_m2 <- forecast_fdom_ar(
  ref_date   = test_ref_date,
  model      = mod_m2,
  model_name = "M2_lag_env",
  horizon    = 30,
  all_data   = ar_df,
  covar_cols = c("Temp_1_C_daily", "DOsat_1_pct_daily", "SW_Wm2_daily")
)

test_m3 <- forecast_fdom_ar(
  ref_date   = test_ref_date,
  model      = mod_m3,
  model_name = "M3_lag_env_RH",
  horizon    = 30,
  all_data   = ar_df,
  covar_cols = c("Temp_1_C_daily", "DOsat_1_pct_daily", "SW_Wm2_daily",
                 "RH_streamflow_DOC_gm2day")
)

test_m4 <- forecast_fdom_ar(
  ref_date   = test_ref_date,
  model      = mod_m4,
  model_name = "M4_lag_env_RH",
  horizon    = 30,
  all_data   = ar_df,
  covar_cols = c("Temp_1_C_daily", "DOsat_1_pct_daily", "SW_Wm2_daily",
                 "RH_DOCload_10day")
)

test_m5 <- forecast_fdom_ar(
  ref_date   = test_ref_date,
  model      = mod_m5,
  model_name = "M5_lag_env_RHQ",
  horizon    = 30,
  all_data   = ar_df,
  covar_cols = c("Temp_1_C_daily", "DOsat_1_pct_daily", "SW_Wm2_daily",
                 "RH_Q_m3day")
)

# Quick plot of single-date test
test_all <- bind_rows(test_m1, test_m2, test_m3, test_m4, test_m5) |>
  left_join(ar_df |> select(Date, fDOM_1_QSU_daily),
            by = c("forecast_date" = "Date"))

ggplot(test_all, aes(x = forecast_date, y = prediction, col = model_id)) +
  geom_line() +
  geom_point(aes(y = fDOM_1_QSU_daily), col = "black", size = 1.5, alpha = 0.7) +
  geom_vline(xintercept = as.numeric(test_ref_date), linetype = "dashed", color = "gray40") +
  labs(
    title = paste("30-day forecast from", test_ref_date),
    x = "Date", y = "fDOM (QSU)", color = "Model"
  ) +
  theme_bw() + theme(legend.position = "top")


#### Run across all 2024 reference dates ----
# Only dates with observed fDOM (needed to seed the lag for step h = 1)
pred_dates_2024 <- ar_df |>
  filter(Date >= ymd("2024-01-01"),
         Date <  ymd("2025-01-01"),
         !is.na(fDOM_1_QSU_daily)) |>
  pull(Date)

message("Running forecasts for ", length(pred_dates_2024), " reference dates...")

forecasts_m1 <- map_dfr(pred_dates_2024, \(d) forecast_fdom_ar(
  ref_date = d, model = mod_m1, model_name = "M1_lag1only",
  horizon = 30, all_data = ar_df, covar_cols = NULL
))

forecasts_m2 <- map_dfr(pred_dates_2024, \(d) forecast_fdom_ar(
  ref_date = d, model = mod_m2, model_name = "M2_lag_env",
  horizon = 30, all_data = ar_df,
  covar_cols = c("Temp_1_C_daily", "DOsat_1_pct_daily", "SW_Wm2_daily")
))

forecasts_m3 <- map_dfr(pred_dates_2024, \(d) forecast_fdom_ar(
  ref_date = d, model = mod_m3, model_name = "M3_lag_env_RH",
  horizon = 30, all_data = ar_df,
  covar_cols = c("Temp_1_C_daily", "DOsat_1_pct_daily", "SW_Wm2_daily",
                 "RH_streamflow_DOC_gm2day")
))

forecasts_m4 <- map_dfr(pred_dates_2024, \(d) forecast_fdom_ar(
  ref_date = d, model = mod_m4, model_name = "M4_lag_env_RH",
  horizon = 30, all_data = ar_df,
  covar_cols = c("Temp_1_C_daily", "DOsat_1_pct_daily", "SW_Wm2_daily",
                 "RH_DOCload_10day")
))

forecasts_m5 <- map_dfr(pred_dates_2024, \(d) forecast_fdom_ar(
  ref_date = d, model = mod_m5, model_name = "M5_lag_env_RHQ",
  horizon = 30, all_data = ar_df,
  covar_cols = c("Temp_1_C_daily", "DOsat_1_pct_daily", "SW_Wm2_daily",
                 "RH_Q_m3day")
))

all_forecasts <- bind_rows(forecasts_m1, forecasts_m2, forecasts_m3,
                           forecasts_m4, forecasts_m5)
# all_forecasts <- bind_rows(forecasts_m3, forecasts_m4)

  #pivot_wider(names_from = model_id, values_from = prediction)

# Optionally save
# write_csv(all_forecasts, "./CCR_AR_Modeling/Data/fDOM_1m_forecasts_2024_30apr26.csv")



# --- Append observed fDOM joined by forecast_date ---
all_forecasts_obs <- all_forecasts |>
  left_join(
    ar_df |> select(Date, fDOM_obs = fDOM_1_QSU_daily),
    by = c("forecast_date" = "Date")
  )

# --- RMSE by horizon ---
rmse_by_horizon <- all_forecasts_obs |>
  filter(!is.na(prediction), !is.na(fDOM_obs)) |>
  group_by(horizon, model_id) |>
  summarise(RMSE = sqrt(mean((prediction - fDOM_obs)^2)), .groups = "drop")

# --- Plot ---
ggplot(rmse_by_horizon, aes(x = horizon, y = RMSE, col = model_id)) +
  geom_line() +
  geom_point() +
  labs(title = "Forecast skill by horizon",
    x = "Forecast horizon (days ahead)",    y = "RMSE (QSU)") +
  theme_bw()
