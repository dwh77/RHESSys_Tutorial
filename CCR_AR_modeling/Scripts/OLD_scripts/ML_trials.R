#### simple xgboost from claude ----
library(tidymodels)
library(xgboost)

data <- read_csv("./CCR_AR_Modeling/Data/Daily_catwalk_met_2021_2025_lagZS.csv")


data_model <- data %>%
  dplyr::mutate(
    fDOM_lag1 = dplyr::lag(fDOM_1_QSU_daily, 1),
    fDOM_lag2 = dplyr::lag(fDOM_1_QSU_daily, 2),
    fDOM_lag7 = dplyr::lag(fDOM_1_QSU_daily, 7)  # weekly lag
  )
# No interpolation needed — XGBoost handles the NAs in covariates natively

xgb_spec <- parsnip::boost_tree(
  trees = 500, tree_depth = 4, learn_rate = 0.05
) %>%
  parsnip::set_engine("xgboost") %>%
  parsnip::set_mode("regression")

xgb_fit <- xgb_spec %>%
  parsnip::fit(
    fDOM_1_QSU_daily ~ #fDOM_lag1 + fDOM_lag2 + fDOM_lag7 +
      Chla_1_ugL_daily_ZS + DOsat_1_pct_daily_ZS +
      Temp_1_C_daily_ZS + SW_Wm2_daily_ZS,
    data = data
  )




train_preds <- predict(xgb_fit, new_data = data) %>%
  dplyr::bind_cols(data %>% dplyr::select(Date, fDOM_1_QSU_daily)) %>%
  dplyr::rename(pred = .pred) %>%
  dplyr::mutate(
    resid   = fDOM_1_QSU_daily - pred,
    dataset = "train"
  )


train_preds |>
  select(Date, pred, fDOM_1_QSU_daily) |>
  rename(obs = fDOM_1_QSU_daily) |>
  pivot_longer(-1) |>
  ggplot(aes(x = Date, y = value, col = name))+
  geom_line()








#### NNETAR conversion trials ----


library(tidyverse)
library(zoo)
library(fable)
library(feasts)
library(tsibble)
library(Metrics)
library(scales)

# ===========================================================================
# 1. LOAD & PREPARE DATA
# ===========================================================================
data <- readr::read_csv("./CCR_AR_Modeling/Data/Daily_catwalk_met_2021_2025_lagZS.csv")

data_model <- data %>%
  dplyr::select(
    Date,
    fDOM_1_QSU_daily,
    Chla_1_ugL_daily_ZS, Chla_1_ugL_10day_ZS,
    DOsat_1_pct_daily_ZS, Temp_1_C_daily_ZS,
    SW_Wm2_daily_ZS
  ) %>%
  dplyr::mutate(Date = as.Date(Date)) %>%
  dplyr::arrange(Date) %>%
  na.omit()

cat("Rows used for modeling:", nrow(data_model), "\n")

# ===========================================================================
# 2. BUILD TSIBBLE
# ===========================================================================
# fable requires a tsibble. If your data has gaps (missing dates), fill them
# first — NNETAR cannot handle implicit gaps.

ts_data <- data_model %>%
  tsibble::as_tsibble(index = Date)

# Check for and report any implicit gaps
gaps <- tsibble::count_gaps(ts_data)
if (nrow(gaps) > 0) {
  cat("WARNING: Implicit gaps found in tsibble — filling with NA and interpolating.\n")
  print(gaps)
  ts_data <- ts_data %>%
    tsibble::fill_gaps() %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ zoo::na.approx(., na.action = na.pass)))
}

# ===========================================================================
# 3. FIT NNETAR MODELS
# ===========================================================================
# Omitting AR() entirely lets fable::NNETAR auto-select p (number of AR lags)
# using the default heuristic: p = max(frequency, 1) for non-seasonal daily
# data this is p = 1, but you can also set a search range via AR(P = 0:5)
# or similar — see ?fable::NNETAR.
#
# size (hidden units) is also auto-selected as floor((p + n_xreg + 1) / 2)
# unless specified.
#
# repeats controls how many networks are averaged to reduce randomness.

## 3a. AR-lag-only (univariate — auto lag selection, no covariates)
fit_AR <- ts_data %>%
  fabletools::model(
    nnetar_AR = fable::NNETAR(fDOM_1_QSU_daily)
  )

## 3b. Full model: auto lag selection + covariates
fit_full <- ts_data %>%
  fabletools::model(
    nnetar_full = fable::NNETAR(
      fDOM_1_QSU_daily ~
        Chla_1_ugL_daily_ZS +
        Chla_1_ugL_10day_ZS +
        DOsat_1_pct_daily_ZS +
        Temp_1_C_daily_ZS +
        SW_Wm2_daily_ZS
    )
  )

# Inspect what lag order (p) each model selected
cat("\n---- AR-only model spec ----\n")
print(fit_AR)

cat("\n---- Full model spec ----\n")
print(fit_full)

# ===========================================================================
# 4. EXTRACT IN-SAMPLE FITTED VALUES
# ===========================================================================
# fable::NNETAR's fitted() returns all NA — use fabletools::augment() instead,
# which correctly retrieves .fitted from the model object.

aug_AR   <- fabletools::augment(fit_AR)
aug_full <- fabletools::augment(fit_full)

# Join fitted values and compute residuals
results <- data_model %>%
  dplyr::left_join(
    aug_AR   %>% tibble::as_tibble() %>% dplyr::select(Date, pred_AR   = .fitted),
    by = "Date"
  ) %>%
  dplyr::left_join(
    aug_full %>% tibble::as_tibble() %>% dplyr::select(Date, pred_full = .fitted),
    by = "Date"
  ) %>%
  dplyr::mutate(
    resid_AR   = fDOM_1_QSU_daily - pred_AR,
    resid_full = fDOM_1_QSU_daily - pred_full
  )

cat("\nFirst rows of results:\n")
print(head(results %>% dplyr::select(Date, fDOM_1_QSU_daily, pred_AR, pred_full)))

# ===========================================================================
# 5. DIAGNOSTIC STATISTICS
# ===========================================================================
r2_sse <- function(obs, pred) {
  keep <- !is.na(pred) & !is.na(obs)
  obs  <- obs[keep]
  pred <- pred[keep]
  ss_res <- sum((obs - pred)^2)
  ss_tot <- sum((obs - mean(obs))^2)
  round(1 - ss_res / ss_tot, 2)
}

rmse_clean <- function(obs, pred) {
  keep <- !is.na(pred) & !is.na(obs)
  round(Metrics::rmse(obs[keep], pred[keep]), 1)
}

cat("\n---- Model performance ----\n")
cat("AR-only | R2:", r2_sse(results$fDOM_1_QSU_daily, results$pred_AR),
    " RMSE:", rmse_clean(results$fDOM_1_QSU_daily, results$pred_AR),
    " SD resid:", round(sd(results$resid_AR, na.rm = TRUE), 2), "\n")

cat("Full    | R2:", r2_sse(results$fDOM_1_QSU_daily, results$pred_full),
    " RMSE:", rmse_clean(results$fDOM_1_QSU_daily, results$pred_full),
    " SD resid:", round(sd(results$resid_full, na.rm = TRUE), 2), "\n")

cat("\nMean residual (AR):  ", round(mean(results$resid_AR,   na.rm = TRUE), 4), "\n")
cat("Mean residual (full):", round(mean(results$resid_full, na.rm = TRUE), 4), "\n")

# ===========================================================================
# 6. PLOTS
# ===========================================================================
mytheme_AS <- theme(
  panel.grid.major  = element_blank(),
  panel.grid.minor  = element_blank(),
  panel.background  = element_blank(),
  axis.line         = element_line(colour = "black"),
  legend.key        = element_blank(),
  legend.background = element_blank(),
  legend.title      = element_text(size = 10),
  legend.text       = element_text(size = 10),
  axis.text         = element_text(size = 10),
  axis.title        = element_text(size = 10),
  plot.title        = element_text(size = 10, face = "bold", hjust = 0.5)
)

## 6a. Observed vs fitted — full model
p_full <- ggplot2::ggplot(results %>% dplyr::filter(!is.na(pred_full))) +
  ggplot2::geom_line(ggplot2::aes(x = Date, y = fDOM_1_QSU_daily, color = "Observed"),    linewidth = 0.5) +
  ggplot2::geom_line(ggplot2::aes(x = Date, y = pred_full,         color = "NNETAR full"), linewidth = 0.5, linetype = "dashed") +
  ggplot2::scale_color_manual("Model", values = c("Observed" = "black", "NNETAR full" = "dodgerblue")) +
  ggplot2::labs(y = "fDOM (QSU)") +
  ggplot2::scale_x_date(labels = scales::date_format("%b %Y"), date_breaks = "3 months") +
  ggplot2::theme(axis.text.x  = ggplot2::element_text(angle = 25, vjust = 1, hjust = 1),
                 axis.title.x = ggplot2::element_blank()) +
  mytheme_AS

## 6b. Observed vs fitted — AR-only model
p_AR <- ggplot2::ggplot(results %>% dplyr::filter(!is.na(pred_AR))) +
  ggplot2::geom_line(ggplot2::aes(x = Date, y = fDOM_1_QSU_daily, color = "Observed"),  linewidth = 0.5) +
  ggplot2::geom_line(ggplot2::aes(x = Date, y = pred_AR,           color = "NNETAR AR"), linewidth = 0.5, linetype = "dashed") +
  ggplot2::scale_color_manual("Model", values = c("Observed" = "black", "NNETAR AR" = "purple")) +
  ggplot2::labs(y = "fDOM (QSU)") +
  ggplot2::scale_x_date(labels = scales::date_format("%b %Y"), date_breaks = "3 months") +
  ggplot2::theme(axis.text.x  = ggplot2::element_text(angle = 25, vjust = 1, hjust = 1),
                 axis.title.x = ggplot2::element_blank()) +
  mytheme_AS

## 6c. Residuals over time — full model
p_resid <- ggplot2::ggplot(results %>% dplyr::filter(!is.na(resid_full))) +
  ggplot2::geom_point(ggplot2::aes(x = Date, y = resid_full), color = "black", size = 1) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::labs(x = "Date", y = "Residual (QSU)") +
  ggplot2::scale_x_date(labels = scales::date_format("%b %Y"), date_breaks = "3 months") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, vjust = 1, hjust = 1)) +
  mytheme_AS

print(p_full)
print(p_AR)
print(p_resid)

# ===========================================================================
# 7. OPTIONAL: SAVE RESULTS
# ===========================================================================
# readr::write_csv(results, "./CCR_AR_Modeling/Data/NNETAR_timeseries_results.csv")
