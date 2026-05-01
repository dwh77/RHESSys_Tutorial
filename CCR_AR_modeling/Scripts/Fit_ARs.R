#### Fit AR models


#packages
library(tidyverse)
library(MuMIn) #dredge
# library(rsq) #for 'rsq' R2 function
# library(Metrics) #for 'rmse' eval function

#### Read in data ----
ar_df <- read_csv("./CCR_AR_Modeling/Data/Daily_catwalk_met_RH_2021_2025_lagZS.csv")
head(ar_df)


#### Nice plot of fDOM at 2 depths ----
ar_df |>
  filter(Date >= ymd("2024-01-01")) |>
  select(Date, fDOM_1_QSU_daily, fDOM_9_QSU_daily) |>
  pivot_longer(-1) |>
  ggplot(aes(x = Date, y = value, col = name ))+
  #shade Helene storm period
  geom_rect(aes(xmin = ymd("2024-09-23"), xmax = ymd("2024-10-02"), ymin = -Inf, ymax = Inf),
            alpha = 1, fill = "gray", color = NA )+
  #shade Feb storm period
  geom_rect(aes(xmin = ymd("2025-02-12"), xmax = ymd("2025-02-16"), ymin = -Inf, ymax = Inf),
            alpha = 1, fill = "gray", color = NA )+
  #plot acutaly data
  geom_point() + theme_bw() + theme(legend.position = "top")

#storm plot
ar_df |>
  ggplot(aes(x = Date, y = Rain_mm_daily))+
  geom_point()




 #----------------------------------  DWH FDOM 9m --------------------------------------------------------
# #### Set up data ----
# fdom9m <- ar_df %>%
#   dplyr::select(Date, fDOM_9_QSU_daily, fDOM_9m_lag1_ZS, Chla_1_ugL_daily_ZS, DOsat_9_pct_daily_ZS ,
#          Temp_9_C_daily_ZS, Chla_1_ugL_10day_ZS,
#          SW_Wm2_daily_ZS,
#          #Rain_mm_daily_ZS, Rain_mm_lag1_ZS, Rain_mm_lag2_ZS
#          ) %>%
#   filter(Date < ymd("2024-01-01")) |>
#   mutate(Date = as.Date(Date))
#
# fdom9m <- na.omit(fdom9m) #removes all rows w/ NAs that prevent model from running
# head(fdom9m)
#
#
# #### Build global model and dredge ----
# # build a global model with the selected variables and then use dredge to see which combinations have the lowest AICc values
# model_fdom9m <- glm(fDOM_9_QSU_daily ~ fDOM_9m_lag1_ZS  +
#                       DOsat_9_pct_daily_ZS + Temp_9_C_daily_ZS +
#                       Chla_1_ugL_daily_ZS + Chla_1_ugL_10day_ZS +
#                       SW_Wm2_daily_ZS, # +
#                       # Rain_mm_daily_ZS + Rain_mm_lag1_ZS + Rain_mm_lag2_ZS ,
#                     data = fdom9m, family = gaussian, na.action = na.fail)
#
#
# glm_fdom9m <- dredge(model_fdom9m, rank = "AICc", fixed = "fDOM_9m_lag1_ZS") #scroll through glm_1316 to get lag only model
#
#
#
# #### Run and evaluate top model ----
# #check preds for top model
# mod1_fdom9m <- glm(fDOM_9_QSU_daily ~ fDOM_9m_lag1_ZS +
#                      DOsat_9_pct_daily_ZS + Temp_9_C_daily_ZS + SW_Wm2_daily_ZS,
#                    data = fdom9m, family = gaussian, na.action = na.fail)
#
# #predict
# pred1_fdom9 <- predict(mod1_fdom9m, newdata = fdom9m)
#
# # plot the predictions for the training dataset
# plot(fdom9m$Date, fdom9m$fDOM_9_QSU_daily, type = 'p', ylab = "fDOM (QSU)", xlab = "Date", pch = 16)
# points(fdom9m$Date, pred1_fdom9, col = 'dodgerblue', type = 'p')
#
# #R2 and RMSE
# round((rsq::rsq(mod1_fdom9m, type = 'sse')), digits = 2)
# round(Metrics::rmse(pred1_fdom9, fdom9m$fDOM_9_QSU_daily), digits = 1)
#
#
# #check preds AR
# modAR_fdom9m <- glm(fDOM_9_QSU_daily ~ fDOM_9m_lag1_ZS ,
#                    data = fdom9m, family = gaussian, na.action = na.fail)
#
# #predict
# predAR_fdom9 <- predict(modAR_fdom9m, newdata = fdom9m)
#
# # plot the predictions for the training dataset
# plot(fdom9m$Date, fdom9m$fDOM_9_QSU_daily, type = 'p', ylab = "fDOM (QSU)", xlab = "Date", pch = 16)
# points(fdom9m$Date, predAR_fdom9, col = 'dodgerblue', type = 'p')
#
# #R2 and RMSE
# round((rsq::rsq(modAR_fdom9m, type = 'sse')), digits = 2)
# round(Metrics::rmse(predAR_fdom9, fdom9m$fDOM_9_QSU_daily), digits = 1)
#
#
#
# ########## FDOM 9m testing -----------
# ### Set up data
# fdom9m_eval <- ar_df %>%
#   dplyr::select(Date, fDOM_9_QSU_daily, fDOM_9m_lag1_ZS, Chla_1_ugL_daily_ZS, DOsat_9_pct_daily_ZS ,
#                 Temp_9_C_daily_ZS, Chla_1_ugL_10day_ZS,
#                 SW_Wm2_daily_ZS,
#                 #Rain_mm_daily_ZS, Rain_mm_lag1_ZS, Rain_mm_lag2_ZS
#   ) %>%
#   filter(Date >= ymd("2024-01-01")) |>
#   mutate(Date = as.Date(Date))
#
# fdom9m_eval <- na.omit(fdom9m_eval) #removes all rows w/ NAs that prevent model from running
#
# #predict
# pred1_fdom9eval <- predict(mod1_fdom9m, newdata = fdom9m_eval)
#
# # plot the predictions for the training dataset
# plot(fdom9m_eval$Date, fdom9m_eval$fDOM_9_QSU_daily, type = 'p', ylab = "fDOM (QSU)", xlab = "Date", pch = 16)
# points(fdom9m_eval$Date, pred1_fdom9eval, col = 'dodgerblue', type = 'p')
#
# #R2 and RMSE
# round(Metrics::rmse(pred1_fdom9eval, fdom9m_eval$fDOM_9_QSU_daily), digits = 1)



#----------------------------------  Claude update for FDOM 9m --------------------------------------------------------

#### Set up data ----
fdom9m <- ar_df %>%
  dplyr::select(Date, fDOM_9_QSU_daily, fDOM_9m_lag1_ZS, Chla_1_ugL_daily_ZS, DOsat_9_pct_daily_ZS,
                Temp_9_C_daily_ZS, Chla_1_ugL_10day_ZS, SW_Wm2_daily_ZS,
                Q_rhessys_ZS, DOC_rhessys_ZS
                ) %>%
  filter(Date < ymd("2024-01-01")) %>%
  mutate(Date = as.Date(Date))

fdom9m <- na.omit(fdom9m)


#### Build global model and dredge ----
model_fdom9m <- glm(fDOM_9_QSU_daily ~ fDOM_9m_lag1_ZS +
                      DOsat_9_pct_daily_ZS + Temp_9_C_daily_ZS +
                      Chla_1_ugL_daily_ZS + Chla_1_ugL_10day_ZS +
                      SW_Wm2_daily_ZS,
                    data = fdom9m, family = gaussian, na.action = na.fail)

glm_fdom9m <- dredge(model_fdom9m, rank = "AICc", fixed = "fDOM_9m_lag1_ZS")


#### Run top model ----
mod1_fdom9m <- glm(fDOM_9_QSU_daily ~ fDOM_9m_lag1_ZS +
                     DOsat_9_pct_daily_ZS + Temp_9_C_daily_ZS + SW_Wm2_daily_ZS,
                   data = fdom9m, family = gaussian, na.action = na.fail)

modRH_fdom9m <- glm(fDOM_9_QSU_daily ~ fDOM_9m_lag1_ZS +
                      DOsat_9_pct_daily_ZS + Temp_9_C_daily_ZS + SW_Wm2_daily_ZS +
                      Q_rhessys_ZS + DOC_rhessys_ZS,
                    data = fdom9m, family = gaussian, na.action = na.fail)


#### Set up eval (testing) data ----
fdom9m_eval <- ar_df %>%
  dplyr::select(Date, fDOM_9_QSU_daily, fDOM_9m_lag1_ZS, Chla_1_ugL_daily_ZS, DOsat_9_pct_daily_ZS,
                Temp_9_C_daily_ZS, Chla_1_ugL_10day_ZS, SW_Wm2_daily_ZS,
                Q_rhessys_ZS, DOC_rhessys_ZS,
                ) %>%
  filter(Date >= ymd("2024-01-01")) %>%
  mutate(Date = as.Date(Date))

fdom9m_eval <- na.omit(fdom9m_eval)


#### Build combined prediction data frames ----
# Top model
pred_df_fdom9m <- bind_rows(
  tibble(
    Date             = fdom9m$Date,
    Observed_fDOM    = fdom9m$fDOM_9_QSU_daily,
    Predicted_fDOM   = predict(mod1_fdom9m, newdata = fdom9m),
    Period           = "Training"
  ),
  tibble(
    Date             = fdom9m_eval$Date,
    Observed_fDOM    = fdom9m_eval$fDOM_9_QSU_daily,
    Predicted_fDOM   = predict(mod1_fdom9m, newdata = fdom9m_eval),
    Period           = "Testing"
  )
) %>%
  mutate(Period = factor(Period, levels = c("Training", "Testing")))

# # AR-only model
predRH_df_fdom9m <- bind_rows(
  tibble(
    Date             = fdom9m$Date,
    Observed_fDOM    = fdom9m$fDOM_9_QSU_daily,
    Predicted_fDOM   = predict(modRH_fdom9m, newdata = fdom9m),
    Period           = "Training"
  ),
  tibble(
    Date             = fdom9m_eval$Date,
    Observed_fDOM    = fdom9m_eval$fDOM_9_QSU_daily,
    Predicted_fDOM   = predict(modRH_fdom9m, newdata = fdom9m_eval),
    Period           = "Testing"
  )
) %>%
  mutate(Period = factor(Period, levels = c("Training", "Testing")))


#### Evaluate metrics (R2 from pred vs observed, not model object) ----
calc_metrics <- function(df) {
  r2   <- round(cor(df$Observed_fDOM, df$Predicted_fDOM)^2, digits = 2)
  rmse <- round(Metrics::rmse(df$Predicted_fDOM, df$Observed_fDOM), digits = 1)
  list(R2 = r2, RMSE = rmse)
}

# Top model
calc_metrics(filter(pred_df_fdom9m, Period == "Training"))   # training R2 & RMSE
calc_metrics(filter(pred_df_fdom9m, Period == "Testing"))    # testing  R2 & RMSE

# RH model
calc_metrics(filter(predRH_df_fdom9m, Period == "Training"))
calc_metrics(filter(predRH_df_fdom9m, Period == "Testing"))


#### Plot: Observed vs Predicted over time ----
plot_fdom9m <- function(pred_df, model_label = "Top Model") {

  # Calculate R2 and RMSE per period for annotation
  metrics <- pred_df %>%
    group_by(Period) %>%
    summarise(
      R2   = round(cor(Observed_fDOM, Predicted_fDOM)^2, 2),
      RMSE = round(Metrics::rmse(Predicted_fDOM, Observed_fDOM), 1),
      .groups = "drop"
    ) %>%
    mutate(label = paste0(Period, ": R²=", R2, ", RMSE=", RMSE))

  split_date <- as.Date("2024-01-01")

  ggplot(pred_df, aes(x = Date)) +
     # Vertical split line
    geom_vline(xintercept = split_date,
               linetype = "dashed", color = "gray40", linewidth = 0.8) +
        # geom_line(aes(y = Predicted_fDOM, color = Period),
    #           linewidth = 0.9, alpha = 0.9) +
    geom_point(aes(y = Predicted_fDOM, color = Period),
               size = 1.3, alpha = 0.7) +
    # Observed points
    geom_point(aes(y = Observed_fDOM),
               color = "gray30", size = 1, alpha = 0.7, shape = 16) +
    # Predicted line colored by period
    scale_color_manual(
      values = c("Training" = "dodgerblue", "Testing" = "tomato"),
      name = "Period"
    ) +
    labs(
      title    = paste("fDOM 9m —", model_label),
      x        = "Date",
      y        = "fDOM (QSU)"
    ) +
    theme_bw(base_size = 12) +
    theme(
      legend.position  = "bottom",
      panel.grid.minor = element_blank(),
      plot.subtitle    = element_text(size = 9, color = "gray30")
    )
}

plot_fdom9m(pred_df_fdom9m,   model_label = "AR Top Model (+ env. covariates)")
plot_fdom9m(predRH_df_fdom9m, model_label = "AR + RH Model")









#----------------------------------  Claude update for FDOM 1m --------------------------------------------------------

#### Set up data ----
fdom1m <- ar_df %>%
  dplyr::select(Date, fDOM_1_QSU_daily, fDOM_1m_lag1_ZS, Chla_1_ugL_daily_ZS, DOsat_1_pct_daily_ZS,
                Temp_1_C_daily_ZS, Chla_1_ugL_10day_ZS, SW_Wm2_daily_ZS,
                Q_rhessys_ZS, DOC_rhessys_ZS
  ) %>%
  filter(Date < ymd("2024-01-01")) %>%
  mutate(Date = as.Date(Date))

fdom1m <- na.omit(fdom1m)


#### Build global model and dredge ----
model_fdom1m <- glm(fDOM_1_QSU_daily ~ fDOM_1m_lag1_ZS +
                      DOsat_1_pct_daily_ZS + Temp_1_C_daily_ZS +
                      Chla_1_ugL_daily_ZS + Chla_1_ugL_10day_ZS +
                      SW_Wm2_daily_ZS,
                    data = fdom1m, family = gaussian, na.action = na.fail)

glm_fdom1m <- dredge(model_fdom1m, rank = "AICc", fixed = "fDOM_1m_lag1_ZS")


#### Run top model ----
mod1_fdom1m <- glm(fDOM_1_QSU_daily ~ fDOM_1m_lag1_ZS +
                     DOsat_1_pct_daily_ZS + SW_Wm2_daily_ZS,
                   data = fdom1m, family = gaussian, na.action = na.fail)

modRH_fdom1m <- glm(fDOM_1_QSU_daily ~ fDOM_1m_lag1_ZS +
                      DOsat_1_pct_daily_ZS + SW_Wm2_daily_ZS +
                      Q_rhessys_ZS + DOC_rhessys_ZS,
                    data = fdom1m, family = gaussian, na.action = na.fail)


#### Set up eval (testing) data ----
fdom1m_eval <- ar_df %>%
  dplyr::select(Date, fDOM_1_QSU_daily, fDOM_1m_lag1_ZS, Chla_1_ugL_daily_ZS, DOsat_1_pct_daily_ZS,
                Temp_1_C_daily_ZS, Chla_1_ugL_10day_ZS, SW_Wm2_daily_ZS,
                Q_rhessys_ZS, DOC_rhessys_ZS,
  ) %>%
  filter(Date >= ymd("2024-01-01")) %>%
  mutate(Date = as.Date(Date))

fdom1m_eval <- na.omit(fdom1m_eval)


#### Build combined prediction data frames ----
# Top model
pred_df_fdom1m <- bind_rows(
  tibble(
    Date             = fdom1m$Date,
    Observed_fDOM    = fdom1m$fDOM_1_QSU_daily,
    Predicted_fDOM   = predict(mod1_fdom1m, newdata = fdom1m),
    Period           = "Training"
  ),
  tibble(
    Date             = fdom1m_eval$Date,
    Observed_fDOM    = fdom1m_eval$fDOM_1_QSU_daily,
    Predicted_fDOM   = predict(mod1_fdom1m, newdata = fdom1m_eval),
    Period           = "Testing"
  )
) %>%
  mutate(Period = factor(Period, levels = c("Training", "Testing")))

# # AR-only model
predRH_df_fdom1m <- bind_rows(
  tibble(
    Date             = fdom1m$Date,
    Observed_fDOM    = fdom1m$fDOM_1_QSU_daily,
    Predicted_fDOM   = predict(modRH_fdom1m, newdata = fdom1m),
    Period           = "Training"
  ),
  tibble(
    Date             = fdom1m_eval$Date,
    Observed_fDOM    = fdom1m_eval$fDOM_1_QSU_daily,
    Predicted_fDOM   = predict(modRH_fdom1m, newdata = fdom1m_eval),
    Period           = "Testing"
  )
) %>%
  mutate(Period = factor(Period, levels = c("Training", "Testing")))


#### Evaluate metrics (R2 from pred vs observed, not model object) ----
calc_metrics <- function(df) {
  r2   <- round(cor(df$Observed_fDOM, df$Predicted_fDOM)^2, digits = 2)
  rmse <- round(Metrics::rmse(df$Predicted_fDOM, df$Observed_fDOM), digits = 1)
  list(R2 = r2, RMSE = rmse)
}

# Top model
calc_metrics(filter(pred_df_fdom1m, Period == "Training"))   # training R2 & RMSE
calc_metrics(filter(pred_df_fdom1m, Period == "Testing"))    # testing  R2 & RMSE

# RH model
calc_metrics(filter(predRH_df_fdom1m, Period == "Training"))
calc_metrics(filter(predRH_df_fdom1m, Period == "Testing"))


#### Plot: Observed vs Predicted over time ----
plot_fdom1m <- function(pred_df, model_label = "Top Model") {

  # Calculate R2 and RMSE per period for annotation
  metrics <- pred_df %>%
    group_by(Period) %>%
    summarise(
      R2   = round(cor(Observed_fDOM, Predicted_fDOM)^2, 2),
      RMSE = round(Metrics::rmse(Predicted_fDOM, Observed_fDOM), 1),
      .groups = "drop"
    ) %>%
    mutate(label = paste0(Period, ": R²=", R2, ", RMSE=", RMSE))

  split_date <- as.Date("2024-01-01")

  ggplot(pred_df, aes(x = Date)) +
    # Vertical split line
    geom_vline(xintercept = split_date,
               linetype = "dashed", color = "gray40", linewidth = 0.8) +
    # geom_line(aes(y = Predicted_fDOM, color = Period),
    #           linewidth = 0.9, alpha = 0.9) +
    geom_point(aes(y = Predicted_fDOM, color = Period),
               size = 1.3, alpha = 0.7) +
    # Observed points
    geom_point(aes(y = Observed_fDOM),
               color = "gray30", size = 1, alpha = 0.7, shape = 16) +
    # Predicted line colored by period
    scale_color_manual(
      values = c("Training" = "dodgerblue", "Testing" = "tomato"),
      name = "Period"
    ) +
    labs(
      title    = paste("fDOM 1m —", model_label),
      x        = "Date",
      y        = "fDOM (QSU)"
    ) +
    theme_bw(base_size = 12) +
    theme(
      legend.position  = "bottom",
      panel.grid.minor = element_blank(),
      plot.subtitle    = element_text(size = 9, color = "gray30")
    )
}

plot_fdom1m(pred_df_fdom1m,   model_label = "AR Top Model (+ env. covariates)")
plot_fdom1m(predRH_df_fdom1m, model_label = "AR + RH Model")














































































#----------------------------------  FDOM 1m --------------------------------------------------------


#### Set up data ----
fdom1m <- ar_df %>%
  dplyr::select(Date, fDOM_1_QSU_daily, fDOM_1m_lag1_ZS, Chla_1_ugL_daily_ZS, DOsat_1_pct_daily_ZS ,
                Temp_1_C_daily_ZS, Chla_1_ugL_10day_ZS,
                Rain_mm_daily_ZS, Rain_mm_lag1_ZS, Rain_mm_lag2_ZS, SW_Wm2_daily_ZS) %>%
  mutate(Date = as.Date(Date))

fdom1m <- na.omit(fdom1m) #removes all rows w/ NAs that prevent model from running
head(fdom1m)


# build a global model with the selected variables and then use dredge to see which combinations have the lowest AICc values
model_fdom1m <- glm(fDOM_1_QSU_daily ~ fDOM_1m_lag1_ZS  +
                      DOsat_1_pct_daily_ZS +  Temp_1_C_daily_ZS +
                      Chla_1_ugL_daily_ZS + Chla_1_ugL_10day_ZS +
                      SW_Wm2_daily_ZS, # +
                      #Rain_mm_daily_ZS + Rain_mm_lag1_ZS + Rain_mm_lag2_ZS,
                    data = fdom1m, family = gaussian, na.action = na.fail)


glm_fdom1m <- dredge(model_fdom1m, rank = "AICc", fixed = "fDOM_1m_lag1_ZS") #scroll through glm_1316 to get lag only model




#check preds for top model
mod1_fdom1m <- glm(fDOM_1_QSU_daily ~ fDOM_1m_lag1_ZS + SW_Wm2_daily_ZS + Rain_mm_lag1_ZS,
                   data = fdom1m, family = gaussian, na.action = na.fail)

#these arent 2 and 3; jsut trying other combos
mod1_fdom1m <- glm(fDOM_1_QSU_daily ~ fDOM_1m_lag1_ZS + SW_Wm2_daily_ZS,
                   data = fdom1m, family = gaussian, na.action = na.fail)

mod1_fdom1m <- glm(fDOM_1_QSU_daily ~ fDOM_1m_lag1_ZS + SW_Wm2_daily_ZS +
                     Temp_1_C_daily_ZS + Chla_1_ugL_daily_ZS + DOsat_1_pct_daily_ZS,
                   data = fdom1m, family = gaussian, na.action = na.fail)

#predict
pred1_fdom1 <- predict(mod1_fdom1m, newdata = fdom1m)




# plot the predictions for the 2014 training dataset
plot(fdom1m$Date, fdom1m$fDOM_1_QSU_daily, type = 'p', ylab = "fDOM (QSU)", xlab = "Date", pch = 16)
points(fdom1m$Date, pred1_fdom1, col = 'dodgerblue', type = 'p')

#R2 and RMSE
round((rsq::rsq(mod1_fdom1m, type = 'sse')), digits = 2)
round(Metrics::rmse(pred1_fdom1, fdom1m$fDOM_1_QSU_daily), digits = 1)


















