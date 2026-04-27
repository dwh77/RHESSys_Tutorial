#### Fit AR models


#packages
library(tidyverse)
library(MuMIn) #dredge
# library(rsq) #for 'rsq' R2 function
# library(Metrics) #for 'rmse' eval function

#### Read in data ----
ar_df <- read_csv("./CCR_AR_Modeling/Data/Daily_catwalk_met_2021_2025_lagZS.csv")
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




 #----------------------------------  FDOM 9m --------------------------------------------------------

# fdom9m_2025 <- ar_df %>%
#   dplyr::select(Date, fDOM_9_QSU_daily, fDOM_9m_lag1_ZS, Chla_1_ugL_daily_ZS, DOsat_9_pct_daily_ZS ,
#                 Temp_9_C_daily_ZS,
#                 Rain_mm_daily_ZS, Rain_mm_lag1_ZS, Rain_mm_lag2_ZS, SW_Wm2_daily_ZS) %>%
#   mutate(Date = as.Date(Date)) |>
#   filter(Date >= ymd("2025-01-01"))
#
# fdom9m_2025 <- na.omit(fdom9m_2025) #removes all rows w/ NAs that prevent model from running


#### Set up data ----
fdom9m <- ar_df %>%
  dplyr::select(Date, fDOM_9_QSU_daily, fDOM_9m_lag1_ZS, Chla_1_ugL_daily_ZS, DOsat_9_pct_daily_ZS ,
         Temp_9_C_daily_ZS, Chla_1_ugL_10day_ZS,
         Rain_mm_daily_ZS, Rain_mm_lag1_ZS, Rain_mm_lag2_ZS, SW_Wm2_daily_ZS) %>%
  mutate(Date = as.Date(Date))
  # filter(Date < ymd("2025-01-01"))

fdom9m <- na.omit(fdom9m) #removes all rows w/ NAs that prevent model from running
head(fdom9m)


#### Build global model and dredge ----
# build a global model with the selected variables and then use dredge to see which combinations have the lowest AICc values
model_fdom9m <- glm(fDOM_9_QSU_daily ~ fDOM_9m_lag1_ZS  +
                      DOsat_9_pct_daily_ZS + Temp_9_C_daily_ZS +
                      Chla_1_ugL_daily_ZS + Chla_1_ugL_10day_ZS +
                      SW_Wm2_daily_ZS, # +
                      # Rain_mm_daily_ZS + Rain_mm_lag1_ZS + Rain_mm_lag2_ZS ,
                    data = fdom9m, family = gaussian, na.action = na.fail)


glm_fdom9m <- dredge(model_fdom9m, rank = "AICc", fixed = "fDOM_9m_lag1_ZS") #scroll through glm_1316 to get lag only model



#### Run and evaluate top model ----
#check preds for top model
mod1_fdom9m <- glm(fDOM_9_QSU_daily ~ fDOM_9m_lag1_ZS + DOsat_9_pct_daily_ZS +
                                   Temp_9_C_daily_ZS + Rain_mm_lag2_ZS,
                                 data = fdom9m, family = gaussian, na.action = na.fail)

#Top with no Rain variables
mod1_fdom9m <- glm(fDOM_9_QSU_daily ~ fDOM_9m_lag1_ZS +
                     DOsat_9_pct_daily_ZS + Temp_9_C_daily_ZS ,
                   data = fdom9m, family = gaussian, na.action = na.fail)

#predict
pred1_fdom9 <- predict(mod1_fdom9m, newdata = fdom9m)

# plot the predictions for the 2014 training dataset
plot(fdom9m$Date, fdom9m$fDOM_9_QSU_daily, type = 'p', ylab = "fDOM (QSU)", xlab = "Date", pch = 16)
points(fdom9m$Date, pred1_fdom9, col = 'dodgerblue', type = 'p')

#R2 and RMSE
round((rsq::rsq(mod1_fdom9m, type = 'sse')), digits = 2)
round(Metrics::rmse(pred1_fdom9, fdom9m$fDOM_9_QSU_daily), digits = 1)







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


















