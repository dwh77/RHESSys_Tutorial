#### Compile data sets for CCR AR modeling


##packages
library(tidyverse)
library(slider) # for rolling windows 'slide_dbl'


#### EDI data sets ----
catwalk <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/1069/4/42e6d8bb3d379d40a4a4fb566d4ff36e" )

options(timeout = 300)  # 5 minutes
met <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/1105/4/8ebf27393ccafe518328468a260d2e18")


#### Daily Met ----

#Get daily summed rain and mean SW
daily_met <- met |>
  select(DateTime, Rain_Total_mm, ShortwaveRadiationUp_Average_W_m2) |>
  mutate(Date = as.Date(DateTime)) |>
  group_by(Date) |>
  summarise(Rain_mm_daily = sum(Rain_Total_mm, na.rm = T),
            SW_Wm2_daily = mean(ShortwaveRadiationUp_Average_W_m2, na.rm = T))





#### Format catwalk data ----
p <- -0.01

catwalk_daily <- catwalk |>
  mutate(fdom1_TC = EXOfDOM_QSU_1/(1 + (p*(EXOTemp_C_1 - 20)) ) ,
         fdom9_TC = EXOfDOM_QSU_9/(1 + (p*(EXOTemp_C_9 - 20)) )
  ) |>
  mutate(Date = as.Date(DateTime)) |>
   group_by(Date) |>
  summarise(
    # fDOM (temperature corrected)
    fDOM_1_QSU_daily     = mean(fdom1_TC,              na.rm = TRUE),
    fDOM_9_QSU_daily     = mean(fdom9_TC,              na.rm = TRUE),
    # Water temperature
    Temp_1_C_daily       = mean(EXOTemp_C_1,           na.rm = TRUE),
    Temp_9_C_daily       = mean(EXOTemp_C_9,           na.rm = TRUE),
    # Chlorophyll-a
    Chla_1_ugL_daily     = mean(EXOChla_ugL_1,         na.rm = TRUE),
    # DO % saturation
    DOsat_1_pct_daily    = mean(EXODOsat_percent_1,    na.rm = TRUE),
    DOsat_9_pct_daily    = mean(EXODOsat_percent_9,    na.rm = TRUE),
    # Specific conductance
    SC_1_uScm_daily  = mean(EXOSpCond_uScm_1,      na.rm = TRUE),
    SC_9_uScm_daily  = mean(EXOSpCond_uScm_9,      na.rm = TRUE)
  )


#### Join met to catwalk ----
AR_df <- full_join(daily_met, catwalk_daily, by = "Date")

# write.csv(AR_df, "./CCR_AR_Modeling/Data/Daily_catwalk_met_2021_2025.csv", row.names = F)

#### Make lags and ZT data ----
# AR_df <- read_csv("./CCR_AR_Modeling/Data/Daily_catwalk_met_2021_2025.csv")
#
# AR_df_lag <- AR_df |>
#   mutate(fDOM_1m_lag1 = lag(fDOM_1_QSU_daily , 1),
#          fDOM_9m_lag1 = lag(fDOM_9_QSU_daily , 1),
#          Rain_mm_lag1 = lag(Rain_mm_daily, 1),
#          Rain_mm_lag2 = lag(Rain_mm_daily, 2)) |>
#   mutate(
#     Rain_mm_10day    = slider::slide_dbl(Rain_mm_daily,    sum,  .before = 10, .after = 0, .complete = TRUE),
#     Chla_1_ugL_10day = slider::slide_dbl(Chla_1_ugL_daily, mean, .before = 10, .after = 0, .complete = F)
#   )
#
# AR_df_lag_ZS <- AR_df_lag |>
#   mutate(across(
#     .cols = !Date,
#     .fns  = ~ scale(.x)[,1],
#     .names = "{.col}_ZS"
#   ))


# write.csv(AR_df_lag_ZS, "./CCR_AR_Modeling/Data/Daily_catwalk_met_2021_2025_lagZS.csv", row.names = F)


#### Get RHESSys data and format to bind ----
## read in rest of daily
AR_df <- read_csv("./CCR_AR_Modeling/Data/Daily_catwalk_met_2021_2025.csv")

##read in RHESSys
workpath <- "C:/Users/dwh18/OneDrive/Desktop/R_Projects/RHESSys_development/ccr_rhessys/out/ccr_patch1500_KEEP"

output_grow <- read_delim(paste0(workpath, "/harvest1850_2025run_grow_basin.daily"),
                          delim = " ", col_names = T)

output_h2o <- read_delim(paste0(workpath, "/harvest1850_2025run_basin.daily"),
                         delim = " ", col_names = T)

# HPB area is 4.299446 km^2
# hpb_area_m2 <- 4.299446 * 1000000
ccr_area_m2 <- 45.83578 * 1000000


output_h2o_grow <- left_join(output_h2o, output_grow, by = c("day", "month", "year", "basinID")) |>
  mutate(date = ymd(paste(year, month, day, sep = "-"))) |>
  filter(date >= ymd("2020-01-01")) |>
  select(date, streamflow, streamflow_NO3, streamflow_DOC, lai.y) |>
  rename(lai = lai.y) |>
  #Q unit conversions
  mutate(streamflow_m_day = streamflow / 1000) |> #convert mm/day to m/day
  mutate(streamflow_m3_day = streamflow_m_day * ccr_area_m2) |> #convert m/day to m3/day
  #chem conversions from g/m2/day to mg/L
  mutate(DOC_mgL = streamflow_DOC / streamflow_m_day,
         NO3_mgL = streamflow_NO3 / streamflow_m_day)


rhessys_AR <- output_h2o_grow |>
  rename(Date = date) |>
  select(Date, streamflow_DOC, streamflow_m3_day, DOC_mgL) |>
  rename(RH_streamflow_DOC_gm2day = streamflow_DOC,
         RH_Q_m3day = streamflow_m3_day,
         RH_DOC_mgL = DOC_mgL)



#### Z score and export
AR_df_lag_ZS <- AR_df |>
    mutate(fDOM_1m_lag1 = lag(fDOM_1_QSU_daily , 1),
           fDOM_9m_lag1 = lag(fDOM_9_QSU_daily , 1),
           Rain_mm_lag1 = lag(Rain_mm_daily, 1),
           Rain_mm_lag2 = lag(Rain_mm_daily, 2)) |>
    mutate(
      Rain_mm_10day    = slider::slide_dbl(Rain_mm_daily,    sum,  .before = 10, .after = 0, .complete = TRUE),
      Chla_1_ugL_10day = slider::slide_dbl(Chla_1_ugL_daily, mean, .before = 10, .after = 0, .complete = F)
    ) |>
  left_join(rhessys_AR, by = "Date") |>
  mutate(across(
    .cols = !Date,
    .fns  = ~ scale(.x)[,1],
    .names = "{.col}_ZS"
  ))


 # write.csv(AR_df_lag_ZS, "./CCR_AR_Modeling/Data/Daily_catwalk_met_RH_2021_2025_lagZS.csv", row.names = F)


