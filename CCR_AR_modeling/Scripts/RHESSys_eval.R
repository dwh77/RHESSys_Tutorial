####  RHESSys model evaluation for fDOM prediction project

#### packages
library(tidyverse)
library(patchwork)

#### Read in RHESSys outputs ----
workpath <- "C:/Users/dwh18/OneDrive/Desktop/R_Projects/RHESSys_development/ccr_rhessys/out/ccr_patch1500_cow1"  #HPBspin_cowCal7"



################################################################################
#### Basin level ----
output_grow <- read_delim(paste0(workpath, "/harvest1850_2026run_grow_basin.daily"),
                          delim = " ", col_names = T)

output_h2o <- read_delim(paste0(workpath, "/harvest1850_2026run_basin.daily"),
                         delim = " ", col_names = T)

ccr_area_m2 <- 45.83578 * 1000000

output_h2o_grow <- left_join(output_h2o, output_grow, by = c("day", "month", "year", "basinID")) |>
  mutate(date = ymd(paste(year, month, day, sep = "-"))) |>
  filter(date >= ymd("2020-01-01")) |>
  select(date, streamflow, return, baseflow, streamflow_NO3, streamflow_DOC, lai.y) |>
  rename(lai = lai.y) |>
  #Q unit conversions
  #streamflow output is in mm/day
  mutate(streamflow_m_day = streamflow / 1000) |> #convert mm/day to m/day
  mutate(streamflow_m3_day = streamflow_m_day * ccr_area_m2) |> #convert m/day to m3/day
  #chem conversions from g/m2/day to mg/L
  #streamflow_DOC is in g/m2/day
  mutate(DOC_mgL = streamflow_DOC / streamflow_m_day,
         NO3_mgL = streamflow_NO3 / streamflow_m_day)



################################################################################
#### Stream routing eval ----
output_streamrouting <- read_delim(paste0(workpath, "/harvest1850_2026run_streamrouting.daily"),
                                   delim = " ", col_names = T)

#get reaches of interest
sr_df <- output_streamrouting  |>
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) |>
  filter(date >= ymd("2020-01-01")) |>
  # filter(reachID == 2) |>
  filter(reachID %in% c(2,62,36,28)) |>
  mutate(reach_ID_name = ifelse(reachID == 2, "CCR dam", NA),
         reach_ID_name = ifelse(reachID == 62, "HPB", reach_ID_name),
         reach_ID_name = ifelse(reachID == 36, "SMB", reach_ID_name),
         reach_ID_name = ifelse(reachID == 28, "CCS", reach_ID_name)
  ) |>
  #Q_out units are in m3/day
  #DOC_out units are in kg C/day
  #DOC_out/Qout = kg/m3 == g/L
  #so then multiply by 1000 to get mg/L
  mutate(NO3_mgL_mod = (NO3_out / Qout) * 1000,
         DOC_mgL_mod = (DOC_out / Qout) * 1000,
         #DON_mgL_mod = (DON_out / Qout) * 1000,
         Q_m3day_mod = Qout, #convert to m3/day
         ) |>
  dplyr::select(date, reach_ID_name, Q_m3day_mod, NO3_mgL_mod, DOC_mgL_mod) |>
  pivot_longer(cols = c(Q_m3day_mod, NO3_mgL_mod, DOC_mgL_mod), names_to = "variable", values_to = "value")


#SR for a few vars
sr_df |>
  ggplot(aes(x = date, y = value, col = as.factor(reach_ID_name))) +
  geom_line() +
  theme_bw() + theme(legend.position = "top")+
  facet_wrap(~variable, scales = "free_y")

#SR Q in 2024
sr_df |>
  filter(variable == "Q_m3day_mod") |>
  filter(date >= ymd("2024-01-01")) |>
  ggplot(aes(x = date, y = value, col = as.factor(reach_ID_name))) +
  geom_line(linewidth = 1) + scale_y_log10()+
  theme_bw() + theme(legend.position = "top")+ ylab("Q m3/day")+
  facet_wrap(~variable, scales = "free_y")






################################################################################
### Eval data
#### READ in and format eval data
target <- read_csv("C:/Users/dwh18/OneDrive/Desktop/R_Projects/RHESSys_development/ccr_rhessys/Target_Data_comp/TargetData_2020_2025.csv")

# eval <- target |>
#   mutate(date = as.Date(Date)) |>
#   filter(date >= ymd("2020-01-01")) |>
#   select(date, HPB_Q_lm_m3day, NO3_mgL, DOC_mgL, lai_MODIS) |>
#   rename(lai = lai_MODIS,
#          Q_m3_day = HPB_Q_lm_m3day )





################################################################################
#### LAI evals ----
#site 101 is only LAI
lai_eval <- target |>
  mutate(date = as.Date(Date)) |>
  filter(date >= ymd("2020-01-01")) |>
  select(date, lai_MODIS)

output_h2o_grow |>
  select(date, lai) |>  rename(lai_RHESSys = lai) |>
  left_join(lai_eval, by = "date") |>
  # pivot_longer(-1) |>
  ggplot(aes(x = date))+
  geom_line(aes(y = lai_RHESSys), color = "blue")+
  geom_point(aes(y = lai_MODIS), color = "black")+
  theme_bw()+
  labs(x = "Date", y = "LAI")




################################################################################
#### HPB Q evals ----
hpb_PT_eval <- target |>
  mutate(date = as.Date(Date)) |>
  filter(date >= ymd("2020-01-01"),
         Site == 100) |>
  select(date, HPB_Q_PT_m3day, HPB_Q_lm_m3day)


### HPB PT discharge
sr_df |>
  filter(reach_ID_name == "HPB") |>
  pivot_wider(names_from = variable, values_from = value) |>
  left_join(hpb_PT_eval, by = "date") |>
  select(date, Q_m3day_mod, HPB_Q_PT_m3day, HPB_Q_lm_m3day) |>
  pivot_longer(cols = c(HPB_Q_PT_m3day, HPB_Q_lm_m3day), names_to = "HPB_pt_type", values_to = "HPB_pt_value") |>
  ggplot(aes(x = date))+
  geom_point(aes(y = Q_m3day_mod))

# Prepare data and convert m3/day to L/s (divide by 86.4)
hpb_plot <- sr_df |>
  filter(reach_ID_name == "HPB") |>
  pivot_wider(names_from = variable, values_from = value) |>
  left_join(hpb_PT_eval, by = "date") |>
  select(date, Q_m3day_mod, HPB_Q_PT_m3day, HPB_Q_lm_m3day) |>
  mutate(across(c(Q_m3day_mod, HPB_Q_PT_m3day, HPB_Q_lm_m3day), ~ . / 86.4))

# Calculate RMSE for each comparison
rmse_PT <- hpb_plot |>
  filter(!is.na(HPB_Q_PT_m3day)) |>
  summarise(RMSE = round(sqrt(mean((Q_m3day_mod - HPB_Q_PT_m3day)^2)), 2)) |>
  pull(RMSE)

rmse_lm <- hpb_plot |>
  filter(!is.na(HPB_Q_lm_m3day)) |>
  summarise(RMSE = round(sqrt(mean((Q_m3day_mod - HPB_Q_lm_m3day)^2)), 2)) |>
  pull(RMSE)

# Plot 1: Modeled vs PT
p1 <- ggplot(hpb_plot, aes(x = date)) +
  geom_point(aes(y = HPB_Q_PT_m3day), color = "black", size = 1.5, alpha = 0.6) +
  geom_line(aes(y = Q_m3day_mod), color = "blue") +
  labs(title = paste0("HPB: Modeled vs PT  |  RMSE = ", rmse_PT, " L/s"),
       x = NULL, y = "Q (L/s)") +
  scale_y_log10()+
  theme_bw()

# Plot 2: Modeled vs LM
p2 <- ggplot(hpb_plot, aes(x = date)) +
  geom_point(aes(y = HPB_Q_lm_m3day), color = "black", size = 1.5, alpha = 0.6) +
  geom_line(aes(y = Q_m3day_mod), color = "blue") +
  labs(title = paste0("HPB: Modeled vs LM  |  RMSE = ", rmse_lm, " L/s"),
       x = NULL, y = "Q (L/s)") +
  scale_y_log10()+
  theme_bw()

# Display together
p1 / p2

################################################################################
#### Stream chem and flowmate eval ----

##set up obs data
obs_long <- target |>
  mutate(date = as.Date(Date)) |>
  filter(date >= ymd("2020-01-01"),
         Site != 101) |>
  mutate(Site_name = ifelse(Site == 100, "HPB", NA),
         Site_name = ifelse(Site == 300, "SMB", Site_name),
         Site_name = ifelse(Site == 200, "CCS", Site_name)
         ) |>
  select(date, Site_name, Q_m3day_flowmate, NO3_mgL, DOC_mgL) |>
  rename(Site = Site_name) |>
  pivot_longer(cols = c(Q_m3day_flowmate, DOC_mgL, NO3_mgL),
               names_to = "Variable", values_to = "Value") |>
  mutate(
    Data_type = "Observed",
    Variable  = case_when(
      str_detect(Variable, "Q")   ~ "Q_m3day",
      str_detect(Variable, "DOC") ~ "DOC_mgL",
      str_detect(Variable, "NO3") ~ "NO3_mgL"
    )
  ) |>
  select(Date = date, Site, Data_type, Variable, Value)


# Pivot sr_df long, remove CCR dam, clean variable names
sr_long <- sr_df |>
  filter(reach_ID_name != "CCR dam") |>
  rename(Site = reach_ID_name) |>
  mutate(
    Data_type = "Modeled",
    Variable  = case_when(
      str_detect(variable, "Q")   ~ "Q_m3day",
      str_detect(variable, "DOC") ~ "DOC_mgL",
      str_detect(variable, "NO3") ~ "NO3_mgL"
    )
  ) |>
  select(Date = date, Site, Data_type, Variable, Value = value)

# Bind together
combined_long <- bind_rows(sr_long, obs_long)

## plot
combined_long |>
ggplot(aes(x = Date, y = Value)) +
  geom_line(data = filter(combined_long, Data_type == "Modeled"), color = "lightblue") +
  geom_point(data = filter(combined_long, Data_type == "Observed"), size = 1.5, alpha = 0.6) +
  facet_grid(Variable~Site, scales = "free_y") +
  labs(title = "Modeled vs Observed", y = NULL, color = "Site") +
  theme_bw()

# Q only plot
ggplot(filter(combined_long, Variable == "Q_m3day"),
       aes(x = Date, y = Value)) +
  geom_line(data = filter(combined_long, Variable == "Q_m3day", Data_type == "Modeled"), color = "lightblue") +
  geom_point(data = filter(combined_long, Variable == "Q_m3day", Data_type == "Observed"),
             size = 1.5, alpha = 0.6) +
  scale_y_log10() +
  facet_wrap(~Site)+
  labs(title = "Modeled vs Observed Discharge", y = "Q (m³/day, log scale)", color = "Site") +
  theme_bw()


## stats
stats_df <- combined_long |>
  summarise(Value = mean(Value, na.rm = TRUE),
            .by = c(Date, Site, Variable, Data_type)) |>
  pivot_wider(names_from = Data_type, values_from = Value) |>
  filter(!is.na(Observed), !is.na(Modeled))
##weird dups in SMB on one day averaged above
# stats_df <- combined_long |>
#   pivot_wider(names_from = Data_type, values_from = Value) |>
#   filter(!is.na(Observed), !is.na(Modeled))
#
# combined_long |>
#   summarise(n = n(), .by = c(Date, Site, Variable, Data_type)) |>
#   filter(n > 1L)

# Helper function
calc_metrics <- function(df) {
  df |>
    summarise(
      RMSE = sqrt(mean((Observed - Modeled)^2)),
      MAE  = mean(abs(Observed - Modeled)),
      R2   = cor(Observed, Modeled)^2
    )
}

# Aggregated across sites
metrics_overall <- stats_df |>
  group_by(Variable) |>
  calc_metrics()

# By site
metrics_by_site <- stats_df |>
  group_by(Variable, Site) |>
  calc_metrics()

metrics_overall
metrics_by_site

