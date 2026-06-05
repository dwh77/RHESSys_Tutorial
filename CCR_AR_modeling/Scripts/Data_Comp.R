#### Compile data sets for CCR AR modeling


##packages
library(tidyverse)
library(rLakeAnalyzer) #for density
library(slider) # for rolling windows 'slide_dbl'


#### EDI data sets ----
options(timeout = 300)  # 5 minutes
catwalk_EDI <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/1069/4/42e6d8bb3d379d40a4a4fb566d4ff36e" )
summary(ymd_hms(catwalk_EDI$DateTime))

catwalk_githubL1 <- read.csv("https://raw.githubusercontent.com/FLARE-forecast/CCRE-data/refs/heads/ccre-dam-data-qaqc/ccre-waterquality_L1.csv")
summary(ymd_hms(catwalk_githubL1$DateTime))


catwalk <- rbind(catwalk_EDI, catwalk_githubL1) |>
  mutate(DateTime = ymd_hms(DateTime))



#### Daily Met ----
# met <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/1105/4/8ebf27393ccafe518328468a260d2e18")

# #Get daily summed rain and mean SW
# daily_met <- met |>
#   select(DateTime, Rain_Total_mm, ShortwaveRadiationUp_Average_W_m2) |>
#   mutate(Date = as.Date(DateTime)) |>
#   group_by(Date) |>
#   summarise(Rain_mm_daily = sum(Rain_Total_mm, na.rm = T),
#             SW_Wm2_daily = mean(ShortwaveRadiationUp_Average_W_m2, na.rm = T))





#### Format catwalk data ----

#### Get daily fDOM and EXO variables
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
    #fDOM_9_QSU_daily     = mean(fdom9_TC,              na.rm = TRUE),
    # Water temperature
    Temp_1_C_daily       = mean(EXOTemp_C_1,           na.rm = TRUE),
    #Temp_9_C_daily       = mean(EXOTemp_C_9,           na.rm = TRUE),
    # Chlorophyll-a
    Chla_1_ugL_daily     = mean(EXOChla_ugL_1,         na.rm = TRUE),
    # DO % saturation
    DOsat_1_pct_daily    = mean(EXODOsat_percent_1,    na.rm = TRUE)
    #DOsat_9_pct_daily    = mean(EXODOsat_percent_9,    na.rm = TRUE),
    # # Specific conductance
    # SC_1_uScm_daily  = mean(EXOSpCond_uScm_1,      na.rm = TRUE),
    # SC_9_uScm_daily  = mean(EXOSpCond_uScm_9,      na.rm = TRUE)
  )


#### Get stratification values
source("./CCR_AR_modeling/Scripts/find_depths.R")
# depth_offsets_df <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/1069/4/23caf92df7e665597ebc329d9e406637")

catwalk_EDI_link <- "https://pasta.lternet.edu/package/data/eml/edi/1069/4/42e6d8bb3d379d40a4a4fb566d4ff36e"
catwalk_github_link <- "https://raw.githubusercontent.com/FLARE-forecast/CCRE-data/refs/heads/ccre-dam-data-qaqc/ccre-waterquality_L1.csv"
depth_offsets_link <- "https://pasta.lternet.edu/package/data/eml/edi/1069/4/23caf92df7e665597ebc329d9e406637"


depths_EDI <- find_depths(data_file = catwalk_EDI_link,
                      depth_offset = depth_offsets_link)

depths_git <- find_depths(data_file = catwalk_github_link,
                          depth_offset = depth_offsets_link)


unique(depths_EDI$variable)

##get daily depths
depths_daily <- rbind(depths_EDI, depths_git) |>
  filter(variable == "ThermistorTemp") |>
  mutate(depth1 = round(sensor_depth, 1)) |>
  mutate(Date = as.Date(DateTime)) |>
  group_by(Date, variable, Position) |>
  summarise(Temp_C = mean(observation, na.rm = T),
            sensor_depth_m = mean(sensor_depth, na.rm = T),
            .groups = "drop")


#get top and bottom temp
Dens_diff <- depths_daily %>%
  group_by(Date) %>%
  summarise(
    # Closest to 1 meter
    Depth_1m   = sensor_depth_m[which.min(abs(sensor_depth_m - 1))],
    Temp_1m    = Temp_C[which.min(abs(sensor_depth_m - 1))],
    # Max depth
    Depth_max  = max(sensor_depth_m),
    Temp_max   = Temp_C[which.max(sensor_depth_m)],
    .groups = "drop"
  ) |>
  mutate(depth_1m_check = 1 - Depth_1m) |>
  #Get strat metrics
  mutate(Diff_C_1_max = Temp_1m  - Temp_max,
         Diff_Dens_1_max = water.density(Temp_max) - water.density(Temp_1m))

## check how far off the 1m measurements are
summary(Dens_diff)

density_join <- Dens_diff |>
  select(Date, Diff_C_1_max, Diff_Dens_1_max)

density_join |>
  pivot_longer(-1) |>
  ggplot(aes(x = Date, y = value))+
  geom_point()+
  facet_wrap(~name, scales = "free_y", ncol = 1)+ theme_bw()



#### Join exo to density ----
Catwalk_df <- full_join(catwalk_daily, density_join,  by = "Date")



#### Get RHESSys data and format to bind ----
##read in RHESSys
workpath <- "C:/Users/dwh18/OneDrive/Desktop/R_Projects/RHESSys_development/ccr_rhessys/out/ccr_patch1500_cow1"  #ccr_patch1500_KEEP

output_grow <- read_delim(paste0(workpath, "/harvest1850_2026run_grow_basin.daily"),
                          delim = " ", col_names = T)

output_h2o <- read_delim(paste0(workpath, "/harvest1850_2026run_basin.daily"),
                         delim = " ", col_names = T)

# HPB area is 4.299446 km^2
# hpb_area_m2 <- 4.299446 * 1000000
ccr_area_m2 <- 45.83578 * 1000000


output_h2o_grow <- left_join(output_h2o, output_grow, by = c("day", "month", "year", "basinID")) |>
  mutate(date = ymd(paste(year, month, day, sep = "-"))) |>
  filter(date >= ymd("2021-04-01")) |>
  select(date, streamflow, streamflow_NO3, streamflow_DOC, lai.y) |>
  rename(lai = lai.y) |>
  #Q unit conversions
  mutate(streamflow_m_day = streamflow / 1000) |> #convert mm/day to m/day
  mutate(streamflow_m3_day = streamflow_m_day * ccr_area_m2) |> #convert m/day to m3/day
  #chem conversions from g/m2/day to mg/L
  mutate(DOC_mgL = streamflow_DOC / streamflow_m_day,
         NO3_mgL = streamflow_NO3 / streamflow_m_day)


rhessys_df <- output_h2o_grow |>
  rename(Date = date) |>
  select(Date, streamflow_m3_day, DOC_mgL) |> #streamflow_DOC
  rename(#RH_streamflow_DOC_gm2day = streamflow_DOC,
         RH_Q_m3day = streamflow_m3_day,
         RH_DOC_mgL = DOC_mgL)



#### Z score and export
datecheck <- seq(ymd("2021-08-19"), ymd("2026-04-01"), by = "day")

Catwalk_RH_df <- full_join(Catwalk_df, rhessys_df, by = "Date") |>
  filter(Date >= ymd("2021-08-19"),
         Date <= ymd("2026-04-01")) |>
  arrange(Date) |>
  mutate(fDOM_1m_lag1 = lag(fDOM_1_QSU_daily , 1)) |>
  mutate(
    Chla_1_ugL_7day = slider::slide_dbl(Chla_1_ugL_daily, mean, .before = 7, .after = 0, .complete = F),
    Chla_1_ugL_14day = slider::slide_dbl(Chla_1_ugL_daily, mean, .before = 14, .after = 0, .complete = F),
    RH_Q_m3day_7day = slider::slide_dbl(RH_Q_m3day, mean, .before = 7, .after = 0, .complete = F),
    RH_Q_m3day_14day = slider::slide_dbl(RH_Q_m3day, mean, .before = 14, .after = 0, .complete = F),
  )
  # # Z-score
  # mutate(across(
  #   .cols = !Date,
  #   .fns  = ~ scale(.x)[,1],
  #   .names = "{.col}_ZS"
  # ))


#### Check lags and coor matrix

## ACF
library(astsa)
astsa::acf2(Catwalk_RH_df$fDOM_1_QSU_daily, xlim=c(1,20), na.action = na.pass) # Plots the ACF of x for lags 1 to 19
pacf(Catwalk_RH_df$fDOM_1_QSU_daily, xlim = c(1,20), na.action = na.pass)


##cor matrix
library(corrplot)

df_corr1m <- Catwalk_RH_df |>
  select(-Date)
  select(-c(Date, Chla_1_ugL_7day, Chla_1_ugL_14day, RH_Q_m3day_7day, RH_Q_m3day_14day,
            Temp_1_C_daily, Diff_C_1_max))


cor_matrix <- cor(df_corr1m, use = "pairwise.complete.obs")

# Step 1: plot with all numbers in normal weight
corrplot(cor_matrix,
         method      = "color",
         type        = "upper",
         addCoef.col = "black",
         tl.col      = "black",
         tl.srt      = 45,
         number.font = 1,          # all normal weight first
         col         = colorRampPalette(c("#D73027", "#FFFFBF", "#1A9850"))(200))

# Step 2: overlay bold numbers only where |r| >= 0.5
bold_matrix <- cor_matrix
bold_matrix[abs(cor_matrix) < 0.5] <- NA  # hide the weak ones

corrplot(bold_matrix,
         method      = "color",
         type        = "upper",
         add         = TRUE,       # overlay on existing plot
         addCoef.col = "black",
         tl.pos      = "n",        # suppress repeated labels
         cl.pos      = "n",        # suppress repeated legend
         number.font = 2,          # bold
         col         = colorRampPalette(c("#D73027", "#FFFFBF", "#1A9850"))(200))






### Select data for export
export_df <- Catwalk_RH_df |>
  dplyr::select(Date, fDOM_1_QSU_daily, fDOM_1m_lag1,
                Diff_Dens_1_max, DOsat_1_pct_daily, Chla_1_ugL_daily,
                RH_Q_m3day, RH_DOC_mgL)


summary(export_df)


#write.csv(export_df, "./CCR_AR_Modeling/Data/Daily_catwalk_RH_2021_2026.csv", row.names = F)

##make plots
driverdf <- read_csv("./CCR_AR_Modeling/Data/Daily_catwalk_RH_2021_2026.csv")

driverplot <- driverdf |>
  select(-fDOM_1m_lag1) |>
  mutate(RH_Q_cms = RH_Q_m3day / 86400) |> select(-RH_Q_m3day) |>
  # filter(Date > ymd("2024-04-01")) |>
  filter(Date > ymd("2021-08-19"), Date < ymd("2026-01-31")) |>
  pivot_longer(-1) |>
  ggplot(aes(x = Date, y = value))+
  geom_point()+ facet_wrap(~name, scales = "free_y")+
  geom_vline(xintercept = ymd("2024-03-01"), linetype = 2, linewidth = 1.2, color = "red")+
  theme_bw()

driverplot
plotly::ggplotly(driverplot)

driverdf |>
  # filter(Date > ymd("2024-04-01")) |>
  filter(Date > ymd("2021-08-19"), Date < ymd("2026-01-31")) |>
  ggplot(aes(x = Date, y = fDOM_1_QSU_daily))+
  geom_point()+ labs(x = "Date", y = "fDOM (QSU)")+
  geom_vline(xintercept = ymd("2024-03-01"), linetype = 2, linewidth = 1.2, color = "red")+
  theme_bw()


##monthly hydro
driverdf |>
  mutate(year = year(Date),
         month_year = floor_date(Date, "month")) |>
  filter(year > 2023) |>
  ggplot(aes(x = factor(month_year), y = (RH_Q_m3day/86400))) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.size = 1) +
  geom_jitter(width = 0.2, size = 1.2, alpha = 0.5, color = "gray30") +
  scale_x_discrete(labels = function(x) format(as.Date(x), "%b %Y")) +
  labs(x = NULL, y = "RH Discharge (m³/sec)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        text = element_text(size = 13))


##USGS data
USGS_daily <- dataRetrieval::readNWISdv(
  siteNumbers = "02055100",
  parameterCd = "00060",          # discharge in cfs
  startDate = "2020-01-01",
  endDate   = "2026-04-01"
) |>  dataRetrieval::renameNWISColumns()


#Monthly usgs plots
USGS_daily |>
  mutate(year = year(Date),
         month_year = floor_date(Date, "month")) |>
  filter(year > 2023) |>
  ggplot(aes(x = factor(month_year), y = (Flow/35.3))) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.size = 1) +
  geom_jitter(width = 0.2, size = 1.2, alpha = 0.5, color = "gray30") +
  scale_x_discrete(labels = function(x) format(as.Date(x), "%b %Y")) +
  scale_y_log10()+
  labs(x = NULL, y = "USGS Tinker Discharge (cms)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        text = element_text(size = 13))


#get percentiles for days based on USGS for forecast eval timeframe
head(USGS_daily)

USGS_daily |>
  filter(Date >= ymd("2024-01-01"), Date <= ymd("2026-01-31")) |>
  ggplot(aes(x = Date, y = Flow))+geom_point()

flow_percentiles <- USGS_daily |>
  filter(Date >= ymd("2024-01-01"), Date <= ymd("2026-01-31")) |>
  filter(!is.na(Flow)) |>
  mutate(flow_percentile = percent_rank(Flow) * 100)

# Step 2: flag top and bottom 10th percentile dates
# flow_flags <- flow_percentiles |>
#   mutate(flow_class = case_when(
#     flow_percentile < 10  ~ "Low flow",
#     flow_percentile > 90  ~ "High flow",
#     TRUE                   ~ "Normal"
#   )) |>
#   select(Date, Flow, flow_percentile, flow_class)

flow_flags <- flow_percentiles |>
  mutate(
    decile     = ntile(Flow, 10),
    flow_class = case_when(
      decile == 1  ~ "Low flow",
      decile == 10 ~ "High flow",
      TRUE         ~ "Normal"
    )
  ) |>
  select(Date, Flow, flow_percentile, decile, flow_class)

# Step 3: join to your other data frame and filter extreme days
# other_df_filtered <- other_df |>
#   left_join(flow_flags, by = "Date") |>
#   filter(flow_class %in% c("Low flow", "High flow"))



