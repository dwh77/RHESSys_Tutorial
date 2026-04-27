## DWH additon to get climate data preprocessed

##first clean up airport csv

library(tidyverse)

roa <- read.csv("./Data_PreProcessing/Data_sources/NOAA_ROA/4043554_NOAA_ROAdaily_1jan1948_2jun2025.csv")
##units are in inches and F, need to convert to meters and C
df <- roa |>
  dplyr::select(DATE, PRCP, TMIN, TMAX) |>
  mutate(PRCP = PRCP/39.37,
         TMIN = ((TMIN-32) * (5/9)),
         TMAX = ((TMAX-32) * (5/9))
  )

##Look at roa climate data over time
df |> pivot_longer(-1) |>
  ggplot(aes(x = as.Date(DATE), y = value))+
  geom_point()+ facet_wrap(~name, scales = "free_y")

yearly <- df |>
  mutate(date = as.Date(DATE),
         year = year(date)) |>
  # mutate(wet = PRCP > 0) |>
  filter(date < ymd("2025-01-01")) |>
  group_by(year) |>
  summarise(Tmax_mean = mean(TMAX, na.rm = T),
            Tmin_mean = mean(TMAX, na.rm = T),
            Rain_sum = sum(PRCP, na.rm = T),
            Rain_days = sum(PRCP > 0, na.rm = T))


yearly |> pivot_longer(-1) |>
  ggplot(aes(x = year, y = value))+
  geom_point()+ geom_smooth() + facet_wrap(~name, scales = "free_y")






#Function to structure to climate data in RHESSys format
#Note: the 1 is a place holder for hour but data is daily sequence. see 'Format for Time Series Input Files' from 'Climate-Inputs' link below

write_variable_file <- function(df, var_name, file_name) {
  # Ensure DATE is in Date format
  df$DATE <- as.Date(df$DATE)

  # Extract first date components without leading zeros
  first_date <- df$DATE[1]
  header <- paste(format(first_date, "%Y"),
                  as.integer(format(first_date, "%m")),
                  as.integer(format(first_date, "%d")),
                  "1")

  # Extract variable values only
  values <- df[[var_name]]

  # Format values as character
  value_lines <- as.character(values)

  # Write to file
  writeLines(c(header, value_lines), con = file_name)
}


# Write climate files from precip, tmin, and tmax
write_variable_file(df, "PRCP", "ClimateFiles/clim/ccr_daily.rain")
write_variable_file(df, "TMIN", "ClimateFiles/clim/ccr_daily.tmin")
write_variable_file(df, "TMAX", "ClimateFiles/clim/ccr_daily.tmax")



### Extend climate data for spinup by 40 years
spinup <- df |>
  filter(DATE < ymd("1988-01-01")) |>
  mutate(year = year(DATE),
         month = month(DATE),
         day = day(DATE),
         year_new = year-40) |>
  mutate(Date_new = ymd(paste(year_new, month, day, sep = "-"))) |>
  select(Date_new, PRCP, TMIN, TMAX) |>
  rename(DATE = Date_new)


climate_df_spinup <- rbind(spinup, df)

# Write climate files from precip, tmin, and tmax
write_variable_file(climate_df_spinup, "PRCP", "ClimateFiles/clim_spinup_40years/ccr_daily.rain")
write_variable_file(climate_df_spinup, "TMIN", "ClimateFiles/clim_spinup_40years/ccr_daily.tmin")
write_variable_file(climate_df_spinup, "TMAX", "ClimateFiles/clim_spinup_40years/ccr_daily.tmax")



### Extend climate data for spinup by 200 years
spinup_200_1872_1947 <- df |>
  filter(DATE < ymd("2024-01-01")) |>
  mutate(year = year(DATE),
         year_new = year-76) |>
  mutate(Date_new = ymd(paste(year_new, month(DATE), day(DATE), sep = "-"))) |>
  filter(!is.na(Date_new)) |>  #1900 was not a leap year so tossing that row
  select(Date_new, PRCP, TMIN, TMAX) |>
  rename(DATE = Date_new)


spinup_200_1796_1871 <- df |>
  filter(DATE < ymd("2024-01-01")) |>
  mutate(year = year(DATE),
         year_new = year-152) |>
  mutate(Date_new = ymd(paste(year_new, month(DATE), day(DATE), sep = "-"))) |>
  filter(!is.na(Date_new)) |>  #1800 was not a leap year so tossing that row
  select(Date_new, PRCP, TMIN, TMAX) |>
  rename(DATE = Date_new)

spinup_200_1720_1795 <- df |>
  filter(DATE < ymd("2024-01-01")) |>
  mutate(year = year(DATE),
         year_new = year-228) |>
  mutate(Date_new = ymd(paste(year_new, month(DATE), day(DATE), sep = "-"))) |>
  select(Date_new, PRCP, TMIN, TMAX) |>
  rename(DATE = Date_new)


spinup_200 <- rbind(spinup_200_1720_1795, spinup_200_1796_1871, spinup_200_1872_1947)

#check right # of days
qqq <- seq(ymd("1720-01-01"), ymd("1947-12-31"), by = "day")

#bind all together
spinup_200_with_observed <- rbind(spinup_200, df)

#write long spinup files
# Write climate files from precip, tmin, and tmax
write_variable_file(spinup_200_with_observed, "PRCP", "ClimateFiles/clim_spinup_200years/ccr_daily.rain")
write_variable_file(spinup_200_with_observed, "TMIN", "ClimateFiles/clim_spinup_200years/ccr_daily.tmin")
write_variable_file(spinup_200_with_observed, "TMAX", "ClimateFiles/clim_spinup_200years/ccr_daily.tmax")






# from https://github.com/RHESSys/RHESSys/wiki/Climate-Inputs
# code for reading in RHESSys formatting climate data: https://github.com/RHESSys/RHESSysIOinR/wiki/Climate


## set up climate base file for CCR
library(RHESSysIOinR)

#lai changed from 3.5 to 2.0 based on: https://doi.org/10.3390/f9010026

ccr_base <- IOin_clim(
  base_station_id = 101,
  x_coordinate = 100.0,
  y_coordinate = 100.0,
  z_coordinate = 346.7,
  effective_lai = 2.0,
  screen_height = 2,
  daily_prefix = "/clim/ccr_daily")


getwd()

write.table(ccr_base, file="ClimateFiles/clim/ccr_base", row.names=F, col.names=F, quote=F)


