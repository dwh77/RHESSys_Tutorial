#### CCR DOC ~ fDOM

# packages 
library(tidyverse)
library(ggpmisc)


# Data from EDI
catwalk <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/1069/4/42e6d8bb3d379d40a4a4fb566d4ff36e" )

chem <- read.csv( "https://pasta.lternet.edu/package/data/eml/edi/199/13/3f09a3d23b7b5dd32ed7d28e9bc1b081" )


# Filter chem
exo_chem <- chem |> 
  mutate(Date = as.Date(DateTime)) |> 
  filter(Date > ymd("2021-04-08")) |> 
  filter(Reservoir == "CCR",
         Site %in% c(50,51)) |> 
  select(Reservoir, Site, Date, Depth_m, Rep, DOC_mgL) |> 
  group_by(Reservoir, Site, Date, Depth_m) |> 
  summarise(across(c(DOC_mgL), mean, na.rm = TRUE)) |> 
  filter(Depth_m %in% c(1.5,9)) |> 
  pivot_wider(                                        
    names_from  = Depth_m,
    values_from = DOC_mgL,
    names_prefix = "DOC_",
    names_glue  = "DOC_{Depth_m}m"
  )


#filter sensors 
p <- -0.01

ccr_fdom <- catwalk |> 
  mutate(fdom1_TC = EXOfDOM_QSU_1/(1 + (p*(EXOTemp_C_1 - 20)) ) ,
         fdom9_TC = EXOfDOM_QSU_9/(1 + (p*(EXOTemp_C_9 - 20)) )  
         ) |> 
  mutate(Date = as.Date(DateTime)) |> 
  group_by(Date) |> 
  summarise(fDOM_1_QSU_daily = mean(fdom1_TC, na.rm = T),
            fDOM_9_QSU_daily = mean(fdom9_TC, na.rm = T)) 


ccr_fdom |> 
  pivot_longer(-1) |> 
  ggplot(aes(x = Date, y = value, col = name))+
  geom_point()


#### Join data together

df <- full_join(ccr_fdom, exo_chem, by = "Date")


## look at 1.5m
df |> 
  mutate(yday = yday(Date)) |> 
  # filter(DOC_1.5m > 2.5) |> 
  ggplot(aes(x = fDOM_1_QSU_daily, y = DOC_1.5m, col = (yday)))+
  geom_point()+
  stat_poly_line(method = "lm", linewidth = 2) +
  stat_poly_eq(formula = y ~ x, label.x = "left", label.y = "top", parse = TRUE,
               inherit.aes = FALSE, aes(x = fDOM_1_QSU_daily, y = DOC_1.5m,
                                        label = paste(..adj.rr.label.., ..p.value.label.., sep = "~~~"), size = 3)  ) +
  theme_bw()


## look at 9m
df |> 
  mutate(yday = yday(Date)) |> 
  # filter(DOC_9m > 2) |>
  ggplot(aes(x = fDOM_1_QSU_daily, y = DOC_9m, col = (yday)))+
  geom_point()+
  stat_poly_line(method = "lm", linewidth = 2) +
  stat_poly_eq(formula = y ~ x, label.x = "left", label.y = "top", parse = TRUE,
               inherit.aes = FALSE, aes(x = fDOM_1_QSU_daily, y = DOC_9m,
                                        label = paste(..adj.rr.label.., ..p.value.label.., sep = "~~~"), size = 3)  ) +
  theme_bw()



## look at both together.. Doesn't work
# df_long <- df |>
#   pivot_longer( cols = c(fDOM_1_QSU_daily, fDOM_9_QSU_daily, DOC_1.5m, DOC_9m),
#     names_to  = "variable", values_to = "value" ) |>
#   mutate(Depth_m = case_when(
#       grepl("_1_|_1\\.", variable) ~ 1.5,  # treat 1 and 1.5 as same
#       grepl("_9",        variable) ~ 9  ),
#     var_name = case_when(    grepl("fDOM", variable) ~ "fDOM",
#       grepl("DOC",  variable) ~ "DOC" ) ) |>
#   select(-variable) |>
#   pivot_wider(names_from  = var_name,   values_from = value )
# 
# df_long |> 
#   mutate(yday = yday(Date)) |> 
#   ggplot(aes(x = fDOM, y = DOC, col = (yday)))+
#   geom_point()+
#   stat_poly_line(method = "lm", linewidth = 2) +
#   stat_poly_eq(formula = y ~ x, label.x = "left", label.y = "top", parse = TRUE,
#                inherit.aes = FALSE, aes(x = fDOM, y = DOC,
#                                         label = paste(..adj.rr.label.., ..p.value.label.., sep = "~~~"), size = 3)  ) +
#   theme_bw()



##### LOOK at chem across stream sites ----
chem_stream <- chem |> 
  mutate(Date = as.Date(DateTime)) |> 
  filter(Date > ymd("2020-01-01")) |> 
  filter(Reservoir == "CCR", Site >= 100, Site != 110) |> 
  mutate(Date = as.Date(DateTime),
         Site = case_when(
           Site %in% c(100, 101) ~ 101,
           Site %in% c(300, 301) ~ 301,
           Site %in% c(500, 501) ~ 500,
           .default = Site
         ) ) |> 
  select(Reservoir, Site, Date, Depth_m, Rep, DOC_mgL, DN_mgL, NO3NO2_ugL, NH4_ugL, SRP_ugL) |> 
  group_by(Reservoir, Site, Date, Depth_m) |> 
  summarise(across(c(DOC_mgL, DN_mgL, NO3NO2_ugL, NH4_ugL, SRP_ugL), mean, na.rm = TRUE))

#plots
chem_stream |> 
  ggplot(aes(x = Date, y = DOC_mgL))+
  geom_point()+
  facet_wrap(~Site)

chem_stream |> 
  filter(Site == 201) |> 
  ggplot(aes(x = Date, y = DOC_mgL))+
  geom_point()+
  facet_wrap(~Site)




#### Look at flowmate Q across stream sites ----

flowmate_edi <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/454/9/0e7fe16623a1ad2a67774c23ce8a29d8")

ccrQ <- flowmate_edi |> 
  filter(Flow_cms < 1,
         Site != 400.1) |> 
  filter(Reservoir == "CCR") |> 
  mutate(Date = as.Date(DateTime),
    Site = case_when(
      Site %in% c(100, 101) ~ 101,
      Site %in% c(300, 301) ~ 301,
      Site %in% c(500, 501) ~ 500,
      .default = Site
    ) ) 
  
#plot
ccrQ |> 
  ggplot(aes(x = Date, y = Flow_cms)) +
  geom_point()+ facet_wrap(~Site)+
  scale_y_log10()


ccrQ |> 
  ggplot(aes(x = Date, y = Flow_cms, col = as.factor(Site))) +
  geom_point()+ geom_line()


ccrQ |> 
  filter(Site %in% c(101,201,301)) |> 
  ggplot(aes(x = Date, y = Flow_cms, col = as.factor(Site))) +
  geom_point()



#### C~Q for CCR stream ----
head(chem_stream)
head(ccrQ)




