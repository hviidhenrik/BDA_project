# init
rm(list=ls()); setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(lubridate)


df <- read_csv("C:\\datasets\\SSV cooling water pumps\\data_cwp_pump_10_large.csv")
df <- df[,-c(3, 16)]
df <- drop_na(df)

hist(df$rotation, breaks=30)
summary(df$rotation)

# group on rotation
df_grouped_rpm <- df %>% mutate(rpm_rounded = as.character(round(rotation))) %>% group_by(rpm_rounded) %>% 
    select(-timelocal) %>% mutate(n=n()) %>% summarise(across(everything(), mean)) %>% 
  mutate(rpm_rounded = as.numeric(rpm_rounded))

# filter the df and only keep rows with rotation in a small interval
df_filtered <- df %>% filter(rotation > 250 & rotation < 270)

# select important measurements and add a date / month column
df_filtered <- df_filtered %>% select(c(timelocal, flow, rotation, effect_pump_10,
                                        vibr_bear_as_x, vibr_bear_as_y, 
                                        vibr_bear_bs_x, vibr_bear_bs_y,
                                        vibr_motor_x, vibr_motor_y,
                                        temp_winding_max)) %>% 
  mutate(month = format(timelocal, format="%b"), 
         month_indicator = as.numeric(as.factor(format(timelocal, format="%m")))) %>%
  select(-timelocal)

df_filtered %>% select(c(month, month_indicator)) %>% unique()

# group data by month to look for gross outliers based on min and max
df_filtered_outliers <- df_filtered %>% group_by(month) %>% 
  summarise(across(everything(), list(min = min, max = max)), n_month = n())

# group data by month and summarise mean and sd 
df_filtered_grouped_month  <- df_filtered %>% group_by(month) %>% 
  summarise(across(everything(), list(mean = mean, sd = sd)), n_month = n()) %>%
  relocate(n_month, .after=month)



## statistics and plots

# plot rotation vs count
df_grouped_rpm %>% ggplot() + geom_line(aes(rpm_rounded, n))

# plot rotation vs vibrations
df_grouped_rpm %>% select(c(rpm_rounded, starts_with("vibr_bear"))) %>% 
  pivot_longer(cols = starts_with("vibr_bear"), names_to = "location", values_to = "velocity") %>% 
  ggplot(aes(rpm_rounded, velocity, color=location)) + 
  geom_line(size=1.5) +
  labs(title="Vibrations vs pump rotation speed",
       x="Rotation [rpm]",
       y="Vibration [mm/s]",
       color="Location")

# plot vibrations for each months as box plots
df_filtered %>% ggplot() + 
  geom_violin(aes(month, vibr_bear_bs_x))

# save data files
df_filtered %>% write_csv("data//CWP10_2020_months.csv")
df_filtered_grouped_month %>% write_csv("data//CWP10_2020_aggregated.csv")
