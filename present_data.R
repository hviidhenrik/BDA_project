# init
rm(list=ls()); setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(rstan)
library(bayesplot)
library(loo)
library(gridExtra)

Sys.setlocale("LC_ALL","English")
rstan_options(auto_write = TRUE)
month_names <- c("Mar", "Apr", "May", "Sep", "Oct", "Nov", "Dec")

source("helpers.R")

set.seed(1234)

# load data
df_original <- read_csv("data//CWP10_2020_months.csv")

df_original %>% ggplot() + geom_line(aes(timelocal, vibr_bear_as_x))
