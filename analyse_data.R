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

# load data
df_original <- read_csv("data//CWP10_2020_months.csv")

while(TRUE){
  # sample subset and sort on month
  df <- df_original %>% slice_sample(n=1200)
  df <- df[order(match(df$month, month.abb)), ]
  df_sample_summary <- df %>% group_by(month) %>% 
    summarise(across(vibr_bear_as_x, 
                                              list(mean = mean, sd = sd)), 
                                                 n=n()) 
  counts <- df_sample_summary %>% select(n) %>% pull()
  if(length(counts) == 7 & all(counts > 10))
    break
}

print(df_sample_summary)

# prepare the data for stan
data_anova <- list(N = dim(df)[1], 
                   K = length(unique(df$month)),
                   N_ypred = max(counts),
                   x = df$month_indicator,
                   y = df$vibr_bear_as_x
                   
                   )

# anova - common mean and variance
fit_anova <- stan(file = "common_variance.stan", 
                  data = data_anova, seed = 100, chains=1)

# check convergence
# monitor(fit_anova)
# check_hmc_diagnostics(fit_anova)

# plotting
# plot_areas(fit_anova)



# predictive checks
plot_predictive_diagnostics(df, fit_anova, "may", reps=20, title="March")

# LOO
m1 <- loo(fit_anova)
loo_compare(m1, m1)



# probabilities
paste('P(March > April) = ', mean(vibrations$March > vibrations$April))

# hierarchical prior on the mean, common variance

                                     