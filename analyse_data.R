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

feature_to_analyse <- "vibr_bear_as_x"

sub_sample <- make_random_subset(df_original, feature_name = feature_to_analyse,
                                 n_samples = 4000)
df <- sub_sample$df
print(sub_sample$df_summary)


# prepare the data for stan
data_anova <- list(N = dim(df)[1], 
                   K = length(unique(df$month)),
                   N_ypred = max(sub_sample$counts),
                   x = df$month_indicator,
                   y = df %>% select(feature_to_analyse) %>% pull
                   )

# anova - common mean and variance
fit_anova <- stan(file = "common_variance.stan", 
                  data = data_anova, seed = 1001, chains=1)

# check convergence
# monitor(fit_anova)
check_hmc_diagnostics(fit_anova)

# plotting
plot_areas(fit_anova)


# predictive checks
plot_predictive_diagnostics(df, fit_anova, "oct", reps=20, title="March")

# TODO: make function to check how many y's are outside the predictive distributions
#       for each y. For 80% prediction intervals, we would expect 20% to be outside

# probabilities
draws <- extract(fit_anova, permuted = T)
vibrations <- data.frame(draws$mu) %>% setNames(c(month_names))

paste('P(May > December) = ', mean(vibrations$May > vibrations$Dec))

# hierarchical prior on the mean, common variance


# model comparison
# LOO
# m1 <- loo(fit_anova)
# loo_compare(m1, m1)
                                     