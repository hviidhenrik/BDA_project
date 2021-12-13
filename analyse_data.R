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
data <- list(N = dim(df)[1], 
                   K = length(unique(df$month)),
                   N_ypred = max(sub_sample$counts),
                   x = df$month_indicator,
                   y = df %>% select(feature_to_analyse) %>% pull
                   )

### anova - common mean and variance
fit_anova <- stan(file = "common_variance.stan", 
                  data = data, seed = 1001, chains=1)

# check convergence
check_hmc_diagnostics(fit_anova)
summary_anova <- monitor(fit_anova)
print(tail(summary_anova, 1))

# plotting
plot_areas(fit_anova)


# predictive checks
plot_predictive_diagnostics(df, fit_anova, "oct", reps=20, title="March")

# probabilities
draws <- extract(fit_anova, permuted = T)
vibrations <- data.frame(draws$mu) %>% setNames(c(month_names))

paste('P(May > December) = ', mean(vibrations$May > vibrations$Dec))

### hierarchical prior on the mean, common variance
fit_hier <- stan(file = "common_variance_hierarchical_prior.stan", 
                 data = data, seed = 1001, chains=1)

# check convergence
check_hmc_diagnostics(fit_hier)
summary_hier <- monitor(fit_hier)
print(tail(summary_hier, 1))

# plotting
plot_areas(fit_hier)


# predictive checks
plot_predictive_diagnostics(df, fit_hier, "oct", reps=20, title="March")

# model comparison
# LOO
m1 <- loo(fit_anova)
m2 <- loo(fit_hier)
loo_compare(m1, m2)
                                     