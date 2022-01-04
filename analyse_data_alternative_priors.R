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

sub_sample <- make_random_subset2(df_original, feature_name = feature_to_analyse,
                                 n_samples_per_month = 14)

df <- sub_sample$df
print(sub_sample$df_summary)


# prepare the data for stan
data <- list(N = dim(df)[1], 
                   K = length(unique(df$month)),
                   N_ypred = max(sub_sample$counts),
                   x = df$month_indicator,
                   y = df %>% select(feature_to_analyse) %>% pull
                   )

#####################################################
### model 1 - common mean and variance
fit_m1 <- stan(file = "model_pooled.stan", 
               data = data, seed = 1001, chains=1)

# check convergence
check_hmc_diagnostics(fit_m1)
summary_m1 <- monitor(fit_m1, permuted=T)
print(tail(summary_m1, 2))

# plotting
draws <- extract(fit_m1, permuted=T)
vibrations <- data.frame(draws$mu) %>% rename(mu = draws.mu) # %>% cbind(draws$sigma)
mcmc_areas(vibrations) + xlab('Vibration\nvelocity [mm/s]') + ylab("") + ggtitle("Posterior draws")
ggsave("figures\\posterior_pooled.png")


# predictive checks
p <- plot_predictive_diagnostics(df, fit_m1, "mar", reps=20, title="March")
ggsave("figures\\ppc_pooled.png", plot = p)




#####################################################
### model 2 - separate mean, common variance
fit_m2 <- stan(file = "model_common_variance.stan", 
                  data = data, seed = 1001, chains=1)

# check convergence
check_hmc_diagnostics(fit_m2)
summary_m2 <- monitor(fit_m2)
print(tail(summary_m2, 2))

# plotting
plot_areas(fit_m2)
ggsave("figures\\posterior_common_variance.png")



# predictive checks
p <- plot_predictive_diagnostics(df, fit_m2, "mar", reps=20, title="March")
ggsave("figures\\ppc_common_variance.png", plot = p)


# probabilities
draws <- extract(fit_m2, permuted = T)
vibrations <- data.frame(draws$mu) %>% setNames(c(month_names))

paste('P(May > December) = ', mean(vibrations$May > vibrations$Dec))



#####################################################
### hierarchical prior on the mean, common variance
fit_m3 <- stan(file = "model_separate.stan", 
                 data = data, seed = 1001, chains=1)

# check convergence
check_hmc_diagnostics(fit_m3)
summary_m3 <- monitor(fit_m3)
print(tail(summary_m3, 1))

# plotting
plot_areas(fit_m3)
ggsave("figures\\posterior_separate.png")


# predictive checks
p <- plot_predictive_diagnostics(df, fit_m3, "mar", reps=100, title="March")
ggsave("figures\\ppc_separate.png", plot = p)



######################################################
# model comparison
# LOO
m1 <- loo(fit_m1)
m2 <- loo(fit_m2)
m3 <- loo(fit_m3)

plot_loo_diagnostics(m1)
ggsave("figures\\loo_diag_pooled.png")
plot_loo_diagnostics(m2)
ggsave("figures\\loo_diag_common_variance.png")
plot_loo_diagnostics(m3)
ggsave("figures\\loo_diag_separate.png")

loo_compare(m1, m2, m3)
