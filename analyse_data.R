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

any(summary_m1$Rhat > 1.04)
max(summary_m1$Rhat)

print(tail(summary_m1, 2))

# plotting
draws <- extract(fit_m1, permuted=T)
vibrations <- data.frame(draws$mu) %>% rename(mu = draws.mu) # %>% cbind(draws$sigma)
mcmc_areas(vibrations) + xlab('Vibration\nvelocity [mm/s]') + ylab("") + ggtitle("Posterior draws")
ggsave("figures\\posterior_pooled.png")


# predictive checks
p1 <- plot_predictive_diagnostics(df, fit_m1, "mar", reps=20, title="Common mean and variance (March)")
ggsave("figures\\ppc_pooled.png", plot = p1)




#####################################################
### model 2 - separate mean, common variance
fit_m2 <- stan(file = "model_common_variance.stan", 
                  data = data, seed = 1001, chains=1)

# check convergence
check_hmc_diagnostics(fit_m2)
summary_m2 <- monitor(fit_m2)
any(summary_m2$Rhat > 1.04)
max(summary_m2$Rhat)
print(tail(summary_m2, 2))

# plotting
plot_areas(fit_m2)
ggsave("figures\\posterior_common_variance.png", width = 6, height = 3.57)


# predictive checks
p2 <- plot_predictive_diagnostics(df, fit_m2, "mar", reps=20, title="Monthly mean and common variance (March)")
ggsave("figures\\ppc_common_variance.png", plot = p2)


# probabilities
draws <- extract(fit_m2, permuted = T)
vibrations <- data.frame(draws$mu) %>% setNames(c(month_names))

paste('P(May > December) = ', mean(vibrations$May > vibrations$Dec))

round(c(mean(vibrations$Dec > vibrations$Mar),
        mean(vibrations$Dec > vibrations$Apr),
        mean(vibrations$Dec > vibrations$May),
        mean(vibrations$Dec > vibrations$Sep),
        mean(vibrations$Dec > vibrations$Oct),
        mean(vibrations$Dec > vibrations$Nov)),2)

#####################################################
### model 3 - separate mean and variance
fit_m3 <- stan(file = "model_separate.stan", 
                 data = data, seed = 1001, chains=1)

# check convergence
check_hmc_diagnostics(fit_m3)
summary_m3 <- monitor(fit_m3)
any(summary_m3$Rhat > 1.04)
max(summary_m3$Rhat)
print(tail(summary_m3, 1))

# plotting
plot_areas(fit_m3)
ggsave("figures\\posterior_separate.png")


# predictive checks
p3 <- plot_predictive_diagnostics(df, fit_m3, "mar", reps=100, title="Monthly mean and variance (March)")
ggsave("figures\\ppc_separate.png", plot = p3)


#####################################################
### model 4 - hierarchical prior on the mean and variance
fit_m4 <- stan(file = "model_hierarchical.stan", 
               data = data, seed = 1001, chains=1)

# check convergence
check_hmc_diagnostics(fit_m4)
summary_m4 <- monitor(fit_m4)
any(summary_m4$Rhat > 1.04)
max(summary_m4$Rhat)
print(tail(summary_m4, 1))

# plotting
plot_areas(fit_m4)
ggsave("figures\\posterior_hierarchical.png")


# predictive checks
p4 <- plot_predictive_diagnostics(df, fit_m4, "mar", reps=100, title="Hierarchical (March)")
ggsave("figures\\ppc_hierarchical.png", plot = p4)


##
p_all <- grid.arrange(p1, p2, p3, p4, nrow = 2)
ggsave("figures\\ppc_all.png", plot = p_all, scale = 3)


######################################################
# model comparison
# LOO
m1 <- loo(fit_m1)
m2 <- loo(fit_m2)
m3 <- loo(fit_m3)
m4 <- loo(fit_m4)

p1 <- plot_loo_diagnostics(m1, title="Common mean and variance")
ggsave("figures\\loo_diag_pooled.png")
p2 <- plot_loo_diagnostics(m2, title="Monthly mean and common variance")
ggsave("figures\\loo_diag_common_variance.png")
p3 <- plot_loo_diagnostics(m3, title="Monthly mean and variance")
ggsave("figures\\loo_diag_separate.png")
p4 <- plot_loo_diagnostics(m4, title="Hierarchical")
ggsave("figures\\loo_diag_hierarchical.png")

p_all <- grid.arrange(p1, p2, p3, p4, nrow = 2)
ggsave("figures\\loo_diag_all.png", plot = p_all, scale = 2)


loo_compare(m1, m2, m3, m4)


######################################################
## prior sensitivity analysis

## lognormal prior
fit_m2_lognormal <- stan(file = "model_common_variance_lognormal.stan", 
                         data = data, seed = 1001, chains=1)

# plotting
plot_areas(fit_m2_lognormal)
ggsave("figures\\posterior_lognormal.png")


# predictive checks
p1 <- plot_predictive_diagnostics(df, fit_m2_lognormal, "mar", reps=100, title="Log normal prior (March)")
ggsave("figures\\ppc_lognormal.png", plot = p1)

m2_lognormal <- loo(fit_m2_lognormal)
(p2 <- plot_loo_diagnostics(m2_lognormal, title="Log normal prior"))


## chi square prior
fit_m2_chisq <- stan(file = "model_common_variance_chisq.stan", 
                     data = data, seed = 1001, chains=1)

# plotting
plot_areas(fit_m2_chisq)
ggsave("figures\\posterior_chisq.png")


# predictive checks
p3 <- plot_predictive_diagnostics(df, fit_m2_chisq, "mar", reps=100, title="Chi square prior (March)")
ggsave("figures\\ppc_chisq.png", plot = p1)

m2_chisq <- loo(fit_m2_chisq)
(p4 <- plot_loo_diagnostics(m2_chisq, title="Chi square prior"))


loo_compare(m2, m2_lognormal, m2_chisq)
