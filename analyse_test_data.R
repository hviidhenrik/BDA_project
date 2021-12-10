# init
rm(list=ls()); setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(tidyverse)
library(rstan)
library(bayesplot)

Sys.setlocale("LC_ALL","English")
rstan_options(auto_write = TRUE)
theme_set(bayesplot::theme_default(base_family = "sans"))

# load data
data_kilpis <- read_csv2("data//test-data.csv")

data_grp <-list(N = 3*nrow(data_kilpis),
                K = 3,
                x = rep(1:3, nrow(data_kilpis)),
                y = c(t(data_kilpis[,2:4]))
                )

fit_grp <- stan(file = "grp_aov.stan", data = data_grp, seed = 48927)

monitor(fit_grp)
check_hmc_diagnostics(fit_grp)

draws_grp <- extract(fit_grp, permuted = T)
temps <- data.frame(draws_grp$mu) %>%
  setNames(c('June','July','August'))
mcmc_areas(temps) + xlab('Temperature')