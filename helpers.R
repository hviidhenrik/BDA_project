## Helper functions for BDA project

month_names <- c("Mar", "Apr", "May", "Sep", "Oct", "Nov", "Dec")

plot_areas <- function(fitted_model, title="Posterior mean draws"){
  draws <- extract(fitted_model, permuted=T)
  df <- data.frame(draws$mu) # %>% cbind(draws$sigma)
  vibrations <- df %>% setNames(c(month_names))
  mcmc_areas(vibrations) + xlab('Vibration\nvelocity [mm/s]') + ggtitle(title)
}

get_y_and_yrep <- function(df, fitted_model, month_abbr="mar", reps=20){
  yrep_month  <- paste0("ypred_", str_to_lower(month_abbr))
  filter_month <- str_to_title(month_abbr)
  y <- df %>% filter(month==filter_month) %>% 
    select(vibr_bear_as_x) %>% pull
  yrep_matrix <- as.matrix(fitted_model, pars=yrep_month)
  yrep <- yrep_matrix # [sample.int(dim(yrep_matrix)[1], size=length(y)),] %>% t
  yrep <- yrep[1:reps, sample.int(dim(yrep_matrix)[2], size=length(y))]
  return(list(y=y, yrep=yrep))
}

plot_predictive_diagnostics <- function(df, fitted_model, month="mar", reps=20, 
                                        title="March"){
  ys <- get_y_and_yrep(df, fitted_model, month_abbr=month, reps=reps)
  p1 <- ys$y %>% ppc_dens_overlay(yrep=ys$yrep) + ggtitle(title)
  p2 <- ys$y %>% ppc_intervals(yrep=ys$yrep)
  p3 <- ys$y %>% ppc_ecdf_overlay(yrep=ys$yrep)
  grid.arrange(p1, p2, p3, nrow = 3)
}

make_random_subset <- function(df_original, feature_name = "vibr_bear_as_x",
                               n_samples=1200){
  while(TRUE){
    # sample subset and sort on month
    df <- df_original %>% filter(month == "Mar")
    df <- df_original %>% filter(month != "Mar") %>% 
      slice_sample(n=n_samples) %>% rbind(df)
    df <- df[order(match(df$month, month.abb)), ]
    df_sample_summary <- df %>% group_by(month) %>% 
      summarise(across(all_of(feature_name), list(mean = mean, sd = sd)), n=n()) 
    counts <- df_sample_summary %>% select(n) %>% pull()
    if(length(counts) == 7 & all(counts > 10))
      break
  }
  return(list(df=df, df_summary=df_sample_summary, counts=counts))
}


make_random_subset2 <- function(df_original, feature_name = "vibr_bear_as_x",
                                n_samples_per_month=1200){
  max_n <- df_original %>% group_by(month) %>% summarise(n=n()) %>% pull %>% min()
  n_samples_per_month <- ifelse(n_samples_per_month > max_n, max_n, n_samples_per_month)
  # sample subset and sort on month
  df <- df_original %>% filter(month == "Mar") %>% slice_sample(n=n_samples_per_month)
  for(i in 2:7){
    df <- df_original %>% filter(month == month_names[i]) %>% 
      slice_sample(n=n_samples_per_month) %>% rbind(df)
  }
  df <- df[order(match(df$month, month.abb)), ]
  df_sample_summary <- df %>% group_by(month) %>% 
    summarise(across(all_of(feature_name), list(mean = mean, sd = sd)), n=n()) 
  counts <- df_sample_summary %>% select(n) %>% pull()
  return(list(df=df, df_summary=df_sample_summary, counts=counts))
}


plot_loo_diagnostics <- function(loo_object, title="PSIS-LOO diagnostics"){
  elpd <- round(c(loo_object$estimates[1], loo_object$estimates[4]),2)
  df_diag <- tibble(obs = seq(length(loo_object$diagnostics$pareto_k)[1]), 
                    pareto_k = loo_object$diagnostics$pareto_k, 
                    n_eff = loo_object$diagnostics$n_eff)
  p <- df_diag %>% ggplot() + geom_point(aes(n_eff, pareto_k), color="#4099DA") +
    geom_hline(yintercept = c(0.5, 0.7), lty="dashed", color=c("#B7ADA5", "#E85757")) +
    labs(title=title, subtitle=paste0("ELPD: ", elpd[1], ", SE: ", elpd[2]),
         y="Pareto shape k", x="N effective") +
    theme_classic()
  return(p)
}
