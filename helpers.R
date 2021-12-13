## Helper functions for BDA project

plot_areas <- function(fitted_model){
  draws <- extract(fitted_model, permuted=T)
  df <- data.frame(draws$mu) # %>% cbind(draws$sigma)
  vibrations <- df %>% setNames(c(month_names))
  mcmc_areas(vibrations) + xlab('Vibration\nvelocity [mm/s]')
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
  ys <- get_y_and_yrep(df, fit_anova, month_abbr=month, reps=reps)
  p1 <- ys$y %>% ppc_dens_overlay(yrep=ys$yrep) + ggtitle(title)
  p2 <- ys$y %>% ppc_intervals(yrep=ys$yrep)
  p3 <- ys$y %>% ppc_ecdf_overlay(yrep=ys$yrep)
  grid.arrange(p1, p2, p3, nrow = 3)
}

make_random_subset <- function(df_original, feature_name = "vibr_bear_as_x",
                               n_samples=1200){
  while(TRUE){
    # sample subset and sort on month
    df <- df_original %>% slice_sample(n=n_samples)
    df <- df[order(match(df$month, month.abb)), ]
    df_sample_summary <- df %>% group_by(month) %>% 
      summarise(across(all_of(feature_name), list(mean = mean, sd = sd)), n=n()) 
    counts <- df_sample_summary %>% select(n) %>% pull()
    if(length(counts) == 7 & all(counts > 10))
      break
  }
  return(list(df=df, df_summary=df_sample_summary, counts=counts))
}