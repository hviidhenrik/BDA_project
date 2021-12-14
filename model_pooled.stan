// Comparison of k groups with common variance (ANOVA)
data {
  int<lower=0> N;            // number of observations
  int<lower=0> K;            // number of groups
  int<lower=0> N_ypred;      // number of values to sample from predictive distributions
  vector[N] y;               // real valued observations
}
parameters {
  real<lower=0> mu;             // common mean
  real<lower=0> sigma;          // common standard deviation constrained to be positive
}
model {
  mu ~ normal(1, 1);         // weakly informative prior
  sigma ~ normal(0, 1);      // weakly informative prior
  y ~ normal(mu, sigma);     // observation model / likelihood
}
generated quantities {
  // sample from the predictive distribution
  real ypred_mar[N_ypred];
  real ypred_apr[N_ypred];
  real ypred_may[N_ypred];
  real ypred_sep[N_ypred];
  real ypred_oct[N_ypred];
  real ypred_nov[N_ypred];
  real ypred_dec[N_ypred];
  vector[N] log_lik;
		
		
  for (i in 1:N_ypred){
	ypred_mar[i] = normal_rng(mu, sigma);
	ypred_apr[i] = normal_rng(mu, sigma);
    ypred_may[i] = normal_rng(mu, sigma);
    ypred_sep[i] = normal_rng(mu, sigma);
    ypred_oct[i] = normal_rng(mu, sigma);
    ypred_nov[i] = normal_rng(mu, sigma);
    ypred_dec[i] = normal_rng(mu, sigma);
  }

  // compute log predictive densities to be used for LOO-CV
  for(i in 1:N)
	log_lik[i] = normal_lpdf(y[i] | mu, sigma);
}
