// Comparison of k groups with common variance (ANOVA)
data {
  int<lower=0> N;            // number of observations
  int<lower=0> K;            // number of groups
  int<lower=0> N_ypred;      // number of values to sample from predictive distributions
  int<lower=1,upper=K> x[N]; // discrete group indicators
  vector[N] y;               // real valued observations
}
parameters {
  real<lower=0> mu0;                // prior mean
  real<lower=0> sigma0;    // prior std constrained to be positive
  vector<lower=0>[K] mu;   // group means
  real<lower=0> sigma;     // common standard deviation constrained to be positive
}
model {
  mu0 ~ normal(1, 1);       // weakly informative prior
  sigma0 ~ normal(0, 1);     // weakly informative prior
  mu ~ normal(mu0, sigma0); // population prior with unknown parameters
  sigma ~ normal(0, 1);     // weakly informative prior
  y ~ normal(mu[x], sigma); // observation model / likelihood
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
	ypred_mar[i] = normal_rng(mu[1], sigma);
	ypred_apr[i] = normal_rng(mu[2], sigma);
    ypred_may[i] = normal_rng(mu[3], sigma);
    ypred_sep[i] = normal_rng(mu[4], sigma);
    ypred_oct[i] = normal_rng(mu[5], sigma);
    ypred_nov[i] = normal_rng(mu[6], sigma);
    ypred_dec[i] = normal_rng(mu[7], sigma);
  }

  // compute log predictive densities to be used for LOO-CV
  // to make appropriate comparison to other models
  for (i in 1:N)
    log_lik[i] = normal_lpdf(y[i] | mu[1], sigma);
}
