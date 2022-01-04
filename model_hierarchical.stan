// Comparison of k groups with common variance (ANOVA)
data {
  int<lower=0> N;            // number of observations
  int<lower=0> K;            // number of groups
  int<lower=0> N_ypred;      // number of values to sample from predictive distributions
  int<lower=1,upper=K> x[N]; // discrete group indicators
  vector[N] y;               // real valued observations
}
parameters {
  vector<lower=0>[K] mu;        // month means
  vector<lower=0>[K] mu0;
  vector<lower=0>[K] sigma;     // month standard deviation
  vector<lower=0>[K] sigma0;
}
model {
  mu0 ~ normal(1, 1);
  mu ~ normal(mu0, 1);          // weakly informative prior
  sigma0 ~ normal(0, 1);        // weakly informative prior
  sigma ~ normal(sigma0, 1);    // weakly informative prior
  y ~ normal(mu[x], sigma[x]);  // observation model / likelihood
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
	ypred_mar[i] = normal_rng(mu[1], sigma[1]);
	ypred_apr[i] = normal_rng(mu[2], sigma[2]);
    ypred_may[i] = normal_rng(mu[3], sigma[3]);
    ypred_sep[i] = normal_rng(mu[4], sigma[4]);
    ypred_oct[i] = normal_rng(mu[5], sigma[5]);
    ypred_nov[i] = normal_rng(mu[6], sigma[6]);
    ypred_dec[i] = normal_rng(mu[7], sigma[7]);
  }

  // compute log predictive densities to be used for LOO-CV
  for(i in 1:N)
	log_lik[i] = normal_lpdf(y[i] | mu[x[i]], sigma[x[i]]);
}
