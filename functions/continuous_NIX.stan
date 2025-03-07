data {
  real<lower=0> alpha_t;
  real<lower=0> beta_t;
  real<lower=0> alpha_c;
  real<lower=0> beta_c;
  int<lower=0> n_t;
  int<lower=0> n_c;
  // int<lower=0> n_e_t;
  int<lower=0> n_e_c;
  int<lower=0> y_t;
  int<lower=0> y_c;
  // int<lower=0> y_e_t;
  int<lower=0> y_e_c;
  int<lower=0> m_t;
  int<lower=0> m_c;
  real<lower=0,upper = 1> alpha_0;
  // int<lower = 0, upper = 1> external_treatment;
 }

parameters {
  real<lower=0,upper=1> pi_t;
  real<lower=0,upper=1> pi_c;
}

model {
  target += binomial_lpmf(y_t | n_t, pi_t);
  target += binomial_lpmf(y_c | n_c, pi_c);
  // if (external_treatment)
  //   target += alpha_0 * binomial_lpmf(y_e_t | n_e_t, pi_t);
  target += alpha_0 * binomial_lpmf(y_e_c | n_e_c, pi_c);
  pi_t ~ beta(alpha_t,beta_t);
  pi_c ~ beta(alpha_c, beta_c);
}

generated quantities {
  real theta;
  theta = pi_t - pi_c;
  int<lower=0> y_p_t = binomial_rng(m_t,pi_t);
  int<lower=0> y_p_c = binomial_rng(m_c,pi_c);
}
