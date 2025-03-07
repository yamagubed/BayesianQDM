data {
  int<lower=0> n_t;
  int<lower=0> n_c;
  int<lower=0> n_e_t;
  int<lower=0> n_e_c;
  int<lower=0> m_t;
  int<lower=0> m_c;
  real<lower=0,upper = 1> alpha_0t;
  real<lower=0,upper = 1> alpha_0c;
  real y_bar_c;
  real y_bar_t;
  real<lower=0> s_t;
  real<lower=0> s_c;
  // vector[n_t] y_t;
  // vector[n_c] y_c;
  // vector[n_e_c] y_e_c;
  real<lower=0> s_ec;
  real<lower=0> s_et;
  real y_bar_et;
  real y_bar_ec;
}

parameters {
  real mu_t;
  real mu_c;
  real<lower=0> sigma_t;
  real<lower=0> sigma_c;
}

model {
  target += (-n_t)*log(sigma_t) - ((n_t - 1)*s_t^2 + n_t * (y_bar_t - mu_t)^2)/(2*sigma_t^2);
  target += (-n_c)*log(sigma_c) - ((n_c - 1)*s_c^2 + n_c * (y_bar_c - mu_c)^2)/(2*sigma_c^2);
  if (n_e_t != 0)
    target += alpha_0t * ((-n_e_t)*log(sigma_t) - ((n_e_t - 1)*s_et^2 + n_e_t * (y_bar_et - mu_t)^2)/(2*sigma_t^2));
  if (n_e_c != 0)
    target += alpha_0c * ((-n_e_c)*log(sigma_c) - ((n_e_c - 1)*s_ec^2 + n_e_c * (y_bar_ec - mu_c)^2)/(2*sigma_c^2));
  target += log(1/sigma_t^2);
  target += log(1/sigma_c^2);
}

generated quantities {
  real theta;
  theta = mu_t - mu_c;
  real theta_pred;
  {
    real y_p_t [m_t] = normal_rng(rep_vector(1, m_t) * mu_t, rep_vector(1, m_t) * sigma_t);
    real y_p_c [m_c] = normal_rng(rep_vector(1, m_c) * mu_c, rep_vector(1, m_c) * sigma_c);
    theta_pred = mean(y_p_t) - mean(y_p_c);
  }
}

