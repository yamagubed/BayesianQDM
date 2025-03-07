#------------Posterior---------
continuous_pos.E.vague <- function(model,
                                   n_t,n_c,n_et,n_ec,
                                   y_bar_t,y_bar_c,y_bar_et,y_bar_ec,
                                   s_t,s_c,s_et,s_ec,
                                   alpha_0t, alpha_0c,
                                   theta) {
  if (!isS4(model)) {
    model <- stan_model("./functions/continuous_vague.stan")
  }
  fit <- sampling(model,
                  data=list(
                    y_bar_t = y_bar_t,y_bar_c = y_bar_c, y_bar_et = y_bar_et, y_bar_ec = y_bar_ec,
                    n_t = n_t, n_c = n_c,
                    n_e_t = n_et, 
                    n_e_c = n_ec, 
                    m_t = 1, m_c = 1,
                    s_t = s_t, s_c = s_c, s_et = s_et, s_ec = s_ec,
                    alpha_0t = alpha_0t, alpha_0c = alpha_0c
                  ),iter=50000,chains=5, seed = 800)
  
  #By posterior distribution
  pos_theta <- as.matrix(fit, pars = "theta")
  mean(pos_theta > theta)
}
# continuous_pos.GO.sce.E
continuous_pos.GO.sce.E.vague <- function(model,
                                          mu_t, mu_c, sigma2_t,sigma2_c,
                                          n_t,n_c,n_et,n_ec,
                                          y_bar_et,y_bar_ec,s_et,s_ec,
                                          alpha_0t, alpha_0c,
                                          TV, MAV, gamma_1, gamma_2,
                                          N = 1000) {#number of repitation
  #scenarior accessment for Go/No-Go decision
  y_t <- matrix(rnorm(n_t*N, mean = mu_t, sd = sqrt(sigma2_t)), nrow = N, ncol = n_t)
  y_c <- matrix(rnorm(n_c*N, mean = mu_c, sd = sqrt(sigma2_c)), nrow = N, ncol = n_c)
  
  
  prob.go.sce <- mean(
    sapply(1:N, 
           function(i){ 
             continuous_pos.E.vague(model, 
                                    n_t,n_c,n_et,n_ec,
                                    mean(y_t[i,]), mean(y_c[i,]), y_bar_et,y_bar_ec,
                                    sd(y_t[i,]), sd(y_c[i,]), s_et,s_ec,
                                    alpha_0t, alpha_0c,
                                    TV) 
           }) >= gamma_1)
  prob.nogo.sce <- mean(
    sapply(1:N, function(i){ 
      continuous_pos.E.vague(model, 
                             n_t,n_c,n_et,n_ec,
                             mean(y_t[i,]), mean(y_c[i,]), y_bar_et,y_bar_ec,
                             sd(y_t[i,]), sd(y_c[i,]), s_et,s_ec,
                             alpha_0t, alpha_0c,
                             MAV) 
    }) <= gamma_2)
  
  return(list(prob.go.sce = prob.go.sce, 
              prob.nogo.sce = prob.nogo.sce,
              prob.grey.sce = 1 - prob.go.sce - prob.nogo.sce))
}

# continuous_pos.OC.E
continuous_pos.OC.E.vague <- function(lower,upper,mu_c.fix,sigma2_t,sigma2_c,
                                      n_t,n_c,n_et,n_ec,
                                      y_bar_et,y_bar_ec,s_et,s_ec,
                                      alpha_0t, alpha_0c,
                                      TV, MAV, gamma_1, gamma_2,
                                      N = 10,
                                      mu_t.length = 10) {
  model <- stan_model("./functions/continuous_vague.stan")
  mu_t <- seq(lower,upper,length.out = mu_t.length)
  res <- sapply(1:mu_t.length, function(i) {
    continuous_pos.GO.sce.E.vague(model,mu_t[i], mu_c.fix, sigma2_t,sigma2_c,
                                  n_t,n_c,n_et,n_ec,
                                  y_bar_et,y_bar_ec,s_et,s_ec,
                                  alpha_0t, alpha_0c,
                                  TV, MAV, gamma_1, gamma_2,
                                  N = N)
  })
  
  res <- as.data.frame(apply(res,1,unlist))
  # View(res)
  res$mu_t <- mu_t
  res$diff <- res$mu_t - mu_c.fix
  graph_OC1(res, endpoint = "continuous", decision_criteria = "pos", 
            controlled_POC = TRUE)
}

#------------Predictive---------
continuous_pred.E.vague <- function(model, 
                                    n_t,n_c,n_et,n_ec,
                                    y_bar_t,y_bar_c,y_bar_et,y_bar_ec,
                                    s_t,s_c,s_et,s_ec,
                                    alpha_0t, alpha_0c,
                                    m_t,m_c,
                                    theta) {
  if (!isS4(model)) {
    model <- stan_model("./functions/continuous_vague.stan")
  }
  fit <- sampling(model,
                  data=list(
                    y_bar_t = y_bar_t,y_bar_c = y_bar_c, y_bar_et = y_bar_et, y_bar_ec = y_bar_ec,
                    n_t = n_t, n_c = n_c,
                    n_e_t = n_et, 
                    n_e_c = n_ec, 
                    m_t = m_t, m_c = m_c,
                    s_t = s_t, s_c = s_c, s_et = s_et, s_ec = s_ec,
                    alpha_0t = alpha_0t, alpha_0c = alpha_0c
                  ),iter=50000,chains=5, seed = 800)
  
  pred_theta <- as.matrix(fit, pars = "theta_pred") 
  mean(pred_theta > theta)
}
continuous_pred.GO.sce.E.vague<- function(model,
                                          mu_t, mu_c, sigma2_t,sigma2_c,
                                          n_t,n_c,n_et,n_ec,
                                          y_bar_et,y_bar_ec,s_et,s_ec,
                                          alpha_0t, alpha_0c,
                                          m_t,m_c,
                                          TV, MAV, gamma_1, gamma_2,
                                          N = 1000) {#number of repitation
  #scenarior accessment for Go/No-Go decision
  y_t <- matrix(rnorm(n_t*N, mean = mu_t, sd = sqrt(sigma2_t)), nrow = N, ncol = n_t)
  y_c <- matrix(rnorm(n_c*N, mean = mu_c, sd = sqrt(sigma2_c)), nrow = N, ncol = n_c)
  
  prob.go.sce <- mean(
    sapply(1:N, 
           function(i){ 
             continuous_pred.E.vague(model, 
                                     n_t,n_c,n_et,n_ec,
                                     mean(y_t[i,]), mean(y_c[i,]),y_bar_et,y_bar_ec,
                                     sd(y_t[i,]), sd(y_t[i,]),s_et,s_ec,
                                     alpha_0t, alpha_0c,
                                     m_t,m_c,
                                     TV) 
           }) >= gamma_1)
  prob.nogo.sce <- mean(
    sapply(1:N, function(i){ 
      continuous_pred.E.vague(model, 
                              n_t,n_c,n_et,n_ec,
                              mean(y_t[i,]), mean(y_c[i,]),y_bar_et,y_bar_ec,
                              sd(y_t[i,]), sd(y_t[i,]),s_et,s_ec,
                              alpha_0t, alpha_0c,
                              m_t,m_c,
                              MAV)
    }) <= gamma_2)
  
  return(list(prob.go.sce = prob.go.sce, prob.nogo.sce = prob.nogo.sce,prob.grey.sce = 1 - prob.go.sce - prob.nogo.sce))
}

continuous_pred.OC.E.vague <- function(lower,upper,mu_c.fix,sigma2_t,sigma2_c,
                                       n_t,n_c,n_et,n_ec,
                                       m_t,m_c,
                                       y_bar_et,y_bar_ec,s_et,s_ec,
                                       alpha_0t, alpha_0c,
                                       TV, MAV, gamma_1, gamma_2,
                                       N = 10,
                                       mu_t.length = 10) {
  model <- stan_model("./functions/continuous_vague.stan")
  mu_t <- seq(lower,upper,length.out = mu_t.length)
  res <- sapply(1:mu_t.length, function(i) {
    continuous_pred.GO.sce.E.vague(mdoel,
                                   mu_t[i], mu_c.fix, sigma2_t,sigma2_c,
                                   n_t,n_c,n_et,n_ec,
                                   y_bar_et,y_bar_ec,s_et,s_ec,
                                   alpha_0t, alpha_0c,
                                   m_t,m_c,
                                   TV, MAV, gamma_1, gamma_2,
                                   N = N)
  })
  
  res <- as.data.frame(apply(res,1,unlist))
  # View(res)
  res$mu_t <- mu_t
  res$diff <- res$mu_t - mu_c.fix
  graph_OC1(res, endpoint = "continuous", decision_criteria = "pred", 
            controlled_POC = TRUE)
}
