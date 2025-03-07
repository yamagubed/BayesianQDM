#---------------posterior--------------------
binary_pos.E <- function(n_t,n_c,n_et,n_ec,#if any arm doesn't exist, set the sample size to zero
                     y_t,y_c,y_et,y_ec,#if any arm doesn't exist, set the observation to zero
                     alpha_t, alpha_c,
                     beta_t, beta_c,a_t,a_c,#a is the power
                     theta) {
  res <- tryCatch({
    temp <- cubintegrate(d_beta.density,lower = theta,upper = 1, 
                         alpha_1 = alpha_t+y_t+a_t*y_et, 
                         alpha_2 = alpha_c + y_c + a_c*y_ec,
                         beta_1 = beta_t + n_t - y_t + a_t*(n_et - y_et),
                         beta_2 = beta_c + n_c - y_c + a_c*(n_ec - y_ec))$integral
    if (temp > 1) {
      warning("The integral is ",temp," which is larger than 1.")
    }
    temp
  },
  error = function(e) {
    message(e," alpha_t = ",alpha_t," alpha_c = ",alpha_c,
            " beta_t = ",beta_t," beta_c = ",beta_c,
            ";y_t = ",y_t, ";y_c = ", y_c, "\n")
    1 # If the integral is divergent, the returning result is 1.
  },
  warning = function(w) {
    message(w," alpha_t = ",alpha_t," alpha_c = ",alpha_c,
            " beta_t = ",beta_t," beta_c = ",beta_c,
            ";y_t = ",y_t, ";y_c = ", y_c, "\n")
    1 # If the integral is larger than 1, the returning result is truncated to 1.
  })
  return(res)
}

binary_pos.GO.sce.E <- function(pi_t, pi_c, 
                                alpha_t, alpha_c,
                                beta_t, beta_c,a_t,a_c,#a is the power
                                n_t, n_c, n_et, n_ec,
                                y_et,y_ec,
                                TV, MAV, gamma_1, gamma_2) {
  prob.go.sce <- 0
  prob.nogo.sce <- 0
  for (y_t in seq(0,n_t)) {
    for (y_c in seq(0,n_c)) {
      prob.go.sce <- prob.go.sce + 
        (dbinom(y_t,n_t,pi_t)*dbinom(y_c,n_c,pi_c)) * (binary_pos.E(n_t,n_c,n_et,n_ec,
                                                                    y_t,y_c,y_et,y_ec,
                                                                    alpha_t, alpha_c,beta_t,beta_c,a_t,a_c,TV) >= gamma_1)
      
      prob.nogo.sce <- prob.nogo.sce + 
        (dbinom(y_t,n_t,pi_t)*dbinom(y_c,n_c,pi_c)) * (binary_pos.E(n_t,n_c,n_et,n_ec,
                                                                    y_t,y_c,y_et,y_ec,
                                                                    alpha_t, alpha_c,beta_t,beta_c,a_t,a_c,MAV) <= gamma_2)
    }
    
  }
  
  return(list(prob.go.sce = prob.go.sce, prob.nogo.sce = prob.nogo.sce,prob.grey.sce = 1 - prob.go.sce - prob.nogo.sce))
}

binary_pos.OC.E <- function(pi_c.fix,
                            alpha_t, alpha_c,beta_t, beta_c,a_t,a_c,#a is the power
                            n_t, n_c, n_et,n_ec,
                            y_et,y_ec,
                            TV, MAV, gamma_1, gamma_2,
                            # N = 10,
                            pi_t.length = 10) {
  pi_t <- seq(0,1,length.out = pi_t.length)
  res <- sapply(1:pi_t.length, function(i) {
    binary_pos.GO.sce.E(pi_t[i], pi_c.fix, alpha_t, alpha_c,beta_t, beta_c,a_t,a_c,
                        n_t, n_c, n_et,n_ec,
                        y_et,y_ec,
                        TV, MAV, gamma_1, gamma_2)
  })
  
  res <- as.data.frame(apply(res,1,unlist))
  res$pi_t <- pi_t
  res$diff <- res$pi_t - pi_c.fix
  graph_OC1(res, endpoint = "binary", decision_criteria = "pos", 
            controlled_POC = TRUE)
}
#---------------predictive--------------------
binary_pred.E <- function(m_t,m_c,
                          alpha_t, beta_t,
                          alpha_c,beta_c,a_t,a_c,#a is the power
                          y_t,y_c,y_et,y_ec,
                          n_t,n_c,n_et,n_ec,
                          theta, N = 10000) {
  1 - d_betabinomial.cdf(x = theta,#theta := x_t/m_t - x_c/m_c,
                         m_1 = m_t,alpha_1 = alpha_t + y_t + a_t*y_et,
                         beta_1 = beta_t + n_t - y_t + a_t*(n_et - y_et),
                         m_2 = m_c,alpha_2 = alpha_c + y_c + a_c*y_ec, 
                         beta_2 = beta_c + n_c - y_c + a_c*(n_ec - y_ec),
                         N = N)
}

binary_pred.GO.sce.E <- function(pi_t,pi_c,
                                 n_t,n_c,n_et,n_ec,
                                 y_et,y_ec,
                                 m_t,m_c,
                                 alpha_t, beta_t,
                                 alpha_c,beta_c,a_t,a_c,#a is the power
                                 TV,MAV,gamma_1,gamma_2, N = 10000) {
  prob.go.sce <- 0
  prob.nogo.sce <- 0
  for (y_t in seq(0,n_t)) {
    for (y_c in seq(0,n_c)) {
      prob.go.sce <- prob.go.sce + 
        (dbinom(y_t,n_t,pi_t)*dbinom(y_c,n_c,pi_c)) * (binary_pred.E(m_t,m_c,
                                                                     alpha_t, beta_t,
                                                                     alpha_c,beta_c,a_t,a_c,
                                                                     y_t,y_c,y_et,y_ec,
                                                                     n_t,n_c,n_et,n_ec,
                                                                     TV, N = N)  >= gamma_1)
      
      prob.nogo.sce <- prob.nogo.sce + 
        (dbinom(y_t,n_t,pi_t)*dbinom(y_c,n_c,pi_c)) * (binary_pred.E(m_t,m_c,
                                                                     alpha_t, beta_t,
                                                                     alpha_c,beta_c,a_t,a_c,
                                                                     y_t,y_c,y_et,y_ec,
                                                                     n_t,n_c,n_et,n_ec,
                                                                     MAV, N = N)  <= gamma_2)
    }
  }
  return(list(prob.go.sce = prob.go.sce, 
              prob.nogo.sce = prob.nogo.sce,
              prob.grey.sce = 1 - prob.go.sce - prob.nogo.sce))
}

binary_pred.OC.E <- function(pi_c.fix,
                             n_t, n_c, n_et,n_ec,
                             y_et,y_ec,
                             m_t,m_c,
                             alpha_t, alpha_c,beta_t, beta_c,a_t,a_c,#a is the power
                             TV, MAV, gamma_1, gamma_2,
                             N = 10000,
                             pi_t.length = 10) {
  pi_t <- seq(0,1,length.out = pi_t.length)
  res <- sapply(1:pi_t.length, function(i) {
    binary_pred.GO.sce.E(pi_t[i], pi_c.fix, n_t, n_c, n_et,n_ec,
                         y_et,y_ec,
                         m_t,m_c,
                         alpha_t, alpha_c,beta_t, beta_c, a_t,a_c,
                         TV, MAV, gamma_1, gamma_2,N)
  })
  
  res <- as.data.frame(apply(res,1,unlist))
  res$pi_t <- pi_t
  res$diff <- res$pi_t - pi_c.fix
  
  graph_OC1(res,
            endpoint = "binary", decision_criteria = "pred",
            controlled_POC = TRUE)
}