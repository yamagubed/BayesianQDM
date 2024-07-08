d_betabinomial.cdf <- function(x,#theta := x_t/m_t - x_c/m_c,
                               m_1,alpha_1,beta_1,
                               m_2,alpha_2, beta_2,
                               N = 10000) {
  pi_1 <- rbeta(N, alpha_1,beta_1)
  pi_2 <- rbeta(N,alpha_2,beta_2)
  
  x_1 <- sapply(pi_1, function(pi) {
    rbinom(1,size = m_1, prob = pi)
  })
  x_2 <- sapply(pi_2, function(pi) {
    rbinom(1,size = m_2, prob = pi)
  })
  
  theta <- x_1/m_1 - x_2/m_2 #N theta generated
  
  return(mean(theta <= x))#Probability of theta < x
}
binary_pred.W <- function(m_t,m_c,
                          alpha_t, beta_t,
                          alpha_c,beta_c,
                          y_t,y_c,
                          n_t,n_c,
                          theta, N = 10000) {
  1 - d_betabinomial.cdf(x = theta,#theta := x_t/m_t - x_c/m_c,
                         m_1 = m_t,alpha_1 = alpha_t + y_t,beta_1 = beta_t + n_t - y_t,
                         m_2 = m_c,alpha_2 = alpha_c + y_c, beta_2 = beta_c + n_c - y_c,
                         N = N)
}
binary_pred.O <- function(m_t,
                          alpha_t, beta_t,z,
                          y_t,
                          n_t,
                          theta, N = 10000) {
  1 - d_betabinomial.cdf(x = theta,#theta := x_t/m_t - x_c/m_c,
                         m_1 = m_t,alpha_1 = alpha_t + y_t,beta_1 = beta_t + n_t - y_t,
                         m_2 = m_t,alpha_2 = alpha_t + z, beta_2 = beta_t + n_t - z,
                         N = N)
}
binary_pred.Go.sce.W <- function(pi_t,pi_c,
                                 n_t,n_c,
                                 m_t,m_c,
                                 alpha_t, beta_t,
                                 alpha_c,beta_c,
                                 TV,MAV,gamma_1,gamma_2, N = 10000) {
  # y_t <- rbinom(M, size = n_t, prob = pi_t)
  # y_c <- rbinom(M, size = n_c, prob = pi_c)
  prob.go.sce <- 0
  prob.nogo.sce <- 0
  for (y_t in seq(0,n_t)) {
    for (y_c in seq(0,n_c)) {
      prob.go.sce <- prob.go.sce + 
        (dbinom(y_t,n_t,pi_t)*dbinom(y_c,n_c,pi_c)) * (binary_pred.W(m_t,m_c,alpha_t, beta_t,alpha_c,beta_c,
                                                                     y_t,y_c,n_t,n_c,
                                                                     TV, N = N) >= gamma_1)
      
      prob.nogo.sce <- prob.nogo.sce + 
        (dbinom(y_t,n_t,pi_t)*dbinom(y_c,n_c,pi_c)) * (binary_pred.W(m_t,m_c,alpha_t, beta_t,alpha_c,beta_c,
                                                                     y_t,y_c,n_t,n_c,
                                                                     MAV, N = N) <= gamma_2)
    }
  }
  return(list(prob.go.sce = prob.go.sce, 
              prob.nogo.sce = prob.nogo.sce,
              prob.grey.sce = 1 - prob.go.sce - prob.nogo.sce))
}

binary_pred.Go.sce.O <- function(pi_t,
                                 n_t,
                                 m_t,
                                 alpha_t, beta_t,z,
                                 TV,MAV,gamma_1,gamma_2, N = 10000) {
  # y_t <- rbinom(M, size = n_t, prob = pi_t)
  # y_c <- rbinom(M, size = n_c, prob = pi_c)
  prob.go.sce <- 0
  prob.nogo.sce <- 0
  for (y_t in seq(0,n_t)) {
    prob.go.sce <- prob.go.sce + 
      (dbinom(y_t,n_t,pi_t)) * (binary_pred.O(m_t,alpha_t, beta_t,z,
                                              y_t,n_t,
                                              TV, N = N) >= gamma_1)
    
    prob.nogo.sce <- prob.nogo.sce + 
      (dbinom(y_t,n_t,pi_t)) * (binary_pred.O(m_t,alpha_t, beta_t,z,
                                              y_t,n_t,
                                              MAV, N = N) <= gamma_2)
  }
  return(list(prob.go.sce = prob.go.sce, 
              prob.nogo.sce = prob.nogo.sce,
              prob.grey.sce = 1 - prob.go.sce - prob.nogo.sce))
}
# binary_pred.Go.W
# binary_pred.Go.O
binary_pred.OC.W <- function(pi_c.fix,
                             n_t, n_c, m_t,m_c,
                             alpha_t, alpha_c,beta_t, beta_c,
                             TV, MAV, gamma_1, gamma_2,
                             N = 10000,
                             pi_t.length = 10) {
  pi_t <- seq(0,1,length.out = pi_t.length)
  res <- sapply(1:pi_t.length, function(i) {
    binary_pred.Go.sce.W(pi_t[i], pi_c.fix, n_t, n_c, m_t,m_c,
                         alpha_t, alpha_c,beta_t, beta_c, TV, MAV, gamma_1, gamma_2,N)
  })
  
  res <- as.data.frame(apply(res,1,unlist))
  res$pi_t <- pi_t
  res$diff <- res$pi_t - pi_c.fix
  
  graph_OC1(res,
            endpoint = "binary", decision_criteria = "pred",
            controlled_POC = TRUE)
  
}

binary_pred.OC.O <- function(n_t, m_t,
                             alpha_t, beta_t, z,
                             TV, MAV, gamma_1, gamma_2,
                             N = 10000, 
                             pi_t.length = 10) {
  pi_t <- seq(0,1,length.out = pi_t.length)
  res <- sapply(1:pi_t.length, function(i) {
    binary_pred.Go.sce.O(pi_t[i], n_t, m_t,
                         alpha_t, beta_t, z,
                         TV, MAV, gamma_1, gamma_2,N)
  })
  
  res <- as.data.frame(apply(res,1,unlist))
  res$pi_t <- pi_t
  graph_OC1(res,
            endpoint = "binary", decision_criteria = "pred",
            controlled_POC = FALSE)
}
#######TEST######
# d_betabinomial.cdf(-1, 10,1,1,10,1,1)
# m_t <- 12; m_c <- 12; n_t <- 12; n_c <- 12;
# alpha_t <- 0.5; alpha_c <- 0.5; beta_t<- 0.5; beta_c <- 0.5;
# y_t <- 6; y_c <- 3; theta <- 0.15
# # binary_pred.W(m_t,m_c,
# #               alpha_t, beta_t,
# #               alpha_c,beta_c,
# #               y_t,y_c,
# #               n_t,n_c,
# #               theta, N = 10000)
# pi_t <- 0.5; pi_c <- 3/12;
# TV <- 0.5 - 3/12; MAV <- 0;
# gamma_1 <- 0.8; gamma_2 <- 0.3
# binary_pred.Go.sce.W(pi_t,pi_c,
#                      n_t,n_c,
#                      m_t,m_c,
#                      alpha_t, beta_t,
#                      alpha_c,beta_c,
#                      TV,MAV,gamma_1,gamma_2, N = 100)
# pi_c.fix <- 3/12
# binary_pred.OC.W(pi_c.fix,
#                  n_t, n_c, m_t,m_c,
#                  alpha_t, alpha_c,beta_t, beta_c,
#                  TV, MAV, gamma_1, gamma_2,
#                  N = 1000,
#                  pi_t.length = 10)
# z <- 3
# binary_pred.OC.O(n_t, m_t,
#                  alpha_t, beta_t, z,
#                  TV, MAV, gamma_1, gamma_2,
#                  N = 100, M = 100,
#                  pi_t.length = 10)
