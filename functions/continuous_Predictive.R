library(ggplot2)
continuous_pred.W <- function(n_t,n_c, m_t,m_c,
                              y_bar_t,y_bar_c,s_t,s_c,
                              mu_t,mu_c,
                              k_t,k_c,
                              nu_t,nu_c,
                              sigma2_t,sigma2_c,theta,
                              N = 1000) {
  k_nt <- k_t + n_t;k_nc <- k_c + n_c
  
  nu_nt <- nu_t + n_t; nu_nc <- nu_c + n_c
  
  center_t <- (k_t * mu_t + n_t * y_bar_t)/k_nt
  center_c <- (k_c * mu_c + n_c * y_bar_c)/k_nc
  sigma2_nt <- (nu_t * sigma2_t + (n_t - 1)*s_t^2 + n_t * k_t *(mu_t - y_bar_t)^2/(k_t + n_t))/nu_nt
  sigma2_nc <- (nu_c * sigma2_c + (n_c - 1)*s_c^2 + n_c * k_c *(mu_c - y_bar_c)^2/(k_c + n_c))/nu_nc
  scale_t <- sigma2_nt*(1 + 1/k_nt); scale_c <- sigma2_nc*(1 + 1/k_nc)
  df_t <- nu_nt; df_c <- nu_nc
  
  1 - d_t.cdf(x = theta, df1 = df_t, df2 = df_c, center1 = center_t, center2 = center_c,
              scale1 = scale_t/m_t, scale2 = scale_c/m_c, N = N)
}

continuous_pred.O <- function(n_t, m_t,m_c,
                              y_bar_t,s_t, mu_0c,r,
                              mu_t, k_t, nu_t, sigma2_t,
                              theta) {
  k_nt <- k_t + n_t
  nu_nt <- nu_t + n_t
  center_t <- (k_t * mu_t + n_t * y_bar_t)/k_nt
  sigma2_nt <- (nu_t * sigma2_t + (n_t - 1)*s_t^2 + n_t * k_t *(mu_t - y_bar_t)^2/(k_t + n_t))/nu_nt
  df_t <- nu_nt
  
  center <- center_t - mu_0c
  scale <- (1/m_t + r/m_c)*(1 + k_nt)*sigma2_nt/k_nt
  cubintegrate(dt,lower = (theta - center)/sqrt(scale),upper = Inf,
               df = df_t)$integral
  
}

continuous_pred.W.vague <- function(n_t,n_c,
                              y_bar_t,y_bar_c,
                              s_t, s_c,#sample standard deviations
                              m_t,m_c,
                              theta, N = 10000) {
  1 - d_t.cdf(theta,
              df1 = n_t - 1, df2 = n_c - 1,
              center1 = y_bar_t, center2 = y_bar_c,
              scale1 = (1 + 1/n_t)*s_t^2/m_t, scale2 = (1 + 1/n_c)*s_c^2/m_c,
              N = N)
  
}

continuous_pred.O.vague <- function(n_t,
                                    y_bar_t,
                                    s_t,#sample standard deviations
                                    m_t,m_c,mu_0c,r,
                                    theta, N = 10000) {
  center <- y_bar_t - mu_0c
  scale <- (1/m_t + r/m_c)*(1 + 1/n_t)*s_t^2
  df_t <- n_t - 1
  cubintegrate(dt,lower = (theta - center)/sqrt(scale),upper = Inf,
               df = df_t)$integral
  
}

continuous_pred.GO.sce.W <- function(mu_t, mu_c, sigma2_t,sigma2_c,
                                     n_t,n_c, m_t,m_c,
                                     mu_0t,mu_0c,
                                     k_0t,k_0c,
                                     nu_0t,nu_0c,
                                     sigma2_0t,sigma2_0c,
                                     TV, MAV, gamma_1, gamma_2,
                                     N = 1000) {#number of repitation
  #scenarior accessment for Go/No-Go decision
  y_t <- matrix(rnorm(n_t*N, mean = mu_t, sd = sqrt(sigma2_t)), nrow = N, ncol = n_t)
  y_c <- matrix(rnorm(n_c*N, mean = mu_c, sd = sqrt(sigma2_c)), nrow = N, ncol = n_c)
  
  
  prob.go.sce <- mean(
    sapply(1:N, 
           function(i){ 
             continuous_pred.W(n_t,n_c,m_t,m_c,
                               mean(y_t[i,]), mean(y_c[i,]),
                               sd(y_t[i,]), sd(y_c[i,]),
                               mu_0t,mu_0c,k_0t,k_0c,
                               nu_0t,nu_0c,sigma2_0t,sigma2_0c,
                               TV) 
           }) >= gamma_1)
  prob.nogo.sce <- mean(
    sapply(1:N, function(i){ 
      continuous_pred.W(n_t,n_c,m_t,m_c,
                        mean(y_t[i,]), mean(y_c[i,]),
                        sd(y_t[i,]), sd(y_c[i,]),
                        mu_0t,mu_0c,k_0t,k_0c,
                        nu_0t,nu_0c,sigma2_0t,sigma2_0c,
                        MAV) 
    }) <= gamma_2)
  
  return(list(prob.go.sce = prob.go.sce, prob.nogo.sce = prob.nogo.sce,prob.grey.sce = 1 - prob.go.sce - prob.nogo.sce))
}

continuous_pred.GO.sce.O <- function(mu_t, sigma2_t,
                                     n_t,m_t,m_c,mu_0c,r,
                                     mu_0t,k_0t, nu_0t, sigma2_0t,
                                     TV, MAV, gamma_1, gamma_2,
                                     N = 1000) {#number of repitation
  #scenarior accessment for Go/No-Go decision
  y_t <- matrix(rnorm(n_t*N, mean = mu_t, sd = sqrt(sigma2_t)), nrow = N, ncol = n_t)
  
  
  prob.go.sce <- mean(
    sapply(1:N, 
           function(i){ 
             continuous_pred.O(n_t, m_t, m_c,
                               mean(y_t[i,]), sd(y_t[i,]), mu_0c,r,
                               mu_0t,k_0t, nu_0t, sigma2_0t,
                               TV) 
           }) >= gamma_1)
  prob.nogo.sce <- mean(
    sapply(1:N, function(i){ 
      continuous_pred.O(n_t, m_t, m_c,
                        mean(y_t[i,]), sd(y_t[i,]), mu_0c,r,
                        mu_0t,k_0t, nu_0t, sigma2_0t,
                        MAV) 
    }) <= gamma_2)
  
  return(list(prob.go.sce = prob.go.sce, prob.nogo.sce = prob.nogo.sce,prob.grey.sce = 1 - prob.go.sce - prob.nogo.sce))
}

continuous_pred.GO.sce.W.vague <- function(mu_t, mu_c, sigma2_t,sigma2_c,
                                     n_t,n_c,
                                     m_t,m_c,
                                     TV, MAV, gamma_1, gamma_2,
                                     N = 1000) {#number of repitation
  #scenarior accessment for Go/No-Go decision
  y_t <- matrix(rnorm(n_t*N, mean = mu_t, sd = sqrt(sigma2_t)), nrow = N, ncol = n_t)
  y_c <- matrix(rnorm(n_c*N, mean = mu_c, sd = sqrt(sigma2_c)), nrow = N, ncol = n_c)
  
  prob.go.sce <- mean(
    sapply(1:N, 
           function(i){ 
             continuous_pred.W.vague(n_t,n_c,
                                     mean(y_t[i,]), mean(y_c[i,]),
                                     sd(y_t[i,]), sd(y_t[i,]),
                                     m_t,m_c,
                                     TV) 
           }) >= gamma_1)
  prob.nogo.sce <- mean(
    sapply(1:N, function(i){ 
      continuous_pred.W.vague(n_t,n_c,
                              mean(y_t[i,]), mean(y_c[i,]),
                              sd(y_t[i,]), sd(y_t[i,]),
                              m_t,m_c,
                              MAV) 
    }) <= gamma_2)
  
  return(list(prob.go.sce = prob.go.sce, prob.nogo.sce = prob.nogo.sce,prob.grey.sce = 1 - prob.go.sce - prob.nogo.sce))
}

continuous_pred.GO.sce.O.vague <- function(mu_t, sigma2_t,
                                           n_t,
                                           m_t,m_c,mu_0c,r,
                                           TV, MAV, gamma_1, gamma_2,
                                           N = 1000) {#number of repitation
  #scenarior accessment for Go/No-Go decision
  y_t <- matrix(rnorm(n_t*N, mean = mu_t, sd = sqrt(sigma2_t)), nrow = N, ncol = n_t)
  
  
  prob.go.sce <- mean(
    sapply(1:N, 
           function(i){ 
             continuous_pred.O.vague(n_t,
                                     mean(y_t[i,]), sd(y_t[i,]), 
                                     m_t,m_c,mu_0c,r,
                                     TV) 
           }) >= gamma_1)
  prob.nogo.sce <- mean(
    sapply(1:N, function(i){ 
      continuous_pred.O.vague(n_t,
                              mean(y_t[i,]), sd(y_t[i,]), 
                              m_t,m_c,mu_0c,r,
                              MAV) 
    }) <= gamma_2)
  
  return(list(prob.go.sce = prob.go.sce, prob.nogo.sce = prob.nogo.sce,prob.grey.sce = 1 - prob.go.sce - prob.nogo.sce))
}
continuous_pred.OC.W <- function(lower,upper,mu_c.fix,sigma2_t,sigma2_c,
                                 n_t,n_c, m_t,m_c,
                                 mu_0t,mu_0c,
                                 k_0t,k_0c,
                                 nu_0t,nu_0c,
                                 sigma2_0t,sigma2_0c,
                                 TV, MAV, gamma_1, gamma_2,
                                 N = 10,
                                 mu_t.length = 10) {
  mu_t <- seq(lower,upper,length.out = mu_t.length)
  res <- sapply(1:mu_t.length, function(i) {
    continuous_pred.GO.sce.W(mu_t[i], mu_c.fix, sigma2_t,sigma2_c,
                             n_t,n_c, m_t,m_c,
                             mu_0t,mu_0c,
                             k_0t,k_0c,
                             nu_0t,nu_0c,
                             sigma2_0t,sigma2_0c,
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

continuous_pred.OC.O <- function(lower,upper,sigma2_t,
                                 n_t,m_t, m_c,mu_0c,r,
                                 mu_0t,k_0t, nu_0t, sigma2_0t,
                                 TV, MAV, gamma_1, gamma_2,
                                 N = 10,
                                 mu_t.length = 10) {
  mu_t <- seq(lower,upper,length.out = mu_t.length)
  res <- sapply(1:mu_t.length, function(i) {
    continuous_pred.GO.sce.O(mu_t[i], sigma2_t, n_t, m_t, m_c,mu_0c,r,
                             mu_0t,k_0t, nu_0t, sigma2_0t,
                             TV, MAV, gamma_1, gamma_2,
                             N = N)
  })
  
  res <- as.data.frame(apply(res,1,unlist))
  res$mu_t <- mu_t
  graph_OC1(res, endpoint = "continuous", decision_criteria = "pred", 
            controlled_POC = FALSE)
}

continuous_pred.OC.W.vague <- function(lower,upper,mu_c.fix,sigma2_t,sigma2_c,
                                 n_t,n_c,
                                 m_t,m_c,
                                 TV, MAV, gamma_1, gamma_2,
                                 N = 10,
                                 mu_t.length = 10) {
  mu_t <- seq(lower,upper,length.out = mu_t.length)
  res <- sapply(1:mu_t.length, function(i) {
    continuous_pred.GO.sce.W.vague(mu_t[i], mu_c.fix, sigma2_t,sigma2_c,
                             n_t,n_c,
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

continuous_pred.OC.O.vague <- function(lower,upper,sigma2_t,
                                       n_t,
                                       m_t,m_c,mu_0c,r,
                                       TV, MAV, gamma_1, gamma_2,
                                       N = 10,
                                       mu_t.length = 10) {
  mu_t <- seq(lower,upper,length.out = mu_t.length)
  res <- sapply(1:mu_t.length, function(i) {
    continuous_pred.GO.sce.O.vague(mu_t[i], sigma2_t,
                                   n_t,
                                   m_t,m_c,mu_0c,r,
                                   TV, MAV, gamma_1, gamma_2,
                                   N = N)
  })
  
  res <- as.data.frame(apply(res,1,unlist))
  # View(res)
  res$mu_t <- mu_t
  graph_OC1(res, endpoint = "continuous", decision_criteria = "pred", 
            controlled_POC = FALSE)
}
#####TEST#####
# n_t <- 12; n_c <- 12
# y_bar_t <- 3; y_bar_c <- 0
# s_t <- 1; s_c <- 1
# m_t <- 120; m_c <- 120
# nu_t <- 5; nu_c <- 0;
# k_t <- 1; k_c <- 1
# alpha <- 0.5; beta <- 0.5
# theta <- 15
# continuous_pred.W(n_t,n_c,
#                   y_bar_t,y_bar_c,
#                   s_t, s_c,
#                   m_t,m_c,
#                   nu_t,nu_c,k_t,k_c,
#                   alpha,beta,theta)
