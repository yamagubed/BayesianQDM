library(ggplot2)
continuous_pred.W <- function(n_t,n_c,
                              y_bar_t,y_bar_c,
                              s_t, s_c,#sample standard deviations
                              m_t,m_c,
                              nu_t,nu_c,k_t,k_c,
                              alpha,beta,theta) {
  theta_star <- c((k_t*nu_t+n_t*y_bar_t)/(k_t+n_t), (k_c*nu_c+n_c*y_bar_c)/(k_c+n_c))
  X_tilde <- matrix(c(rep(1,m_t),rep(0,m_c),rep(0,m_t),rep(1,m_c)),ncol = 2,byrow = F)
  B <- c(1/m_t * rep(1,m_t), 1/m_c * rep(1,m_c))
  
  center <- t(B) %*% X_tilde %*% theta_star 
  
  K_s_inv <- diag(c(1/k_t+n_t,1/k_c+n_c))
  beta_s <- beta+0.5*( (n_t - 1)*s_t^2 + (n_c - 1)*s_c^2 ) + 
    (k_t*n_t*((y_bar_t - nu_t)**2)/(2*(k_t+n_t))) + (k_c*n_c*((y_bar_c - nu_c)**2)/(2*(k_c+n_c)))
  alpha_s <- alpha + n_t/2 + n_c/2
  scale <- (beta_s/alpha_s)*(t(B) %*% (diag(rep(1,m_t+m_c)) + X_tilde %*% K_s_inv %*% t(X_tilde)) %*% B)
  
  # cat((theta - center)/scale, "\n")
  cubintegrate(dt,lower = (theta - center)/sqrt(scale),upper = Inf, 
               df = 2*alpha+n_t+n_c)$integral
  
}



d_t_scale.cdf <- function(x,
                    df1,df2,
                    center1,center2,
                    scale1,scale2,
                    n1,n2,
                    N = 10000) {
  
  t1 <- rt(N,df = df1) * sqrt(scale1) + center1
  t2 <- rt(N,df = df2) * sqrt(scale2) + center2
  
  return(mean((t1/n1 - t2/n2) <= x))
}
continuous_pred.O <- function(n_t,
                              y_bar_t,
                              s_t,#sample standard deviations
                              m_t,m_c,r,
                              nu_t,k_t,
                              alpha,beta,theta) {
  beta_s <- beta+0.5*( (n_t - 1)*s_t^2 ) +
    (k_t*n_t*((y_bar_t - nu_t)**2)/(2*(k_t+n_t)))
  alpha_s <- alpha + n_t/2
  1 - d_t_scale.cdf(theta,
                    df1 = 2*alpha + n_t, df2 = 2*alpha + n_t,
                    center1 = m_t*(k_t + nu_t + n_t * y_bar_t)/(k_t+n_t), center2 = 0,
                    scale1 = m_t*beta_s*(k_t + n_t + 1)/(alpha_s*(k_t+n_t)), scale2 = r*m_c*beta_s*(k_t + n_t + 1)/(alpha_s*(k_t+n_t)),
                    n1 = m_t, n2 = m_c,
  )
  
}

continuous_pred.W.vague <- function(n_t,n_c,
                              y_bar_t,y_bar_c,
                              s_t, s_c,#sample standard deviations
                              m_t,m_c,
                              theta, N = 10000) {
1 - d_t_scale.cdf(theta,
                  df1 = n_t - 1, df2 = n_c - 1,
                  center1 = m_t * y_bar_t, center2 = m_c *y_bar_c,
                  scale1 = m_t * (1 + 1/n_t)*s_t^2, scale2 = m_c * (1 + 1/n_c)*s_c^2,
                  n1 = m_t, n2 = m_c,
                  N = N)
  
}

continuous_pred.O.vague <- function(n_t,
                                    y_bar_t,
                                    s_t,#sample standard deviations
                                    m_t,m_c,r,
                                    theta, N = 10000) {
  1 - d_t_scale.cdf(theta,
                    df1 = n_t - 1, df2 = n_t - 1,
                    center1 =m_t * y_bar_t, center2 = 0,
                    scale1 = m_t * (1 + 1/n_t)*s_t^2, scale2 = m_c * r * (1 + 1/n_t)*s_t^2,
                    n1 = m_t, n2 = m_c,
                    N = N)
  
}

continuous_pred.GO.sce.W <- function(mu_t, mu_c, sigma2,
                                     nu_t,nu_c,k_t,k_c,n_t,n_c,
                                     m_t,m_c,
                                     alpha, beta, 
                                     TV, MAV, gamma_1, gamma_2,
                                     N = 1000) {#number of repitation
  #scenarior accessment for Go/No-Go decision
  y_t <- matrix(rnorm(n_t*N, mean = mu_t, sd = sqrt(sigma2/k_t)), nrow = N, ncol = n_t)
  y_c <- matrix(rnorm(n_c*N, mean = mu_c, sd = sqrt(sigma2/k_c)), nrow = N, ncol = n_c)
  
  
  prob.go.sce <- mean(
    sapply(1:N, 
           function(i){ 
             continuous_pred.W(n_t,n_c,
                               mean(y_t[i,]), mean(y_c[i,]),
                               sd(y_t[i,]), sd(y_c[i,]),
                               m_t,m_c,
                               nu_t,nu_c,k_t,k_c,alpha,beta, TV) 
           }) >= gamma_1)
  prob.nogo.sce <- mean(
    sapply(1:N, function(i){ 
      continuous_pred.W(n_t,n_c,
                        mean(y_t[i,]), mean(y_c[i,]),
                        sd(y_t[i,]), sd(y_c[i,]),
                        m_t,m_c,
                        nu_t,nu_c,k_t,k_c,alpha,beta, MAV) 
    }) <= gamma_2)
  
  return(list(prob.go.sce = prob.go.sce, prob.nogo.sce = prob.nogo.sce,prob.grey.sce = 1 - prob.go.sce - prob.nogo.sce))
}

continuous_pred.GO.sce.O <- function(mu_t, sigma2,
                                     nu_t,k_t,n_t,
                                     m_t,m_c,r,
                                     alpha, beta, 
                                     TV, MAV, gamma_1, gamma_2,
                                     N = 1000) {#number of repitation
  #scenarior accessment for Go/No-Go decision
  y_t <- matrix(rnorm(n_t*N, mean = mu_t, sd = sqrt(sigma2/k_t)), nrow = N, ncol = n_t)
  
  
  prob.go.sce <- mean(
    sapply(1:N, 
           function(i){ 
             continuous_pred.O(n_t,
                               mean(y_t[i,]), 
                               sd(y_t[i,]), 
                               m_t,m_c,r,
                               nu_t,k_t,alpha,beta, TV) 
           }) >= gamma_1)
  prob.nogo.sce <- mean(
    sapply(1:N, function(i){ 
      continuous_pred.O(n_t,
                        mean(y_t[i,]), 
                        sd(y_t[i,]), 
                        m_t,m_c,r,
                        nu_t,k_t,alpha,beta, MAV)
    }) <= gamma_2)
  
  return(list(prob.go.sce = prob.go.sce, prob.nogo.sce = prob.nogo.sce,prob.grey.sce = 1 - prob.go.sce - prob.nogo.sce))
}

continuous_pred.GO.sce.W.vague <- function(mu_t, mu_c, sigma2,
                                     n_t,n_c,
                                     m_t,m_c,
                                     TV, MAV, gamma_1, gamma_2,
                                     N = 1000) {#number of repitation
  #scenarior accessment for Go/No-Go decision
  y_t <- matrix(rnorm(n_t*N, mean = mu_t, sd = sqrt(sigma2)), nrow = N, ncol = n_t)
  y_c <- matrix(rnorm(n_c*N, mean = mu_c, sd = sqrt(sigma2)), nrow = N, ncol = n_c)
  
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

continuous_pred.GO.sce.O.vague <- function(mu_t, sigma2,
                                           n_t,
                                           m_t,m_c,r,
                                           TV, MAV, gamma_1, gamma_2,
                                           N = 1000) {#number of repitation
  #scenarior accessment for Go/No-Go decision
  y_t <- matrix(rnorm(n_t*N, mean = mu_t, sd = sqrt(sigma2)), nrow = N, ncol = n_t)
  
  
  prob.go.sce <- mean(
    sapply(1:N, 
           function(i){ 
             continuous_pred.O.vague(n_t,
                                     mean(y_t[i,]), 
                                     sd(y_t[i,]), 
                                     m_t,m_c,r,
                                     TV) 
           }) >= gamma_1)
  prob.nogo.sce <- mean(
    sapply(1:N, function(i){ 
      continuous_pred.O.vague(n_t,
                              mean(y_t[i,]), 
                              sd(y_t[i,]), 
                              m_t,m_c,r,
                              MAV) 
    }) <= gamma_2)
  
  return(list(prob.go.sce = prob.go.sce, prob.nogo.sce = prob.nogo.sce,prob.grey.sce = 1 - prob.go.sce - prob.nogo.sce))
}
continuous_pred.OC.W <- function(lower,upper,mu_c.fix,sigma2,
                                nu_t,nu_c,k_t,k_c,
                                alpha, beta, 
                                n_t,n_c,
                                m_t,m_c,
                                TV, MAV, gamma_1, gamma_2,
                                N = 10,
                                mu_t.length = 10) {
  mu_t <- seq(lower,upper,length.out = mu_t.length)
  res <- sapply(1:mu_t.length, function(i) {
    continuous_pred.GO.sce.W(mu_t[i], mu_c.fix, sigma2,
                             nu_t,nu_c,k_t,k_c,n_t,n_c,
                             m_t,m_c,
                             alpha, beta, 
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

continuous_pred.OC.O <- function(lower,upper,sigma2,
                                 nu_t,k_t,
                                 alpha, beta, 
                                 n_t,
                                 m_t,m_c,r,
                                 TV, MAV, gamma_1, gamma_2,
                                 N = 10,
                                 mu_t.length = 10) {
  mu_t <- seq(lower,upper,length.out = mu_t.length)
  res <- sapply(1:mu_t.length, function(i) {
    continuous_pred.GO.sce.O(mu_t[i], sigma2,
                             nu_t,k_t,n_t,
                             m_t,m_c,r,
                             alpha, beta, 
                             TV, MAV, gamma_1, gamma_2,
                             N = N)
  })
  
  res <- as.data.frame(apply(res,1,unlist))
  res$mu_t <- mu_t
  graph_OC1(res, endpoint = "continuous", decision_criteria = "pred", 
            controlled_POC = FALSE)
}

continuous_pred.OC.W.vague <- function(lower,upper,mu_c.fix,sigma2,
                                 n_t,n_c,
                                 m_t,m_c,
                                 TV, MAV, gamma_1, gamma_2,
                                 N = 10,
                                 mu_t.length = 10) {
  mu_t <- seq(lower,upper,length.out = mu_t.length)
  res <- sapply(1:mu_t.length, function(i) {
    continuous_pred.GO.sce.W.vague(mu_t[i], mu_c.fix, sigma2,
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

continuous_pred.OC.O.vague <- function(lower,upper,sigma2,
                                       n_t,
                                       m_t,m_c,r,
                                       TV, MAV, gamma_1, gamma_2,
                                       N = 10,
                                       mu_t.length = 10) {
  mu_t <- seq(lower,upper,length.out = mu_t.length)
  res <- sapply(1:mu_t.length, function(i) {
    continuous_pred.GO.sce.O.vague(mu_t[i], sigma2,
                                   n_t,
                                   m_t,m_c,r,
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
