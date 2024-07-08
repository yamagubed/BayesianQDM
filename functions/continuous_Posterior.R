library(cubature)
# library(ggplot2)
d_t.cdf <- function(x,
                    df1,df2,
                    center1,center2,
                    scale1,scale2,
                    N = 10000) {
  
  t1 <- rt(N,df = df1) * sqrt(scale1) + center1
  t2 <- rt(N,df = df2) * sqrt(scale2) + center2
  
  return(mean((t1 - t2) <= x))
}

continuous_pos.W <- function(n_t,n_c, y_bar_t,y_bar_c,s_t,s_c,
                             mu_t,mu_c,
                             k_t,k_c,
                             nu_t,nu_c,
                             sigma2_t,sigma2_c,theta,
                             N = 1000) { #Number of simulations to be used to approximate the CDF
  # #NIG prior
  # center <- ((k_t*nu_t+n_t*y_bar_t)/(k_t+n_t)) - ((k_c*nu_c+n_c*y_bar_c)/(k_c+n_c))
  # A <- c(1,-1)
  # 
  # beta_s <- beta+0.5*( (n_t - 1)*s_t^2 + (n_c - 1)*s_c^2 ) + 
  #   (k_t*n_t*((y_bar_t - nu_t)**2)/(2*(k_t+n_t))) + (k_c*n_c*((y_bar_c - nu_c)**2)/(2*(k_c+n_c)))
  # alpha_s <- alpha + n_t/2 + n_c/2
  # scale <- (beta_s/alpha_s)*(t(A) %*% diag(1/c(k_t+n_t,k_c+n_c)) %*% A)
  # 
  # cubintegrate(dt,lower = (theta - center)/sqrt(scale),upper = Inf, 
  #              df = 2*alpha+n_t+n_c)$integral
  
  k_nt <- k_t + n_t;k_nc <- k_c + n_c
  
  nu_nt <- nu_t + n_t; nu_nc <- nu_c + n_c
  
  center_t <- (k_t * mu_t + n_t * y_bar_t)/k_nt
  center_c <- (k_c * mu_c + n_c * y_bar_c)/k_nc
  sigma2_nt <- (nu_t * sigma2_t + (n_t - 1)*s_t^2 + n_t * k_t *(mu_t - y_bar_t)^2/(k_t + n_t))/nu_nt
  sigma2_nc <- (nu_c * sigma2_c + (n_c - 1)*s_c^2 + n_c * k_c *(mu_c - y_bar_c)^2/(k_c + n_c))/nu_nc
  scale_t <- sigma2_nt/k_nt; scale_c <- sigma2_nc/k_nc
  df_t <- nu_nt; df_c <- nu_nc
  
  1 - d_t.cdf(x = theta, df1 = df_t, df2 = df_c, center1 = center_t, center2 = center_c,
              scale1 = scale_t, scale2 = scale_c, N = N)
  
}
continuous_pos.O <- function(n_t, y_bar_t, s_t,
                             nu_t,k_t,mu_0c,r,alpha,beta, theta) {
  
  # center <- ((k_t*nu_t+n_t*y_bar_t)/(k_t+n_t))
  # beta_s <- beta+0.5*( (n_t - 1)*s_t^2 ) +
  #   (k_t*n_t*((y_bar_t - nu_t)**2)/(2*(k_t+n_t)))
  # alpha_s <- alpha + n_t/2
  # scale <- ((r+1)*beta_s)/(alpha_s*(k_t+n_t))
  # 
  # cubintegrate(dt,lower = (theta - center)/sqrt(scale),upper = Inf,
  #              df = 2*alpha+n_t)$integral
  
  k_nt <- k_t + n_t
  
  nu_nt <- nu_t + n_t
  
  center_t <- (k_t * mu_t + n_t * y_bar_t)/k_nt
  center <- center_t - mu_0c
  sigma2_nt <- (nu_t * sigma2_t + (n_t - 1)*s_t^2 + n_t * k_t *(mu_t - y_bar_t)^2/(k_t + n_t))/nu_nt
  scale_t <- sigam2_nt/k_nt
  scale <- (r + 1)*scale_t
  df_t <- nu_nt
  
  cubintegrate(dt,lower = (theta - center)/sqrt(scale),upper = Inf,
               df = df_t)$integral
}

continuous_pos.W.vague <- function(n_t,n_c,
                             y_bar_t,y_bar_c,
                             s_t,s_c,
                             theta,
                             N = 10000) {
  #Vague prior
  1 - d_t.cdf(x = theta,
              df1 = n_t - 1, df2 = n_c - 1,
              center1 = y_bar_t, center2 = y_bar_c,
              scale1 = s_t^2/n_t, scale2 = s_c^2/n_c,
              N = N)
}
continuous_pos.O.vague <- function(n_t,
                                   y_bar_t,
                                   s_t,r,
                                   theta) {
  #Vague prior
  center <- y_bar_t
  scale <- (r+1)*s_t^2/n_t
  cubintegrate(dt,lower = (theta - center)/sqrt(scale),
               upper = Inf,
               df = n_t-1)$integral
}


continuous_pos.GO.sce.W <- function(mu_t, mu_c, sigma2_t,sigma2_c,
                                    mu_0t,mu_0c,k_0t,k_0c,
                                    nu_0t,nu_0c,sigma2_0t,sigma2_0c,
                                    n_t,n_c,
                                    TV, MAV, gamma_1, gamma_2,
                                    N = 1000) {#number of repitation
  #scenarior accessment for Go/No-Go decision
  y_t <- matrix(rnorm(n_t*N, mean = mu_t, sd = sqrt(sigma2_t)), nrow = N, ncol = n_t)
  y_c <- matrix(rnorm(n_c*N, mean = mu_c, sd = sqrt(sigma2_c)), nrow = N, ncol = n_c)
  
  
  prob.go.sce <- mean(
    sapply(1:N, 
           function(i){ 
             continuous_pos.W(n_t,n_c,
                              mean(y_t[i,]), mean(y_c[i,]),
                              sd(y_t[i,]), sd(y_c[i,]),
                              mu_0t,mu_0c,k_0t,k_0c,
                              nu_0t,nu_0c,sigma2_0t,sigma2_0c,
                              TV) 
           }) >= gamma_1)
  prob.nogo.sce <- mean(
    sapply(1:N, function(i){ 
      continuous_pos.W(n_t,n_c,
                       mean(y_t[i,]), mean(y_c[i,]),
                       sd(y_t[i,]), sd(y_c[i,]),
                       mu_0t,mu_0c,k_0t,k_0c,
                       nu_0t,nu_0c,sigma2_0t,sigma2_0c,
                       MAV) 
    }) <= gamma_2)
  
  return(list(prob.go.sce = prob.go.sce, prob.nogo.sce = prob.nogo.sce,prob.grey.sce = 1 - prob.go.sce - prob.nogo.sce))
}
continuous_pos.GO.sce.O <- function(mu_t, sigma2,
                                    nu_t,k_t,n_t,
                                    r,alpha, beta, 
                                    TV, MAV, gamma_1, gamma_2,
                                    N = 1000) {#number of repitation
  #scenarior accessment for Go/No-Go decision
  y_t <- matrix(rnorm(n_t*N, mean = mu_t, sd = sqrt(sigma2/k_t)), nrow = N, ncol = n_t)
  
  
  prob.go.sce <- mean(
    sapply(1:N, 
           function(i){ 
             continuous_pos.O(n_t,
                              mean(y_t[i,]), 
                              sd(y_t[i,]), 
                              nu_t,k_t,r,alpha,beta, TV) 
           }) >= gamma_1)
  prob.nogo.sce <- mean(
    sapply(1:N, function(i){ 
      continuous_pos.O(n_t,
                       mean(y_t[i,]), 
                       sd(y_t[i,]), 
                       nu_t,k_t,r,alpha,beta, MAV)
    }) <= gamma_2)
  
  return(list(prob.go.sce = prob.go.sce, prob.nogo.sce = prob.nogo.sce,prob.grey.sce = 1 - prob.go.sce - prob.nogo.sce))
}

continuous_pos.GO.sce.W.vague <- function(mu_t, mu_c, sigma2,
                                          n_t,n_c,
                                          TV, MAV, gamma_1, gamma_2,
                                          N = 1000) {#number of repitation
  #scenarior accessment for Go/No-Go decision
  y_t <- matrix(rnorm(n_t*N, mean = mu_t, sd = sqrt(sigma2)), nrow = N, ncol = n_t)
  y_c <- matrix(rnorm(n_c*N, mean = mu_c, sd = sqrt(sigma2)), nrow = N, ncol = n_c)
  
  
  prob.go.sce <- mean(
    sapply(1:N, 
           function(i){ 
             continuous_pos.W.vague(n_t,n_c,
                              mean(y_t[i,]), mean(y_c[i,]),
                              sd(y_t[i,]), sd(y_c[i,]),
                              TV) 
           }) >= gamma_1)
  prob.nogo.sce <- mean(
    sapply(1:N, function(i){ 
      continuous_pos.W.vague(n_t,n_c,
                       mean(y_t[i,]), mean(y_c[i,]),
                       sd(y_t[i,]), sd(y_c[i,]),
                       MAV) 
    }) <= gamma_2)
  
  return(list(prob.go.sce = prob.go.sce, prob.nogo.sce = prob.nogo.sce,prob.grey.sce = 1 - prob.go.sce - prob.nogo.sce))
}
continuous_pos.GO.sce.O.vague <- function(mu_t, sigma2,
                                          n_t,r,
                                          TV, MAV, gamma_1, gamma_2,
                                          N = 1000) {#number of repitation
  #scenarior accessment for Go/No-Go decision
  y_t <- matrix(rnorm(n_t*N, mean = mu_t, sd = sqrt(sigma2)), nrow = N, ncol = n_t)
  
  
  prob.go.sce <- mean(
    sapply(1:N, 
           function(i){ 
             continuous_pos.O.vague(n_t,
                                    mean(y_t[i,]), 
                                    sd(y_t[i,]), r,
                                    TV) 
           }) >= gamma_1)
  prob.nogo.sce <- mean(
    sapply(1:N, function(i){ 
      continuous_pos.O.vague(n_t,
                             mean(y_t[i,]), 
                             sd(y_t[i,]), r,
                             MAV) 
    }) <= gamma_2)
  
  return(list(prob.go.sce = prob.go.sce, prob.nogo.sce = prob.nogo.sce,prob.grey.sce = 1 - prob.go.sce - prob.nogo.sce))
}

continuous_pos.OC.W <- function(lower,upper,mu_c.fix,sigma2_t,sigma2_c,
                                mu_0t,mu_0c,k_0t,k_0c,
                                nu_0t,nu_0c,sigma2_0t,sigma2_0c,
                                n_t,n_c,
                                TV, MAV, gamma_1, gamma_2,
                                N = 10,
                                mu_t.length = 10) {
  mu_t <- seq(lower,upper,length.out = mu_t.length)
  res <- sapply(1:mu_t.length, function(i) {
    continuous_pos.GO.sce.W(mu_t[i], mu_c.fix, sigma2_t,sigma2_c,
                            mu_0t,mu_0c,k_0t,k_0c,
                            nu_0t,nu_0c,sigma2_0t,sigma2_0c,
                            n_t,n_c,
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

continuous_pos.OC.O <- function(lower,upper,sigma2,
                                nu_t,k_t,
                                r,alpha, beta, 
                                n_t,
                                TV, MAV, gamma_1, gamma_2,
                                N = 10,
                                mu_t.length = 10) {
  mu_t <- seq(lower,upper,length.out = mu_t.length)
  res <- sapply(1:mu_t.length, function(i) {
    continuous_pos.GO.sce.O(mu_t[i], sigma2,
                            nu_t,k_t,n_t,
                            r, alpha, beta, 
                            TV, MAV, gamma_1, gamma_2,
                            N = N)
  })
  
  res <- as.data.frame(apply(res,1,unlist))
  # View(res)
  res$mu_t <- mu_t
  graph_OC1(res, endpoint = "continuous", decision_criteria = "pos", 
            controlled_POC = FALSE)
}

continuous_pos.OC.W.vague <- function(lower,upper,mu_c.fix,sigma2,
                                      n_t,n_c,
                                      TV, MAV, gamma_1, gamma_2,
                                      N = 10,
                                      mu_t.length = 10) {
  mu_t <- seq(lower,upper,length.out = mu_t.length)
  res <- sapply(1:mu_t.length, function(i) {
    continuous_pos.GO.sce.W.vague(mu_t[i], mu_c.fix, sigma2,
                            n_t,n_c,
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

continuous_pos.OC.O.vague <- function(lower,upper,sigma2,
                                      n_t,r,
                                      TV, MAV, gamma_1, gamma_2,
                                      N = 10,
                                      mu_t.length = 10) {
  mu_t <- seq(lower,upper,length.out = mu_t.length)
  res <- sapply(1:mu_t.length, function(i) {
    continuous_pos.GO.sce.O.vague(mu_t[i], sigma2,
                                  n_t,r,
                                  TV, MAV, gamma_1, gamma_2,
                                  N = N)
  })
  
  res <- as.data.frame(apply(res,1,unlist))
  # View(res)
  res$mu_t <- mu_t
  graph_OC1(res, endpoint = "continuous", decision_criteria = "pos", 
            controlled_POC = FALSE)
}
# ###TEST####
# y_t <- rnorm(10,mean = 10,sd = 1)
# y_c <- rnorm(8,mean = 10,sd = 1)
# nu_t <- 10
# nu_c <- 8
# k_t <- 1
# k_c <- 1
# alpha <- 0.5
# beta <- 0.5
# continuous_pos.W(y_t,y_c,nu_t,nu_c,k_t,k_c,alpha,beta,0)
# 
# nu_t <- 1
# y_t <- rnorm(10,mean = nu_t,sd = 1)
# k_t <- 1
# alpha <- 0.5
# beta <- 0.5
# r<- 1
# continuous_pos.O(y_t,nu_t,k_t,r = 1,
#                  alpha,beta, 2)
# 

# mu_c <- 0
# mu_t <- 2
# sigma2 <- 1
# nu_t <- 2
# nu_c <- 0
# k_t <- 1
# k_c <- 1
# gamma_1 <- 0.8; gamma_2 <- 0.3
# TV <- 2;MAV <- 0
# alpha <- 0.5
# beta <- 0.5
# continuous_pos.GO.sce.W(mu_t, mu_c, sigma2,
#                         nu_t,nu_c,k_t,k_c,n_t,n_c,
#                         alpha, beta,
#                         TV, MAV, gamma_1, gamma_2,
#                         N = 1000)
# mu_t.length <- 100
# N <- 100
# mu_t <- seq(lower,upper,length.out = mu_t.length)
# res <- sapply(1:mu_t.length, function(i) {
#   continuous_pos.GO.sce.W(mu_t[i], mu_c.fix, sigma2,
#                           nu_t,nu_c,k_t,k_c,n_t,n_c,
#                           alpha, beta, 
#                           TV, MAV, gamma_1, gamma_2,
#                           N = N)
# })
# continuous_pos.GO.sce.O(mu_t, sigma2,
#                         nu_t,k_t,n_t,r,
#                         alpha, beta,
#                         TV, MAV, gamma_1, gamma_2,
#                         N = 1000)
# lower <- -1
# upper <- 3
# mu_c.fix <- 0
# sigma2 <- 1
# nu_t <- 2
# nu_c <- 0
# k_t <- 1
# k_c <- 1
# gamma_1 <- 0.8; gamma_2 <- 0.3
# TV <- 2;MAV <- 0
# alpha <- 0.5
# beta <- 0.5
# continuous_pos.OC.W (lower,upper,mu_c.fix,sigma2,
#                      nu_t,nu_c,k_t,k_c,
#                      alpha, beta, 
#                      n_t,n_c,
#                      TV, MAV, gamma_1, gamma_2,
#                      N = 100,
#                      mu_t.length = 100)
