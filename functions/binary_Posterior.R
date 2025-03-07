library(cubature)
library(tolerance)

d_beta.density <- function(pi, alpha_1, alpha_2, beta_1, beta_2) {
  k <- 1/(beta(alpha_1,beta_1)*beta(alpha_2,beta_2))
  gamma <- alpha_1+alpha_2+beta_1+beta_2-2
  if (pi < 0 & pi >= -1) { # -1 \leq pi < 0
    return((k*beta(alpha_1,beta_2)*((-pi)^(beta_1+beta_2-1))*((1+pi)^(alpha_1+beta_2-1))*F1(beta_2,1-alpha_2,gamma,alpha_1+beta_2,1-pi^2,1+pi)))
  } else if (pi >= 0 & pi < 1) { #0 \leq pi < 1
    if (pi == 0 & alpha_1 + alpha_2 > 1 & beta_1 + beta_2 > 1) {
      return(k*beta(alpha_1 + alpha_2 - 1, beta_1 + beta_2 - 1))
    } else {
      return(k*beta(alpha_2,beta_1)*((pi)^(beta_1+beta_2-1))*((1-pi)^(alpha_2+beta_1-1))*F1(beta_1,gamma,1-alpha_1,beta_1+alpha_2,1-pi,1-pi^2))
    }
  } else {
    return(0)
  }
}
binary_pos.W <- function(n_t,n_c,
                         y_t,y_c,
                         alpha_t, alpha_c,
                         beta_t, beta_c,
                         theta) {
  res <- tryCatch({
    temp <- cubintegrate(d_beta.density,lower = theta,upper = 1, 
                 alpha_1 = alpha_t+y_t, alpha_2 = alpha_c + y_c,
                 beta_1 = beta_t + n_t - y_t, beta_2 = beta_c + n_c - y_c)$integral
    if (temp > 1) {
      warning("The integral is ",temp," which is larger than 1.")
    }
    temp
  },
  error = function(e) {
    message(e,
            "y_t = ",y_t, ";y_c = ", y_c, "\n")
    1#TBD: use MC approach
  },
  warning = function(w) {
    message(w,
            "y_t = ",y_t, ";y_c = ", y_c, "\n")
    1
  })
  return(res)

}

binary_pos.O <- function(n_t,
                         y_t,
                         alpha_t,
                         beta_t,
                         z,
                         theta) {
  cubintegrate(d_beta.density,lower = theta,upper = 1, 
               alpha_1 = alpha_t+y_t, alpha_2 = alpha_t + z,
               beta_1 = beta_t + n_t - y_t, beta_2 = beta_t + n_t - z)$integral
}

binary_pos.GO.sce.W <- function(pi_t, pi_c, 
                                alpha_t, alpha_c,
                                beta_t, beta_c,
                                n_t, n_c, TV, MAV, gamma_1, gamma_2) {
  #scenarior accessment for Go/No-Go decision
  prob.go.sce <- 0
  prob.nogo.sce <- 0
  for (y_t in seq(0,n_t)) {
    for (y_c in seq(0,n_c)) {
      prob.go.sce <- prob.go.sce + 
        (dbinom(y_t,n_t,pi_t)*dbinom(y_c,n_c,pi_c)) * (binary_pos.W(n_t,n_c,y_t,y_c,
                                               alpha_t, alpha_c,beta_t,beta_c,TV) >= gamma_1)
      
      prob.nogo.sce <- prob.nogo.sce + 
        (dbinom(y_t,n_t,pi_t)*dbinom(y_c,n_c,pi_c)) * (binary_pos.W(n_t,n_c,y_t,y_c,
                                                                    alpha_t, alpha_c,beta_t,beta_c, MAV) <= gamma_2)
    }
    
  }
  
  return(list(prob.go.sce = prob.go.sce, prob.nogo.sce = prob.nogo.sce,prob.grey.sce = 1 - prob.go.sce - prob.nogo.sce))
}
binary_pos.Go.sce.O <- function(pi_t,alpha_t, beta_t, n_t,
                                z,
                                TV, MAV, gamma_1, gamma_2) {
  # y_t <- rbinom(N, size = n_t, prob = pi_t)
  # y_c <- rbinom(N, size = n_t, prob = z) #hypothetical control
  prob.go.sce <- 0
  prob.nogo.sce <- 0
  for (y_t in seq(0,n_t)) {
    prob.go.sce <- prob.go.sce + 
      dbinom(y_t,n_t,pi_t) * (binary_pos.O(n_t,y_t,alpha_t,beta_t,z,TV) >= gamma_1)
    
    prob.nogo.sce <- prob.nogo.sce + 
      dbinom(y_t,n_t,pi_t) * (binary_pos.O(n_t,y_t,alpha_t,beta_t,z,MAV) <= gamma_2)
    
  }
  
  return(list(prob.go.sce = prob.go.sce, prob.nogo.sce = prob.nogo.sce,prob.grey.sce = 1 - prob.go.sce - prob.nogo.sce))
}
binary_pos.Go.W <- function(alpha_t, alpha_c,
                            beta_t, beta_c,
                            n_t, n_c, TV, MAV, gamma_1, gamma_2,
                            N = 10, M = 100) {
  pi_t <- rbeta(n = M, shape1 = alpha_t, shape2 = beta_t)
  pi_c <- rbeta(n = M, shape1 = alpha_c, shape2 = beta_c)
  res <- sapply(1:M, function(i) {
    binary_pos.GO.sce.W(pi_t[i], pi_c[i], alpha_t, alpha_c,beta_t, beta_c,n_t, n_c, 
                        TV, MAV, gamma_1, gamma_2)
  })
  return(list(prob.go = mean(unlist(res["prob.go.sce",])), 
              prob.nogo = mean(unlist(res["prob.nogo.sce",])),
              prob.grey = mean(unlist(res["prob.grey.sce",]))))
}
binary_pos.Go.O <- function(alpha_t,
                            beta_t, z,
                            n_t, TV, MAV, gamma_1, gamma_2,
                            N = 10, M = 100) {
  pi_t <- rbeta(n = M, shape1 = alpha_t, shape2 = beta_t)
  # pi_c <- rbeta(n = M, shape1 = alpha_c, shape2 = beta_c)
  res <- sapply(1:M, function(i) {
    binary_pos.Go.sce.O(pi_t[i], alpha_t, beta_t, n_t, z,TV, MAV, gamma_1, gamma_2)
  })
  return(list(prob.go = mean(unlist(res["prob.go.sce",])), 
              prob.nogo = mean(unlist(res["prob.nogo.sce",])),
              prob.grey = mean(unlist(res["prob.grey.sce",]))))
}

binary_pos.OC.W <- function(pi_c.fix,
                            alpha_t, alpha_c,beta_t, beta_c,
                            n_t, n_c, 
                            TV, MAV, gamma_1, gamma_2,
                            # N = 10,
                            pi_t.length = 10) {
  pi_t <- seq(0,1,length.out = pi_t.length)
  res <- sapply(1:pi_t.length, function(i) {
    binary_pos.GO.sce.W(pi_t[i], pi_c.fix, alpha_t, alpha_c,beta_t, beta_c,n_t, n_c, 
                        TV, MAV, gamma_1, gamma_2)
  })
  
  res <- as.data.frame(apply(res,1,unlist))
  res$pi_t <- pi_t
  res$diff <- res$pi_t - pi_c.fix
  graph_OC1(res, endpoint = "binary", decision_criteria = "pos", 
            controlled_POC = TRUE)
    
  
}

binary_pos.OC.O <- function(alpha_t,beta_t,n_t,z, TV, MAV, gamma_1, gamma_2,
                            N = 10,
                            pi_t.length = 10) {
  pi_t <- seq(0,1,length.out = pi_t.length)
  res <- sapply(1:pi_t.length, function(i) {
    binary_pos.Go.sce.O(pi_t[i], alpha_t,beta_t,n_t,z, TV, MAV, gamma_1, gamma_2)
  })
  # plot(pi_t,res["prob.go.sce",])

  res <- as.data.frame(apply(res,1,unlist))
  # View(res)
  res$pi_t <- pi_t
  graph_OC1(res, endpoint = "binary", decision_criteria = "pos", 
            controlled_POC = FALSE)
  
}

#####TEST#####
# d_beta.density(0.2,0.5,0.1,1,2)
# binary_pos.W(12,12,6,6,0.5,0.5,0.5,0.5,0.1,0.1)
# binary_pos.GO.sce.W(0.8,0.9,0.5,0.5,0.2,0.2,12,13,0.3,0.1,0.8,0.1)
# binary_pos.Go.W(0.5,0.5,0.2,0.2,12,13,0.3,0.1,0.8,0.1)
# binary_pos.Go.O(0.3,0.4,10,12,0.3,0.2,0.8,0.3)
# binary_pos.OC(0.5,0.5,0.5,0.5,0.5,12,12,0.3,0.2,0.8,0.1, N = 100)
# binary_pos.OC.O(0.5,0.5,12,6,0.3,0.1,0.8,0.2,N = 100)
# 
# binary_pos.OC.W


