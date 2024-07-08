n_t <- 12; n_c <- 12
y_bar_t <- 3; y_bar_c <- 0
s_t <- 1; s_c <- 1
m_t <- 120; m_c <- 120
nu_t <- 5; nu_c <- 0;
k_t <- 1; k_c <- 1
alpha <- 0.5; beta <- 0.5
theta <- 15

lower <- -5; upper <- 5
mu_t.length <- 10
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
ggplot(data = res, aes(x = diff)) + 
  geom_line(aes(y = prob.go.sce,color = "green"),linewidth = 0.7)+
  geom_point(aes(y = prob.go.sce,color = "green"),shape = 17,size = 2) +
  geom_line(aes(y = prob.nogo.sce,color = "red"),linewidth = 0.7)+
  geom_point(aes(y = prob.nogo.sce,color = "red"),shape = 18,size = 2) +
  geom_line(aes(y = prob.grey.sce,color = "grey"),linewidth = 0.7)+
  geom_point(aes(y = prob.grey.sce,color = "grey"),shape = 19,size = 2) +
  scale_colour_manual(name = "Decision",
                      labels = c("Go", "Grey","NoGo"),
                      values = c("green", "grey", "red")) +   
  scale_shape_manual(name = "Decision",
                     labels = c("Go", "NoGo","Grey"),
                     values = c(17,18,19)) +
  theme_bw() +
  xlab(latex2exp::TeX("$\\mu_t - \\mu_c$")) +
  ylab(latex2exp::TeX("Probability")) +
  ggtitle("Operational Characteristic")

#Binary Predictive With Control
source("functions/binary_Predictive.R")
library(ggplot2)
m_t <- 12; m_c <- 12; n_t <- 12; n_c <- 12;
alpha_t <- 0.5; alpha_c <- 0.5; beta_t<- 0.5; beta_c <- 0.5;
y_c <- 3; theta_null <- 0.15
obs_diff <- seq(-3,3,length.out = 10)
y_t <- y_c + obs_diff
pred.prob <- sapply(y_t, function(y) {
  binary_pred.W(m_t,m_c,
                alpha_t, beta_t,
                alpha_c,beta_c,
                y,y_c,
                n_t,n_c,
                theta = theta_null, N = 10000)
})
gamma_1 <- 0.3; gamma_2 <- 0.15
pred.prob <- cbind(y_t,pred.prob,obs_diff)

segment <- function(gamma) {
  x <- uniroot(function(y) {
    binary_pred.W(m_t,m_c,
                  alpha_t, beta_t,
                  alpha_c,beta_c,
                  y,y_c,
                  n_t,n_c,
                  theta = theta_null, N = 10000) - gamma
  } , c(0, n_t), tol = 0.0001)$root
  x_0 <- x-y_c
  list(
    geom_segment(aes(x = x_0, xend = x_0, y = -Inf, yend = gamma),
               linetype = 2),
    geom_segment(aes(x = -Inf, xend = x_0, y = gamma, yend = gamma),
                 linetype = 2)
    # geom_polygon(aes(x = c(x_0,x_0,-Inf,x_0),
    #                  y = c(-Inf,gamma,gamma,gamma)))
  )
}
ggplot(data = pred.prob, aes(x = obs_diff)) + 
  geom_line(aes(y = pred.prob),linewidth = 0.7)+
  geom_point(aes(y = pred.prob),shape = 17,size = 2) +
  segment(gamma_1) +
  segment(gamma_2) +
  theme_bw() +
  xlab(latex2exp::TeX("Observed $y_t - y_c$")) +
  ylab(latex2exp::TeX("Predictive Probability")) +
  ggtitle("Decision Criteria based on Predictive Probability of Phase 3 Success")

