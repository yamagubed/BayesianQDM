#Problem of difference of two betas
alpha_1 <- 5
alpha_2 <- 0.5
beta_1 <- 5
beta_2 <- 0.5
pi <- 0.00001
gamma <- alpha_1 + beta_1 + alpha_2 + beta_2 - 2
F1(beta_1,gamma,1-alpha_1,beta_1+alpha_2,1 - 0.00001,1-pi**2) #divergent

d_beta.density(0.001,5,0.5,5,0.5) #density is really large
d_beta.density(-0.1,5,0.5,5,0.5)

alpha_t <- 0.5; beta_t <- 0.5; alpha_c <- 0.5; beta_c <- 0.5
y_t <- 7; n_t <- 12
y_c <- 9; n_c <- 15
theta <- 0.5
binary_pos.W(n_t,n_c,y_t,y_c,alpha_t, alpha_c,beta_t, beta_c,theta)
cubintegrate(d_beta.density,lower = 0.15,upper = 1,
             alpha_1 = alpha_t+y_t, alpha_2 = alpha_c + y_c,
             beta_1 = beta_t + n_t - y_t, beta_2 = beta_c + n_c - y_c)#integral is larger than one!
pcubature(d_beta.density,lower = 0.15,upper = 1,
             alpha_1 = alpha_t+y_t, alpha_2 = alpha_c + y_c,
             beta_1 = beta_t + n_t - y_t, beta_2 = beta_c + n_c - y_c)#integral is larger than one!
cubintegrate(d_beta.density,lower = 0.1,upper = 0.2,
             alpha_1 = 0.5, alpha_2 = 0.5,
             beta_1 = 0.5, beta_2 = 0.5)#integral is larger than one!

prob <- sapply(seq(-1,1,length.out = 100), function(x){
  d_beta.density(x,alpha_1 = alpha_t+y_t, alpha_2 = alpha_c + y_c,
                 beta_1 = beta_t + n_t - y_t, beta_2 = beta_c + n_c - y_c)
})

plot(x = seq(-1,1,length.out = 100), y = prob)#Density plot is also very large

a1<-alpha_t+y_t
b1<-beta_t + n_t - y_t
a2<-alpha_c + y_c
b2<-beta_c + n_c - y_c

d_beta.density <- function(pi, alpha_1, alpha_2, beta_1, beta_2) {
  k <- 1/(beta(alpha_1,beta_1)*beta(alpha_2,beta_2))
  gamma <- alpha_1+alpha_2+beta_1+beta_2-2
  if (pi < 0 & pi >= -1) { # -1 \leq pi < 0
    return((k*beta(alpha_1,beta_2)*((-pi)**(beta_1+beta_2-1))*((1+pi)**(alpha_1+beta_2-1))*F1(beta_2,1-alpha_2,gamma,alpha_1+beta_2,1-pi^2,1+pi)))
  } else if (pi >= 0 & pi < 1) { #0 \leq pi < 1
    if (pi == 0 & alpha_1 + alpha_2 > 1 & beta_1 + beta_2 > 1) {
      return(k*beta(alpha_1 + alpha_2 - 1, beta_1 + beta_2 - 1))
    } else {
      return(k*beta(alpha_2,beta_1)*((pi)**(beta_1+beta_2-1))*((1-pi)**(alpha_2+beta_1-1))*F1(beta_1,gamma,1-alpha_1,beta_1+alpha_2,1-pi,1-pi**2))
    }
  } else {
    return(0)
  }
}

done<-function(x){
  if (x>0 & x<=1){beta(a2,b1)/beta(a1,b1)/beta(a2,b2)*x^(b1+b2-1)*(1-x)^(a2+b1-1)*F1(b1,a1+b1+a2+b2-2,1-a1,b1+a2,1-x,1-x^2)}
  else if (x<0 & x>=-1) {beta(a1,b2)/beta(a1,b1)/beta(a2,b2)*(-x)^(b1+b2-1)*(1+x)^(a1+b2-1)*F1(b2,1-a2,a1+b1+a2+b2-2,a1+b2,1-x^2,1+x)}
  else if (x==0 & a1+a2>1 & b1+b2>1) {beta(a1+a2-1,b1+b2-1)/beta(a1,b1)/beta(a2,b2)}
  else {0}
}
d_beta.density(0.01,alpha_1 = a1,beta_1 = b1,alpha_2 = a2,beta_2 = b2)
done(0.01)
