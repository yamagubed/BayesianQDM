library(ggplot2)
segment <- function(gamma,x_0) {
  list(
    geom_segment(aes(x = x_0, xend = x_0, y = -Inf, yend = gamma),
                 linetype = 2),
    geom_segment(aes(x = -Inf, xend = x_0, y = gamma, yend = gamma),
                 linetype = 2)
  )
}
graph_OC2 <- function(pred.prob,#With three columns
                      endpoint = "binary", #or continuous
                      controlled_POC = TRUE,#or FALSE for uncontrolled PoC
                      gamma_1 = 0.3,gamma_2 = 0.15,x_01 = NA,x_02 = NA) { 
  basics <- function(endpoint = "binary", controlled_POC = TRUE) {
    if (endpoint == "binary") {
      if (controlled_POC) {
        xlab_content <- latex2exp::TeX("Observed $y_t - y_c$")#TBD: whether it should be ratios
      } else {
        xlab_content <- latex2exp::TeX("Observed $y_t$")
      }
    }
    else if (endpoint == "continuous") {
      if (controlled_POC) {
        xlab_content <- latex2exp::TeX("Observed $\\bar y_t - \\bar y_c$")
      } else {
        xlab_content <- latex2exp::TeX("Observed $\\bar y_t$")
      }
    }
    
    basic_graph_setting <- list(
      xlab(xlab_content),
      ylab(latex2exp::TeX("Predictive Probability")),
      ggtitle("Decision Criteria based on Predictive Probability of Phase 3 Success"),
      theme_bw()
    )
    basic_graph_setting
  } #End basics
  
  if (controlled_POC) {
    p <- ggplot(data = pred.prob, aes(x = obs_diff)) + 
      basics(endpoint,TRUE)
  } else {
    p <- ggplot(data = pred.prob, aes(x = y_t)) + 
      basics(endpoint,FALSE)
    # if (endpoint == "binary") {
    #   p <- ggplot(data = pred.prob, aes(x = y_t)) + 
    #     basics(endpoint,FALSE)
    # } else if (endpoint == "continuous"){
    #   p <- ggplot(data = pred.prob, aes(x = y_t)) + 
    #     basics(endpoint,FALSE)
  # }
  }
  
  p + geom_line(aes(y = pred.prob),linewidth = 0.7)+
    geom_point(aes(y = pred.prob),shape = 17,size = 2)+
    segment(gamma_1,x_01)+
    segment(gamma_2,x_02) -> p
  p
}
  
  