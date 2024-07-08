library(ggplot2)
graph_OC1 <- function(
    res,
    endpoint = "binary",#or continuous
    decision_criteria = "pred",#or "pos"
    controlled_POC = TRUE) {#Controlled PoC, false if uncontrolled PoC
  basics <- function(endpoint = "binary",controlled_POC = TRUE){
    if (endpoint == "binary") {
      if (controlled_POC) {
        xlab_content <- latex2exp::TeX("$\\pi_t - \\pi_c$")
      } else {
        xlab_content <- latex2exp::TeX("$\\pi_t$")
      }
    } else if (endpoint == "continuous") {
      if (controlled_POC) {
        xlab_content <- latex2exp::TeX("$\\mu_t - \\mu_c$")
      } else {
        xlab_content <- latex2exp::TeX("$\\mu_t$")
      }
    }
    
    basic_graph_setting <- list(
      scale_colour_manual(name = "Decision",
                          labels = c("Go", "Grey","NoGo"),
                          values = c("green", "grey", "red")),    
      scale_shape_manual(name = "Decision",
                         labels = c("Go", "NoGo","Grey"),
                         values = c(17,18,19)),
      theme_bw(),
      ylab(latex2exp::TeX("Probability")),
      ggtitle("Operational Characteristic"),
      xlab(xlab_content)
    )
    basic_graph_setting
  }
  if (controlled_POC) {
    p <- ggplot(data = res, aes(x = diff)) + 
      basics(endpoint,TRUE)
  } else {
    if (endpoint == "binary") {
      p <- ggplot(data = res, aes(x = pi_t)) + 
        basics(endpoint,FALSE)
    } else if (endpoint == "continuous")
      p <- ggplot(data = res, aes(x = mu_t)) + 
        basics(endpoint,FALSE)
  }
  p + geom_line(aes(y = prob.go.sce,color = "green"),linewidth = 0.7)+
    geom_point(aes(y = prob.go.sce,color = "green"),shape = 17,size = 2) +
    geom_line(aes(y = prob.nogo.sce,color = "red"),linewidth = 0.7)+
    geom_point(aes(y = prob.nogo.sce,color = "red"),shape = 18,size = 2) +
    geom_line(aes(y = prob.grey.sce,color = "grey"),linewidth = 0.7)+
    geom_point(aes(y = prob.grey.sce,color = "grey"),shape = 19,size = 2)
}