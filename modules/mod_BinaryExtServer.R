BinaryExtServer <- function(id = "BinaryExt") {
  retModule <- function(input,output,session) {
    lDataValues <- reactiveValues(dfAll = NULL) #This returns the result
  
  observeEvent(input$Calculate_pos,
               ({
                 #Calculate probabilities with controls
                 
                 ns <- session$ns
                 shinyBS::updateButton(session,
                                       inputId = ns("Calculate_pos"), label = "Calculating", size = "default", disabled = TRUE)
                 #TBD: implementing the posterior probability calculation of uncontrolled PoC Study
                 n_t <- input$n_t
                 n_c <- input$n_c
                 y_t <- input$y_t_pos
                 y_c <- input$y_c_pos
                 alpha_t <- input$alpha_t
                 beta_t <- input$beta_t
                 alpha_c <- input$alpha_c
                 beta_c <- input$beta_c
                 TV <- input$theta_0
                 MAV <- input$theta_1
                 a_0t <- input$a_0t
                 a_0c <- input$a_0c
                 y_et <- input$y_et
                 y_ec <- input$y_ec
                 n_et <- input$n_et
                 n_ec <- input$n_ec
                 
                 lDataValues$Probability_pos <- c("TV" = binary_pos.E(n_t,n_c,n_et,n_ec,
                                                                      y_t,y_c,y_et,y_ec,
                                                                      alpha_t, alpha_c,beta_t,beta_c,
                                                                      a_0t,a_0c,
                                                                      TV),
                                                  "MAV" = binary_pos.E(n_t,n_c,n_et,n_ec,
                                                                       y_t,y_c,y_et,y_ec,
                                                                       alpha_t, alpha_c,beta_t,beta_c,
                                                                       a_0t,a_0c,
                                                                       MAV))
                 # browser()
                 shinyBS::updateButton(session,
                                       inputId = ns("Calculate_pos"), label = "Calculate", size = "default", disabled = FALSE)
               }))
  output$Probability_pos <- renderText({
    if (!is.null(lDataValues$Probability_pos)) {
      paste("The probability of exceeding TV is: ",lDataValues$Probability_pos["TV"],
            "\n",
            "The probability of exceeding MAV is: ", lDataValues$Probability_pos["MAV"],
            sep = "")
    }
  })
  observeEvent(input$Calculate_pred,
               ({
                 #Calculate probabilities with controls
                 
                 ns <- session$ns
                 shinyBS::updateButton(session,
                                       inputId = ns("Calculate_pred"), label = "Calculating", size = "default", disabled = TRUE)
                 #TBD: implementing the posterior probability calculation of uncontrolled PoC Study
                 n_t <- input$n_t
                 n_c <- input$n_c
                 y_t <- input$y_t_pred
                 y_c <- input$y_c_pred
                 alpha_t <- input$alpha_t
                 beta_t <- input$beta_t
                 alpha_c <- input$alpha_c
                 beta_c <- input$beta_c
                 TV <- input$theta_null
                 MAV <- input$theta_null
                 m_t <- input$m_t_pred
                 m_c <- input$m_c_pred
                 
                 a_0t <- input$a_0t
                 a_0c <- input$a_0c
                 y_et <- input$y_et
                 y_ec <- input$y_ec
                 n_et <- input$n_et
                 n_ec <- input$n_ec
                 lDataValues$Probability_pred <- c("TV" = binary_pred.E(
                   m_t,m_c,
                   alpha_t, beta_t, alpha_c, beta_c,a_0t,a_0c,
                   y_t,y_c,y_et,y_ec,n_t,n_c,n_et,n_ec,TV),
                   "MAV" = binary_pred.E(
                     m_t,m_c,
                     alpha_t, beta_t, alpha_c, beta_c,a_0t,a_0c,
                     y_t,y_c,y_et,y_ec,n_t,n_c,n_et,n_ec,MAV))
                 # browser()
                 shinyBS::updateButton(session,
                                       inputId = ns("Calculate_pred"), label = "Calculate", size = "default", disabled = FALSE)
               }))
  
  output$Probability_pred <- renderText({
    if (!is.null(lDataValues$Probability_pred)) {
      paste("The probability of exceeding TV is: ",lDataValues$Probability_pred["TV"],
            "\n",
            "The probability of exceeding MAV is: ", lDataValues$Probability_pred["MAV"],
            sep = "")
    }
  })
  
  observeEvent(input$Generate_pos,
               {
                 ns <- session$ns
                 shinyBS::updateButton(session,
                                       inputId = ns("Generate_pos"), label = "Generating", size = "default", disabled = TRUE)
                 #TBD: implementing the OC of controlled PoC Study
                 pi_c.fix <- input$pi_c
                 n_t <- input$n_t
                 n_c <- input$n_c
                 alpha_t <- input$alpha_t
                 beta_t <- input$beta_t
                 alpha_c <- input$alpha_c
                 beta_c <- input$beta_c
                 TV <- input$theta_0
                 MAV <- input$theta_1
                 gamma_1 <- input$gamma_1
                 gamma_2 <- input$gamma_2
                 
                 a_0t <- input$a_0t
                 a_0c <- input$a_0c
                 y_et <- input$y_et
                 y_ec <- input$y_ec
                 n_et <- input$n_et
                 n_ec <- input$n_ec
                 lDataValues$plot_OC_pos <- binary_pos.OC.E(pi_c.fix,
                                                            alpha_t, alpha_c,beta_t, beta_c,a_0t,a_0c,#a is the power
                                                            n_t, n_c, n_et,n_ec,
                                                            y_et,y_ec,
                                                            TV, MAV, gamma_1, gamma_2,
                                                            # N = 10,
                                                            pi_t.length = 10)
                 
                 pi_t_seq <- input$select_pi_t
                 pi_t_seq <- as.numeric(strsplit(pi_t_seq,split = ",")[[1]])
                 res <- sapply(1:length(pi_t_seq), function(i) {
                   binary_pos.GO.sce.E(pi_t_seq[i], pi_c.fix, 
                                       alpha_t, alpha_c,
                                       beta_t, beta_c,a_0t,a_0c,#a is the power
                                       n_t, n_c, n_et, n_ec,
                                       y_et,y_ec,
                                       TV, MAV, gamma_1, gamma_2)
                   
                 })
                 res <- as.data.frame(apply(res,1,unlist))
                 res$pi_t <- pi_t_seq
                 lDataValues$table_OC_pos <- res[,c(4,1,2,3)]
                 # colnames(lDataValues$table_OC_pos) <- c("\\(\\pi_t\\)",
                 #                                        "\\(P(Go|\\pi_t,\\pi_c)\\)",
                 #                                        "\\(P(NoGo|\\pi_t,\\pi_c)\\)",
                 #                                        "\\(P(Grey|\\pi_t,\\pi_c)\\)")
                 shinyBS::updateButton(session,
                                       inputId = ns("Generate_pos"), label = "Generate", size = "default", disabled = FALSE)
               })
  output$OC_pos <- renderPlot({
    lDataValues$plot_OC_pos 
  })
  output$OC_pos_table <- renderTable({
    lDataValues$table_OC_pos
  },
  include.colnames = TRUE)
  observeEvent(input$Generate_pred,
               {
                 ns <- session$ns
                 shinyBS::updateButton(session,
                                       inputId = ns("Generate_pred"), label = "Generating", size = "default", disabled = TRUE)
                 #TBD: implementing the OC of controlled PoC Study
                 pi_c.fix <- input$pi_c
                 n_t <- input$n_t
                 n_c <- input$n_c
                 m_t <- input$m_t_pred
                 m_c <- input$m_c_pred
                 alpha_t <- input$alpha_t
                 beta_t <- input$beta_t
                 alpha_c <- input$alpha_c
                 beta_c <- input$beta_c
                 TV <- input$theta_null
                 MAV <- input$theta_null
                 gamma_1 <- input$gamma_1
                 gamma_2 <- input$gamma_2
                 
                 a_0t <- input$a_0t
                 a_0c <- input$a_0c
                 y_et <- input$y_et
                 y_ec <- input$y_ec
                 n_et <- input$n_et
                 n_ec <- input$n_ec
                 
                 lDataValues$plot_OC_pred <- binary_pred.OC.E(pi_c.fix,
                                                               n_t, n_c, n_et,n_ec,
                                                               y_et,y_ec,
                                                               m_t,m_c,
                                                               alpha_t, alpha_c,beta_t, beta_c,a_0t,a_0c,#a is the power
                                                               TV, MAV, gamma_1, gamma_2,
                                                               N = input$N,
                                                               pi_t.length = 10)
                
                 pi_t_seq <- input$select_pi_t
                 pi_t_seq <- as.numeric(strsplit(pi_t_seq,split = ",")[[1]])
                 res <- sapply(1:length(pi_t_seq), function(i) {
                   binary_pred.GO.sce.E(pi_t_seq[i],pi_c.fix,
                                        n_t,n_c,n_et,n_ec,
                                        y_et,y_ec,
                                        m_t,m_c,
                                        alpha_t, beta_t,
                                        alpha_c,beta_c,a_0t,a_0c,#a is the power
                                        TV,MAV,gamma_1,gamma_2, N = input$N) 
                   
                 })
                 res <- as.data.frame(apply(res,1,unlist))
                 res$pi_t <- pi_t_seq
                 lDataValues$table_OC_pred <- res[,c(4,1,2,3)]
                 # colnames(lDataValues$table_OC_pred) <- c("\\(\\pi_t\\)",
                 #                                         "\\(P(Go|\\pi_t,\\pi_c)\\)",
                 #                                         "\\(P(NoGo|\\pi_t,\\pi_c)\\)",
                 #                                         "\\(P(Grey|\\pi_t,\\pi_c)\\)")
                 #---The Second OC for Predictive Probability Only
                 y_c <- input$y_c_h; theta_null <- input$theta_null
                 y_t <- seq(0,n_t,length.out = 10)
                 
                 #TBD:
                 x <- uniroot(function(y) {
                   binary_pred.E(m_t,m_c,
                                 alpha_t, beta_t,
                                 alpha_c,beta_c,a_0t,a_0c,#a is the power
                                 y,y_c,y_et,y_ec,
                                 n_t,n_c,n_et,n_ec,
                                 theta = theta_null, N = 10000)  - gamma_1
                 } , c(0, n_t), tol = 0.0001)$root
                 x_01 <- x-y_c

                 x <- uniroot(function(y) {
                   binary_pred.E(m_t,m_c,
                                 alpha_t, beta_t,
                                 alpha_c,beta_c,a_0t,a_0c,#a is the power
                                 y,y_c,y_et,y_ec,
                                 n_t,n_c,n_et,n_ec,
                                 theta = theta_null, N = 10000)  - gamma_2
                 } , c(0, n_t), tol = 0.0001)$root
                 x_02 <- x-y_c

                 y_t <- c(y_t,x_01+y_c,x_02+y_c)
                 obs_diff <- y_t - y_c;
                 pred.prob <- sapply(y_t, function(y) {
                   binary_pred.E(m_t,m_c,
                                 alpha_t, beta_t,
                                 alpha_c,beta_c,a_0t,a_0c,#a is the power
                                 y,y_c,y_et,y_ec,
                                 n_t,n_c,n_et,n_ec,
                                 theta = theta_null, N = 10000)

                 })
                 pred.prob <- cbind(y_t,pred.prob,obs_diff)

                 # # lDataValues$plot_OC2_pred <- ggplot(data = pred.prob, aes(x = obs_diff)) + 
                 # #   geom_line(aes(y = pred.prob),linewidth = 0.7)+
                 # #   geom_point(aes(y = pred.prob),shape = 17,size = 2) +
                 # #   segment(gamma_1) +
                 # #   segment(gamma_2) +
                 # #   theme_bw() +
                 # #   xlab(latex2exp::TeX("Observed $y_t - y_c$")) +
                 # #   ylab(latex2exp::TeX("Predictive Probability")) +
                 # #   ggtitle("Decision Criteria based on Predictive Probability of Phase 3 Success")
                 # 
                 lDataValues$plot_OC2_pred <- graph_OC2(pred.prob,"binary",TRUE,gamma_1,gamma_2,x_01,x_02)
                 
                 
                 shinyBS::updateButton(session,
                                       inputId = ns("Generate_pred"), label = "Generate", size = "default", disabled = FALSE)
               })
  #TBD: also render the P(Go)
  #TBD: include the related information
  output$OC_pred <- renderPlot({
    lDataValues$plot_OC_pred 
  })
  output$OC2_pred <- renderPlot({
    lDataValues$plot_OC2_pred 
  })
  output$OC_pred_table <- renderTable({
    lDataValues$table_OC_pred
  },
  include.colnames = TRUE)
}
  retServer <- moduleServer(id,module = retModule)
  return(retServer)
}