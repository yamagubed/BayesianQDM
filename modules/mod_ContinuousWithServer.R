ContinuousWithServer <- function(id = "ContinuousWith") {
  retModule <- function(input,output,session) {
    lDataValues <- reactiveValues(dfAll = NULL) #This returns the result
    
    observeEvent(input$Calculate_pos,
                 ({
                   #Calculate posterior probabilities with controls
                   
                   ns <- session$ns
                   shinyBS::updateButton(session,
                                         inputId = ns("Calculate_pos"), label = "Calculating", size = "default", disabled = TRUE)
                   #TBD: implementing the posterior probability calculation of controlled PoC Study
                   
                   n_t <- input$n_t
                   n_c <- input$n_c
                   y_bar_t <- input$y_bar_t_pos
                   y_bar_c <- input$y_bar_c_pos
                   s_t <- input$s_t_pos
                   s_c <- input$s_c_pos
                   TV <- input$theta_0
                   MAV <- input$theta_1
                   # browser()
                   
                   if (input$prior == 'Normal-Inverse-Chi-Squared Prior') {
                     mu_0t <- input$mu_0t
                     mu_0c <- input$mu_0c
                     k_0t <- input$k_0t;k_0c <- input$k_0c
                     nu_0t <- input$nu_0t; nu_0c <- input$nu_0c
                     sigma2_0t <- input$sigma2_0t; sigma2_0c <- input$sigma2_0c
                     # browser()
                     lDataValues$Probability_pos["TV"] <- continuous_pos.W(n_t,n_c,
                                                                           y_bar_t,y_bar_c,
                                                                           s_t,s_c,
                                                                           mu_0t,mu_0c,k_0t,k_0c,
                                                                           nu_0t,nu_0c,sigma2_0t,sigma2_0c,
                                                                           TV) 
                     lDataValues$Probability_pos["MAV"] <- continuous_pos.W(n_t,n_c,
                                                                            y_bar_t,y_bar_c,
                                                                            s_t,s_c,
                                                                            mu_0t,mu_0c,k_0t,k_0c,
                                                                            nu_0t,nu_0c,sigma2_0t,sigma2_0c, 
                                                                            MAV)  
                   } else if (input$prior == 'Vague Prior') {
                     lDataValues$Probability_pos["TV"] <- continuous_pos.W.vague(n_t,n_c,
                                                                                   y_bar_t,y_bar_c,
                                                                                   s_t,s_c,
                                                                                   TV) 
                     lDataValues$Probability_pos["MAV"] <- continuous_pos.W.vague(n_t,n_c,
                                                                                    y_bar_t,y_bar_c,
                                                                                    s_t,s_c,
                                                                                    MAV)  
                   }
                   shinyBS::updateButton(session,
                                         inputId = ns("Calculate_pos"), label = "Calculate", size = "default", disabled = FALSE)
                 }))
    
    observeEvent(input$Calculate_pred,
                 {
                   #Calculate predictive probabilities 
                   ns <- session$ns
                   shinyBS::updateButton(session,
                                         inputId = ns("Calculate_pred"), label = "Calculating", size = "default", disabled = TRUE)
                   #TBD: implementing the posterior probability calculation of controlled PoC Study
                   n_t <- input$n_t
                   n_c <- input$n_c
                   y_bar_t <- input$y_bar_t_pred
                   y_bar_c <- input$y_bar_c_pred
                   s_t <- input$s_t_pred
                   s_c <- input$s_c_pred
                   m_t <- input$m_t
                   m_c <- input$m_c
                   TV <- input$theta_0
                   MAV <- input$theta_1
                   # browser()
                   if (input$prior == 'Normal-Inverse-Chi-Squared Prior') {
                     nu_t <- input$nu_t
                     nu_c <- input$nu_c
                     k_t <- input$k_t
                     k_c <- input$k_c
                     alpha <- input$alpha
                     beta <- input$beta
                     
                     lDataValues$Probability_pred["TV"] <- continuous_pred.W(n_t,n_c,
                                                                             y_bar_t,y_bar_c,
                                                                             s_t,s_c,
                                                                             m_t,m_c,
                                                                             nu_t,nu_c,k_t,k_c,alpha,beta, 
                                                                             TV) 
                     lDataValues$Probability_pred["MAV"] <- continuous_pred.W(n_t,n_c,
                                                                              y_bar_t,y_bar_c,
                                                                              s_t,s_c,
                                                                              m_t,m_c,
                                                                              nu_t,nu_c,k_t,k_c,alpha,beta, 
                                                                              MAV)  
                   } else if (input$prior == 'Vague Prior') {
                     lDataValues$Probability_pred["TV"] <- continuous_pred.W.vague(n_t,n_c,
                                                                             y_bar_t,y_bar_c,
                                                                             s_t,s_c,
                                                                             m_t,m_c,
                                                                             TV) 
                     lDataValues$Probability_pred["MAV"] <- continuous_pred.W.vague(n_t,n_c,
                                                                              y_bar_t,y_bar_c,
                                                                              s_t,s_c,
                                                                              m_t,m_c,
                                                                              MAV) 
                     
                   }
                   
                   shinyBS::updateButton(session,
                                         inputId = ns("Calculate_pred"), label = "Calculate", size = "default", disabled = FALSE)
                 })
    
    output$Probability_pos <- renderText({
      if (!is.null(lDataValues$Probability_pos)) {
        paste("The probability of exceeding TV is: ",lDataValues$Probability_pos["TV"],
              "\n",
              "The probability of exceeding MAV is: ", lDataValues$Probability_pos["MAV"],
              sep = "")
      }
    })
    
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
                   #TBD: implementing the OC using posterior probability
                   lower <- input$lower
                   upper <- input$upper
                   mu_c.fix <- input$mu_c.fix
                   sigma2 <- input$sigma2
                   n_t <- input$n_t
                   n_c <- input$n_c
                   TV <- input$theta_0
                   MAV <- input$theta_1
                   gamma_1 <- input$gamma_1
                   gamma_2 <- input$gamma_2
                   mu_t_seq <- input$select_mu_t
                   mu_t_seq <- as.numeric(strsplit(mu_t_seq,split = ",")[[1]])
                   sigma2_t <- input$sigma2_t;sigma2_c <- input$sigma2_c
                   # browser()
                   if (input$prior == 'Normal-Inverse-Chi-Squared Prior') {
                     mu_0t <- input$mu_0t
                     mu_0c <- input$mu_0c
                     k_0t <- input$k_0t;k_0c <- input$k_0c
                     nu_0t <- input$nu_0t; nu_0c <- input$nu_0c
                     sigma2_0t <- input$sigma2_0t; sigma2_0c <- input$sigma2_0c
                     # browser()
                     lDataValues$plot_OC_pos <- continuous_pos.OC.W(lower,upper,mu_c.fix,sigma2_t,sigma2_c,
                                                                    mu_0t,mu_0c,k_0t,k_0c,
                                                                    nu_0t,nu_0c,sigma2_0t,sigma2_0c,
                                                                    n_t,n_c,
                                                                    TV, MAV, gamma_1, gamma_2,
                                                                    N = input$N,
                                                                    mu_t.length = 10) 
                     res <- sapply(1:length(mu_t_seq), function(i) {
                       continuous_pos.GO.sce.W(mu_t_seq[i], mu_c.fix, sigma2_t,sigma2_c,
                                               mu_0t,mu_0c,k_0t,k_0c,
                                               nu_0t,nu_0c,sigma2_0t,sigma2_0c,
                                               n_t,n_c,
                                               TV, MAV, gamma_1, gamma_2,
                                               N = input$N)
                     })
                     
                   } else if (input$prior == 'Vague Prior') {
                     lDataValues$plot_OC_pos <- continuous_pos.OC.W.vague(lower,upper,mu_c.fix,sigma2,
                                                                          n_t,n_c,
                                                                          TV, MAV, gamma_1, gamma_2,
                                                                          N = input$N,
                                                                          mu_t.length = 10) 
                     res <- sapply(1:length(mu_t_seq), function(i) {
                       continuous_pos.GO.sce.W.vague(mu_t_seq[i], mu_c.fix, sigma2,
                                                     n_t,n_c,
                                                     TV, MAV, gamma_1, gamma_2,
                                                     N = input$N)
                     })
                     
                     
                   }
                   res <- as.data.frame(apply(res,1,unlist))
                   res$mu_t <- mu_t_seq
                   lDataValues$table_OC_pos <- res[,c(4,1,2,3)]
                   
                   shinyBS::updateButton(session,
                                         inputId = ns("Generate_pos"), label = "Generate", size = "default", disabled = FALSE)
                 })
    
    observeEvent(input$Generate_pred, {
      ns <- session$ns
      shinyBS::updateButton(session,
                            inputId = ns("Generate_pred"), label = "Generating", size = "default", disabled = TRUE)
      #TBD: implementing the OC using predictive probability
      
      lower <- input$lower
      upper <- input$upper
      mu_c.fix <- input$mu_c.fix
      sigma2 <- input$sigma2
      m_t <- input$m_t
      m_c <- input$m_c
      n_t <- input$n_t
      n_c <- input$n_c
      TV <- input$theta_0
      MAV <- input$theta_1
      gamma_1 <- input$gamma_1
      gamma_2 <- input$gamma_2
      mu_t_seq <- input$select_mu_t
      mu_t_seq <- as.numeric(strsplit(mu_t_seq,split = ",")[[1]])
      if (input$prior == 'Normal-Inverse-Chi-Squared Prior') {
        nu_t <- input$nu_t
        nu_c <- input$nu_c
        k_t <- input$k_t
        k_c <- input$k_c
        alpha <- input$alpha
        beta <- input$beta
        lDataValues$plot_OC_pred <- continuous_pred.OC.W(lower,upper,mu_c.fix,sigma2,
                                                         nu_t,nu_c,k_t,k_c,
                                                         alpha,beta,
                                                         n_t,n_c,
                                                         m_t,m_c,
                                                         TV, MAV, gamma_1, gamma_2,
                                                         N = input$N,
                                                         mu_t.length = 10) 
        res <- sapply(1:length(mu_t_seq), function(i) {
          continuous_pred.GO.sce.W(mu_t_seq[i], mu_c.fix, sigma2,
                                   nu_t,nu_c,k_t,k_c,n_t,n_c,
                                   m_t,m_c,
                                   alpha, beta, 
                                   TV, MAV, gamma_1, gamma_2,
                                   N = input$N)
        })
        
      } else if (input$prior == 'Vague Prior') {
        lDataValues$plot_OC_pred <- continuous_pred.OC.W.vague(lower,upper,mu_c.fix,sigma2,
                                                               n_t,n_c,
                                                               m_t,m_c,
                                                               TV, MAV, gamma_1, gamma_2,
                                                               N = input$N,
                                                               mu_t.length = 10) 
        res <- sapply(1:length(mu_t_seq), function(i) {
          continuous_pred.GO.sce.W.vague(mu_t_seq[i], mu_c.fix, sigma2,
                                         n_t,n_c,
                                         m_t,m_c,
                                         TV, MAV, gamma_1, gamma_2,
                                         N = input$N)
        })
      }
      res <- as.data.frame(apply(res,1,unlist))
      res$mu_t <- mu_t_seq
      lDataValues$table_OC_pred <- res[,c(4,1,2,3)]
      
      shinyBS::updateButton(session,
                            inputId = ns("Generate_pred"), label = "Generate", size = "default", disabled = FALSE)
    })
    #TBD: also render the P(Go)
    #TBD: include the related information
    output$OC_pos <- renderPlot({
      lDataValues$plot_OC_pos 
    })
    output$OC_pos_table <- renderTable({
      lDataValues$table_OC_pos
    })
    output$OC_pred <- renderPlot({
      lDataValues$plot_OC_pred
    })
    output$OC_pred_table <- renderTable({
      lDataValues$table_OC_pred
    })
  }
  retServer <- moduleServer(id,module = retModule)
  return(retServer)
}
