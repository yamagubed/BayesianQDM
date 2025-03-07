ContinuousWithoutServer <- function(id = "ContinuousWithout") {
  retModule <- function(input,output,session) {
    lDataValues <- reactiveValues(dfAll = NULL) #This returns the result
    
    observeEvent(input$Calculate_pos,
                 ({
                   #Calculate posterior probabilities without controls
                   
                   ns <- session$ns
                   shinyBS::updateButton(session,
                                         inputId = ns("Calculate_pos"), label = "Calculating", size = "default", disabled = TRUE)
                   #TBD: implementing the posterior probability calculation of uncontrolled PoC Study
                   
                   n_t <- input$n_t
                   y_bar_t <- input$y_bar_t_pos
                   s_t <- input$s_t_pos
                   TV <- input$theta_0
                   MAV <- input$theta_1
                   r <- input$r
                   mu_0c <- input$mu_0c
                   # browser()
                   
                   if (input$prior == 'Normal-Inverse-Chi-Squared Prior') {
                     mu_0t <- input$mu_0t
                     k_0t <- input$k_0t
                     nu_0t <- input$nu_0t
                     sigma2_0t <- input$sigma2_0t
                     lDataValues$Probability_pos["TV"] <- continuous_pos.O(n_t,y_bar_t,s_t,
                                                                           mu_0c, r,
                                                                           mu_0t,k_0t, nu_0t,sigma2_0t,
                                                                           TV)
                     lDataValues$Probability_pos["MAV"] <- continuous_pos.O(n_t,y_bar_t,s_t,
                                                                            mu_0c, r,
                                                                            mu_0t,k_0t, nu_0t,sigma2_0t,
                                                                            MAV)
                   } else if (input$prior == 'Vague Prior') {
                     lDataValues$Probability_pos["TV"] <- continuous_pos.O.vague(n_t,
                                                                                 y_bar_t,
                                                                                 s_t,mu_0c,r,
                                                                                 TV)
                     lDataValues$Probability_pos["MAV"] <- continuous_pos.O.vague(n_t,
                                                                                  y_bar_t,
                                                                                  s_t,mu_0c,r,
                                                                                  MAV)
                   }
                   shinyBS::updateButton(session,
                                         inputId = ns("Calculate_pos"), label = "Calculate", size = "default", disabled = FALSE)
                   
                 }))
    observeEvent(input$Calculate_pred,
                 {
                   #Calculate predictive probabilities without controls
                   ns <- session$ns
                   shinyBS::updateButton(session,
                                         inputId = ns("Calculate_pred"), label = "Calculating", size = "default", disabled = TRUE)
                   #TBD: implementing the posterior probability calculation of uncontrolled PoC Study
                   n_t <- input$n_t
                   y_bar_t <- input$y_bar_t_pred
                   s_t <- input$s_t_pred
                   m_t <- input$m_t
                   m_c <- input$m_c
                   TV <- input$theta_null
                   MAV <- input$theta_null
                   r <- input$r
                   mu_0c <- input$mu_0c
                   # browser()
                   if (input$prior == 'Normal-Inverse-Chi-Squared Prior') {
                     mu_0t <- input$mu_0t
                     k_0t <- input$k_0t
                     nu_0t <- input$nu_0t
                     sigma2_0t <- input$sigma2_0t
                     lDataValues$Probability_pred["TV"] <- continuous_pred.O(n_t,m_t,m_c,
                                                                             y_bar_t,s_t,mu_0c,r,
                                                                             mu_0t, k_0t, nu_0t, sigma2_0t,
                                                                             TV)
                     lDataValues$Probability_pred["MAV"] <- continuous_pred.O(n_t,m_t,m_c,
                                                                              y_bar_t,s_t,mu_0c,r,
                                                                              mu_0t, k_0t, nu_0t, sigma2_0t,
                                                                              MAV)
                   } else if (input$prior == 'Vague Prior') {
                     lDataValues$Probability_pred["TV"] <- continuous_pred.O.vague(n_t,
                                                                                   y_bar_t,
                                                                                   s_t,
                                                                                   m_t,m_c,mu_0c,r,
                                                                                   TV)
                     lDataValues$Probability_pred["MAV"] <- continuous_pred.O.vague(n_t,
                                                                                    y_bar_t,
                                                                                    s_t,
                                                                                    m_t,m_c,mu_0c,r,
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
                   #TBD: implementing the OC using posterior probability for uncontrolled PoC
                   lower <- input$lower
                   upper <- input$upper
                   sigma2_t <- input$sigma2_t
                   n_t <- input$n_t
                   TV <- input$theta_0
                   MAV <- input$theta_1
                   gamma_1 <- input$gamma_1
                   gamma_2 <- input$gamma_2
                   mu_t_seq <- input$select_mu_t
                   mu_t_seq <- as.numeric(strsplit(mu_t_seq,split = ",")[[1]])
                   r <- input$r
                   mu_0c <- input$mu_0c
                   if (input$prior == 'Normal-Inverse-Chi-Squared Prior') {
                     mu_0t <- input$mu_0t
                     k_0t <- input$k_0t
                     nu_0t <- input$nu_0t
                     sigma2_0t <- input$sigma2_0t
                     # browser()
                     lDataValues$plot_OC_pos <- continuous_pos.OC.O(lower,upper,sigma2_t,
                                                                    mu_0t,k_0t,nu_0t,sigma2_0t,
                                                                    mu_0c, r, #mu_0c is the hypothetical control center
                                                                    n_t,
                                                                    TV, MAV, gamma_1, gamma_2,
                                                                    N = input$N,
                                                                    mu_t.length = 10)
                     
                     res <- sapply(1:length(mu_t_seq), function(i) {
                       continuous_pos.GO.sce.O(mu_t_seq[i], sigma2_t,
                                               mu_0t,k_0t,nu_0t,sigma2_0t,
                                               mu_0c, r, #mu_0c is the hypothetical control center
                                               n_t,
                                               TV, MAV, gamma_1, gamma_2,
                                               N = input$N)
                     })
                   } else if (input$prior == 'Vague Prior') {
                     lDataValues$plot_OC_pos <- continuous_pos.OC.O.vague(lower,upper,sigma2_t,
                                                                          n_t,mu_0c,r,
                                                                          TV, MAV, gamma_1, gamma_2,
                                                                          N = input$N,
                                                                          mu_t.length = 10)
                     res <- sapply(1:length(mu_t_seq), function(i) {
                       continuous_pos.GO.sce.O.vague(mu_t_seq[i], sigma2_t,
                                                     n_t,mu_0c,r,
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
      #TBD: implementing the OC using predictive probability for uncontrolled PoC
      
      lower <- input$lower
      upper <- input$upper
      sigma2_t <- input$sigma2_t
      m_t <- input$m_t
      m_c <- input$m_c
      n_t <- input$n_t
      TV <- input$theta_null
      MAV <- input$theta_null
      theta_null <- input$theta_null
      gamma_1 <- input$gamma_1
      gamma_2 <- input$gamma_2
      r <- input$r
      mu_0c <- input$mu_0c
      mu_t_seq <- input$select_mu_t
      mu_t_seq <- as.numeric(strsplit(mu_t_seq,split = ",")[[1]])
      
      y_t_h_lower <- input$y_t_h_lower
      y_t_h_upper <- input$y_t_h_upper
      s_t_h <- input$s_t_h
      if (input$prior == 'Normal-Inverse-Chi-Squared Prior') {
        mu_0t <- input$mu_0t
        k_0t <- input$k_0t
        nu_0t <- input$nu_0t
        sigma2_0t <- input$sigma2_0t
        # browser()
        lDataValues$plot_OC_pred <- continuous_pred.OC.O(lower,upper,sigma2_t,
                                                         n_t,m_t,m_c, mu_0c,r,
                                                         mu_0t,k_0t, nu_0t, sigma2_0t,
                                                         TV, MAV, gamma_1, gamma_2,
                                                         N = input$N,
                                                         mu_t.length = 10)
        res <- sapply(1:length(mu_t_seq), function(i) {
          continuous_pred.GO.sce.O(mu_t_seq[i], sigma2_t,
                                   n_t,m_t,m_c,mu_0c,r,
                                   mu_0t,k_0t, nu_0t, sigma2_0t,
                                   TV, MAV, gamma_1, gamma_2,
                                   N = input$N)
        })
        #---OC2---
        y_t <- seq(y_t_h_lower,y_t_h_upper,length.out = 10)
        
        x <- uniroot(function(y) {
          continuous_pred.O(n_t, m_t,m_c,
                            y,s_t_h,
                            mu_0c,r,
                            mu_0t,k_0t,nu_0t,sigma2_0t,
                            theta = theta_null) - gamma_1
        } , c(y_t_h_lower, y_t_h_upper), tol = 0.0001)$root
        x_01 <- x
        
        x <- uniroot(function(y) {
          continuous_pred.O(n_t, m_t,m_c,
                            y,s_t_h,
                            mu_0c,r,
                            mu_0t,k_0t,nu_0t,sigma2_0t,
                            theta = theta_null) - gamma_2
        } , c(y_t_h_lower, y_t_h_upper), tol = 0.0001)$root
        x_02 <- x
        
        y_t <- c(y_t,x_01,x_02)
        pred.prob <- sapply(y_t, function(y) {
          continuous_pred.O(n_t, m_t,m_c,
                            y,s_t_h,
                            mu_0c,r,
                            mu_0t,k_0t,nu_0t,sigma2_0t,
                            theta = theta_null)
        })
        
        pred.prob <- cbind(y_t,pred.prob)
        
        lDataValues$plot_OC2_pred <- graph_OC2(pred.prob,"continuous",FALSE,gamma_1,gamma_2,x_01,x_02)
        
      } else if (input$prior == 'Vague Prior') {
        lDataValues$plot_OC_pred <- continuous_pred.OC.O.vague(lower,upper,sigma2_t,
                                                               n_t,
                                                               m_t,m_c,mu_0c,r,
                                                               TV, MAV, gamma_1, gamma_2,
                                                               N = input$N,
                                                               mu_t.length = 10)
        res <- sapply(1:length(mu_t_seq), function(i) {
          continuous_pred.GO.sce.O.vague(mu_t_seq[i], sigma2_t,
                                         n_t,
                                         m_t,m_c,mu_0c,r,
                                         TV, MAV, gamma_1, gamma_2,
                                         N = input$N)
        })
        
        #---OC2---
        y_t <- seq(y_t_h_lower,y_t_h_upper,length.out = 10)
        
        x <- uniroot(function(y) {
          continuous_pred.O.vague(n_t,
                                  y,
                                  s_t_h,#sample standard deviations
                                  m_t,m_c,mu_0c,r,
                                  theta = theta_null) - gamma_1
        } , c(y_t_h_lower, y_t_h_upper), tol = 0.0001)$root
        x_01 <- x
        
        x <- uniroot(function(y) {
          continuous_pred.O(n_t,
                            y,
                            s_t_h,#sample standard deviations
                            m_t,m_c,mu_0c,r,
                            theta = theta_null) - gamma_2
        } , c(y_t_h_lower, y_t_h_upper), tol = 0.0001)$root
        x_02 <- x
        
        y_t <- c(y_t,x_01,x_02)
        pred.prob <- sapply(y_t, function(y) {
          continuous_pred.O(n_t,
                            y,
                            s_t_h,#sample standard deviations
                            m_t,m_c,mu_0c,r,
                            theta = theta_null)
        })
        
        pred.prob <- cbind(y_t,pred.prob)
        
        lDataValues$plot_OC2_pred <- graph_OC2(pred.prob,"continuous",FALSE,gamma_1,gamma_2,x_01,x_02)
      }
      res <- as.data.frame(apply(res,1,unlist))
      res$mu_t <- mu_t_seq
      lDataValues$table_OC_pred <- res[,c(4,1,2,3)]
      
      res <- sapply(1:length(mu_t_seq), function(i) {
        continuous_pred.GO.sce.O(mu_t_seq[i], sigma2_t,
                                 n_t,m_t,m_c,mu_0c,r,
                                 mu_0t,k_0t, nu_0t, sigma2_0t,
                                 TV, MAV, gamma_1, gamma_2,
                                 N = input$N)
      })
      
      y_t <- seq(y_t_h_lower,y_t_h_upper,length.out = 10)
      
      x <- uniroot(function(y) {
        continuous_pred.O(n_t, m_t,m_c,
                          y,s_t_h,
                          mu_0c,r,
                          mu_0t,k_0t,nu_0t,sigma2_0t,
                          theta = theta_null) - gamma_1
      } , c(y_t_h_lower, y_t_h_upper), tol = 0.0001)$root
      x_01 <- x
      
      x <- uniroot(function(y) {
        continuous_pred.O(n_t, m_t,m_c,
                          y,s_t_h,
                          mu_0c,r,
                          mu_0t,k_0t,nu_0t,sigma2_0t,
                          theta = theta_null) - gamma_2
      } , c(y_t_h_lower, y_t_h_upper), tol = 0.0001)$root
      x_02 <- x
      
      y_t <- c(y_t,x_01,x_02)
      pred.prob <- sapply(y_t, function(y) {
        continuous_pred.O(n_t, m_t,m_c,
                          y,s_t_h,
                          mu_0c,r,
                          mu_0t,k_0t,nu_0t,sigma2_0t,
                          theta = theta_null)
      })
      
      pred.prob <- cbind(y_t,pred.prob)
      
      lDataValues$plot_OC2_pred <- graph_OC2(pred.prob,"continuous",FALSE,gamma_1,gamma_2,x_01,x_02)
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
    output$OC2_pred <- renderPlot({
      lDataValues$plot_OC2_pred 
    })
    output$OC_pred_table <- renderTable({
      lDataValues$table_OC_pred
    })
  }
  retServer <- moduleServer(id,module = retModule)
  return(retServer)
}