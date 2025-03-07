ContinuousExtServer <- function(id = "ContinuousExt") {
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
                   alpha_0t <- input$a_0t
                   alpha_0c <- input$a_0c
                   y_bar_et <- input$y_et_bar
                   y_bar_ec <- input$y_ec_bar
                   n_et <- input$n_et
                   n_ec <- input$n_ec
                   s_et<- input$s_et
                   s_ec <- input$s_ec
                   # browser()
                   
                   lDataValues$Probability_pos["TV"] <- continuous_pos.E.vague(NA,
                                                                               n_t,n_c,n_et,n_ec,
                                                                               y_bar_t,y_bar_c,y_bar_et,y_bar_ec,
                                                                               s_t,s_c,s_et,s_ec,
                                                                               alpha_0t, alpha_0c,
                                                                               TV) 
                   
                   lDataValues$Probability_pos["MAV"] <- continuous_pos.E.vague(NA,
                                                                                n_t,n_c,n_et,n_ec,
                                                                                y_bar_t,y_bar_c,y_bar_et,y_bar_ec,
                                                                                s_t,s_c,s_et,s_ec,
                                                                                alpha_0t, alpha_0c,
                                                                                MAV)  
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
                   TV <- input$theta_null
                   MAV <- input$theta_null
                   alpha_0t <- input$a_0t
                   alpha_0c <- input$a_0c
                   y_bar_et <- input$y_et_bar
                   y_bar_ec <- input$y_ec_bar
                   n_et <- input$n_et
                   n_ec <- input$n_ec
                   s_et <- input$s_et
                   s_ec <- input$s_ec
                   # browser()
                   lDataValues$Probability_pred["TV"] <- continuous_pred.E.vague(NA, 
                                                                                 n_t,n_c,n_et,n_ec,
                                                                                 y_bar_t,y_bar_c,y_bar_et,y_bar_ec,
                                                                                 s_t,s_c,s_et,s_ec,
                                                                                 alpha_0t, alpha_0c,
                                                                                 m_t,m_c,
                                                                                 TV) 
                   lDataValues$Probability_pred["MAV"] <- continuous_pred.E.vague(NA, 
                                                                                  n_t,n_c,n_et,n_ec,
                                                                                  y_bar_t,y_bar_c,y_bar_et,y_bar_ec,
                                                                                  s_t,s_c,s_et,s_ec,
                                                                                  alpha_0t, alpha_0c,
                                                                                  m_t,m_c,
                                                                                  MAV) 
                   
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
                   
                   alpha_0t <- input$a_0t
                   alpha_0c <- input$a_0c
                   y_bar_et <- input$y_et_bar
                   y_bar_ec <- input$y_ec_bar
                   n_et <- input$n_et
                   n_ec <- input$n_ec
                   s_et <- input$s_et
                   s_ec <- input$s_ec
                   # browser()
                   
                   lDataValues$plot_OC_pos <- continuous_pos.OC.E.vague(lower,upper,mu_c.fix,sigma2_t,sigma2_c,
                                                                        n_t,n_c,n_et,n_ec,
                                                                        y_bar_et,y_bar_ec,s_et,s_ec,
                                                                        alpha_0t, alpha_0c,
                                                                        TV, MAV, gamma_1, gamma_2,
                                                                        N = input$N,
                                                                        mu_t.length = 10) 
                   model <- stan_model("./functions/continuous_vague.stan")
                   res <- sapply(1:length(mu_t_seq), function(i) {
                     continuous_pos.GO.sce.E.vague(model,
                                                   mu_t_seq[i], mu_c.fix, sigma2_t,sigma2_c,
                                                   n_t,n_c,n_et,n_ec,
                                                   y_bar_et,y_bar_ec,s_et,s_ec,
                                                   alpha_0t, alpha_0c,
                                                   TV, MAV, gamma_1, gamma_2,
                                                   N = input$N)
                   })
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
      sigma2_t <- input$sigma2_t
      sigma2_c <- input$sigma2_c
      m_t <- input$m_t
      m_c <- input$m_c
      n_t <- input$n_t
      n_c <- input$n_c
      TV <- input$theta_null
      MAV <- input$theta_null
      theta_null <- input$theta_null
      gamma_1 <- input$gamma_1
      gamma_2 <- input$gamma_2
      mu_t_seq <- input$select_mu_t
      mu_t_seq <- as.numeric(strsplit(mu_t_seq,split = ",")[[1]])
      y_t_h_lower <- input$y_t_h_lower
      y_t_h_upper <- input$y_t_h_upper
      y_c_h <- input$y_c_h
      s_t_h <- input$s_t_h
      s_c_h <- input$s_c_h
      
      alpha_0t <- input$a_0t
      alpha_0c <- input$a_0c
      y_bar_et <- input$y_et_bar
      y_bar_ec <- input$y_ec_bar
      n_et <- input$n_et
      n_ec <- input$n_ec
      s_et <- input$s_et
      s_ec <- input$s_ec
      
       
        lDataValues$plot_OC_pred <- continuous_pred.OC.E.vague(lower,upper,mu_c.fix,sigma2_t,sigma2_c,
                                                               n_t,n_c,n_et,n_ec,
                                                               m_t,m_c,
                                                               y_bar_et,y_bar_ec,s_et,s_ec,
                                                               alpha_0t, alpha_0c,
                                                               TV, MAV, gamma_1, gamma_2,
                                                               N = input$N, mu_t.length = 10)
        model <- stan_model("./functions/continuous_vague.stan")
        res <- sapply(1:length(mu_t_seq), function(i) {
          continuous_pred.GO.sce.E.vague(model,
                                         mu_t_seq[i], mu_c.fix, sigma2_t,sigma2_c,
                                         n_t,n_c,n_et,n_ec,
                                         y_bar_et,y_bar_ec,s_et,s_ec,
                                         alpha_0t, alpha_0c,
                                         m_t,m_c,
                                         TV, MAV, gamma_1, gamma_2,
                                         N = input$N)
        })
        
        #----OC2----
        y_t <- seq(y_t_h_lower,y_t_h_upper,length.out = 10)
        
        x <- uniroot(function(y) {
          continuous_pred.E.vague(n_t,n_c,
                                  y,y_c_h,
                                  s_t_h, s_c_h,
                                  m_t,m_c,
                                  theta = theta_null, N = 10000) - gamma_1
        } , c(y_t_h_lower, y_t_h_upper), tol = 0.0001)$root
        x_01 <- x-y_c_h
        
        x <- uniroot(function(y) {
          continuous_pred.E.vague(NA, 
                                  n_t,n_c,n_et,n_ec,
                                  y,y_c_h,y_bar_et,y_bar_ec,
                                  s_t_h,s_c_h,s_et,s_ec,
                                  alpha_0t, alpha_0c,
                                  m_t,m_c,
                                  theta = theta_null,N = 10000)  - gamma_2
        } , c(y_t_h_lower, y_t_h_upper), tol = 0.0001)$root
        x_02 <- x-y_c_h
        
        y_t <- c(y_t,x_01+y_c_h,x_02+y_c_h)
        obs_diff <- y_t - y_c_h
        pred.prob <- sapply(y_t, function(y) {
          continuous_pred.E.vague(NA, 
                                  n_t,n_c,n_et,n_ec,
                                  y,y_c_h,y_bar_et,y_bar_ec,
                                  s_t_h,s_c_h,s_et,s_ec,
                                  alpha_0t, alpha_0c,
                                  m_t,m_c,
                                  theta = theta_null,N = 10000) 
        })
        pred.prob <- cbind(y_t,pred.prob,obs_diff)
        
        lDataValues$plot_OC2_pred <- graph_OC2(pred.prob,"continuous",TRUE,gamma_1,gamma_2,x_01,x_02)
        
      
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
