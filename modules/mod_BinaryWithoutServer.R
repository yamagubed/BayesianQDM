BinaryWithoutServer <- function(id = "BinaryWithout") {
  retModule <- function(input,output,session) {
    lDataValues <- reactiveValues(dfAll = NULL) #This returns the result
    observeEvent(input$Calculate_pos,
                 {
                   ns <- session$ns
                   shinyBS::updateButton(session,
                                         inputId = ns("Calculate_pos"), label = "Calculating", size = "default", disabled = TRUE)
                   n_t <- input$n_t
                   y_t <- input$y_t_pos
                   z <- input$z
                   alpha_t <- input$alpha_t
                   beta_t <- input$beta_t
                   TV <- input$theta_0
                   MAV <- input$theta_1
                   # browser()
                   lDataValues$Probability_pos["TV"] <- binary_pos.O(n_t, y_t, alpha_t, beta_t,z,TV)
                   lDataValues$Probability_pos["MAV"] <- binary_pos.O(n_t, y_t, alpha_t, beta_t,z,MAV)
                   
                   shinyBS::updateButton(session,
                                         inputId = ns("Calculate_pos"), label = "Calculate", size = "default", disabled = FALSE)
                 })
    output$Probability_pos <- renderText({
      if (!is.null(lDataValues$Probability_pos)) {
        paste("The probability of exceeding TV is: ",lDataValues$Probability_pos["TV"],
              "\n",
              "The probability of exceeding MAV is: ", lDataValues$Probability_pos["MAV"],
              sep = "")
      }
    })
    
    observeEvent(input$Calculate_pred,
                 {
                   #Calculate probabilities without controls
                   ns <- session$ns
                   shinyBS::updateButton(session,
                                         inputId = ns("Calculate_pred"), label = "Calculating", size = "default", disabled = TRUE)
                   n_t <- input$n_t
                   y_t <- input$y_t_pred
                   z <- input$z
                   alpha_t <- input$alpha_t
                   beta_t <- input$beta_t
                   TV <- input$theta_0
                   MAV <- input$theta_1
                   m_t <- input$m_t_pred
                   lDataValues$Probability_pred["TV"] <- binary_pred.O(m_t,
                                                                       alpha_t, beta_t,z,
                                                                       y_t, n_t,TV, N = input$N)
                   lDataValues$Probability_pred["MAV"] <- binary_pred.O(m_t,
                                                                        alpha_t, beta_t,z,
                                                                        y_t, n_t,MAV, N = input$N)
                   
                   shinyBS::updateButton(session,
                                         inputId = ns("Calculate_pred"), label = "Calculate", size = "default", disabled = FALSE)
                 })
    output$Probability_pred <- renderText({
      if (!is.null(lDataValues$Probability_pred)) {
        paste("The probability of exceeding TV is: ",lDataValues$Probability_pred["TV"],
              "\n",
              "The probability of exceeding MAV is: ", lDataValues$Probability_pred["MAV"],
              sep = "")
      }
    })
    
    observeEvent(input$Generate_pos, {
      ns <- session$ns
      shinyBS::updateButton(session,
                            inputId = ns("Generate_pos"), label = "Generating", size = "default", disabled = TRUE)
      n_t <- input$n_t
      alpha_t <- input$alpha_t
      beta_t <- input$beta_t
      TV <- input$theta_0
      MAV <- input$theta_1
      gamma_1 <- input$gamma_1
      gamma_2 <- input$gamma_2
      z <- input$z
      lDataValues$plot_OC_pos <- binary_pos.OC.O(alpha_t,beta_t,n_t,z, TV, MAV, gamma_1, gamma_2,
                                               N = input$N,pi_t.length = 10)
      pi_t_seq <- input$select_pi_t
      pi_t_seq <- as.numeric(strsplit(pi_t_seq,split = ",")[[1]])
      res <- sapply(1:length(pi_t_seq), function(i) {
        binary_pos.Go.sce.O(pi_t_seq[i], alpha_t,beta_t,n_t,z, TV, MAV, gamma_1, gamma_2)
      })

      res <- as.data.frame(apply(res,1,unlist))
      res$pi_t <- pi_t_seq
      lDataValues$table_OC_pos <- res[,c(4,1,2,3)]
      shinyBS::updateButton(session,
                            inputId = ns("Generate_pos"), label = "Generate", size = "default", disabled = FALSE)
    })
    output$OC_pos <- renderPlot({
      lDataValues$plot_OC_pos
    })
    output$OC_pos_table <- renderTable({
      lDataValues$table_OC_pos
    })
    
    observeEvent(input$Generate_pred, {
      ns <- session$ns
      shinyBS::updateButton(session,
                            inputId = ns("Generate_pred"), label = "Generating", size = "default", disabled = TRUE)
      n_t <- input$n_t
      alpha_t <- input$alpha_t
      beta_t <- input$beta_t
      TV <- input$theta_0
      MAV <- input$theta_1
      gamma_1 <- input$gamma_1
      gamma_2 <- input$gamma_2
      z <- input$z
      m_t <- input$m_t_pred
      # browser()
      lDataValues$plot_OC_pred <- binary_pred.OC.O(n_t,m_t,
                                                   alpha_t,beta_t,z,
                                                   TV,MAV, gamma_1, gamma_2,
                                                   N = input$N, pi_t.length = 10)
      pi_t_seq <- input$select_pi_t
      pi_t_seq <- as.numeric(strsplit(pi_t_seq,split = ",")[[1]])
      res <- sapply(1:length(pi_t_seq), function(i) {
        binary_pred.Go.sce.O(pi_t_seq[i], n_t, m_t,
                             alpha_t, beta_t, z,
                             TV, MAV, gamma_1, gamma_2,input$N)
      })
      
      res <- as.data.frame(apply(res,1,unlist))
      res$pi_t <- pi_t_seq
      lDataValues$table_OC_pred <- res[,c(4,1,2,3)]
      
      shinyBS::updateButton(session,
                            inputId = ns("Generate_pred"), label = "Generate", size = "default", disabled = FALSE)
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