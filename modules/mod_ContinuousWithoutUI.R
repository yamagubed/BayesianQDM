ContinuousWithoutUI <- function(id = "ContinuousWithout") {
  ns <- NS(id)
  fldRow <- fluidRow(
    box(width = 12,
        title = "Continuous Endpoint: Without Controlled PoC Study",
        wellPanel(
          withMathJax(includeMarkdown("documents/ContinuousWithout.Rmd"))
          ),
        wellPanel(
          fluidRow(column(12,
                          HTML("<b>Confidence Levels of Making Go/NoGo Decision:</b>"))),
          fluidRow(
            column(3, numericInput(ns("gamma_1"), label = withMathJax("\\(\\gamma_1\\)"), value = 0.8)),
            column(3, numericInput(ns("gamma_2"), label = withMathJax("\\(\\gamma_2\\)"), value = 0.3))
          ),
          fluidRow(column(12,
                          HTML("<b>Sample Size in PoC Study and Parameters for the Hypothetical Control: </b>"))),
          
          fluidRow(column(3,numericInput(ns("n_t"), label = withMathJax("\\(n_t \\)"),value = 12)),
                   column(3,numericInput(ns("mu_0c"), label = withMathJax("\\(\\mu_{0c} \\)"),value = 0)),#hypothetical control center
                   column(3,numericInput(ns("r"), label = withMathJax("\\(r\\)"),value = 12))
          ),
          fluidRow(column(6,sliderInput(ns("N"), "Number of PoC Data Generation",
                                        min = 100, max = 10000,
                                        value = 100, step = 100)),
          ),
          fluidRow(
            column(12,
                   HTML("<b>Assumed Parameters for Operating Characteristic</b>"))
          ),

          fluidRow(
            column(6,
                   textInput(ns("select_mu_t"), label = withMathJax("Selected Values for \\(\\mu_{t}\\)"), value = "1,2,4"))
          ),
          fluidRow(
                   column(3,numericInput(ns("sigma2_t"), label = withMathJax("\\(\\sigma_t^2 \\)"),value = 1)),
                   column(3,numericInput(ns("lower"), label = withMathJax("lower bound of \\( \\mu_t\\)"),value = -1)),
                   column(3,numericInput(ns("upper"), label = withMathJax("upper bound of \\(\\mu_t \\)"),value = 3))
          )
        ),
        selectInput(ns("prior"), "Prior Selection",
                    c("N-Inv-\u03C7\u00B2" = "Normal-Inverse-Chi-Squared Prior", 
                      "Vague" = "Vague Prior")
        ),
        conditionalPanel(
          condition = "input.prior == 'Normal-Inverse-Chi-Squared Prior'",
          #Some Common Parameters
          wellPanel(
            fluidRow(column(12,
                            "Parameters for the Prior Distribution:")),
            fluidRow(
              column(3, numericInput(ns("mu_0t"), label = withMathJax("\\(\\mu_{0t}\\)"), value = 5)),
              column(3, numericInput(ns("k_0t"), label = withMathJax("\\(k_{0t}\\)"), value = 5)),
              column(3, numericInput(ns("nu_0t"), label = withMathJax("\\(\\nu_{0t}\\)"), value = 5)),
              column(3, numericInput(ns("sigma2_0t"), label = withMathJax("\\(\\sigma^2_{0t}\\)"), value = 5)),
            ),
          ),
          ns=NS(id)
        ),
        tabsetPanel(
          tabPanel("Posterior Probability as Decision Criteria",
                   box(width = 12,
                       title = "Operating Characteristic",
                       wellPanel(
                         fluidRow(column(12,
                                         "Target Value (TV) and Minimum Acceptable Value (MAV): ")),
                         fluidRow(column(3, numericInput(ns("theta_0"), label = withMathJax("$$\\theta_{TV}$$"), value = 2)),
                                  column(3, numericInput(ns("theta_1"), label = withMathJax("$$\\theta_{MAV}$$"), value = 0))
                         ),
                         fluidRow(
                           column(3, div(style = "margin-top:25px",
                                         bsButton(ns("Generate_pos"), strong("Generate"), icon("stethoscope")))))
                       ),
                       wellPanel(
                         fluidRow(column(12,
                                         plotOutput(ns("OC_pos")))),
                         fluidRow(column(12,
                                         tableOutput(ns("OC_pos_table"))))
                       )
                   ),
                   box(width = 12,
                       title = "Posterior Probability Calculation",
                       wellPanel(
                         fluidRow(column(12,
                                         HTML("<b>Observations in PoC Study:</b>"))),
                         fluidRow(
                           column(3, numericInput(ns("y_bar_t_pos"), label = withMathJax("$$\\bar y_t$$"), value = 2)),
                           column(3, numericInput(ns("s_t_pos"), label = withMathJax("$$s_t$$"), value = 1)),
                           ),
                         fluidRow(
                           column(3, div(style = "margin-top:53px",
                                         bsButton(ns("Calculate_pos"), strong("Calculate"), icon("paper-plane"))))
                         )
                       ),
                       wellPanel( 
                         verbatimTextOutput(ns("Probability_pos"))
                       )
                   )),
          tabPanel("Predictive Probability as Decision Criteria",
                   #With Control + predictive
                   box(width = 12,
                       title = "Operating Characteristic",
                       wellPanel(
                         fluidRow(column(12,
                                         HTML("<b>Sample Sizes of Pivotal Study: </b>"))),
                         fluidRow(
                           column(3, numericInput(ns("m_t"), label = withMathJax("$$m_t$$"), value = 120)),
                           column(3, numericInput(ns("m_c"), label = withMathJax("$$m_c$$"), value = 120))
                         ),
                         fluidRow(column(12,
                                         HTML("<b>Hypothetical Observed PoC Data: </b>"))),
                         fluidRow(
                           column(3,
                                  numericInput(ns("y_t_h_lower"), label = withMathJax("Lower bound of \\(\\bar y_t\\)"), value = -5)),
                           column(3,
                                  numericInput(ns("y_t_h_upper"), label = withMathJax("Upper bound of \\(\\bar y_t\\)"), value = 5)),
                           column(3,
                                  numericInput(ns("s_t_h"), label = withMathJax("\\(s_t\\)"), value = 1))
                           ),
                         
                         fluidRow(column(12,
                                         HTML("<b>Smallest Treatment Effect that Indicates a Successful Pivotal Study:</b>"))),
                         fluidRow(column(3, numericInput(ns("theta_null"), label = withMathJax("$$\\theta_{null}$$"),
                                                         value = 0.5)),
                                  column(3, 
                                         div(style = "margin-top:53px",
                                             bsButton(ns("Generate_pred"), strong("Generate"), icon("stethoscope"))))
                                  ),
                       ),
                       wellPanel(
                         fluidRow(column(12,
                                         plotOutput(ns("OC_pred")))),
                         fluidRow(column(12,
                                         tableOutput(ns("OC_pred_table")))),
                         fluidRow(column(12,
                                         plotOutput(ns("OC2_pred")))),
                       )
                   ),
                   box(width = 12,
                       title = "Predictive Probability Calculation",
                       wellPanel(
                         fluidRow(column(12,
                                         HTML("<b>Observations in PoC Study:</b>"))),
                         fluidRow(column(3, numericInput(ns("y_bar_t_pred"), label = withMathJax("$$\\bar y_t$$"), value = 2)),
                                  column(3, numericInput(ns("s_t_pred"), label = withMathJax("$$s_t$$"), value = 1)),
                         ),
                         fluidRow(
                           column(3, div(style = "margin-top:53px",
                                         bsButton(ns("Calculate_pred"), strong("Calculate"), icon("paper-plane"))))
                         )
                         
                       ),
                       wellPanel( 
                         verbatimTextOutput(ns("Probability_pred"))
                       )
                   ))
          
        )
    )
    
  )#end fluidRow
  return(fldRow)
}

