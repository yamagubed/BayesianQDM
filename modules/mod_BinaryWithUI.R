BinaryWithUI <- function(id = "BinaryWith") {
  ns <- NS(id)
  fldRow <- fluidRow(
    box(width = 12,
        title = "Binary Endpoint: With Controlled PoC Study",
        wellPanel(
          withMathJax(includeMarkdown("documents/BinaryWith.Rmd"))
          ),
        wellPanel(
          fluidRow(column(12,
                          HTML("<b>Confidence Levels of Making Go/NoGo Decision:</b>"))),
          fluidRow(column(3, numericInput(ns("gamma_1"), label = withMathJax("\\(\\gamma_1\\)"), value = 0.8)),
                   column(3, numericInput(ns("gamma_2"), label = withMathJax("\\(\\gamma_2\\)"), value = 0.3)),
          ),
          fluidRow(column(12,
                          HTML("<b>Parameters for the Prior Distributions: </b>"))),
          fluidRow(
            column(3, numericInput(ns("alpha_t"), label = withMathJax("$$\\alpha_t$$"), value = 0.5)),
            column(3, numericInput(ns("beta_t"), label = withMathJax("$$\\beta_t$$"), value = 0.5)),
            column(3, numericInput(ns("alpha_c"), label = withMathJax("$$\\alpha_c$$"), value = 0.5)),
            column(3, numericInput(ns("beta_c"), label = withMathJax("$$\\beta_c$$"), value = 0.5))
          ),
          #Parameters for PoC Study Assumptions
          fluidRow(column(12,
                          HTML("<b>Sample Sizes in PoC Study: </b>"))),
          fluidRow(
            column(3, numericInput(ns("n_t"), label = withMathJax("$$n_t$$"), value = 12)),
            column(3, numericInput(ns("n_c"), label = withMathJax("$$n_c$$"), value = 15))),
          fluidRow(
            column(12,
                   HTML("<b>Assumed Response Rates for Operating Characteristic</b>"))
          ),
          fluidRow(
                  column(3,
                          numericInput(ns("pi_c"), label = withMathJax("Fix \\(\\pi_c\\)"), value = 0.2)),
                   column(6,
                          textInput(ns("select_pi_t"), label = withMathJax("Selected Values for \\(\\pi_t\\)"),
                                    value = "0.2,0.5,0.6"))),
          
        ),
        tabsetPanel(
          tabPanel("Posterior Probability as Decision Criteria",
                   box(width = 12,
                       title = "Operating Characteristic",
                       wellPanel(
                         fluidRow(column(12,
                                         HTML("<b>Target Value (TV) and Minimum Acceptable Value (MAV): </b>"))),
                         fluidRow(column(3, numericInput(ns("theta_0"), label = withMathJax("$$\\theta_{TV}$$"), value = 0.5)),
                                  column(3, numericInput(ns("theta_1"), label = withMathJax("$$\\theta_{MAV}$$"), value = 0.15)),
                                  column(3, 
                                         div(style = "margin-top:53px",
                                             bsButton(ns("Generate_pos"), strong("Generate"), icon("stethoscope"))))
                         ),
                       ),
                       wellPanel(
                         fluidRow(column(12,
                                         plotOutput(ns("OC_pos")))),
                         fluidRow(column(12,
                                         tableOutput(ns("OC_pos_table"))))
                       )),
                   box(width = 12,
                       title = "Posterior Probability Calculation",
                       wellPanel(
                         
                         fluidRow(column(12,
                                         HTML("<b>Observations in PoC Study:</b>"))),
                         fluidRow(
                           column(3, numericInput(ns("y_t_pos"), label = withMathJax("$$y_t$$"), value = 7)),
                           column(3, numericInput(ns("y_c_pos"), label = withMathJax("$$y_c$$"), value = 9)),
                           column(3, div(style = "margin-top:53px",
                                         bsButton(ns("Calculate_pos"), strong("Calculate"), icon("paper-plane"))))
                         ),
                         
                         
                       ),
                       wellPanel( 
                         verbatimTextOutput(ns("Probability_pos"))
                       ))),
          tabPanel("Predictive Probability as Decision Criteria",
                   box(width = 12,
                       title = "Operating Characteristic",
                       wellPanel(
                         fluidRow(
                           column(9,sliderInput(ns("N"), "Number of Simulations to Calculate Density of Two Random variable Difference",
                                                min = 1000, max = 10000,
                                                value = 1000, step = 1000))
                         ),
                         fluidRow(column(12,
                                         HTML("<b>Sample Sizes of Pivotal Study: </b>"))),
                         fluidRow(column(3,
                                         numericInput(ns("m_t_pred"), label = withMathJax("\\(m_t\\)"), value = 12)),
                                  column(3,
                                         numericInput(ns("m_c_pred"), label = withMathJax("\\(m_c\\)"), value = 12)),
                                  ),
                         fluidRow(column(12,
                                         HTML("<b>Hypothetical Observed PoC Data: </b>"))),
                         fluidRow(
                                  column(3,
                                         numericInput(ns("y_c_h"), label = withMathJax("\\(y_c\\)"), value = 0)),
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
                         # tagList(withMathJax()),
                         fluidRow(column(12,
                                         plotOutput(ns("OC_pred")))),
                         fluidRow(column(12,
                                         tableOutput(ns("OC_pred_table")))),
                         fluidRow(column(12,
                                         plotOutput(ns("OC2_pred")))),
                       )),
                   box(width = 12,
                       title = "Predictive Probability Calculation",
                       wellPanel(
                         
                         fluidRow(column(12,
                                         HTML("<b>Observations in PoC Study:</b>"))),
                         fluidRow(
                           column(3, numericInput(ns("y_t_pred"), label = withMathJax("$$y_t$$"), value = 7)),
                           column(3, numericInput(ns("y_c_pred"), label = withMathJax("$$y_c$$"), value = 7)),
                           column(3, div(style = "margin-top:53px",
                                         bsButton(ns("Calculate_pred"), strong("Calculate"), icon("paper-plane"))))
                         ),
                         
                         
                       ),
                       wellPanel( 
                         verbatimTextOutput(ns("Probability_pred"))
                       ))
          )
        )
    )
  )
  return(fldRow)
}