BinaryExtUI <- function(id = "BinaryExt") {
  ns <- NS(id)
  fldRow <- fluidRow(
    box(width = 12,
        title = "Binary Endpoint: With External Control",
        wellPanel(
          fluidRow(
            "TBD: some definition of the parameters"
          )),
        wellPanel(
          "Some Parameters"
          
        ),
        tabsetPanel(
          tabPanel("Posterior Probability as Decision Criteria",
                   box(width = 12,
                       title = "Operating Characteristic"),
                   box(width = 12,
                       title = "Posterior Probability Calculation")
          ),
          tabPanel("Predictive Probability as Decision Criteria",
                   box(width = 12,
                       title = "Operating Characteristic"),
                   box(width = 12,
                       title = "Predictive Probability Calculation")
          )
        )
    )
  )
  return(fldRow)
}