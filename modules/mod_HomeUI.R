HomeUI <- function(id = "HomePage") {
  ns <- NS(id)
  fldRow <- fluidRow(

    box(width = 12,
        title = "Introduction",
        solidHeader = TRUE,
        withMathJax(includeMarkdown("documents/Introduction.Rmd"))
        ),
    # box(width = 12,
    #     title = "Instructions",
    #     solidHeader = TRUE,
    #     withMathJax(includeText("documents/"))),
  )
  return(fldRow)
}