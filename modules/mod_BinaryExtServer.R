BinaryExtServer <- function(id = "BinaryExt") {
  retModule <- function(input,output,session) {
    lDataValues <- reactiveValues(dfAll = NULL) #This returns the result
  }
  retServer <- moduleServer(id,module = retModule)
  return(retServer)
}