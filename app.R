rm(list = ls())
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinybusy)
library(shinyWidgets)
library(shinyBS)
library(rstan)

source("functions/binary_Posterior.R")
source("functions/binary_Predictive.R")
source("functions/binary_External.R")
source("functions/continuous_Posterior.R")
source("functions/continuous_Predictive.R")
source("functions/continuous_External.R")
source("functions/graph_OC1.R")
source("functions/graph_OC2.R")

source("modules/mod_HomeServer.R")
source("modules/mod_HomeUI.R")
source("modules/mod_BinaryWithServer.R")
source("modules/mod_BinaryWithUI.R")
source("modules/mod_BinaryWithoutServer.R")
source("modules/mod_BinaryWithoutUI.R")
source("modules/mod_BinaryExtServer.R")
source("modules/mod_BinaryExtUI.R")
source("modules/mod_ContinuousWithServer.R")
source("modules/mod_ContinuousWithUI.R")
source("modules/mod_ContinuousWithoutServer.R")
source("modules/mod_ContinuousWithoutUI.R")
source("modules/mod_ContinuousExtServer.R")
source("modules/mod_ContinuousExtUI.R")

### UI ####
app_ui <- function() {
  coverMenuItem <- function(mi, tabName) {
    mi$childre[[1]]$attribs['data-toggle'] <- "tab"
    mi$children[[1]]$attribs['data-value'] <- tabName
    if (length(mi$attribs$class) > 0 && mi$attribs$class == "treeview") {
      mi$attribs$class = NULL
    }
    mi
  }
  
  header <- shinydashboard::dashboardHeader(title = "BayesianQDM")
  
  sidebar <- shinydashboard::dashboardSidebar(
    sidebarMenu(
      id = "side_manu",
      menuItem("Home Page", tabName = "HomePage", icon = icon("right-to-bracket")),
      coverMenuItem(
        menuItem("Binary Endpoint",tabName = "BinaryEndpoint", icon = icon("right-to-bracket"), selected = T,
                 menuSubItem("Case with Controlled PoC",tabName = "BinaryWith"),
                 menuSubItem("Case without Controlled PoC",tabName = "BinaryWithout"),
                 menuSubItem("Case with External Control",tabName = "BinaryExt")
                 ), 
        "BinaryEndpoint"),
      coverMenuItem(
        menuItem("Continuous Endpoint",tabName = "ContinuousEndpoint", icon = icon("right-to-bracket"), selected = T,
                 menuSubItem("Case with Controlled PoC",tabName = "ContinuousWith"),
                 menuSubItem("Case without Controlled PoC",tabName = "ContinuousWithout"),
                 menuSubItem("Case with External Control",tabName = "ContinuousExt")), 
        "ContinuousEndpoint")
  ))
  
  body <- shinydashboard::dashboardBody(
    tabItems(
      tabItem(tabName = "HomePage", HomeUI()),
      tabItem(tabName = "BinaryWith", BinaryWithUI()),
      tabItem(tabName = "BinaryWithout", BinaryWithoutUI()),
      tabItem(tabName = "ContinuousWith", ContinuousWithUI()),
      tabItem(tabName = "ContinuousWithout", ContinuousWithoutUI()),
      tabItem(tabName = "BinaryExt", BinaryExtUI()),
      tabItem(tabName = "ContinuousExt", ContinuousExtUI())
    )
  )
  
  return(shinydashboard::dashboardPage(header = header, sidebar = sidebar, body = body))
}

### Server ####
app_server <- function(input, output, session) {
  HomeServer()
  BinaryWithServer()
  BinaryWithoutServer()
  ContinuousWithServer()
  ContinuousWithoutServer()
  BinaryExtServer()
  ContinuousExtServer()
}

shinyApp(ui = app_ui,server = app_server)
