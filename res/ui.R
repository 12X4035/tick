#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Hello Shiny!"),
  
  fluidRow(
    selectInput("val",label = "col",choices = colnames(bid)[2:10]),
    selectInput("ctg",label = "col",choices = bid$category)
    ),
  mainPanel(
    plotOutput("Plot")
    )
  )
)
