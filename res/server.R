#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(data.table)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$Plot <- renderPlot({
    bid <- fread("bid_16_one_day_30minutes.csv")
    
    data<-bid%>%
      dplyr::filter(category==input$ctg)
    
    g<-ggplot(data, aes(y=bid[, input$val], x=stock))+geom_bar(stat="identity",position="dodge",width =0.6)
    g<-g+geom_text(aes(y=data[, input$val], x=stock), position=position_dodge(width=0.6), size =3, angle=45)
    print(g)
  })
  
})
