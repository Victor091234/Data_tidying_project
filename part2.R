library(shiny)
library(factoextra)

source("data_tidying_inc.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Shots to Goal"),
  selectInput("Team1", label = h5("Select first Team"), 
              choices = unique(matrix$Team), 
              selected = unique(matrix$Team)[7]),
  selectInput("Team2", label = h5("Select second Team"), 
              choices = unique(matrix$Team), 
              selected = unique(matrix$Team)[5]),
  selectInput("Stat", label = h5("Select Statistic"), 
              choices = colnames(matrix[,-c(1:2)]), 
              selected = unique(matrix$Team)[5]),
  
  plotOutput(outputId = "plot_shots")
)

server=function(input,output){
  output$plot_shots<-renderPlot({
    h=matrix[(matrix$Team==input$Team1),]
    n=matrix[(matrix$Team==input$Team2),]
    hn = rbind(h,n)
    ggplot(hn)+aes_string(x="Round", y=input$Stat)+
      geom_line(aes(group=hn$Team, colour=hn$Team))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)