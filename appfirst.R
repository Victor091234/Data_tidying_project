#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(factoextra)

source("data_tidying_inc.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Cluster Plot"),
   selectInput("Round", label = h5("Select Round"), 
               choices = c(5:26), 
               selected = 7),
   selectInput("Clusters", label = h5("Select Number of Clusters"), 
               choices = c(2:6), 
               selected = 3),
   
  
   plotOutput(outputId = "plot_clusters")
)

   server=function(input,output){
   output$plot_clusters<-renderPlot({
     a=kmeans(scale(matrix[matrix$Round==input$Round, -c(1:2)]), centers=input$Clusters)
     b=scale(matrix[matrix$Round==input$Round, -c(1:2)])
     rownames(b)=unique(matrix$Team)
     fviz_cluster(a, b)
   })
   
   }
   

# Run the application 
shinyApp(ui = ui, server = server)

