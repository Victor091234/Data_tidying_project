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
ui <- navbarPage("Shiny App",
                 tabPanel("Cluster Analysis",
                 fluidPage(
   

   
   selectInput("Round", label = h5("Select Round"), 
               choices = c(5:26), 
               selected = 7),
   selectInput("Clusters", label = h5("Select Number of Clusters"), 
               choices = c(2:6), 
               selected = 3),
   
  
   plotOutput(outputId = "plot_clusters")
                 )
                 ),
   tabPanel("Evolution of the points",
            fluidPage(
              selectInput("Round2", label=h5("Select Round"),
                          choices=c(1:26), 
                          selected=5),
              
              selectInput("variable", label=h5("Select Variable"),
                          choices=colnames(matrix), selected=colnames(matrix)[5]),
              
              plotOutput(outputId = "Points_bar")
              
            )
            )

   
)

   server=function(input,output){
   output$plot_clusters<-renderPlot({
     a=kmeans(scale(matrix[matrix$Round==input$Round, -c(1:2)]), centers=input$Clusters)
     b=scale(matrix[matrix$Round==input$Round, -c(1:2)])
     rownames(b)=unique(matrix$Team)
     fviz_cluster(a, b)
   })
   output$Points_bar<-renderPlot({
     a=matrix[matrix$Round==input$Round2,]
     ggplot(a)+aes_string(x="Team", y=input$variable, fill="Team")+geom_bar(stat="identity")
       
   })
   
   
   
   
   
   
   }
   

# Run the application 
shinyApp(ui = ui, server = server)

