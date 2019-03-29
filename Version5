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
                 ),
                 tabPanel("Evolution of Shots to Goal",
                          fluidPage(
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
                 ),
                 tabPanel("Shots to Goal Heatmap",
                          fluidPage(
                            mainPanel( 
                              #this will create a space for us to display our map
                              leafletOutput(outputId = "mymap"), 
                              #this allows me to put the checkmarks ontop of the map to allow people to view STG depth or overlay a heatmap
                              absolutePanel(top = 60, left = 20, 
                                            checkboxInput("markers", "Depth", FALSE),
                                            checkboxInput("heat", "Heatmap", FALSE)
                              )
                            )))
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
    output$plot_shots<-renderPlot({
      h=matrix[(matrix$Team==input$Team1),]
      n=matrix[(matrix$Team==input$Team2),]
      hn = rbind(h,n)
      ggplot(hn)+aes_string(x="Round", y=input$Stat)+
        geom_line(aes(group=hn$Team, colour=hn$Team))
    })
    #define the color pallate for the 
    pal <- colorNumeric(
      palette = c('gold','orange red', 'dark red'),
      domain = data$STG,
      na.color = "lightgray")
    
    #define the color of for the depth of the earquakes
    pal2 <- colorFactor(
      palette = c('blue', 'yellow', 'red'),
      domain = data$depth_type,
      na.color="lightgray"
    )
    
    #create the map
    output$mymap <- renderLeaflet({
      leaflet(data) %>% 
        setView(lng = -2, lat = 40, zoom = 4)  %>% #setting the view over ~ center of Europe
        addTiles() %>% 
        addCircles(data = data, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~sqrt(STG)*25000, popup = ~as.character(STG), label = ~as.character(paste0("Magnitude: ", sep = " ", STG)), color = ~pal(STG), fillOpacity = 0.5)
    })
    
    #next we use the observe function to make the checkboxes dynamic. If you leave this part out you will see that the checkboxes, when clicked on the first time, display our filters...But if you then uncheck them they stay on. So we need to tell the server to update the map when the checkboxes are unchecked.
    observe({
      proxy <- leafletProxy("mymap", data = data)
      proxy %>% clearMarkers()
      if (input$markers) {
        proxy %>% addCircleMarkers(stroke = FALSE, color = ~pal2(depth_type), fillOpacity = 0.2,      label = ~as.character(paste0("Magnitude: ", sep = " ", STG))) %>%
          addLegend("bottomright", pal = pal2, values = data$depth_type,
                    title = "Depth Type",
                    opacity = 1)}
      else {
        proxy %>% clearMarkers() %>% clearControls()
      }
    })
    
    observe({
      proxy <- leafletProxy("mymap", data = data)
      proxy %>% clearMarkers()
      if (input$heat) {
        proxy %>%  addHeatmap(lng=~longitude, lat=~latitude, intensity = ~STG, blur =  10, max = 0.05, radius = 15) 
      }
      else{
        proxy %>% clearHeatmap()
      }
      
      
    })
}


# Run the application 
shinyApp(ui = ui, server = server)

