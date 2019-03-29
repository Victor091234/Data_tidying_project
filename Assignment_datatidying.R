library(shiny)
library(factoextra)
library(leaflet)
library(dplyr)
library(leaflet.extras)

source("data_tidying_inc.R")
# Define UI for application that draws a histogram
ui <- navbarPage("La Liga Statistics",
                 tabPanel("Explanation of the app",
                          fluidPage(h1("A brief explanation about the app"),
                                     p("The main goal of this app is to draw interesting information about the Spanish football league La Liga."),
                                     p("The dataset was downloaded from the internet and it includes
                                    several statistics about each round of the championship. For example, from the dataset, we can see how many goals were scored by each team at each round, how many yellow cards were received,
                                    how many shoots were made, and so on."),
                                    p("Having said that, one important feature about the dataset is that it includes the specific statistics for each round. However, if we want to analyse which team has more wins - or draws, or losses, or shots to goal -,
                                      it would be more riveting if we could see those statistics on the aggregate. In order to address this, we 'cleaned' the data, and created a matrix that includes information about each round, but in a cumulative way. For example, 
                                      if Barcelona had one win in the first round and a draw in the second, the column 'W', which accounts for the matches the team won, would still return the value 1, instead of the original 0."),
                                    p("The app is structured in the following manner: the first tab performs a cluster analysis in the dataset - namely, using the k-NN algorithm, in order to categorize the teams. It is up to the user to choose the number of clusters and the round."),
                                    p("Subsequenlty, the user can, with the help of a bar plot, analyse the evolution of each team with respect to each variable and the round, which the user can also choose. The main goal of this plot is to analyse the performance of all teams together."),
                                    p("Nevertheless, we also know that it is useful to compare the teams in pairs - for example, after running the first app, we can see that Barcelona and Sevilla are somewhat alike. Hence, it makes sense to analyse those teams separately - and the same would apply to any combination of two teams the user fancies. Hence, the next analysis consists of a comparison between teams, but in pairs."),
                                    p("The next question one may pose, after looking at the previous app, is 'How is the density for each variable?'. The fourth app addresses that question: the user can, again, choose between two of the twenty teams and see the density for any variable of his/her preference."),
                                    p("And, finally, the fifth tab uses a heat map - using the map of Spain - that represents the Shots to Goal. It is interesting to analyse the performance of the teams based on their location. The darkest the colour, the more shots to goal were made."))),
                
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
                                        choices=colnames(matrix), selected=colnames(matrix[,-c(1,2)])[5]),
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
                 tabPanel("Density of Statistics",
                          fluidPage(
                            selectInput("Team3", label = h5("Select first Team"), 
                                        choices = unique(matrix$Team), 
                                        selected = unique(matrix$Team)[11]),
                            selectInput("Team4", label = h5("Select second Team"), 
                                        choices = unique(matrix$Team), 
                                        selected = unique(matrix$Team)[5]),
                            selectInput("Stat3", label = h5("Select Statistic"), 
                                        choices = colnames(matrix[,-c(1:2)]),
                                        selected = unique(matrix$Team)[5]),
                            plotOutput(outputId = "plot_density")
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
  output$plot_density<-renderPlot({
    w=matrix[(matrix$Team==input$Team3),]
    z=matrix[(matrix$Team==input$Team4),]
    k=matrix[(matrix$Round=="26"),]
    wzk = rbind(w,z,k)
    ggplot(wzk)+aes_string(x=input$Stat3)+
      geom_density(aes(group=wzk$Team, colour=wzk$Team))
  })
  #define the color pallate for the 
  pal <- colorNumeric(
    palette = c('gold','orange red', 'dark red'),
    domain = data$STG,
    na.color = "lightgray")
  
  #define the color of for the depth of the shots
  pal2 <- colorFactor(
    
    palette = c('blue', 'yellow', 'red'),
    domain = data$depth_type,
    na.color="lightgray"
  )
  
  #create the map
  output$mymap <- renderLeaflet({
    leaflet(data) %>% 
      setView(lng = -2, lat = 40, zoom = 5)  %>% #setting the view over ~ center of Europe
      addTiles() %>% 
      addCircles(data = data, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~sqrt(STG)*25000, popup = ~as.character(STG), label = ~as.character(paste0("Shots to Goal: ", sep = " ", STG)), color = ~pal(STG), fillOpacity = 0.5)
  })
  
  #next we use the observe function to make the checkboxes dynamic. If you leave this part out you will see that the checkboxes, when clicked on the first time, display our filters...But if you then uncheck them they stay on. So we need to tell the server to update the map when the checkboxes are unchecked.
  observe({
    proxy <- leafletProxy("mymap", data = data)
    proxy %>% clearMarkers()
    if (input$markers) {
      proxy %>% addCircleMarkers(stroke = FALSE, color = ~pal2(depth_type), fillOpacity = 0.2,      label = ~as.character(paste0("Shots to Goal: ", sep = " ", STG))) %>%
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
      proxy %>%  addHeatmap(lng=~longitude, lat=~latitude, intensity = ~STG, blur =  10, max = 0.05, radius = 8) 
    }
    else{
      proxy %>% clearHeatmap()
    }
    
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
