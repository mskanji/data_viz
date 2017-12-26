
#------------------------------CONNECTION SPARK ---------------------------------#

#------------------------------LOADING PACKAGES ----------------------------------#

#########################LOADING PACKAGES ########################################



#------------------------------CONNECTION SQLITE ---------------------------------#

library(networkD3)
#----------------------------  UI   ------------------------------------------#
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  

  pageWithSidebar(
    headerPanel('Global-Terrorism-Attack'),
    sidebarPanel(
      #selectInput('xcol', 'YEAR', GTDB$iyear ),
      selectInput('ycol', 'COUNTRY ', GTDB$country_txt) ,
      
      
      
      sliderInput("range" ,"select years" ,  min= 1970 , max=2016 , value= c(2000,2015) ) ,
      checkboxGroupInput("checkGroup", label = h3("Type of attack"), 
      choices = list("Armed Assault"  , "Assassination"  ,"Bombing/Explosion" ,   "Facility/Infrastructure Attack" , "Hostage Taking (Barricade Incident)" 
                     ,   "Hostage Taking (Kidnapping)" ), selected=c("Armed Assault" ,"Bombing/Explosion")  
                                                                         )      
       
        
      
                  
      ),
    
    #"descriptive_plot" , tabPanel("MOMO" ,  plotOutput('plot') )
    
    mainPanel (
      leafletOutput("mymap") , 
      
  
      tabsetPanel(
        tabPanel("table" , dataTableOutput('table')),
        tabPanel("Summary" , plotOutput('plot1')),
        tabPanel("descriptive_plot " , tabsetPanel( 
          tabPanel("Terrorist attacks  by attack type"    , plotOutput('plot')      ) , 
          tabPanel("Terrorist attacks  by group name"      ,   simpleNetworkOutput('plot2'))
          
          ) )
        )
      
      
   
     
     
    )
    
    )
  
)
  
  


#-------------------SERVER -------------------------------------#
server <- function(input, output) {
   
  # Combine the selected variables into a new data frame
  
  
 
  
  output$mymap<- renderLeaflet({
    
    library(DBI)
    library(leaflet)
    
    con <- dbConnect(RSQLite::SQLite(), dbname = ":memory:")
    a<-as.vector(input$checkGroup)
    
    query<-paste("SELECT * FROM GTDB WHERE iyear>= ?x AND iyear<=?z AND country_txt= ?y AND attacktype1_txt IN ('",
                 paste(a ,collapse = "','"),"')",sep = "") 
    
    sql <- sqlInterpolate(con ,  query 
                          , x=input$range[1] , z=input$range[length(input$range)], y=input$ycol)
    result<-dbGetQuery(sc, sql)
    
    
    result<-dbGetQuery(sc,sql)
    #pal <- colorFactor(c("blue"  , "green" , "red" , "black" , "yellow" , "purple" , "navy"),  c("Armed Assault"  , "Assassination"  ,"Bombing/Explosion" ,   "Facility/Infrastructure Attack" , "Hostage Taking (Barricade Incident)" ,   "Hostage Taking (Kidnapping)",  "Unknown"   )      )
 
    
    pal <- colorFactor(c("blue"  , "green" , "red" , "black" , "yellow" , "purple" , "navy"),  c("Armed Assault"  , "Assassination"  ,"Bombing/Explosion" ,   "Facility/Infrastructure Attack" , "Hostage Taking (Barricade Incident)" ,   "Hostage Taking (Kidnapping)",  "Unknown"   )      )
      
     mapIND <- leaflet() %>% 
      addTiles() 
    mapIND %>% addCircles (data=result , lat= ~latitude, lng = ~longitude, 
                           popup=paste(
                             "<strong>Year: </strong>", result$iyear,
                             "<br><strong>City: </strong>", result$city, 
                             "<br><strong>Country: </strong>", result$country_txt, 
                             "<br><strong>Attack type: </strong>", result$attacktype1_txt, 
                             "<br><strong>Target: </strong>", result$targtype1_txt,
                             "<br><strong>nombre of kill: </strong>", result$nkill,
                             "<br><strong>Target: </strong>", result$summary ),
                           radius = ~result$nkill , 
                           
                           weight = 8, color = ~pal(result$attacktype1_txt),
                            stroke = TRUE  , fillOpacity = 0.8) 
  
    
    
  })
  
  
  
  
  output$table<- renderDataTable({
   
    con <- dbConnect(RSQLite::SQLite(), dbname = ":memory:")
    a<-as.vector(input$checkGroup)
    
    query<-paste("SELECT country_txt , iyear , attacktype1_txt ,nkill , nwound ,gname  FROM GTDB WHERE iyear>= ?x AND iyear<=?z AND country_txt= ?y AND attacktype1_txt IN ('",
                 paste(a ,collapse = "','"),"')",sep = "") 
    
    sql <- sqlInterpolate(con ,  query 
                          , x=input$range[1] , z=input$range[length(input$range)], y=input$ycol)
    result<-dbGetQuery(sc, sql)
  result
    

  })
  
  
  

  output$plot<-renderPlot({
    
    con <- dbConnect(RSQLite::SQLite(), dbname = ":memory:")
    sql<-sqlInterpolate(con,"select iyear , attacktype1_txt FROM GTDB where country_txt = ?y " , y=input$ycol)
    result<-dbGetQuery(sc, sql)
 ggplot(result,aes(x = iyear))+ labs(title =" Terrorist attacks  by attack type", x = "Years", y = "Number of Attacks") + 
      geom_bar(colour = "grey", fill = "navy") + facet_wrap(~attacktype1_txt) + theme(axis.text.x = element_text(hjust = 1, size = 8))+
      theme(strip.text = element_text(size = 8, face = "bold"))
    
    
    
    
   
    
    
  }) 
  
  
  
  
  
  
  
  output$plot1<-renderPlot({
    
    con <- dbConnect(RSQLite::SQLite(), dbname = ":memory:")
    sql<-sqlInterpolate(con,"select summary FROM GTDB where country_txt = ?y " , y=input$ycol)
    result<-dbGetQuery(sc, sql)
    library(wordcloud)
    wordcloud(result$summary,
              max.words = 100 ,colors=brewer.pal(8, "Dark2"),
              random.order = TRUE)
    
    
    
  })
  
  
  
  output$plot2 <-renderSimpleNetwork({
    
    con <- dbConnect(RSQLite::SQLite(), dbname = ":memory:")
    a<-as.vector(input$checkGroup)
    
    query<-paste("SELECT * FROM GTDB WHERE iyear>= ?x AND iyear<=?z AND country_txt= ?y AND attacktype1_txt IN ('",
                 paste(a ,collapse = "','"),"')",sep = "") 
    
    sql <- sqlInterpolate(con ,  query 
                          , x=input$range[1] , z=input$range[length(input$range)], y=input$ycol)
    result<-dbGetQuery(sc, sql)
    
    
    result<-dbGetQuery(sc,sql)
    
    #con <- dbConnect(RSQLite::SQLite(), dbname = ":memory:")
    #sql<-sqlInterpolate(con,"select iyear , gname , target1  FROM GTDB where iyear>= ?x AND iyear<=?z AND country_txt = ?y " , y=input$ycol  , x=input$range[1] , z=input$range[length(input$range)])
    #result<-dbGetQuery(sc, sql)

    networkData <- data.frame(result$gname, result$target1 , zoom=TRUE , fontSize=14 )
    
    simpleNetwork(networkData)
    
    
    
    
  })
  

  
  
}


# Run the application 
shinyApp(ui = ui, server = server)

