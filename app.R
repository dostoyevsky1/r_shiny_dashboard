ui <- dashboardPage(
  dashboardHeader(color = 'grey',title = 'NYC Traffic Citations',inverted=T),
  dashboardSidebar(color = 'black', inverted = T,
    sidebarMenu(
      menuItem(tabName = 'main','Map',icon=icon('map outline')),
      menuItem(tabName = 'dist', 'Charts', icon=icon('chart bar outline')),
      menuItem(tabName = 'cast', 'Series', icon=icon(''))
      
    )
  ),
  dashboardBody(
    tabItems(
      selected = 1,
      tabItem(
        tabName = 'main',
                      fluidRow(width=5,height=1,align='center',
                               sliderInput("map_yr",'',
                                           min = 2014, max = 2017,
                                           value = 2014,step=1,animate =
                                             animationOptions(interval = 1, loop = TRUE))),
                       fluidRow(box( width=16,
          title = 'NYC Traffic Citations', color='red', ribbon=F,title_side='top right',
          leafletOutput("map",height=800)
                       )
        
              )
          )
        
      ,
      tabItem(
        tabName = 'dist',
        fluidRow(column(4,box(width=4,
                     selectizeInput("freq",label=tags$h4('Choose Time Period'),choices = list('Weekly','Monthly','Year','Static'),selected = 'Weekly'))),
                  column(4,box(width=4,
                         selectizeInput("cat",label=tags$h4('Choose Category'),choices = list('Age','Gender','Offense',
                                                                                                  'Borough','State','Agency'),selected = 'Age'))),
                 column(4,box(width=4,
                              selectizeInput("yr",label=tags$h4('Choose Year'),choices = list('Year',2014,2015,
                                                                                              2016,2017),selected = 'Year'))),
                 column(4,box(width=4,
                              selectizeInput("top",label=tags$h4('Sort Top'),choices = list(1,3,5,10,20),selected = 20)))
                 ),
        fluidRow(box(width=16,
              title = "Weekly Citations",
              color = "red", ribbon = F, title_side = "top right",
                     plotlyOutput("barplot1",height=700)
              )
          )
        )
      )
        )
      )






server <- shinyServer(function(input, output, session) {

  data1 <- reactive({
  if(input$map_yr == '2014'){
    nyc_shape@data %>% mutate(tckt_cn = Y2014) %>% head() %>% as.data.frame()
  }else if(input$map_yr == '2015'){
    nyc_shape@data %>% mutate(tckt_cn = Y2015) %>% head() %>% as.data.frame()
  }else if(input$map_yr == '2016'){
    nyc_shape@data %>% mutate(tckt_cn = Y2016) %>% head() %>% as.data.frame()
  }else if(input$map_yr == '2017'){
    nyc_shape@data %>% mutate(tckt_cn = Y2017) %>% head() %>% as.data.frame()
  }
    
  })

  
  
  
  output$map <- renderLeaflet({
    nyc_shape@data <- data1()
    
    pal=colorBin("YlOrRd", domain=nyc_shape@data$tckt_cn)
    leaflet(nyc_shape) %>%
      addProviderTiles(provider = 'CartoDB.Positron') %>%
      addPolygons(
        popup = paste(
          "Borough:",
          nyc_shape@data$boro_nm,
          "<br>",
          "Tickets Issued:",
          nyc_shape@data$tckt_cn,
          "<br>"
        ),
        stroke = T,
        weight = 0.1,
        fillColor = ~ pal(nyc_shape@data$tckt_cn),
        fillOpacity = 0.6,
        highlightOptions = highlightOptions(
          color = "black",
          weight = 3,
          bringToFront = TRUE
        )
      )
  })
  
    
  
  data2 <- reactive({
    if(input$freq != 'Static'){
      
          if(input$yr != 'Year'){
  data2 <- traffic_nyc %>% filter(Year==input$yr) %>% group_by_at(.vars=c(input$freq,input$cat)) %>% summarise(Count=n()) %>% 
  top_n(as.numeric(input$top),wt=Count) %>% as.data.frame()
  
          }else{
            data2 <- traffic_nyc %>% group_by_at(.vars=c(input$freq,input$cat)) %>% summarise(Count=n()) %>% 
      top_n(as.numeric(input$top),wt=Count) %>% as.data.frame()}
      
    }else{
      
      if(input$yr != 'Year'){
        data2 <- traffic_nyc %>% filter(Year==input$yr) %>% group_by_at(.vars=c(input$cat)) %>% summarise(Count=n()) %>% 
          top_n(as.numeric(input$top),wt=Count) %>% as.data.frame()
        
      }else{
        data2 <- traffic_nyc %>% group_by_at(.vars=c(input$cat)) %>% summarise(Count=n()) %>% 
          top_n(as.numeric(input$top),wt=Count) %>% as.data.frame()
        }
    }

    
    
    
    
  })
  
  

  output$barplot1 <- renderPlotly({
    
    data2 = data2()
    data2[,1] <- factor(data2[,1], levels = unique(data2[,1])[order(data2$Count, decreasing = TRUE)])
    
    plot_ly(
      data = data2,
      x = as.factor(data2[, 1]),
      y =  ~ Count,
      type = 'bar',
      color = as.factor(data2[, (ncol(data2) - 1)]),
      colors = colorRampPalette(brewer.pal(11,name = "Pastel1"))(nrow(data2)),
      alpha = 0.85
    ) %>%
      layout(
        plot_bgcolor = '#FFFFFF',
        paper_bgcolor = '#FFFFFF',
        title = 'Traffic Citations Explorer',
        xaxis = list(title = '', color = '#000'),
        yaxis = list(title = 'Total Number of Citations', color = '#000'),
        title = list(color = '#000'),
        marker = list(color = '#000'),
        legend = list(font = list(
          family = "sans-serif",
          size = 12,
          color = "#000"
        ))
      )
    
    
  })
  
  
})

shinyApp(ui, server)