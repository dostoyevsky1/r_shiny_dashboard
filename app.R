ui <- dashboardPage(
  dashboardHeader(color = 'grey',
                  title = 'NYC Traffic Citations',
                  inverted = T),
  dashboardSidebar(
    color = 'black',
    inverted = T,
    sidebarMenu(
      menuItem(tabName = 'main', 'Map', icon = icon('map outline')),
      menuItem(
        tabName = 'dist',
        'Charts',
        icon = icon('chart bar outline')
      ),
      menuItem(tabName = 'cast', 'Series', icon = icon('chart line'))
      
    )
  ),
  dashboardBody(tabItems(
    selected = 1,
    tabItem(
      tabName = 'main',
      fluidRow(box(
        width = 5,
        title = 'Choose Year',
        color = 'red',
        sliderInput(
          "map_yr",
          '',
          min = 2014,
          max = 2017,
          value = 2014,
          step = 1,
          animate =
            animationOptions(interval = 1, loop = TRUE)
        ))
      ),
      fluidRow(
        box(
          width = 16,
          title = 'NYC Total Traffic Citations Map',
          color = 'red',
          ribbon = F,
          title_side = 'top right',
          leafletOutput("map", height = 800)
        )
        
      )
    )
    
    ,
    tabItem(
      tabName = 'dist',
      fluidRow(
        column(4, box(
          width = 4,
          title = 'Choose Time Period',
          color='blue',
          selectizeInput(
            "freq",
            label = tags$h4(''),
            choices = list('Weekly', 'Monthly', 'Year', 'Static'),
            selected = 'Weekly'
          )
        )),
        column(4, box(
          width = 4,
          title = 'Choose Category',
          color='blue',
          selectizeInput(
            "cat",
            label = tags$h4(''),
            choices = list('Age', 'Gender', 'Offense',
                           'Borough', 'State', 'Agency'),
            selected = 'Age'
          )
        )),
        column(4, box(
          width = 4,
          title= 'Choose Year',
          color='blue',
          selectizeInput(
            "yr",
            label = tags$h4(''),
            choices = list('Year', 2014, 2015,
                           2016, 2017),
            selected = 'Year'
          )
        )),
        column(4, box(
          width = 4,
          title= 'Sort Top Period',
          color='blue',
          selectizeInput(
            "top",
            label = tags$h4(''),
            choices = list(1, 3, 5, 10, 20),
            selected = 20
          )
        ))
      ),
      fluidRow(
        box(
          width = 16,
          color = "black",
          title = 'Traffic Citations Explorer',
          ribbon = F,
          title_side = "top right",
          plotlyOutput("barplot1", height = 700)
        )
      )
    ),
    tabItem(tabName = 'cast',
            fluidRow(
              column(3,box(title = 'Moving Average',
                           color = 'green',
                           radioButtons("ma",'', 
                                        choices = list("Simple" = 's', 
                                                       "Triangular" = 't', 
                                                       "Weighted" = 'w',
                                                       "Modified" = 'm',
                                                       "Exponential" = 'e',
                                                       "Running" = 'r'),
                                        selected = 's'))),
              column(3,box(title = 'Moving Average Window',
                           color = 'green',
                           sliderInput("ma_n",
                                       '',
                                       min = 2,
                                       max = 45,
                                       value = 2,
                                       step = 1,
                                       animate =
                                         animationOptions(interval = 1, loop = TRUE,playButton = 'Play',pauseButton = 'Pause')))),
              column(3,box(title= 'ARMA Forecast',
                           color ='green',
                           radioButtons("cats",'',
                                        choices = list('AR' = 'ar',
                                                       'MA' = 'ma'),
                                        selected = NULL)))
            ),
            fluidRow(box(
              width=16,
              color='green',
              title='Citations Series Smoothing',
              ribbon=F,
              title_side='top right',
              plotlyOutput('series',height=700)
            ))
            )
  ))
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

  
  
  nyc_shape@data 
  output$map <- renderLeaflet({
    nyc_shape@data <- data1()
    
    pal=colorBin(ifelse(input$map_yr =='2014','YlOrRd',ifelse(
      input$map_yr =='2015','YlGnBu',ifelse(
        input$map_yr =='2016', 'YlGn','RdPu'))),
          domain=nyc_shape@data$tckt_cn)
    
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
        # title = 'Traffic Citations Explorer',
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
  

  output$series <- renderPlotly({
    
    
    
    plot_ly(data=tseries_df,x=~Date,y=~Count,type='scatter',mode='lines+markers',name = 'Citations') %>% 
      add_trace(y=movavg(tseries_df$Count,as.numeric(input$ma_n),as.character(input$ma)),name='Smoothed',mode='lines+markers')
      
      
      
      
      
      
    
    })
  
  
})

shinyApp(ui, server)