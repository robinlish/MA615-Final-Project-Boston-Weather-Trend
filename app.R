# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("tidyverse")
# install.packages(c("RColorBrewer", "dplyr", "dygraphs"))
library(shiny)
library(shinydashboard)
library(tidyverse)
library(RColorBrewer)
library(dplyr)
library(dygraphs)

load("boston.Rdata")
#1. temp map
datetxt <- boston$DATE
datetxt <- as.Date(datetxt)
boston <- add_column(boston,
                     YEAR = as.numeric(format(datetxt, format = "%Y")),
                     Month = as.numeric(format(datetxt, format = "%m")),
                     DAY = as.numeric(format(datetxt, format = "%d")), .after = 6) %>% 
  mutate(MONTH = month.abb[Month])
boston$month_ordered <- factor(boston$MONTH, levels = month.abb)

#2. time series:
monthly <- boston %>% select(month_ordered,YEAR,TAVG, PRCP, AWND, SNOW) %>% 
  group_by(month_ordered,YEAR) %>% 
  summarise(TAVG = mean(TAVG),PRCP = mean(PRCP), AWND = mean(AWND), SNOW = mean(SNOW)) %>% 
  arrange(YEAR)

##average temp:
myts <- ts(monthly$TAVG,start=c(1936,1), end=c(2018,4), frequency=12)
ts.decompose <- decompose(myts)

ui<- dashboardPage(skin = "green",
                   dashboardHeader(title = 'Boston Weather Trend'),
                   dashboardSidebar(sidebarMenu(
                     menuItem(
                       "Introduction",
                       tabName = "intro",
                       icon = icon("info-circle")
                     ),
                     menuItem(
                       "Temperature Trend",
                       tabName = "WeatherTrend",
                       icon = icon("thermometer"),
                       menuSubItem("Temperature Gradient", tabName = "temperaturegradient"),
                       menuSubItem("Time Series of Average Temp", tabName = "timeseries"),
                       menuSubItem("Temperature Forecast", tabName = "forecast")
                     )
                     ,
                     menuItem(
                       "Correlation Analysis",
                       tabName = "CorrelationAnalysis",
                       icon = icon("random"),
                       menuSubItem("Precipitation vs. Average Temp", tabName = "prcp_temp"),
                       menuSubItem("Wind Speed vs. Average Temp", tabName = "windspeed_temp"),
                       menuSubItem("Snow vs. Average Temp", tabName = "snow_temp")
                     )
                   )),
                   dashboardBody(
                     tabItems(
                       tabItem(tabName = "intro",
                               fluidRow(
                                   box(
                                     title = "Final Project: Boston Weather Trend:",
                                     width = 12,
                                     status = "primary",
                                     htmlOutput("introduction")
                                   )
                                   )),
                       tabItem(tabName = "temperaturegradient",
                               fluidRow(
                                   box(
                                   width = 4,
                                   title = "Select Parameters",
                                   status = "info",
                                   selectInput(
                                     inputId = "gradients",
                                     label = "Temperature Parameters",
                                     choices = c("Average Temperature" = "boston$TAVG",
                                                 "Maximum Temperature" = "boston$TMAX",
                                                 "Minimum Temperature" = "boston$TMIN"),
                                     selected = "Average Temperature"
                                   )),
                                   box(
                                     width = 8,
                                     title = "Temperature Gradient",
                                     status = "primary",
                                     conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                                      div(
                                                        img(src = 'https://icons8.com/preloaders/svg-preview.php?preloader_id=25'),
                                                        style = "text-align: center;"
                                                      )),
                                     plotOutput("grad"))
                                   )),
                       tabItem(tabName = "timeseries",
                               fluidRow(
                                 tabBox(width = 12,
                                   title = "Time series of Average Temperature",
                                   # The id lets us use input$tabset1 on the server to find the current tab
                                   id = "tabset1", height = "500px",
                                   tabPanel("Observed", 
                                            conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                                                         div(
                                                                           img(src = 'https://icons8.com/preloaders/svg-preview.php?preloader_id=25'),
                                                                           style = "text-align: center;"
                                                                         )),
                                            dygraphOutput("timeseries1")),
                                   tabPanel("Trend", 
                                            conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                                             div(
                                                               img(src = 'https://icons8.com/preloaders/svg-preview.php?preloader_id=25'),
                                                               style = "text-align: center;"
                                                             )),
                                            dygraphOutput("timeseries2")),
                                   tabPanel("Seasonal", 
                                            conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                                             div(
                                                               img(src = 'https://icons8.com/preloaders/svg-preview.php?preloader_id=25'),
                                                               style = "text-align: center;"
                                                             )),
                                            dygraphOutput("timeseries3")),
                                   tabPanel("Random", 
                                            conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                                             div(
                                                               img(src = 'https://icons8.com/preloaders/svg-preview.php?preloader_id=25'),
                                                               style = "text-align: center;"
                                                             )),
                                            dygraphOutput("timeseries4")),
                                   br(),
                                   helpText("Drag to zoom in (double click to zoom back out).")
                                 )
                                   )),
                       tabItem(tabName = "forecast",
                               fluidRow(
                                 box(width = 4,
                                   title = "Select Parameters",
                                     status = "primary",
                                     numericInput("months", label = "Months to Predict", 
                                                  value = 24, min = 12, max = 60, step = 12),
                                     selectInput("interval", label = "Prediction Interval",
                                                 choices = c("0.80", "0.90", "0.95", "0.99"),
                                                 selected = "0.95"),
                                     checkboxInput("showgrid", label = "Show Grid", value = TRUE),
                                     hr(),
                                     div(strong("From: "), textOutput("from", inline = TRUE)),
                                     div(strong("To: "), textOutput("to", inline = TRUE)),
                                     div(strong("Date clicked: "), textOutput("clicked", inline = TRUE)),
                                     div(strong("Nearest point clicked: "), textOutput("point", inline = TRUE)),
                                     br(),
                                     helpText("Click and drag to zoom in (double click to zoom back out).")
                                     ),
                                 box(width = 8,
                                   conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                                      div(
                                                        img(src = 'https://icons8.com/preloaders/svg-preview.php?preloader_id=25'),
                                                        style = "text-align: center;"
                                                      )),
                                   dygraphOutput("dygraph")
                                 )
                               )
                       ),
                       tabItem(tabName = "prcp_temp",
                               fluidRow(
                                 box(title = "Overall Correlation between Temperature and Precipitation",
                                   width = 12,
                                   status = "primary",
                                   conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                                                       div(
                                                                         img(src = 'https://icons8.com/preloaders/svg-preview.php?preloader_id=25'),
                                                                         style = "text-align: center;"
                                                                       )),
                                   plotOutput("prcp_temp1")
                                 ),
                                 box(
                                   title = "Correlation between Temperature and Precipitation by Month",
                                   width = 12,
                                   status = "success",
                                   conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                                    div(
                                                      img(src = 'https://icons8.com/preloaders/svg-preview.php?preloader_id=25'),
                                                      style = "text-align: center;"
                                                    )),
                                   plotOutput("prcp_temp2")
                                 ),
                                 box(
                                   width = 12,
                                   status = "danger",
                                   "From the above plots, we can come to the conclusion that the temperature is barely correlated to the precipitation of Boston. 
                                    I also conduct correlation test on these two variables,
                                   the result of which shows that the r value is -0.121, a very weak negative relationship between these two variables.
                                   The precipitation among the 12 months has no obvious difference."
                                  
                                 )
                               )
                       ),
                       tabItem(tabName = "windspeed_temp",
                               fluidRow(
                                 box(title = "Overall Correlation between Wind Speed and Temperature",
                                     width = 12,
                                     status = "primary",
                                     conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                                      div(
                                                        img(src = 'https://icons8.com/preloaders/svg-preview.php?preloader_id=25'),
                                                        style = "text-align: center;"
                                                      )),
                                     plotOutput("wind_temp1")
                                 ),
                                 box(
                                   title = "Correlation between Wind Speed and Temperature by Month",
                                   width = 12,
                                   status = "success",
                                   conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                                    div(
                                                      img(src = 'https://icons8.com/preloaders/svg-preview.php?preloader_id=25'),
                                                      style = "text-align: center;"
                                                    )),
                                   plotOutput("wind_temp2")
                                 ),
                                 box(title = "Conclusion of Correlation between Wind Speed and Temperature",
                                   width = 12,
                                   status = "danger",
                                   "The above plots demonstrate that there is a strongly negative correlation between the temperature and wind speed in Boston. 
                                   As the wind speed increases, the average temperature decreases. Correlation test is conducted on these two variables.
                                   the result shows that the r value is -0.625, a strong negative relationship between these two variables.",
                                   "Besides, the second plot shows an obvious difference in terms of the wind speed among different months. High wind speed are likely to
                                   concentrate in January and February, while June, July and August have relatively low wind speed."
                                 )
                               )
                       ),
                       tabItem(tabName = "snow_temp",
                               fluidRow(
                                 box(title = "Overall Correlation between Snow and Temperature",
                                     width = 12,
                                     status = "primary",
                                     conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                                                         div(
                                                                           img(src = 'https://icons8.com/preloaders/svg-preview.php?preloader_id=25'),
                                                                           style = "text-align: center;"
                                                                         )),
                                     plotOutput("snow_temp1")
                                 ),
                                 box(
                                   title = "Correlation between Snow and Temperature by Month",
                                   width = 12,
                                   status = "success",
                                   conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                                    div(
                                                      img(src = 'https://icons8.com/preloaders/svg-preview.php?preloader_id=25'),
                                                      style = "text-align: center;"
                                                    )),
                                   plotOutput("snow_temp2")
                                 ),
                                 box(title = "Conclusion of Correlation between Snow and Temperature",
                                   width = 12,
                                   status = "danger",
                                   "From the above plots, we can come to the conclusion that the temperature is slightly negatively correlated to the amount of snowfall in Boston. 
                                   As the temperature goes up, the amount of snowfall decreases. I also conduct correlation test on these two variables,
                                   the result of which shows that the r value is -0.215, a weak negative relationship between these two variables.",
                                   "Besides, the second plot shows that the snowfall in Boston concentrates in three months: December, January and February."
                                 )
                               )
                       )
                       )))
                   

server <- shinyServer(function(input, output){
  
  output$introduction <- renderUI({
    HTML(paste("By Senhao Li","",
      "This project tends to explore the weather trend of Boston during the past 82 years, from Jan. 1, 1936 to May 1, 2018.",
          "Specifically, we will be seeing the following analysis in this project：",
          "- Temperature Gradient of the past 82 years by month for horizontal comparison;", 
          "- Decomposed Time series of the average temperature during the past 82 years;",
          "- Temperature Forecast for the future five years;",
          "- Correlations between average temperature and other weather features.", 
      "",
      "I'll also test the following hypothesis through this project:",
      "1. The average temperature is gradually increasing overall;",
      "2. The precipitation is positively correlated with the temperature;",
      "3. The temperature is negatively correlated with the wind speed;",
      "4. The amount of snowfall is negatively correlated with the temperature.",
          sep = "<br/>"))
  })
 
  output$grad <- renderPlot({
    
    ggplot(data=boston, aes(x=YEAR,y=month_ordered)) + 
      geom_tile(aes_string(fill = input$gradients),colour = "white") + 
      scale_fill_gradientn(colours=rev(brewer.pal(10,'Spectral')), na.value = "grey98",
                           limits = c(-20, 100)) + 
      theme(legend.title=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(), plot.title = element_text(hjust = .5)) +
      scale_x_continuous(breaks=seq(1936,2018,5))
  })
  
  output$timeseries1 <- renderDygraph({
    dygraph(ts.decompose$x) %>% dyRangeSelector() %>% dyOptions(drawGrid = F)
  })
  
  output$timeseries2 <- renderDygraph({
    dygraph(ts.decompose$trend) %>% dyRangeSelector() %>% dyOptions(drawGrid = F)
  })
  
  output$timeseries3 <- renderDygraph({
    dygraph(ts.decompose$seasonal) %>% dyRangeSelector() %>% dyOptions(drawGrid = F)
    
  })
  
  output$timeseries4 <- renderDygraph({
    dygraph(ts.decompose$random) %>% dyRangeSelector() %>% dyOptions(drawGrid = F)
    
  })
  
  predicted <- reactive({
    hw <- HoltWinters(myts)
    predict(hw, n.ahead = input$months, 
            prediction.interval = TRUE,
            level = as.numeric(input$interval))
  })
  
  output$dygraph <- renderDygraph({
    dygraph(predicted(), main = "Predicted Average Temperature/Month") %>%
      dySeries(c("lwr", "fit", "upr"), label = "Temperature") %>%
      dyOptions(drawGrid = input$showgrid)
  })
  
  output$from <- renderText({
    strftime(req(input$dygraph_date_window[[1]]), "%d %b %Y")      
  })
  
  output$to <- renderText({
    strftime(req(input$dygraph_date_window[[2]]), "%d %b %Y")
  })
  
  output$clicked <- renderText({
    strftime(req(input$dygraph_click$x), "%d %b %Y")
  })
  
  output$point <- renderText({
    paste0('X = ', strftime(req(input$dygraph_click$x_closest_point), "%d %b %Y"), 
           '; Y = ', req(input$dygraph_click$y_closest_point))
  })
  
  pal <- colorRampPalette(c("blue", "yellow", "red", "green"))
  mycols=pal(12)
  
  output$prcp_temp1 <- renderPlot({
    ggplot(data = monthly,mapping = aes(x=TAVG,y=PRCP)) + geom_point()+
      geom_smooth(method = "glm", method.args = list(family = "gaussian"))+ 
      xlab('Temperature') + ylab('Precipitation')
  })
  
  output$prcp_temp2 <- renderPlot({
    ggplot(data=monthly,aes(x=TAVG,y=PRCP,color=factor(month_ordered))) + 
      geom_point(alpha=.5) + scale_color_manual(values=mycols) + 
      xlab('Temperature') + ylab('Precipitation')
  })
  
  output$wind_temp1 <- renderPlot({
    ggplot(data = monthly,mapping = aes(x = AWND, y = TAVG)) + geom_point()+
      geom_smooth(method = "glm", method.args = list(family = "gaussian")) + 
      xlab('Wind speed') + ylab('Temprature')
  })
  
  output$wind_temp2 <- renderPlot({
    ggplot(data=monthly,aes(x=AWND,y=TAVG,color=factor(month_ordered))) + 
      geom_point(alpha=.5) + scale_color_manual(values=mycols) + 
      xlab('Wind speed') + ylab('Temprature')
  })
  
  output$snow_temp1 <- renderPlot({
    ggplot(data = monthly,mapping = aes(x = TAVG, y = SNOW)) + geom_point()+
      geom_smooth(method = "glm", method.args = list(family = "gaussian"))+ 
      xlab('Temprature') + ylab('Snow')
  })
  
  output$snow_temp2 <- renderPlot({
    ggplot(data=monthly,aes(x=TAVG,y=SNOW,color=factor(month_ordered))) + 
      geom_point(alpha=.5) + scale_color_manual(values=mycols) + 
      xlab('Temprature') + ylab('Snow')
    
  })
  
})

shinyApp(ui = ui, server = server)