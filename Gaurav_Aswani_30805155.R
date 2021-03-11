
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



#Loading libraries
library(shiny)  		 #for shiny app						
library(plotly)      #makes plot interactive
library(ggplot2)     #for creating plots
library(highcharter) #for interactive plots
library(dplyr)       #faster data manipulations
library(countrycode) #for continents

#read the data in suicides variable 
suicides<-read.csv('master.csv')




# Define UI 
ui <- fluidPage(
  
                titlePanel("SUICIDES RATE OVERVIEW (1985 -2015)"),
                  mainPanel(
    
                            tabsetPanel(type = "tabs",
                            #TAB 1
                            tabPanel("YEAR",highchartOutput("TimeSeries")),
                            #TAB 2
                            tabPanel("IMPACT ON GDP",plotlyOutput("gdp")) ,
                            #TAB 3
                            tabPanel("COUNTRY AND GENDER ",plotlyOutput("gender"),plotlyOutput("age"),highchartOutput("map"),highchartOutput("mosaic"))
                             )
    
                            )
)

# Define server logic 
server <- function(input,output) {
  
                                        #Tab 3 Time Series Graph
  
  
  # trend_data data frame for suicidal trend analysis
    trend_data <- suicides %>%
    select(year, suicides_no, population) %>%
    group_by(year) %>% # grouping it by year
    summarise(suicide_rate = round((sum(suicides_no)/sum(population))*100000, 2)) 
  
  
    output$TimeSeries <- renderHighchart({
    
    
    highchart() %>% #calling highchart function
        #plotting time series graph
      hc_add_series(trend_data, hcaes(x = year, y = suicide_rate, color = year), type = "line") %>%
        #assigning tooltip
      hc_tooltip(crosshairs = TRUE, borderWidth = 2.1, headerFormat = "",pointFormat = paste("Year: <b>{point.x}</b> <br> Suicidal Rate: <b>{point.y}</b>")) %>%
        #providing title
      hc_title(text = "SUICIDE TREND OVER THE YEARS 1985-2015") %>% 
        hc_yAxis(title = list(text = "Suicide rates")) %>%  
      hc_xAxis(title = list(text = "Year")) %>%
      hc_legend(enabled = TRUE)
    
  })
 
  
                                                  #TAB 2 IMPACT ON GDP
  
    
    
    
    #creating a new column continent using countrycode library
  suicides$continent <- countrycode(suicides$country, "country.name", "continent")
  
  
  suicides['countryyear'] = sprintf("%s_%s", suicides$country, suicides$year) #creating column merging country and year
  #Dataframe for plotting impact of suicides on GDP
  
  suicides_countrywise <- suicides %>%      
    group_by(countryyear) %>% #grouping by new column
    summarise(
      year = head(year, 1),
      country = head(country, 1),
      population = sum(population),
      suicides_no = sum(suicides_no),
      continent= head(continent,1),
      Suicides100kPop = sum(population * Suicides100kPop) / (as.numeric(sum(population)) * n()),
      GDPPerCapita = mean(GDPPerCapita))
  
  #plotting bubble chart
  output$gdp <- renderPlotly({
                              gdp_plot<-ggplot(suicides_countrywise,aes(x = GDPPerCapita, 
                                        y = Suicides100kPop,
                                        size = population,
                                        color= continent,
                                        frame = year,
                                        group = country)) +
      geom_point(aes(size = population, frame = year, ids = country)) + #size of population
      scale_x_log10() + 
      scale_alpha_continuous(range = c(0.5, 0.9)) +
      labs(title= "Impact of Suicides on GDP with respect to countries") +
      labs(x = "GDP per Capita ")+
      labs(y="Suicides Per 100k Population")+
      theme_minimal()
      #for making plot interactive
      plty<-ggplotly(gdp_plot,tooltip = c('country', 'Suicides100kPop', 'GDPPerCapita'))
    
    
    
    
    
  })
  
  
  
  
                                          #TAB 3 PIE-CHART
  #pie_chart_data
  pie_chart_data <- suicides %>%
    select(sex, suicides_no, population) %>%
    group_by(sex) %>%
    summarise(suicide_capita = round((sum(suicides_no) / sum(population)) * 100000, 2)) 
  
  #plotting pie-chart
  output$gender <- renderPlotly({
                          fig <- plot_ly(pie_chart_data, labels = ~sex, values = ~suicide_capita, type = 'pie')
                           fig <- fig %>% layout(title = 'Suicides rate by Gender from 1985-2015',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
  }) 
  
  pie_chart_data1 <- suicides %>%
    select(age, suicides_no, population) %>%
    group_by(age) %>%
    summarise(suicide_capita = round((sum(suicides_no) / sum(population)) * 100000, 2)) 
  
  #plotting pie-chart
  output$age <- renderPlotly({
    fig <- plot_ly(pie_chart_data1, labels = ~age, values = ~suicide_capita, type = 'pie')
    fig <- fig %>% layout(title = 'Suicides rate by age from 1985-2015',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
  }) 
  
  
                                        # HIGHCHART MAP  
  
  #suicide_data_by_country dataframe for plotting highchart map
    suicide_data_by_country <- suicides %>%
    select(country, suicides_no, population) %>%
    group_by(country) %>%
    summarize(rates = round((sum(suicides_no)/sum(population))*100000, 2))
  

  
  
  output$map <- renderHighchart({
    
      # Create interactive world map.
    highchart() %>%
      hc_chart(backgroundColor = "steelblue") %>%
      #hc_add_series_map function helps to add and remove  series from highchart objects
      hc_add_series_map(worldgeojson, suicide_data_by_country, value = "rates", joinBy = c('name','country'))  %>% 
      hc_colorAxis(stops = color_stops()) %>% 
      hc_title(text = "Suicide Rates Globally") %>%
 
       # Making graph interactive by adding tooltip
      hc_tooltip(borderWidth = 2.1, headerFormat = "") 
  

  })
  
  
  
  
    
  
                                         
                                             #MOSAIC PLOT
  
  
  
  #DATA FRAME
  country_sex_mosaic <- suicides %>%
          select(country, sex, suicides_no, population) %>%
          group_by(country, sex) %>%
          summarise(rate = round((sum(suicides_no)/sum(population))*100000, 2))
  
  
  
  #PLOT
  
  output$mosaic <- renderHighchart({
    
    # Create bar chart of suicide by sex.
   
     highchart() %>%
    
      # #hc_add_series_map function helps to add and remove  series from highchart objects
      hc_add_series(country_sex_mosaic, hcaes(x = country, y = rate, group = sex), type = "bar")  %>% 
      
      # Making graph interactive by adding tooltip
      hc_tooltip(borderWidth = 2.1, pointFormat = paste(" Suicides per 100K: <b>{point.y}</b> <br> Gender: <b>{point.sex} ({point.percentage:.1f}%)</b>")) %>%
      
      #legend
       hc_legend(enabled = TRUE, colorByPoint = FALSE) %>%
      
      hc_title(text = "Suicides by genders across different countries") %>% 
     
      #managing x axis from country variable/column
      hc_xAxis(categories = suicide_data_by_country$country,labels = list(step = 1),min = 0, max = 20,
               scrollbar = list(enabled = TRUE)) %>%
      #naming Y axis
      hc_yAxis(title = list(text = "Suicidal Percentages across genders ")) %>%
      hc_plotOptions(bar = list(stacking = "percent", pointPadding = 0, groupPadding = 0, borderWidth = 0.4))
      
  })
  

  

  

}

# Run the application 
shinyApp(ui, server)

 


