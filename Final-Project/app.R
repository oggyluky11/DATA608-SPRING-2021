library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)
library(RSocrata)
library(lubridate)
library(leaflet)
library(sp)
library(dashboardthemes)
library(shinyWidgets)


#data from year 2013 - 2019
shooting_url1 <- 'https://data.cityofnewyork.us/resource/833y-fsy8.csv'
shooting_data1 <- read.socrata(shooting_url1)

#data from year 2020
shooting_url2 <- 'https://data.cityofnewyork.us/resource/5ucz-vwe8.csv'
shooting_data2 <- read.socrata(shooting_url2)

shooting_data <- shooting_data1 %>%
  rbind(shooting_data2) %>%
  mutate(year = occur_date %>% 
           year(), 
         time = ymd_hms(str_c(occur_date,occur_time)) %>% 
           round_date('hour') %>%
           format('%H:%M:%S'),
         occur_date = ymd(occur_date),
         statistical_murder_flag = case_when(statistical_murder_flag == 'true' ~ 'Yes',
                                             statistical_murder_flag == 'false' ~ 'No',
                                             TRUE ~ 'UNKNOWN'),
         vic_sex = case_when(vic_sex == 'F' ~ 'Female',
                             vic_sex == 'M' ~ 'Male',
                             TRUE ~ 'UNKNOWN'),
         perp_sex = case_when(perp_sex == 'F' ~ 'Female',
                              perp_sex == 'M' ~ 'Male',
                              TRUE ~ 'UNKNOWN'),
         perp_race = if_else(str_trim(perp_race) == '', 
                             'UNKNOWN',
                             perp_race)
  ) %>%
  select(year, time,everything())

murder_flag <- shooting_data %>% 
  select(statistical_murder_flag) %>%
  distinct() %>%
  arrange(desc(statistical_murder_flag)) %>%
  .$statistical_murder_flag

vicRace_value <- shooting_data %>% 
  select(vic_race) %>%
  distinct() %>%
  arrange(vic_race) %>%
  .$vic_race


vicSex_value <- shooting_data %>% 
  select(vic_sex) %>%
  distinct() %>%
  arrange(vic_sex) %>%
  .$vic_sex

vicAge_value <- shooting_data %>% 
  select(vic_age_group) %>%
  distinct() %>%
  arrange(vic_age_group) %>%
  .$vic_age_group

date_value <- shooting_data %>% 
  select(occur_date) %>%
  distinct() %>%
  arrange(occur_date) %>%
  .$occur_date

year_value <- shooting_data %>% 
  select(year) %>%
  distinct() %>%
  arrange(year) %>%
  .$year

boro_value <- shooting_data %>% 
  select(boro) %>%
  distinct() %>%
  arrange(boro) %>%
  .$boro



####Shiny Dashboard####
sidebar <- dashboardSidebar(
  
  setSliderColor('orange', 1),
  
  shinyDashboardThemes(theme = 'grey_light'),
  
  sidebarMenu(
    menuItem("Project Introduction",
             tabName = 'Intro',
             icon = icon("info")
    ),
    
    menuItem("Geographic & Time Series", 
             tabName = "dashboard1", 
             icon = icon("dashboard")
    ),
    menuItem("Other Categorical Information", 
             tabName = "dashboard2",
             icon = icon("dashboard") 
             #badgeLabel = "new", badgeColor = "green"
    )
  )
)

body <- dashboardBody(
  
  
  tabItems(
    
    tabItem(tabName = 'Intro',
            fluidRow(
            column(width = 12,
                   box(title = strong('Introduction'),
                       p('This project is to create a interactive Shiny dashboard with NYPD Shooting Incident Data from NYC Open Data Socrata Open Data API as data source targeting a multi angle view of the data including time series, location, and other informative data attributes. The purpose of this project is to develop interactive data visualization app using public SODA API and dynamic data visualization tools such as leaflet, plotly, etc,.'),
                       br(),
                       h5(strong('Introduction to Data')),
                       h6(strong('1. NYPD Shooting Incident Data (Year 2020)')),
                       p('List of every shooting incident that occurred in NYC during the current calendar year. This is a breakdown of every shooting incident that occurred in NYC during the current calendar year. This data is manually extracted every quarter and reviewed by the Office of Management Analysis and Planning before being posted on the NYPD website. Each record represents a shooting incident in NYC and includes information about the event, the location and time of occurrence. In addition, information related to suspect and victim demographics is also included. This data can be used by the public to explore the nature of police enforcement activity.'),
                       tagList('URL: ', a("https://data.cityofnewyork.us/Public-Safety/NYPD-Shooting-Incident-Data-Year-To-Date-/5ucz-vwe8", 
                                          href="https://data.cityofnewyork.us/Public-Safety/NYPD-Shooting-Incident-Data-Year-To-Date-/5ucz-vwe8")),
                       br(),
                       tagList('SODA API: ', a('https://data.cityofnewyork.us/resource/5ucz-vwe8.csv',
                                               href='https://data.cityofnewyork.us/resource/5ucz-vwe8.csv')),
                       br(),
                       br(),
                       h6(strong('2. NYPD Shooting Incident Data (Historic)')),
                       p('List of every shooting incident that occurred in NYC from year 2006 to 2019.'),
                       tagList('URL: ', a("https://data.cityofnewyork.us/Public-Safety/NYPD-Shooting-Incident-Data-Historic-/833y-fsy8", 
                                          href="https://data.cityofnewyork.us/Public-Safety/NYPD-Shooting-Incident-Data-Historic-/833y-fsy8")),
                       br(),
                       tagList('SODA API: ', a('https://data.cityofnewyork.us/resource/833y-fsy8.csv',
                                               href='https://data.cityofnewyork.us/resource/833y-fsy8.csv')),
                       br(),
                       br(),
                       h5(strong('Data Dictionary')),
                       dataTableOutput('dt')
                   )
                )
            )
    ),
    #Tab 1
    tabItem(tabName = "dashboard1",
            strong(h1("NEW YORK CITY HISTORICAL SHOOTING DATA DASHBOARD")),
            fluidRow(
              column(width = 2,
                     #Box 1 Criteria
                     box(title = strong('Criteria'),
                         width = NULL,
                         height = '820px',
                         #solidHeader = TRUE,
                         #status = "primary",
                         dateRangeInput('dateInput',
                                        'Date Range:',
                                        min = min(date_value),
                                        max = max(date_value),
                                        start = min(date_value),
                                        end = max(date_value)),
                         checkboxGroupInput('murderFlag',
                                            "Shooting resulted in the victim's death:",
                                            choices = murder_flag,
                                            selected = murder_flag,
                                            inline = TRUE),
                         checkboxGroupInput('boroInput',
                                            'Where the shooting occurred:',
                                            choices = boro_value,
                                            selected = boro_value),
                         checkboxGroupInput('vicSexInput',
                                            "Victim's sex description:",
                                            choices = vicSex_value,
                                            selected = vicSex_value,
                                            inline = TRUE),
                         checkboxGroupInput('vicAgeInput',
                                            "Victim's age group:",
                                            choices = vicAge_value,
                                            selected = vicAge_value),
                         checkboxGroupInput('vicRaceInput',
                                            "Victim's race description:",
                                            choices = vicRace_value,
                                            selected = vicRace_value),
                     ),
              ),
              column(width = 8, 
                     #Box 3 Monthly Data Time Series
                     box(title = strong('Location of Shooting Cases'),
                         width = NULL,
                         height = '500px',
                         #solidHeader = TRUE,
                         #status = 'primary',
                         #plotlyOutput('plot1')
                         uiOutput('plot3')
                     ),
                     fluidRow(
                       box(title = strong('Shooting Count by Borough'),
                           width = 4,
                           height = '300px',
                           #solidHeader = TRUE,
                           #status = 'primary',
                           plotlyOutput('plot2')
                       ),
                       box(title = strong('Historical Monthly Shooting Count'),
                           width = 8,
                           height = '300px',
                           #solidHeader = TRUE,
                           #status = 'primary',
                           #uiOutput('plot3')
                           plotlyOutput('plot1')
                       )
                     )
              ),
              column(width = 2, 
                     box(title = strong('Shooting Count by Time'),
                         width = NULL,
                         height = '820px',
                         #solidHeader = TRUE,
                         #status = 'primary',
                         plotlyOutput('plot4')
                     )
              ),
            )
    ),
    
    tabItem(tabName = "dashboard2",
            strong(h1("NEW YORK CITY HISTORICAL SHOOTING DATA DASHBOARD")),
            
            
            fluidRow(
              column(width = 12,
                     box(title = strong('Criteria'),
                         width = NULL,
                         height = '140px',
                         #solidHeader = TRUE,
                         #status = "primary",
                         sliderInput('dateInput2',
                                     'Date Range:',
                                     min = min(date_value),
                                     max = max(date_value),
                                     value = c(min(date_value),max(date_value))))
              )
            ),
            
            
            
            fluidRow(
              column(width = 3,
                     box(title = strong('Victim Count by Age Group'),
                         width = NULL,
                         height = '320px',
                         #solidHeader = TRUE,
                         #status = "primary",
                         plotlyOutput('plot5')),
                     
                     box(title = strong('Perpetrator Count by Age Group'),
                         width = NULL,
                         height = '320px',
                         #solidHeader = TRUE,
                         #status = "primary",
                         plotlyOutput('plot6'))
              ),
              column(width = 2,
                     box(title = strong('Victim Count by Sex'),
                         width = NULL,
                         height = '320px',
                         #solidHeader = TRUE,
                         #status = "primary",
                         plotlyOutput('plot7')),
                     
                     box(title = strong('Perpetrator Count by Sex'),
                         width = NULL,
                         height = '320px',
                         #solidHeader = TRUE,
                         #status = "primary",
                         plotlyOutput('plot8'))
              ),  
              column(width = 5,
                     box(title = strong('Victim Count by Race'),
                         width = NULL,
                         height = '320px',
                         #solidHeader = TRUE,
                         #status = "primary",
                         plotlyOutput('plot9')),
                     
                     box(title = strong('Perpetrator Count by Race'),
                         width = NULL,
                         height = '320px',
                         #solidHeader = TRUE,
                         #status = "primary",
                         plotlyOutput('plot10'))
              ), 
              column(width = 2,
                     
                     valueBoxOutput('vBox1',
                                    width = 12),
                     
                     valueBoxOutput('vBox2',
                                    width = 12),
                     
                     valueBoxOutput('vBox3',
                                    width = 12),
                     
                     valueBoxOutput('vBox4',
                                    width = 12),
                     
                     valueBoxOutput('vBox5',
                                    width = 12),               
                     
              ), 
              
            )
    )
  )
)

ui <- dashboardPage(
  dashboardHeader(title = 'DATA608 Final Project',
                  titleWidth = 350),
  sidebar,
  body
)

server <- function(input, output) {
  
  output$dt <- renderDataTable(
    read_csv('https://raw.githubusercontent.com/oggyluky11/DATA608-SPRING-2021/main/Final-Project/NYPD_Shooting_Incident_Data_Dictionary.csv'),
    options = list(pageLength = 3, autoWidth = FALSE)
    )
  
  
  rv <- reactiveValues(monthlyCnt = shooting_data %>%
                         group_by(occur_date) %>%
                         tally(name = 'Count'),
                       
                       cntByBoro = shooting_data %>%
                         group_by(boro) %>%
                         tally(name = 'Count'),
                       
                       cntByTime = shooting_data %>%
                         group_by(time) %>%
                         tally(name = 'Count'),
                       
                       mapData = shooting_data)
  
  checks <- reactive({
    list(input$dateInput,
         input$vicRaceInput,
         input$vicSexInput,
         input$vicAgeInput,
         input$boroInput,
         input$murderFlag)
  })
  
  observeEvent(checks(), {list(
    
    rv$monthlyCnt <- shooting_data %>%
      filter(occur_date %>% between(min(input$dateInput), max(input$dateInput)),
             vic_race %in% input$vicRaceInput,
             vic_sex %in% input$vicSexInput,
             boro %in% input$boroInput,
             statistical_murder_flag %in% input$murderFlag,
             vic_age_group %in% input$vicAgeInput) %>%
      group_by(occur_date) %>%
      tally(name = 'Count'),
    
    rv$cntByBoro <- shooting_data %>%
      filter(occur_date %>% between(min(input$dateInput), max(input$dateInput)),
             vic_race %in% input$vicRaceInput,
             vic_sex %in% input$vicSexInput,
             boro %in% input$boroInput,
             statistical_murder_flag %in% input$murderFlag,
             vic_age_group %in% input$vicAgeInput) %>%
      group_by(boro) %>%
      tally(name = 'Count'),
    
    rv$cntByTime <- shooting_data %>%
      filter(occur_date %>% between(min(input$dateInput), max(input$dateInput)),
             vic_race %in% input$vicRaceInput,
             vic_sex %in% input$vicSexInput,
             boro %in% input$boroInput,
             statistical_murder_flag %in% input$murderFlag,
             vic_age_group %in% input$vicAgeInput) %>%
      group_by(time) %>%
      tally(name = 'Count'),
    
    rv$mapData <- shooting_data %>%
      filter(occur_date %>% between(min(input$dateInput), max(input$dateInput)),
             vic_race %in% input$vicRaceInput,
             vic_sex %in% input$vicSexInput,
             boro %in% input$boroInput,
             statistical_murder_flag %in% input$murderFlag,
             vic_age_group %in% input$vicAgeInput))
    
  })
  
  observeEvent(input$dateInput2, {
    rv$dash2 <- shooting_data %>%
      filter(occur_date %>% between(min(input$dateInput2), max(input$dateInput2)))
  })
  
  
  output$plot1 <- renderPlotly({
    plot_ly(rv$monthlyCnt,
            x = ~occur_date,
            y = ~Count,
            type = 'bar',
            height = 240,
            marker = list(color = 'grey')) %>%
      layout(xaxis = list(title = 'Year-Month'),
             yaxis = list(title = 'Shooting Case Count'))
  })
  
  output$plot2 <- renderPlotly({
    plot_ly(rv$cntByBoro,
            labels = ~boro,
            values = ~Count,
            type = 'pie',
            textinfo = 'label+percent',
            marker = list(colors=c('burlywood', 'orange', 'lightgrey', 'cornsilk', 'grey')),
                                # Bronx, Brooklyn, Manhattan, Queens, Staten Island
            #textposition = 'outside',
            height = 240) %>%
      layout(showlegend = FALSE)
  })
  
  
  output$plot3 <- renderUI({
    leafletOutput('leafMap',
                  width = '100%',
                  height = 440)
  })
  
  output$leafMap <- renderLeaflet({
    leaflet(rv$mapData) %>%
      addCircles(lng = ~longitude,
                 lat = ~latitude,
                 radius = 100,
                 stroke = FALSE,
                 fillOpacity = 0.2,
                 color = 'orange') %>%
      addProviderTiles(providers$Stamen.TonerLite)
  })
  
  
  
  output$plot4 <- renderPlotly({
    plot_ly(rv$cntByTime,
            x = ~Count,
            y = ~reorder(time, desc(time)),
            type = 'bar',
            marker = list(color = 'grey'),
            orientation = 'h',
            height = 750) %>%
      layout(xaxis = list(title = 'Shooting Case Count'),
             yaxis = list(title = 'Time'))
  })
  
  output$plot5 <- renderPlotly({
    plot_ly(rv$dash2 %>%
              group_by(vic_age_group) %>%
              tally(name = 'Count'),
            x = ~vic_age_group,
            y = ~Count,
            type = 'bar',
            marker = list(color = 'grey'),
            height = 250) %>%
      layout(xaxis = list(title = 'Age Group'),
             yaxis = list(title = 'Victim Count'))
  })
  
  output$plot6 <- renderPlotly({
    plot_ly(rv$dash2 %>%
              mutate(perp_age_group = str_replace(perp_age_group, '^[0-9]*$', 'UNKNOWN')) %>%
              group_by(perp_age_group) %>%
              tally(name = 'Count'),
            x = ~perp_age_group,
            y = ~Count,
            type = 'bar',
            marker = list(color = 'grey'),
            height = 250) %>%
      layout(xaxis = list(title = 'Age Group'),
             yaxis = list(title = 'Perpetrator Count'))
  })
  
  output$plot7 <- renderPlotly({
    plot_ly(rv$dash2 %>%
              group_by(vic_sex) %>%
              tally(name = 'Count'),
            labels = ~vic_sex,
            values = ~Count,
            type = 'pie',
            textinfo = 'label+percent',
            marker = list(colors=c('burlywood', 'orange', 'lightgrey', 'cornsilk', 'grey')),
            height = 260) %>%
      layout(showlegend = FALSE)
  })
  
  output$plot8 <- renderPlotly({
    plot_ly(rv$dash2 %>%
              group_by(perp_sex) %>%
              tally(name = 'Count'),
            labels = ~perp_sex,
            values = ~Count,
            type = 'pie',
            textinfo = 'label+percent',
            marker = list(colors=c('burlywood', 'orange', 'lightgrey', 'cornsilk', 'grey')),
            height = 260) %>%
      layout(showlegend = FALSE)
  })    
  
  
  output$plot9 <- renderPlotly({
    plot_ly(rv$dash2 %>%
              group_by(vic_race) %>%
              tally(name = 'Count'), 
            x = ~Count,
            y = ~reorder(vic_race, Count),
            type = 'bar',
            marker = list(color = 'grey'),
            orientation = 'h',
            height = 250) %>%
      layout(xaxis = list(title = 'Race'),
             yaxis = list(title = ''))
  })  
  
  
  output$plot10 <- renderPlotly({
    plot_ly(rv$dash2 %>%
              group_by(perp_race) %>%
              tally(name = 'Count'), 
            x = ~Count,
            y = ~reorder(perp_race, Count),
            type = 'bar',
            marker = list(color = 'grey'),
            orientation = 'h',
            height = 250) %>%
      layout(xaxis = list(title = 'Race'),
             yaxis = list(title = ''))
  }) 
  
  
  output$vBox1 <- renderValueBox({
    valueBox(
      rv$dash2 %>%
        filter(boro == 'BRONX') %>%
        tally(name = 'Count') %>%
        .$Count, 
      "THE BRONX", 
      icon = icon("ambulance"),
      color = "orange"
    )
  })  
  
  output$vBox2 <- renderValueBox({
    valueBox(
      rv$dash2 %>%
        filter(boro == 'BROOKLYN') %>%
        tally(name = 'Count') %>%
        .$Count, 
      "BROOKLYN", 
      icon = icon("ambulance"),
      color = "orange"
    )
  }) 
  
  output$vBox3 <- renderValueBox({
    valueBox(
      rv$dash2 %>%
        filter(boro == 'MANHATTAN') %>%
        tally(name = 'Count') %>%
        .$Count, 
      "MANHATTAN", 
      icon = icon("ambulance"),
      color = "orange"
    )
  }) 
  
  output$vBox4 <- renderValueBox({
    valueBox(
      rv$dash2 %>%
        filter(boro == 'QUEENS') %>%
        tally(name = 'Count') %>%
        .$Count, 
      "QUEENS", 
      icon = icon("ambulance"),
      color = "orange"
    )
  }) 
  
  output$vBox5 <- renderValueBox({
    valueBox(
      rv$dash2 %>%
        filter(boro == 'STATEN ISLAND') %>%
        tally(name = 'Count') %>%
        .$Count, 
      "STATEN ISLAND", 
      icon = icon("ambulance"),
      color = "orange"
    )
  }) 
  
}

shinyApp(ui, server)
