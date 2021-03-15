library(tidyverse)
library(shiny)
library(shinythemes)
library(plotly)

data <- read_csv('https://raw.githubusercontent.com/oggyluky11/DATA608-SPRING-2021/main/Module_3/cleaned-cdc-mortality-1999-2010-2.csv')

data2 <- data %>%
    add_row(data %>%
                group_by(ICD.Chapter, Year) %>%
                summarise(Crude.Rate = weighted.mean(Crude.Rate, Population), 
                          Deaths = mean(Deaths),
                          Population = mean(Population)) %>%
                mutate(State = 'National Avg')
    )

author <- 'Fan Xu'
date <- '3/14/2021'


#cerulean, cosmo, cyborg, darkly, flatly, journal, lumen, paper, readable, sandstone, simplex, slate, spacelab, superhero, united, yeti

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme('united'),
    
    titlePanel('DATA 608 MODULE 3 - Interactive Visualizations in R'),
    
    p(em(author)),
    
    p(em(date)),
    
    navlistPanel(
        
        #Question 1
        tabPanel('QUESTION 1', 
                 
                 
                 h5(strong('Question:')),
                 
                 p('As a researcher, you frequently compare mortality rates from particular causes across different States. You need a visualization that will let you see (for 2010 only) the crude mortality rate, across all States, from one cause (for example, Neoplasms, which are effectively cancers). Create a visualization that allows you to rank States by crude mortality for each cause of death.'),
                 
                 h5(strong('Answer:')),
                 
                 wellPanel(
                     h5(strong('Criteria')),
                     
                     fluidRow(
                         column(3, selectInput('q1_year', 'Year', unique(data$Year), selected = 2010)),
                         column(9, selectInput('q1_cause', 'Cause', unique(data$ICD.Chapter), selected = 'Neoplasms')),
                     ),
                     fluidRow(
                         column(1, offset = 10, actionButton('q1_run', 'Run')))

                 ),
                 
                 strong(textOutput('q1_title')),
                 plotlyOutput('q1_plot')
                 
                 ),
        
        #Question 2
        tabPanel('QUESTION 2',
                 
                 h5(strong('Question:')),
                 
                 p('Often you are asked whether particular States are improving their mortality rates (per cause) faster than, or slower than, the national average. Create a visualization that lets your clients see this for themselves for one cause of death at the time. Keep in mind that the national average should be weighted by the national population.'),
                 
                 h5(strong('Answer:')),
 
                 wellPanel(
                     h5(strong('Criteria')),
                     selectInput('q2_cause', 'Cause', unique(data2$ICD.Chapter), selected = 'Neoplasms'),
                     checkboxGroupInput('q2_select', 
                                        'States: ', 
                                        selected = c('AL','National Avg'),
                                        unique(data2$State),
                                        inline = TRUE),  
                     fluidRow(
                         column(1, offset = 10, actionButton('q2_run', 'Run')))
                     
                 ),
                 
                 strong(textOutput('q2_title')),
                 plotlyOutput('q2_plot')
                 
        )
                 
        
    )
    
)




# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #Question 1
    rv <- reactiveValues(df = data %>% filter(Year == 2010, ICD.Chapter == 'Neoplasms'),
                         year = 2010,
                         cause = 'Neoplasms')
    
    observeEvent(input$q1_run, {
        rv$df <- data %>% filter(Year == input$q1_year, ICD.Chapter == input$q1_cause)
        rv$year <- input$q1_year
        rv$cause <- input$q1_cause
    })
    
    output$q1_title <- renderText(
        paste('Crude Mortality Rate of', rv$cause, 'in Year', rv$year, 'Across States'))
    
    output$q1_plot <- renderPlotly({
        plot_ly(data = rv$df, 
                x = ~Crude.Rate, 
                y = ~reorder(State, Crude.Rate), 
                color = ~Crude.Rate %>% as.factor(),
                colors = 'Reds',
                type = 'bar', 
                orientation = 'h',
                height = 1000,
                width = 500) %>%
            layout(yaxis = list(title = 'State'),
                   showlegend = FALSE)
        
    })
    
    
    #Question 2
    rv2 <- reactiveValues(df = data2 %>% filter(ICD.Chapter == 'Neoplasms',
                                                State %in% c('AL','National Avg')),
                         cause = 'Neoplasms')
    
    observeEvent(input$q2_run, {
        rv2$df <- data2 %>% filter(ICD.Chapter == input$q2_cause,
                                 State %in% c(input$q2_select))
        rv2$cause <- input$q2_cause
    })
    
    
    output$q2_title <- renderText(
        paste('Comparison: Crude Mortality Rate vs National Average of Cause:', rv2$cause))
    
    output$q2_plot <- renderPlotly({
        rv2$df %>%
            plot_ly(
                x = ~Deaths, 
                y = ~Crude.Rate, 
                size = ~Population, 
                color = ~State, 
                frame = ~Year, 
                text = ~paste('Cause:',ICD.Chapter,'\n', 
                              'State:',State, '\n',
                              'Deaths:',Deaths, '\n',
                              'Population:', Population, '\n',
                              'Crude Mortality Rate:', Crude.Rate),
                hoverinfo = "text",
                type = 'scatter',
                mode = 'markers'
            )
        })

}





# Run the application 
shinyApp(ui = ui, server = server)
