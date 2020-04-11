library(shinydashboard)
library(leaflet)
library(plotly)
source("data_transform.R")



dashboardPage(
    dashboardHeader(
        title = "UG COVID19 Tracker", 
        titleWidth = "450"
    ),
    dashboardSidebar(
        h3("Toll Free Number:"),
        h5("080 020 3033 / 080 010 0066")
    ),
    dashboardBody(
        tabsetPanel(type = "tabs",
                    tabPanel("Summary",
                             fluidRow(
                                 valueBoxOutput("travel"),
                                 valueBoxOutput("follow"),
                                 #            valueBox(2661, "High risk travelers identified"),
                                 #            valueBox(980, "Individuals under follow-up")
                             ),
                             fluidRow(
                                 valueBoxOutput("tested"),
                                 valueBoxOutput("confirmed"),
                                 #            valueBox(2629, "Cumulative number of individuals tested"),
                                 #            vlaueBox(48, "Confirmed cases")            
                             ),
                             fluidRow(
                                 box(
                                     h2("Highlights"),
                                     h4("Zero (0) new COVID-19 cases have been confirmed today. 
                The cumulative total of COVID-19 confirmed cases reported 
                in the country remains 48.")
                                 )
                             ),
                             fluidRow(
                                 box(
                                     title = "Trend of suspect cases",
                                     plotlyOutput("trend", height = "400px", width = "700px")
                                 ),
                                 box(
                                     title = "No. of Suspect cases by district",
                                     plotOutput("ugMap", height = "400px", width = "700px")
                                 )
                                 
                             )
                             
                    ),
                    tabPanel("Case Management"
                        
                    ),
                    tabPanel("Labs"
                             
                    )
                    
            
        )        
    )
)