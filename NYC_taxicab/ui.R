library(shinydashboard)
dashboardPage(
    dashboardHeader(title='NYC Flights'),
    dashboardSidebar(
        sidebarUserPanel("Hao-Wei Chu",
                         image = 'nycdsa.png' ),
        sidebarMenu(
            menuItem("Hello world!", tabName = "welcome",
                     icon = icon("baby-carriage")),
            menuItem("Basic tables", tabName = "starter",
                     icon = icon("angellist")),
            menuItem("Hot spots", tabName = "spots",
                     icon = icon("binoculars")),
            menuItem("Plots", tabName = "plots", icon = icon("map")),
            menuItem("Data", tabName = "data", icon = icon("database"))
        )
        
        # selectizeInput(inputId='origin',label='Departure Airport',
        #                choices=unique(flights$origin)),
        # selectizeInput("dest", "Arrival Airport",
        #                choices=unique(flights$dest))
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = 'welcome',
                    h4("Hello world!\nThe goal of this project is to look at the taxi trip data, and hopefully find some possible strategies for taxi drivers to maximize their profits."),
                    fluidRow(
                        leafletOutput("welcomemap")
                        # column(5, plotOutput("count")),
                        # column(7, plotOutput("delay"))
                    )),
            tabItem(tabName = 'starter',
                    fluidPage(
                        h4("Below are the number of taxi rides across week days."),
                        fluidRow(
                            column(6, plotOutput("dow_pu")),
                            column(6, plotOutput("dow_do"))),
                        h4("Below are the relation of taxi rides and time in a day."),
                        fluidRow(
                            column(6, plotOutput("tod_bar")),
                            column(6, plotOutput("tod_density"))),
                    )),
            tabItem(tabName = 'spots',
                    fluidPage(
                        h4("Below are the most popular pick up and drop off zones."),
                        fluidRow(
                            column(6, leafletOutput("zone_pu")),
                            column(6, leafletOutput("zone_do")))
                    )),
            tabItem(tabName = 'plots'),
            tabItem(tabName = 'data', dataTableOutput('table'))
        )
    )
)
