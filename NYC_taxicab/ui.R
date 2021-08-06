library(shinydashboard)
dashboardPage(
    dashboardHeader(title='NYC Flights'),
    dashboardSidebar(
        sidebarUserPanel("NYC DSA",
                         image = 'nycdsa.jpg' ),
        sidebarMenu(
            menuItem("Plots", tabName = "plots", icon = icon("map")),
            menuItem("Data", tabName = "data", icon = icon("database"))
        ),
        
        selectizeInput(inputId='origin',label='Departure Airport',
                       choices=unique(flights$origin)),
        selectizeInput("dest", "Arrival Airport",
                       choices=unique(flights$dest))
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = 'plots',
                    fluidRow(
                        leafletOutput("mymap"),
                        # column(5, plotOutput("count")),
                        # column(7, plotOutput("delay"))
                    )),
            tabItem(tabName = 'data', dataTableOutput('table'))
        )
    )
)