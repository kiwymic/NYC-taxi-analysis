library(shinydashboard)
dashboardPage(
    dashboardHeader(title='NYC Flights'),
    dashboardSidebar(
        sidebarUserPanel("Hao-Wei Chu",
                         image = 'nycdsa.png' ),
        sidebarMenu(
            menuItem("Hello world!", tabName = "welcome",
                     icon = icon("baby-carriage")),
            menuItem("Basic stats", tabName = "starter",
                     icon = icon("angellist")),
            menuItem("Hot spots", tabName = "spots",
                     icon = icon("binoculars")),
            menuItem("Price related stats", tabName = "price",
                     icon = icon("wallet")),
            menuItem("JFK comparicon", tabName = "jfk",
                     icon = icon("plane")),
            menuItem("Zone comparicon", tabName = "zone",
                     icon = icon("house-user"))
        )
        
        
        # selectizeInput("dest", "Arrival Airport",
        #                choices=unique(flights$dest))
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = 'welcome',
                    h4("Hello world!The goal of this project is to look at the taxi trip data, and hopefully find some possible strategies for taxi drivers to maximize their profits."),
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
                            column(6, leafletOutput("zone_do"))),
                        h4("Let's try this out yourself. Pick a week day and a window of hours which you want to highlight."),
                        fluidRow(
                            column(4, checkboxInput("check_dow1", "Select specific day of week", FALSE),
                                   sliderInput("slider_dow1", label = h5("1 = Sunday, ..., 7=Saturday"), min = 1, 
                                               max = 7, value = 6)),
                            column(4, checkboxInput("check_tod1", "Select specific time of day", FALSE),
                                   sliderInput("slider_tod1", label = h5("Time range"), min = 0, 
                                               max = 23, value = c(14, 16)))
                        ),
                        fluidRow(
                            column(6, leafletOutput("zone_pu_with_dow_tod")),
                            column(6, leafletOutput("zone_do_with_dow_tod"))
                        ),
                        h4("How about a heat map? Pick a week day and a window of hours which you want to highlight."),
                        fluidRow(
                            column(4, checkboxInput("check_dow2", "Select specific day of week", FALSE),
                                   sliderInput("slider_dow2", label = h5("1 = Sunday, ..., 7=Saturday"), min = 1, 
                                               max = 7, value = 6)),
                            column(4, checkboxInput("check_tod2", "Select specific time of day", FALSE),
                                   sliderInput("slider_tod2", label = h5("Time range"), min = 0, 
                                               max = 23, value = c(14, 16)))
                        ),
                        fluidRow(
                            column(6, leafletOutput("zone_pu_with_dow_tod_hm")),
                            column(6, leafletOutput("zone_do_with_dow_tod_hm"))
                        )
                    )),
            tabItem(tabName = 'price',
                    fluidPage(
                        fluidRow(
                            column(6, h4("Distribution of fare"),
                                   plotOutput("fare_dist")),
                            column(6, h4("Distribution of tips (credit cards)"),
                                   plotOutput("tips_dist"))
                        ),
                        h4("Distribution of traveling distance"),
                        fluidRow(
                            column(6, plotOutput("distance_dist")),
                            column(6, plotOutput("distance_box"))),
                        
                        h4("Distribution of traveling time"),
                        fluidRow(
                            column(6, plotOutput("duration_dist")),
                            column(6, plotOutput("duration_box"))),
                        h4("Distribution of hourly revenue"),
                        fluidRow(
                            column(6, plotOutput("revenue_dist")),
                            column(6, plotOutput("revenue_box"))),
                        h4("Variation of hourly revenue over a week"),
                        fluidRow(
                            column(12, plotOutput("revenue_week"))
                        )
                )
            ),
            tabItem(tabName = 'jfk', 
                    fluidPage(
                        h4("Coparison between the average hourly wage among rides picked up at an area and among rides from/to JFK."),
                        fluidRow(
                            column(6, leafletOutput("revenue_jfk")),
                            column(6, leafletOutput("revenue_pu_zone"))),
                        h4("Coparison between the average hourly wage among rides dropped off at an area and among rides from/to JFK."),
                        fluidRow(
                            column(6, leafletOutput("revenue_jfk_dup")),
                            column(6, leafletOutput("revenue_do_zone")))
                    )
                    ),
            tabItem(tabName = 'zone',
                    h4("Freely compare two regions here."),
                    fluidPage(
                        column(6, selectizeInput(inputId='zone1',label='Pickup zone',
                         choices= paste(ny_areas$OBJECTID,
                                        ny_areas$zone, ny_areas$borough)),
                         checkboxInput("check_dow_zone_1", "Select specific day of week", FALSE),
                         sliderInput("slider_dow_zone_1", label = h5("1 = Sunday, ..., 7=Saturday"), min = 1, 
                                     max = 7, value = 6),
                         checkboxInput("check_tod_zone_1", "Select specific time of day", FALSE),
                         sliderInput("slider_tod_zone_1", label = h5("Time range"), min = 0, 
                                     max = 23, value = c(14, 16)),
                         leafletOutput("zone_pu_1")
                        ),
                        column(6, selectizeInput(inputId='zone2',label='Pickup comparison',
                                                 choices= paste(ny_areas$OBJECTID,
                                                                ny_areas$zone, ny_areas$borough)),
                               checkboxInput("check_dow_zone_2", "Select specific day of week", FALSE),
                               sliderInput("slider_dow_zone_2", label = h5("1 = Sunday, ..., 7=Saturday"), min = 1, 
                                           max = 7, value = 6),
                               checkboxInput("check_tod_zone_2", "Select specific time of day", FALSE),
                               sliderInput("slider_tod_zone_2", label = h5("Time range"), min = 0, 
                                           max = 23, value = c(14, 16)),
                               leafletOutput("zone_pu_2"))
                    )
                    )
        )
    )
)
