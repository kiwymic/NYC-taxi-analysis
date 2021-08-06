fluidPage(
  # h1("NYC Flights"),
  # img(src="https://upload.wikimedia.org/wikipedia/commons/9/91/Palestine_sunbird_%28Cinnyris_osea_osea%29_male.jpg", width="50%"),
  # HTML('<img src="https://upload.wikimedia.org/wikipedia/commons/9/91/Palestine_sunbird_%28Cinnyris_osea_osea%29_male.jpg" width="60%">'),
  # img(src="NYCDSA.png",width="20%")
  
  titlePanel("NYC Flights"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "origin_ui",
                     label = "Departure airport",
                     choices = unique(flights$origin)),
      selectizeInput(inputId = "dest_ui",
                     label = "Arrival airport",
                     choices = unique(flights$dest)),
      selectizeInput(inputId= "month",
                     label ="Choose month",
                     choices = unique(flights$month))
    ),
    mainPanel(
      # fluidRow(
      #   column(5,plotOutput("count")),
      #   column(7,plotOutput("delay"))
      # )
      tabsetPanel(
        tabPanel("plots",
                 fluidRow(
                   column(5,plotOutput("count")),
                   column(7,plotOutput("delay"))
                 )
        ),
        tabPanel("table",
                 tableOutput("table1"))
      )
    ) 
  )
)