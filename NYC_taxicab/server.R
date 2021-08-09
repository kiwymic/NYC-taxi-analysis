function(input,output){
    
    flights_delay <- reactive({
        flights %>%
            filter(origin == input$origin & dest == input$dest ) %>%
            group_by(carrier) %>%
            summarise( n = n() , arrdelay = mean(arr_delay), depdelay=mean(dep_delay)) 
    })
    output$welcomemap <- renderLeaflet({
        leaflet(ny_areas) %>% 
            addPolygons(color = "#004499", weight = 1,
                        smoothFactor = 0.5, opacity = 1.0,
                        fillOpacity = 0.3) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView( lng = -74, lat = 40.68, zoom = 10 ) %>%
            addProviderTiles("Esri.WorldImagery") %>%
            addCircles(data = trip %>% select(lng = pickup_longitude,
                                              lat = pickup_latitude) %>%
                           head(5000), weight = 0, color = "pink")
    })
    output$dow_pu <- renderPlot(
        ggplot(data = trip, aes(x=dow))+
            geom_bar(aes(fill=pickup_boro)) + # , stat="identity"
            labs(x = "Day of the week", y = "Data count")
    )
    output$dow_do <- renderPlot(
        ggplot(data = trip, aes(x=dow))+
            geom_bar(aes(fill=dropoff_boro)) + 
            labs(x = "Day of the week", y = "Data count")
    )
    output$tod_bar <- renderPlot(
        ggplot(data = trip, aes(x=hr))+
            geom_bar(aes(fill=pickup_boro)) + 
            labs(x = "Time of the day", y = "Data count")
    )
    output$tod_density <- renderPlot(
        ggplot(data = trip, aes(x=hr))+
            geom_density(aes(color=as.factor(dow))) + 
            labs(x = "Time of the day", y = "Data count")
    )
    
    output$zone_pu <- renderLeaflet({
        trip_selected = trip %>% group_by(pickup_zone) %>%
            summarise(cnt = n()) %>% slice_max(cnt, n=20)
        pal <- colorBin("OrRd", domain = trip_selected$cnt)

        leaflet(ny_areas %>% filter(OBJECTID %in% trip_selected$pickup_zone)) %>%
        addPolygons(fillColor = ~pal(trip_selected$cnt), weight = 1,
                    smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView( lng = -74.0, lat = 40.74, zoom = 10 ) %>%
        addLegend(pal = pal, values = ~trip_selected$cnt, opacity = 0.7,
                       title = "Top20 pu", position = "bottomleft")
    })
    
    output$zone_do <- renderLeaflet({
        trip_selected = trip %>% group_by(dropoff_zone) %>%
            summarise(cnt = n()) %>% slice_max(cnt, n=20)
        pal <- colorBin("BuPu", domain = trip_selected$cnt)
        
        leaflet(ny_areas %>% filter(OBJECTID %in% trip_selected$dropoff_zone)) %>%
            addPolygons(fillColor = ~pal(trip_selected$cnt), weight = 1,
                        smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView( lng = -74.0, lat = 40.74, zoom = 10 ) %>%
            addLegend(pal = pal, values = ~trip_selected$cnt, opacity = 0.7,
                      title = "Top20 do", position = "bottomleft")
    })

    # output$count <- renderPlot(
    #     # flights %>%
    #     #   filter(origin == input$origin_ui & dest == input$dest_ui) %>%
    #     #   group_by(carrier) %>%
    #     #   #count() %>%
    #     #   summarise( n = n()) %>%
    #     flights_delay () %>%
    #         #ggplot(aes(x = carrier, y = n)) +
    #         ggplot(aes(x = factor(carrier,levels = c("AA","UA","DL","B6","VX")), y = n)) +
    #         #geom_col(fill = "lightblue") +
    #         geom_bar(fill="green",stat = "identity") + 
    #         ggtitle("Number of flights") +
    #         xlab("airlines")
    # )
    # output$delay <- renderPlot(
    #     # flights %>%
    #     # filter(origin == input$origin_ui & dest == input$dest_ui) %>%
    #     #   group_by(carrier) %>%
    #     #   summarise( n = n() , arrdelay = mean(arr_delay), depdelay=mean(dep_delay)) %>%
    #     flights_delay() %>%
    #         pivot_longer(c(arrdelay,depdelay),names_to="type",values_to="delay") %>%
    #         ggplot(aes(x =carrier,y=delay)) +
    #         geom_col(aes(fill=type),position = "dodge")+
    #         ggtitle("Avg delay")
    # )
    output$table <- renderDataTable(flights_delay())
}