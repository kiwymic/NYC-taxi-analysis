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
    
    output$zone_pu_with_dow_tod <- renderLeaflet({
        trip_selected <- trip
        if(input$check_dow1)
            trip_selected <- trip_selected %>% filter(dow == input$slider_dow1)
        if(input$check_tod1){
            trip_selected <- trip_selected %>%
                filter(hr >= input$slider_tod1[1] & hr <= input$slider_tod1[2])
        }
        trip_selected = trip_selected %>% group_by(pickup_zone) %>%
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
    
    output$zone_do_with_dow_tod <- renderLeaflet({
        trip_selected <- trip
        if(input$check_dow1)
            trip_selected <- trip_selected %>% filter(dow == input$slider_dow1)
        if(input$check_tod1){
            trip_selected <- trip_selected %>%
                filter(hr >= input$slider_tod1[1] & hr <= input$slider_tod1[2])
        }
        trip_selected = trip_selected %>% group_by(dropoff_zone) %>%
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
    
    output$zone_pu_with_dow_tod_hm <- renderLeaflet({
        ND=7
        NH=24
        trip_selected <- trip
        if(input$check_dow2){
            ND=1
            trip_selected <- trip_selected %>% filter(dow == input$slider_dow2)
        }
            
        if(input$check_tod2){
            NH=input$slider_tod2[2]-input$slider_tod2[1]+1
            trip_selected <- trip_selected %>%
                filter(hr >= input$slider_tod2[1] & hr <= input$slider_tod2[2])
        }
        trip_selected <- trip_selected %>% select(longitude = pickup_longitude, latitude = pickup_latitude)

        leaflet(ny_areas) %>% addPolygons(color = "#331166", weight = 1,
                                          smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.3) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView( lng = -74.0, lat = 40.74, zoom = 10 ) %>%
            addHeatmap(lng=trip_selected$longitude, lat=trip_selected$latitude,
                       max=1, blur = 6, radius = 3, intensity = dim(trip)[1]/1e6/ND/NH)
    })
    
    output$zone_do_with_dow_tod_hm <- renderLeaflet({
        ND=7
        NH=24
        trip_selected <- trip
        if(input$check_dow2){
            ND=1
            trip_selected <- trip_selected %>% filter(dow == input$slider_dow2)
        }
        if(input$check_tod2){
            NH = input$slider_tod2[2]-input$slider_tod2[1]+1
            trip_selected <- trip_selected %>%
                filter(hr >= input$slider_tod2[1] & hr <= input$slider_tod2[2])
        }
        
        trip_selected <- trip_selected %>% select(longitude = dropoff_longitude, latitude = dropoff_latitude)

        leaflet(ny_areas) %>% addPolygons(color = "#116633", weight = 1,
                                          smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.3) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView( lng = -74.0, lat = 40.74, zoom = 10 ) %>%
            addHeatmap(lng=trip_selected$longitude, lat=trip_selected$latitude,
                       max=1, blur = 6, radius = 3, intensity = dim(trip)[1]/1e6/ND/NH)
    })
    
    output$fare_dist <- renderPlot(
        ggplot(data = trip, aes(x=in_pocket))+
            geom_histogram(aes(fill=pickup_boro), binwidth = 3) + 
            coord_cartesian(xlim=c(0,75)) +
            labs(x = "Taxi fare getting into drivers' pockets", y = "Data count")
    )
    
    output$tips_dist <- renderPlot(
        ggplot(data = trip, aes(x=tip_amount))+
            geom_histogram(aes(fill=pickup_boro), binwidth = 1) + 
            coord_cartesian(xlim=c(0,20)) +
            labs(x = "Tips <3", y = "Data count")
    )
    
    output$distance_dist <- renderPlot(
        ggplot(data = trip, aes(x=trip_distance))+
            geom_histogram(aes(fill=pickup_boro), binwidth = 1) + 
            coord_cartesian(xlim=c(0,20)) +
            labs(x = "Distance traveled (miles)", y = "Data count")
    )
    
    output$distance_box <- renderPlot(
        ggplot(data = trip, aes(x=as.factor(dow), y=trip_distance))+
            geom_violin(aes(fill=as.factor(dow))) +
            geom_boxplot(width=0.1, alpha=0.4, outlier.shape = NA) + 
            coord_cartesian(ylim=c(0,30)) + 
            labs(x = "Day of week", y = "Distancee traveled (miles)")
    )
    
    output$duration_dist <- renderPlot(
        ggplot(data = trip, aes(x=duration/60))+
            geom_histogram(aes(fill=pickup_boro), binwidth = 2) + 
            coord_cartesian(xlim=c(0,100)) +
            labs(x = "Duration (minutes)", y = "Data count")
    )
    
    output$duration_box <- renderPlot(
        ggplot(data = trip, aes(x=as.factor(dow), y=duration/60))+
            geom_violin(aes(fill=as.factor(dow))) +
            geom_boxplot(width=0.1, alpha=0.4, outlier.shape = NA) +
            coord_cartesian(ylim=c(0,100)) + 
            labs(x = "Day of week", y = "Duration (minutes)")
    )
    
    output$revenue_dist <- renderPlot(
        ggplot(data = trip, aes(x=hourly_wage))+
            geom_histogram(aes(fill=pickup_boro), binwidth = 5) + 
            coord_cartesian(xlim=c(0,500)) +
            labs(x = "Hourly wage", y = "Data count")
    )
    
    output$revenue_box <- renderPlot(
        ggplot(data = trip, aes(x=as.factor(dow), y=hourly_wage))+
            geom_violin(aes(fill=as.factor(dow))) +
            geom_boxplot(width=0.1, alpha=0.4, outlier.shape = NA) + coord_cartesian(ylim=c(0,150)) + 
            labs(x = "Day of week", y = "Hourly wage")
    )
    
    output$revenue_week <- renderPlot({
        temp <- trip %>% group_by(dow, hr) %>%
            summarise(avg = mean(hourly_wage))
        ggplot(data = temp, aes(x=hr, y=avg))+
            geom_line(aes(color=as.character(dow))) + # , stat="identity"
            labs(x = "Time of the day", y = "Hourly wage")
    })
    
    output$revenue_jfk <- renderLeaflet({
        temp <- trip %>% filter(pickup_zone==132 |
            dropoff_boro==132)
        
        temp_wage <- temp %>% mutate(other_zone =
            ifelse(pickup_zone==132, dropoff_zone, pickup_zone)) %>%
            group_by(other_zone) %>% summarize(avg = mean(hourly_wage))
        
        pal <- colorBin("YlOrRd", domain = c(60,160))
        
        leaflet(ny_areas %>% filter(OBJECTID %in% temp_wage$other_zone)) %>%
            addPolygons(fillColor = ~pal(temp_wage$avg), weight = 1,
                        smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5) %>%                                                                             
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView( lng = -74.0, lat = 40.74, zoom = 10 ) %>%
            addLegend(pal = pal, values = ~temp_wage$avg, opacity = 0.7,
                      title = "Wage, pu/do JFK", position = "bottomright")
    })
    
    output$revenue_pu_zone <- renderLeaflet({
        pal <- colorBin("YlOrRd", domain = c(60,160))
        region_wage <- trip %>% group_by(pickup_zone) %>%
            summarize(avg = mean(hourly_wage))
        leaflet(ny_areas %>% filter(OBJECTID %in% region_wage$pickup_zone)) %>%
            addPolygons(fillColor = ~pal(region_wage$avg), weight = 1,
                        smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5) %>%                                                                             
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView( lng = -74.0, lat = 40.74, zoom = 10 ) %>%
            addLegend(pal = pal, values = ~temp_wage$avg, opacity = 0.7,
                      title = "Wage, pu", position = "bottomright")
    })
    
    output$revenue_jfk_dup <- renderLeaflet({
        temp <- trip %>% filter(pickup_zone==132 |
                                    dropoff_boro==132)
        
        temp_wage <- temp %>% mutate(other_zone =
                                         ifelse(pickup_zone==132, dropoff_zone, pickup_zone)) %>%
            group_by(other_zone) %>% summarize(avg = mean(hourly_wage))
        
        pal <- colorBin("PuBu", domain = c(60,160))
        
        leaflet(ny_areas %>% filter(OBJECTID %in% temp_wage$other_zone)) %>%
            addPolygons(fillColor = ~pal(temp_wage$avg), weight = 1,
                        smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5) %>%                                                                             
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView( lng = -74.0, lat = 40.74, zoom = 10 ) %>%
            addLegend(pal = pal, values = ~temp_wage$avg, opacity = 0.7,
                      title = "Wage, pu/do JFK", position = "bottomright")
    })
    
    output$revenue_do_zone <- renderLeaflet({
        pal <- colorBin("PuBu", domain = c(20,200))
        dropoff_wage <- trip %>% group_by(dropoff_zone) %>%
            summarize(avg = mean(hourly_wage))
        
        leaflet(ny_areas %>% filter(OBJECTID %in% dropoff_wage$dropoff_zone)) %>%
            addPolygons(fillColor = ~pal(dropoff_wage$avg), weight = 1,
                        smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5) %>%                                                                             
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView( lng = -74.0, lat = 40.74, zoom = 10 ) %>%
            addLegend(pal = pal, values = ~temp_wage$avg, opacity = 0.7,
                      title = "Wage, do", position = "bottomright")
    })
    
    output$zone_pu_1 <- renderLeaflet({
        ZONE = as.integer(strsplit(input$zone1, split = ' ')[[1]][1])
        
        trip_selected <- trip %>% filter(pickup_zone==ZONE)
        
        if(input$check_dow_zone_1)
            trip_selected <- trip_selected %>% filter(dow == input$slider_dow_zone_1)
        if(input$check_tod_zone_1){
            trip_selected <- trip_selected %>%
                filter(hr >= input$slider_tod_zone_1[1] & hr <= input$slider_tod_zone_1[2])
        }
        
        temp_wage <- trip_selected %>% group_by(dropoff_zone) %>%
            summarize(avg = mean(hourly_wage))
        
        pal <- colorBin("YlOrRd", domain = c(20,200))
        
        leaflet(ny_areas %>% filter(OBJECTID %in% temp_wage$dropoff_zone)) %>%
            addPolygons(fillColor = ~pal(temp_wage$avg), weight = 1,
                        smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5) %>%   
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView( lng = -74.0, lat = 40.74, zoom = 10 ) %>%
            addLegend(pal = pal, values = ~temp_wage$avg, opacity = 0.7,
                      title = paste("Wage, pu", as.character(ZONE)),
                      position = "bottomleft")
    })
    
    output$zone_pu_2 <- renderLeaflet({
        ZONE = as.integer(strsplit(input$zone2, split = ' ')[[1]][1])
        
        trip_selected <- trip %>% filter(pickup_zone==ZONE)
        
        if(input$check_dow_zone_2)
            trip_selected <- trip_selected %>% filter(dow == input$slider_dow_zone_2)
        if(input$check_tod_zone_2){
            trip_selected <- trip_selected %>%
                filter(hr >= input$slider_tod_zone_2[1] & hr <= input$slider_tod_zone_2[2])
        }
        
        temp_wage <- trip_selected %>% group_by(dropoff_zone) %>%
            summarize(avg = mean(hourly_wage))
        
        pal <- colorBin("YlOrRd", domain = c(20,200))
        
        leaflet(ny_areas %>% filter(OBJECTID %in% temp_wage$dropoff_zone)) %>%
            addPolygons(fillColor = ~pal(temp_wage$avg), weight = 1,
                        smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5) %>%   
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView( lng = -74.0, lat = 40.74, zoom = 10 ) %>%
            addLegend(pal = pal, values = ~temp_wage$avg, opacity = 0.7,
                      title = paste("Wage, pu", as.character(ZONE)),
                      position = "bottomleft")
    })
}