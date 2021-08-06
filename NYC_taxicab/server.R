function(input,output){
    
    flights_delay <- reactive({
        flights %>%
            filter(origin == input$origin_ui & dest == input$dest_ui & month==input$month) %>%
            group_by(carrier) %>%
            summarise( n = n() , arrdelay = mean(arr_delay), depdelay=mean(dep_delay)) 
    })
    output$count <- renderPlot(
        # flights %>%
        #   filter(origin == input$origin_ui & dest == input$dest_ui) %>%
        #   group_by(carrier) %>%
        #   #count() %>%
        #   summarise( n = n()) %>%
        flights_delay () %>%
            #ggplot(aes(x = carrier, y = n)) +
            ggplot(aes(x = factor(carrier,levels = c("AA","UA","DL","B6","VX")), y = n)) +
            #geom_col(fill = "lightblue") +
            geom_bar(fill="green",stat = "identity") + 
            ggtitle("Number of flights") +
            xlab("airlines")
    )
    output$delay <- renderPlot(
        # flights %>%
        # filter(origin == input$origin_ui & dest == input$dest_ui) %>%
        #   group_by(carrier) %>%
        #   summarise( n = n() , arrdelay = mean(arr_delay), depdelay=mean(dep_delay)) %>%
        flights_delay() %>%
            pivot_longer(c(arrdelay,depdelay),names_to="type",values_to="delay") %>%
            ggplot(aes(x =carrier,y=delay)) +
            geom_col(aes(fill=type),position = "dodge")+
            ggtitle("Avg delay")
    )
    output$table1 <- renderTable(flights_delay())
}