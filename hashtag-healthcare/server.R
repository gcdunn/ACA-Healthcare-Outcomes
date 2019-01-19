shinyServer(function(input, output) {
  
  output$medicaid_box <- renderInfoBox({
    expanded <- medicaid %>% 
      filter(State == input$state) %>%
      select(Expanded)
    if (expanded == TRUE) {
      boxInput <- c('expanded', 'green','thumbs-up')
    } else {
      boxInput <- c('did not expand', 'red', 'thumbs-down')
    }
    infoBox(input$state , paste0(boxInput[1], ' Medicaid.'),
            color = boxInput[2],
            icon = icon(boxInput[3])
    )# close infoBox
  })
  
  output$insurance_map <- renderPlot({
    insurance <- bigInsuranceTable %>% filter(Year==toString(input$year))
    plot_usmap(data = insurance, values = "PctUninsured", lines = "white") + 
      scale_fill_continuous(low = "#c79fef", high = "#35063e", name = "Percent\n Uninsured", 
                            label = scales::comma) + 
      theme(legend.position = "right", legend.key.size = unit(1.75, "cm"), 
            legend.text = element_text(size = 14, face = 'bold'), legend.title=element_text(size=16, face = 'bold'))
    
  })
  
  output$insurance_pie <- renderPlot({
    subset <- bigInsuranceTable %>% filter(state==input$state2,Year==toString(input$year2)) %>% select(-PctUninsured) %>% mutate_if(is.numeric, funs(./Total)) %>%
      gather(key=Source,value=Fraction,Employer,`Non-Group`,Medicaid,Medicare,`Other Public`,Uninsured) %>% select(-Total)
    ggplot(subset, aes(x=reorder(Source,Fraction), y=Fraction, fill=Source)) + 
      geom_bar(stat="identity", width=1) +
      coord_flip() +
      geom_text(aes(label = paste0(as.integer(round(Fraction,2)*100),"%")), y=subset$Fraction, size = 5, hjust=1.1,color = "black") +
      scale_fill_manual(values=c("#55DDE0", "#33658A", "#F26419", "#999999", "#F6AE2D", "#887191")) + #alt colors #2F4858
      labs(x = NULL, y = NULL, fill = NULL, title = NULL) +
      theme_tufte() +
      theme(axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_blank(),
            legend.position="bottom",
            legend.key.size = unit(2, "cm"),
            legend.text = element_text(size = 20, face = 'bold'))
    
  })
  
})


