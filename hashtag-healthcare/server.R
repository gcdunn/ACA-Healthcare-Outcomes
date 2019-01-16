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
    plot_usmap(data = insurance2016, values = "PctUninsured", lines = "white") + 
      scale_fill_continuous(low = "#c79fef", high = "#35063e", name = "Percent\n Uninsured", 
                            label = scales::comma) + 
      theme(legend.position = "right", legend.key.size = unit(1.75, "cm"), 
            legend.text = element_text(size = 14, face = 'bold'), legend.title=element_text(size=16, face = 'bold'))
    
  })
  
  output$insurance_pie <- renderPlot({
    df <- insurance2008
    subset <- df %>% filter(state==input$state2) %>% select(-PctUninsured) %>% mutate_if(is.numeric, funs(./Total)) %>%
      gather(key=Source,value=Fraction,Employer,`Non-Group`,Medicaid,Medicare,`Other Public`,Uninsured) %>% select(-Total)
    ggplot(subset, aes(x="", y=Fraction, fill=Source)) + geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) + geom_text(aes(label = percent(round(Fraction,2))), position = position_stack(vjust = 0.5)) +
      scale_fill_manual(values=c("#55DDE0", "#33658A", "#F26419", "#999999", "#F6AE2D", "#887191")) + #alt colors #2F4858
      labs(x = NULL, y = NULL, fill = NULL, title = "Types of Insurance") +
      theme_classic() + theme(axis.line = element_blank(),
                              axis.text = element_blank(),
                              axis.ticks = element_blank(),
                              plot.title = element_text(hjust = 0.5, color = "#363737"))
    
  })
  
})


