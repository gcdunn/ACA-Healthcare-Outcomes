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
    )
  })
  
  
})


