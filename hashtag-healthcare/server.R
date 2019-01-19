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
  
  output$box_plot <- renderPlot({ #Needs to be converted to dynamic selection of year/table
    plot_data <- allMetrics %>% filter(Data==input$metric) %>% 
      filter(Year==input$year)
    ggplot(plot_data, aes(x=`Percent on Medicaid`*100, y=N)) + 
      geom_boxplot(aes(group = cut_number(`Percent on Medicaid`*100,2))) + #ggtitle("ER Visits per 1,000 People") +
      xlab("Percent of population receiving Medicaid") +
      ylab(input$metric) +
      theme_tufte() + theme(axis.ticks = element_blank(),
                            axis.text.x  = element_text(size=16),
                            axis.text.y  = element_text(size=16),
                            axis.title.x  = element_text(size=16),
                            axis.title.y  = element_text(size=16))
  })
  
  output$stat_box <- renderInfoBox({
    expanded <- er_medicaid %>% filter(Year==input$year) %>% filter(Expanded=='TRUE') %>% select(N)
    not_expanded <- er_medicaid %>% filter(Year==input$year) %>% filter(Expanded=='FALSE') %>% select(N)
    er_ttest <- t.test(not_expanded$N,expanded$N)
    if (er_ttest$p.value <= 0.05) {
      boxInput <- c('statistically different', 'blue','check-square-o')
    } else {
      boxInput <- c('not statistically different', 'blue', 'times')
    }
    infoBox(input$metric,paste0(boxInput[1]),
            color = boxInput[2],
            icon = icon(boxInput[3])
    )# close infoBox
  })
  
  output$word_cloud <- renderWordcloud2({
    tweet_table <- healthcare_tweets %>% unnest_tokens(word, text) %>%  anti_join(stop_words) %>% filter(!nchar(word) < 3) %>%
      inner_join(get_sentiments("bing")) %>%
      filter(!tolower(word) %in% c('icymi','healthcare','aca','obamacare','medicaid','dont','im','isnt','didnt','youre','trump','issues','issue','cloud','sap'))
    word_count <- tweet_table %>% count(word, sort = TRUE) %>% filter(n > 2)
    figPath = system.file("examples/t.png",package = "wordcloud2")
    wordcloud2(word_count, figPath=figPath,size = 2.3, minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 1)
   
  })
  
  output$sentiment_graph <- renderPlot({
    sentiment_table <- tweet_table %>% inner_join(get_sentiments("bing")) 
    word_counts <- sentiment_table %>%
      count(word,sentiment)
    top_words <- word_counts %>% group_by(sentiment) %>% top_n(10) %>% 
      ungroup() %>% mutate(word = reorder(word, n)) %>%
      mutate(score = case_when(
        sentiment=='positive' ~ n,
        sentiment=='negative' ~ as.integer(-1*n)
      ))
    
    ggplot(top_words, aes(reorder(word,score), score, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      #facet_wrap(~sentiment, scales = "free") +  
      coord_flip() +
      xlab('Word') +
      ylab('Sentiment Score') + theme(axis.text.x  = element_text(size=16),
                                      axis.text.y  = element_text(size=16),
                                      axis.title.x  = element_text(size=16),
                                      axis.title.y  = element_text(size=16))
    
  })
  
  output$insurance_bar <- renderPlot({
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


