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
    infoBox(input$state , paste0(boxInput[1], ' Medicaid'),
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
  
  output$box_plot <- renderPlot({
    plot_data <- all_access %>% filter(Data == input$metric)
    ggplot(plot_data, aes(x=Medicaid,y=N,fill=Medicaid)) +
      geom_boxplot(legend=FALSE) +
      ylab("Percent") +
      #ggtitle(plot_data$Data) +
      theme_minimal() +
      theme(axis.ticks = element_blank(),
            axis.text.x  = element_text(size=16),
            axis.text.y  = element_text(size=16),
            axis.title.x  = element_text(face='bold',size=16),
            axis.title.y  = element_text(face='bold',size=16))
    
    #plot_data <- allMetrics %>% filter(Data==input$metric) %>% 
    #  filter(Year==input$year)
    #ggplot(plot_data, aes(x=`Percent on Medicaid`*100, y=N)) + 
    #  geom_boxplot(aes(group = cut_number(`Percent on Medicaid`*100,3))) + #ggtitle("ER Visits per 1,000 People") +
    #  xlab("Percent of population receiving Medicaid") +
    #  ylab(input$metric) +
    #  theme_tufte() + theme(axis.ticks = element_blank(),
    #                        axis.text.x  = element_text(size=16),
    #                        axis.text.y  = element_text(size=16),
    #                        axis.title.x  = element_text(size=16),
    #                        axis.title.y  = element_text(size=16))
  })
  
  output$stat_box1 <- renderInfoBox({
    expanded <- all_access %>% filter(Data == input$metric) %>% filter(Expanded=='TRUE') %>% select(N)
    not_expanded <- all_access %>% filter(Data == input$metric) %>% filter(Expanded=='FALSE') %>% select(N)
    er_ttest <- t.test(not_expanded$N,expanded$N)
    if (er_ttest$p.value <= 0.05) {
      boxInput <- c('statistically significant difference', 'blue','check-square-o')
    } else {
      boxInput <- c('not statistically significant difference', 'blue', 'times')
    }
    infoBox("t-test: ", boxInput[1],
            color = boxInput[2],
            icon = icon(boxInput[3])
    )# close infoBox
  })
  
  output$stat_box2 <- renderValueBox({
    expanded <- all_access %>% filter(Data == input$metric) %>% filter(Expanded=='TRUE') %>% select(N)
    not_expanded <- all_access %>% filter(Data == input$metric) %>% filter(Expanded=='FALSE') %>% select(N)
    ttest <- t.test(not_expanded$N,expanded$N)
    valueBox(round(ttest$p.value,5), 'p-value',
            color = 'light-blue',
            icon = icon('calculator'))
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
    subset <- bigInsuranceTable %>% inner_join(medicaid,by=c("state"="State")) %>%
      select(-PctUninsured) %>% mutate_if(is.numeric, funs(./Total)) %>%
      group_by(Era,state) %>% summarize(Medicaid=mean(Medicaid)) %>%
      filter(Era == "2011-2013") %>% #make dynamic
      arrange(Medicaid)
    
    label_data <- subset
    number_of_bar=nrow(label_data)
    angle= 90 - 360 * (as.numeric(rownames(label_data))-0.5) /number_of_bar
    label_data$hjust<-ifelse( angle < -90, 1, 0)
    label_data$angle<-ifelse(angle < -90, angle+180, angle)

    ggplot(subset, aes(x=state, y=Medicaid*100, fill=Era)) + 
      geom_bar(stat="identity", position="dodge") +
      #coord_flip() +
      geom_text(data=label_data,aes(x=state, y=(Medicaid*100)+10, label=state, hjust=hjust), color="black", fontface="bold",alpha=0.6, angle=label_data$angle, size=2.5, inherit.aes = FALSE) +
      #geom_text(aes(label = paste0(as.integer(round(Medicaid,2)*100),"%")), y=subset$Medicaid, size = 5, hjust=1.1,color = "black") +
      #scale_fill_manual(values=c("#55DDE0", "#33658A", "#F26419", "#999999", "#F6AE2D", "#887191")) + #alt colors #2F4858
      labs(x = NULL, y = NULL, fill = NULL, title = NULL) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-2,4), "cm")) + 
      ylim(-10,50) +
      coord_polar(start = 0)
    
  })
  
  output$medicaid_lollipop <- renderPlot({
    subset <- bigInsuranceTable %>% 
      select(-PctUninsured) %>% mutate_if(is.numeric, funs(./Total)) %>%
      group_by(Era,state) %>% summarize(Medicaid=mean(Medicaid)) %>%
      ungroup() %>% spread(key=Era,value=Medicaid) %>% 
      inner_join(medicaidAll,by=c("state"="State"))
    #subset$state <- factor(subset$state, levels = subset$state[order(subset$`2011-2013`)])
    ggplot(subset) +
      geom_segment( aes(x=reorder(state,`2011-2013`), xend=state, y=`2011-2013`, yend=`2014-2016`), color=ifelse(subset$state == input$state, "ivory4","grey"),size=1.3) +
      geom_point( aes(x=reorder(state,`2011-2013`), y=`2011-2013`), color=ifelse(subset$state == input$state, 'salmon', '#cdffeb'), size=ifelse(subset$Expanded == 'TRUE', 5, 4)) +
      geom_point( aes(x=reorder(state,`2011-2013`), y=`2014-2016`), color=ifelse(subset$state == input$state, 'maroon', '#009f9d'), size=ifelse(subset$Expanded == 'TRUE', 5, 4)) +
      #geom_rect(aes(xmin=reorder(state,`2011-2013`)[which(reorder(state,`2011-2013`)==input$state)-1], xmax=reorder(state,`2011-2013`)[which(reorder(state,`2011-2013`)==input$state)+1], ymin=0.05, ymax=0.35), 
      #          color=ifelse(subset$Expanded[which(subset$state==input$state)] == 'TRUE', 'green', 'red'), 
      #          fill="transparent", alpha=0.01) +
      coord_flip() + 
      theme_light() +
      theme(
        axis.text.x  = element_text(face='plain', size=14),
        axis.text.y  = element_text(face='plain', size=14),
        axis.title.x  = element_text(face='bold', size=16),
        axis.title.y  = element_text(face='bold', size=16),
        #legend.position = "none",
        panel.border = element_blank(),
      ) +
      xlab("State") +
      ylab("Percent of population covered by Medicaid") +
      scale_x_discrete(labels=c(  "Arkansas"=expression(bold("Arkansas")),
                                  "Delaware"=expression(bold("Delaware")),
                                  "Colorado"=expression(bold("Colorado")),
                                  "Illinois"=expression(bold("Illinois")),
                                  "Iowa"=expression(bold("Iowa")),
                                  "Kentucky"=expression(bold("Kentucky")),
                                  "Maryland"=expression(bold("Maryland")),
                                  "Massachusetts"=expression(bold("Massachusetts")),
                                  "Minnesota"=expression(bold("Minnesota")),
                                  "Nebraska"=expression(bold("Nebraska")),
                                  "Nevada"=expression(bold("Nevada")),
                                  "New Mexico"=expression(bold("New Mexico")),
                                  "North Dakota"=expression(bold("North Dakota")),
                                  "Ohio"=expression(bold("Ohio")),
                                  "Oregon"=expression(bold("Oregon")),
                                  "Rhode Island"=expression(bold("Rhode Island")),
                                  "West Virginia"=expression(bold("West Virginia")),
                                  parse=TRUE))
    
  })
  
})


