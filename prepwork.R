library(tidyverse)
library(ggplot2)
library(magrittr)
library(usmap)
library(stringr)
# for word clouds
library(tidytext)
library(tm)
library(SnowballC)
library(wordcloud2)
library(RColorBrewer)
# for Twitter API
library(twitteR)
library(rtweet)
library(tweetbotornot)

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
hashtags <- c("#healthcare","#ACA", "#Obamacare", "#Medicaid")
healthcare_tweets_save <- search_tweets(paste(hashtags, collapse = " OR "), n = 18000, include_rts = FALSE)
healthcare_tweets <- data.frame(healthcare_tweets_save)
healthcare_tweets$text <- healthcare_tweets$text %>% lapply(function(x) gsub("&amp", "",x))
healthcare_tweets$text <- healthcare_tweets$text %>% lapply(function(x) gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "",x))
healthcare_tweets$text <- healthcare_tweets$text %>% lapply(function(x) gsub("@\\w+", "",x))
healthcare_tweets$text <- healthcare_tweets$text %>% lapply(function(x) gsub("[[:punct:]]", "",x))
healthcare_tweets$text <- healthcare_tweets$text %>% lapply(function(x) gsub("[[:digit:]]", "",x))
healthcare_tweets$text <- healthcare_tweets$text %>% lapply(function(x) gsub("http\\w+", "",x))
healthcare_tweets$text <- healthcare_tweets$text %>% lapply(function(x) gsub("[ \t]{2,}", "",x))
healthcare_tweets$text <- healthcare_tweets$text %>% lapply(function(x) gsub("^\\s+|\\s+$", "",x))
botability <- healthcare_tweets_save %>% botornot()
healthcare_tweets <- healthcare_tweets %>% inner_join(botability) %>% filter(str_detect(source,"Tw*")) %>% filter(account_lang=='en')

tweet_table <- healthcare_tweets %>% unnest_tokens(word, text) %>% anti_join(stop_words) %>% filter(!nchar(word) < 3) %>%
  filter(!tolower(word) %in% c('icymi','healthcare','aca','obamacare','medicaid','dont','im','isnt','trump','issues','issue','cloud'))
word_count <- tweet_table %>% count(word, sort = TRUE) %>% filter(n > 2)
figPath = system.file("examples/t.png",package = "wordcloud2")
wordcloud2(word_count, figPath=figPath,size = 2.3, minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 1)
sentiment_table <- tweet_table %>% inner_join(get_sentiments("bing")) 
sentiment_table %>% count(word, sort = TRUE) %>% 
  wordcloud2(figPath=figPath,size = 2.3, minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 1)

word_counts <- sentiment_table %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment)

top_words <- word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))

ggplot(top_words, aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +  
  coord_flip() +
  ylab('Word Count') +
  xlab('Word')

# load datsets
medicaid <- readRDS('data/MedicaidExpansion.RDS')
populations <- readRDS('data/StatePopulations.RDS')
ERVisitsPer1k <- readRDS('data/ERVisitsPer1k.RDS') %>% filter(State != 'District of Columbia') %>% 
  gather(key='Year',value='VisitsPer1k','2008','2009','2010','2011','2012','2013','2014','2015','2016')
AllCancerDeathsByState <- readRDS('data/AllCancerDeathsByState.RDS')
birthsLatePrenatalCare6Mo <- readRDS('data/birthsLatePrenatalCare6Mo.RDS')
birthsTeenMothers19OrYounger <- readRDS('data/birthsTeenMothers19OrYounger.RDS')
totalBirths <- readRDS('data/totalBirths.RDS') %>% filter(state != 'District of Columbia') %>% 
  gather(key='Year',value='TotalBirths','2008','2009','2010','2011','2012','2013','2014','2015','2016','2017')

insurance2008 <- readRDS('data/InsuranceCoverageTotal2008.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total, Year='2008')
insurance2009 <- readRDS('data/InsuranceCoverageTotal2009.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total, Year='2009')
insurance2010 <- readRDS('data/InsuranceCoverageTotal2010.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total, Year='2010')
insurance2011 <- readRDS('data/InsuranceCoverageTotal2011.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total, Year='2011')
insurance2012 <- readRDS('data/InsuranceCoverageTotal2012.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total, Year='2012')
insurance2013 <- readRDS('data/InsuranceCoverageTotal2013.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total, Year='2013')
insurance2014 <- readRDS('data/InsuranceCoverageTotal2014.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total, Year='2014')
insurance2015 <- readRDS('data/InsuranceCoverageTotal2015.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total, Year='2015')
insurance2016 <- readRDS('data/InsuranceCoverageTotal2016.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total, Year='2016')
insurance2017 <- readRDS('data/InsuranceCoverageTotal2017.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total, Year='2017')
bigInsuranceTable <- do.call("rbind", list(insurance2008,insurance2009,insurance2010,insurance2011,insurance2012,
                                           insurance2013,insurance2014,insurance2015,insurance2016,insurance2017))
years <- c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)


# childbirth/Medicaid
# replace '2015' with input$Year 
er_medicaid <- ERVisitsPer1k %>% filter(Year=='2008') %>% 
  inner_join(medicaid,by = c("State" = "State")) %>% 
  group_by(Expanded)
# childbirth/Medicaid
# replace '2015' with input$Year 
teens_medicaid <- birthsTeenMothers19OrYounger %>% filter(Year=='2015') %>% 
  inner_join(filter(totalBirths,Year=='2015'),by = c("State" = "state")) %>%
  select(-Year.x,-Year.y) %>%
  inner_join(medicaid,by = c("State" = "State")) %>% 
  mutate(birthsNorm = Births/TotalBirths) %>%
  group_by(Expanded)
# prenatal/Medicaid
# replace '2015' with input$Year 
prenatal_medicaid <- birthsLatePrenatalCare6Mo %>% filter(Year=='2015') %>% 
  inner_join(filter(totalBirths,Year=='2015'),by = c("State" = "state")) %>%
  select(-Year.x,-Year.y) %>%
  inner_join(medicaid,by = c("State" = "State")) %>% 
  mutate(birthsNorm = Births/TotalBirths) %>%
  group_by(Expanded)
# cancerDeaths/Medicaid
# replace '2015' with input$Year 
cancerDeaths_medicaid <- AllCancerDeathsByState %>% filter(Year=='2015') %>% 
  select(-Year,-Deaths,-Population) %>%
  inner_join(medicaid,by = c("State" = "State")) %>%
  group_by(Expanded)

#box blots
ggplot(er_medicaid, aes(x=Expanded, y=VisitsPer1k, fill=Expanded)) + 
  geom_boxplot()
ggplot(teens_medicaid, aes(x=Expanded, y=birthsNorm, fill=Expanded)) + 
  geom_boxplot()
ggplot(prenatal_medicaid, aes(x=Expanded, y=birthsNorm, fill=Expanded)) + 
  geom_boxplot()
ggplot(cancerDeaths_medicaid, aes(x=Expanded, y=DeathsPer100k, fill=Expanded)) + 
  geom_boxplot()

#T-tests
expanded <- er_medicaid %>% filter(Expanded=='TRUE') %>% select(VisitsPer1k)
not_expanded <- er_medicaid %>% filter(Expanded=='FALSE') %>% select(VisitsPer1k)
er_ttest <- t.test(expanded$VisitsPer1k,not_expanded$VisitsPer1k)

expanded <- teens_medicaid %>% filter(Expanded=='TRUE') %>% select(birthsNorm)
not_expanded <- teens_medicaid %>% filter(Expanded=='FALSE') %>% select(birthsNorm)
teens_ttest <- t.test(expanded$birthsNorm,not_expanded$birthsNorm)

expanded <- prenatal_medicaid %>% filter(Expanded=='TRUE') %>% select(birthsNorm)
not_expanded <- prenatal_medicaid %>% filter(Expanded=='FALSE') %>% select(birthsNorm)
prenatal_ttest <- t.test(expanded$birthsNorm,not_expanded$birthsNorm)

# Insurance maps
plot_usmap(data = insurance2016, values = "PctUninsured", lines = "white") + 
  scale_fill_continuous(low = "#c79fef", high = "#35063e", name = "Percent\n Uninsured", 
                        label = scales::comma) + 
  theme(legend.position = "right", legend.key.size = unit(1.75, "cm"), 
        legend.text = element_text(size = 14, face = 'bold'), legend.title=element_text(size=16, face = 'bold'))

# Insurance pie charts
library(scales)
subset <- insurance2008 %>% filter(state=='Tennessee') %>% select(-PctUninsured) %>% mutate_if(is.numeric, funs(./Total)) %>%
  gather(key=Source,value=Fraction,Employer,`Non-Group`,Medicaid,Medicare,`Other Public`,Uninsured) %>% select(-Total)
ggplot(subset, aes(x="", y=Fraction, fill=Source)) + geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + geom_text(aes(label = percent(round(Fraction,2))), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c("#55DDE0", "#33658A", "#F26419", "#999999", "#F6AE2D", "#887191")) + #alt colors #2F4858
  labs(x = NULL, y = NULL, fill = NULL, title = "Types of Insurance") +
  theme_classic() + theme(axis.line = element_blank(),
                            axis.text = element_blank(),
                            axis.ticks = element_blank(),
                            plot.title = element_text(hjust = 0.5, color = "#363737"))
  