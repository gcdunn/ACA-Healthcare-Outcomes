library(tidyverse)
library(ggplot2)
library(magrittr)
library(usmap)
# for word clouds
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
# for Twitter API
library(twitteR)
library(rtweet)

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
hashtags <- c("#healthcare","#ACA", "#Obamacare", "#Medicaid", "#Medicare", "#aca", "#obamacare", "#medicaid", "#medicare")
healthcare_tweets <- search_tweets(paste(hashtags, collapse = " OR "), n = 10000, include_rts = FALSE) #searchTwitter(paste(hashtags, collapse = " OR "), n=10000)
healthcare_tweets <- strip_retweets(healthcare_tweets, strip_manual=TRUE, strip_mt=TRUE)
healthcare_tweets <- twListToDF(healthcare_tweets) %>% select(text,created,longitude,latitude)
healthcare_tweets <- healthcare_tweets %>% select(text,created,longitude,latitude)
tweet_text <- gsub("http.*","",healthcare_tweets$text)
tweet_text <- gsub("https.*","",tweet_text)
tweet_text <- gsub("#","",tweet_text)
tweet_text <- gsub("@*","",tweet_text)
tweet_text <- gsub("&*;","",tweet_text) #does not work

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
  