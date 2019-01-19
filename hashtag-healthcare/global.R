library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(magrittr)
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

# load twitter data
tweetTable <- readRDS('data/tweetTable.RDS')

# load datsets
medicaid <- readRDS('data/MedicaidExpansion.RDS')
populations <- readRDS('data/StatePopulations.RDS')
ERVisitsPer1k <- readRDS('data/ERVisitsPer1k.RDS')
AllCancerDeathsByState <- readRDS('data/AllCancerDeathsByState.RDS')
birthsLatePrenatalCare6Mo <- readRDS('data/birthsLatePrenatalCare6Mo.RDS')
birthsTeenMothers19OrYounger <- readRDS('data/birthsTeenMothers19OrYounger.RDS')
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


