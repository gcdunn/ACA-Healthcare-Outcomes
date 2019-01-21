library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(gghighlight)
library(ggthemes)
library(magrittr)
library(scales)
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
medicaidAll <- readRDS('data/MedicaidExpansion.RDS')
medicaid <- readRDS('data/MedicaidExpansion.RDS')
populations <- readRDS('data/StatePopulations.RDS')
ERVisitsPer1k <- readRDS('data/ERVisitsPer1k.RDS') %>% filter(State != 'District of Columbia') %>% 
  gather(key='Year',value='VisitsPer1k','2008','2009','2010','2011','2012','2013','2014','2015','2016') %>%
  mutate(Year=as.numeric(Year))
AllCancerDeathsByState <- readRDS('data/AllCancerDeathsByState.RDS')
birthsLatePrenatalCare6Mo <- readRDS('data/birthsLatePrenatalCare6Mo.RDS')
birthsTeenMothers19OrYounger <- readRDS('data/birthsTeenMothers19OrYounger.RDS')
totalBirths <- readRDS('data/totalBirths.RDS') %>% filter(state != 'District of Columbia') %>% 
  gather(key='Year',value='TotalBirths','2008','2009','2010','2011','2012','2013','2014','2015','2016','2017')  %>%
  mutate(Year=as.numeric(Year))

menNoHealthcareProvider <- readRDS('data/menNoHealthcareProvider.RDS')
womenNoHealthcareProvider <- readRDS('data/womenNoHealthcareProvider.RDS')
womenNoHealthcareCost <- readRDS('data/womenNoHealthcareCost.RDS')
hispanicCancerIncidence <- readRDS('data/hispanicCancerIncidence.RDS')
whiteCancerIncidence <- readRDS('data/whiteCancerIncidence.RDS')
blackCancerIncidence <- readRDS('data/blackCancerIncidence.RDS')

#insurance2008 <- readRDS('data/InsuranceCoverageTotal2008.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total, Year='2008')
#insurance2009 <- readRDS('data/InsuranceCoverageTotal2009.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total, Year='2009')
#insurance2010 <- readRDS('data/InsuranceCoverageTotal2010.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total, Year='2010')
insurance2011 <- readRDS('data/InsuranceCoverageTotal2011.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total, Year='2011')
insurance2012 <- readRDS('data/InsuranceCoverageTotal2012.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total, Year='2012')
insurance2013 <- readRDS('data/InsuranceCoverageTotal2013.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total, Year='2013')
insurance2014 <- readRDS('data/InsuranceCoverageTotal2014.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total, Year='2014')
insurance2015 <- readRDS('data/InsuranceCoverageTotal2015.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total, Year='2015')
insurance2016 <- readRDS('data/InsuranceCoverageTotal2016.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total, Year='2016')
insurance2017 <- readRDS('data/InsuranceCoverageTotal2017.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total, Year='2017')

bigInsuranceTable <- do.call("rbind", list(insurance2011,insurance2012,
  insurance2013,insurance2014,insurance2015,insurance2016)) %>%
  mutate(Year=as.numeric(Year),Era=case_when(
    Year < 2014 ~ "2011-2013",
    Year >= 2014 ~ "2014-2016"
  ))
years <- c('2011','2012','2013','2014','2015','2016')

men_healthcare_medicaid <- menNoHealthcareProvider %>% inner_join(medicaid,by = c("State" = "State")) %>% 
  select(State,`All Men`,Expanded) %>%
  mutate(Data="Percent of men without a healthcare provider", Medicaid = case_when(
    Expanded==TRUE ~ "Expanded",
    Expanded==FALSE ~ "Not Expanded")) %>%
  rename("N"="All Men")
women_healthcare_medicaid <- womenNoHealthcareProvider %>% inner_join(medicaid,by = c("State" = "State")) %>% 
  select(State,`All Women`,Expanded) %>%
  mutate(Data="Percent of women without a healthcare provider", Medicaid = case_when(
    Expanded==TRUE ~ "Expanded",
    Expanded==FALSE ~ "Not Expanded")) %>%
  rename("N"="All Women")
women_healthcare_cost_medicaid <- womenNoHealthcareCost %>% inner_join(medicaid,by = c("State" = "State")) %>% 
  select(State,`All Women`,Expanded) %>%
  mutate(Data="Percent of women without healthcare due to cost", Medicaid = case_when(
    Expanded==TRUE ~ "Expanded",
    Expanded==FALSE ~ "Not Expanded")) %>%
  rename("N"="All Women")
all_access <- do.call(rbind,list(men_healthcare_medicaid,women_healthcare_medicaid,women_healthcare_cost_medicaid))

white_cancer_medicaid <- whiteCancerIncidence %>% inner_join(medicaid,by = c("State" = "State")) %>% 
  select(State,Year,`Age-Adjusted Rate`,Expanded) %>%
  mutate(Data="Age-adjusted rate of cancer in white non-Hispanic patients", Medicaid = case_when(
    Expanded==TRUE ~ "Expanded",
    Expanded==FALSE ~ "Not Expanded")) %>%
  rename("N"="Age-Adjusted Rate")
hispanic_cancer_medicaid <- hispanicCancerIncidence %>% inner_join(medicaid,by = c("State" = "State")) %>% 
  select(State,Year,`Age-Adjusted Rate`,Expanded) %>%
  mutate(Data="Age-adjusted rate of cancer in Hispanic patients", Medicaid = case_when(
    Expanded==TRUE ~ "Expanded",
    Expanded==FALSE ~ "Not Expanded")) %>%
  rename("N"="Age-Adjusted Rate")
black_cancer_medicaid <- blackCancerIncidence %>% inner_join(medicaid,by = c("State" = "State")) %>% 
  select(State,Year,`Age-Adjusted Rate`,Expanded) %>%
  mutate(Data="Age-adjusted rate of cancer in Black or African-American patients", Medicaid = case_when(
    Expanded==TRUE ~ "Expanded",
    Expanded==FALSE ~ "Not Expanded")) %>%
  rename("N"="Age-Adjusted Rate")

er_medicaid <- ERVisitsPer1k %>% inner_join(medicaid,by = c("State" = "State")) %>% 
  inner_join(bigInsuranceTable,by = c("State" = "state","Year"="Year")) %>%
  select(State,Year,VisitsPer1k,Expanded,Medicaid,Total) %>%
  mutate(Data="ER Visits per 1,000 People",`Percent on Medicaid`=Medicaid/Total) %>%
  select(-Medicaid,-Total) %>%
  rename("N"="VisitsPer1k")
cancer_medicaid <- AllCancerDeathsByState %>% inner_join(medicaid,by = c("State" = "State")) %>%
  inner_join(bigInsuranceTable,by = c("State" = "state","Year"="Year")) %>%
  select(State,Year,DeathsPer100k,Expanded,Medicaid,Total) %>%
  mutate(Data="Cancer Deaths per 100,000 People",`Percent on Medicaid`=Medicaid/Total) %>%
  select(-Medicaid,-Total) %>%
  rename("N"="DeathsPer100k")
birthsTeen_medicaid <- birthsTeenMothers19OrYounger %>% inner_join(medicaid,by = c("State" = "State")) %>%
  inner_join(bigInsuranceTable,by = c("State" = "state","Year"="Year")) %>%
  select(State,Year,Births,Expanded,Medicaid,Total) %>%
  inner_join(totalBirths,by = c("State" = "state","Year"="Year")) %>%
  mutate(birthsNorm = Births/TotalBirths,`Percent on Medicaid`=Medicaid/Total,Data="Teen Birth Rate") %>%
  select(-Births,-TotalBirths,-Medicaid,-Total) %>% 
  rename("N"="birthsNorm")
birthsPrenatal_medicaid <- birthsLatePrenatalCare6Mo %>% inner_join(medicaid,by = c("State" = "State")) %>%
  inner_join(bigInsuranceTable,by = c("State" = "state","Year"="Year")) %>%
  select(State,Year,Births,Expanded,Medicaid,Total) %>%
  inner_join(totalBirths,by = c("State" = "state","Year"="Year")) %>%
  mutate(birthsNorm = Births/TotalBirths, `Percent on Medicaid`=Medicaid/Total, Data="Late Prenatal Care Birth Rate") %>%
  select(-Births,-TotalBirths,-Medicaid,-Total) %>% 
  rename("N"="birthsNorm")
allMetrics <- do.call("rbind", list(er_medicaid,cancer_medicaid,birthsTeen_medicaid,birthsPrenatal_medicaid))

