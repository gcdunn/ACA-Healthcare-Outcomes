library(tidyverse)
library(ggplot2)
library(magrittr)
library(usmap)
# for word clouds
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# load datsets
medicaid <- readRDS('data/MedicaidExpansion.RDS')
populations <- readRDS('data/StatePopulations.RDS')
ERVisitsPer1k <- readRDS('data/ERVisitsPer1k.RDS')
AllCancerDeathsByState <- readRDS('data/AllCancerDeathsByState.RDS')
birthsLatePrenatalCare6Mo <- readRDS('data/birthsLatePrenatalCare6Mo.RDS')
birthsTeenMothers19OrYounger <- readRDS('data/birthsTeenMothers19OrYounger.RDS')
insurance2008 <- readRDS('data/InsuranceCoverageTotal2008.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total)
insurance2009 <- readRDS('data/InsuranceCoverageTotal2009.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total)
insurance2010 <- readRDS('data/InsuranceCoverageTotal2010.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total)
insurance2011 <- readRDS('data/InsuranceCoverageTotal2011.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total)
insurance2012 <- readRDS('data/InsuranceCoverageTotal2012.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total)
insurance2013 <- readRDS('data/InsuranceCoverageTotal2013.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total)
insurance2014 <- readRDS('data/InsuranceCoverageTotal2014.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total)
insurance2015 <- readRDS('data/InsuranceCoverageTotal2015.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total)
insurance2016 <- readRDS('data/InsuranceCoverageTotal2016.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total)
insurance2017 <- readRDS('data/InsuranceCoverageTotal2017.RDS') %>% filter(state != 'District of Columbia') %>% mutate(PctUninsured = 100*Uninsured/Total)

years <- c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)

# Insurance maps
plot_usmap(data = insurance2016, values = "PctUninsured", lines = "white") + 
  scale_fill_continuous(low = "#c79fef", high = "#35063e", name = "Percent\n Uninsured", 
                        label = scales::comma) + 
  theme(legend.position = "right", legend.key.size = unit(1.75, "cm"), 
        legend.text = element_text(size = 14, face = 'bold'), legend.title=element_text(size=16, face = 'bold'))

# Insurance pie charts
subset <- insurance2008 %>% filter(state=='Tennessee') %>% select(-PctUninsured) %>% mutate_if(is.numeric, funs(./Total)) %>%
  gather(key=Source,value=Fraction,Employer,`Non-Group`,Medicaid,Medicare,`Other Public`,Uninsured) %>% select(-Total)

library(scales)
ggplot(subset, aes(x="", y=Fraction, fill=Source)) + geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + geom_text(aes(label = percent(round(Fraction,2))), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c("#55DDE0", "#33658A", "#F26419", "#999999", "#F6AE2D", "#887191")) + #alt colors #2F4858
  labs(x = NULL, y = NULL, fill = NULL, title = "Types of Insurance") +
  theme_classic() + theme(axis.line = element_blank(),
                            axis.text = element_blank(),
                            axis.ticks = element_blank(),
                            plot.title = element_text(hjust = 0.5, color = "#363737"))
  
  


