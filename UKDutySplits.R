rm(list=ls())

library(tidyverse)
library(readODS)
library(readxl)
library(paletteer)
library(curl)
library(ragg)
library(ggtext)
library(ggrepel)
library(extrafont)
library(scales)
library(geomtextpath)

options(scipen=99999999)

#Set common font for all plots
font <- "Lato"

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"),
          plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
          plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),
          axis.text=element_text(colour="Grey40"),
          axis.title=element_text(colour="Grey20"),
          legend.text=element_text(colour="Grey40"),
          legend.title=element_text(colour="Grey20"))
}

#Download disaggregated duty take data
url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/853132/Disaggregated_tax_and_NICs_receipts_-_statistics_table.ods"
temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

Eng <- read_ods(temp, range="A24:AM44") %>% 
  select(c("England", "Tobacco duties", "Spirits duties", "Beer duties", "Wines duties", 
           "Cider duties")) %>% 
  set_names("Year", "Tobacco", "Spirits", "Beer", "Wine", "Cider") %>% 
  gather(Product, Proportion, c(2:6)) %>% 
  mutate(Country="England")

Sco <- read_ods(temp, range="A108:AM128") %>% 
  select(c("Scotland", "Tobacco duties", "Spirits duties", "Beer duties", "Wines duties", 
           "Cider duties")) %>% 
  set_names("Year", "Tobacco", "Spirits", "Beer", "Wine", "Cider") %>% 
  gather(Product, Proportion, c(2:6)) %>% 
  mutate(Country="Scotland")

NI <- read_ods(temp, range="A150:AM170") %>% 
  select(c("Northern Ireland", "Tobacco duties", "Spirits duties", "Beer duties", "Wines duties", 
           "Cider duties")) %>% 
  set_names("Year", "Tobacco", "Spirits", "Beer", "Wine", "Cider") %>% 
  gather(Product, Proportion, c(2:6)) %>% 
  mutate(Country="Northern Ireland")

Wal <- read_ods(temp, range="A66:AM86") %>% 
  select(c("Wales", "Tobacco duties", "Spirits duties", "Beer duties", "Wines duties", 
           "Cider duties")) %>% 
  set_names("Year", "Tobacco", "Spirits", "Beer", "Wine", "Cider") %>% 
  gather(Product, Proportion, c(2:6)) %>% 
  mutate(Country="Wales")

#Get relative population of each UK nation
url2 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland/mid2001tomid2020detailedtimeseries/ukpopulationestimates18382020.xlsx"
temp <- tempfile()
temp <- curl_download(url=url2, destfile=temp, quiet=FALSE, mode="wb")

Pop <- read_excel(temp, sheet="Table 10", range="A5:B27") %>% 
  set_names("Year", "England") %>% 
  merge(read_excel(temp, sheet="Table 12", range="A5:B27") %>% 
          set_names("Year", "Wales")) %>% 
  merge(read_excel(temp, sheet="Table 14", range="A5:B27") %>% 
          set_names("Year", "Scotland")) %>%   
  merge(read_excel(temp, sheet="Table 17", range="A5:B27") %>% 
          set_names("Year", "Northern Ireland")) %>% 
  rowwise() %>% 
  mutate(Total=England+Wales+Scotland+`Northern Ireland`) %>% 
  gather(Country, Pop, c(2:5)) %>% 
  mutate(Proportion=Pop/Total,
         Year=as.numeric(substr(Year, 5, 9)),
         Year=paste0(Year, "-", substr(as.character(Year+1), 3, 4))) %>% 
  select(Year, Country, Proportion) %>% 
  filter(!Year %in% c("2019-20", "2020-21")) %>% 
  mutate(Product="Population")
  
data <- bind_rows(Eng, Sco, NI, Wal, Pop) %>% 
  mutate(Product=factor(Product, levels=c("Population", "Tobacco", "Beer", "Cider", 
                                          "Wine", "Spirits")))

agg_tiff("Outputs/DutySplits.tiff", units="in", width=8, height=6, res=600)
ggplot(data, aes(x=Year, y=Proportion, colour=Product, group=Product, linetype=Product))+
  geom_line()+
  scale_x_discrete(breaks=c("1999-00", "2009-10", "2018-19"))+
  scale_y_continuous(labels=label_percent(accuracy=1))+
  scale_linetype_manual(values=c(2,1,1,1,1,1), guide="none")+
  scale_colour_manual(values=c("Black", "Red", "#ffc000", "#00b050", "#7030a0", "#00b0f0"),
                      name="Proportion of", labels=c("Population", "Tobacco duty", "Beer duty",
                                                     "Cider duty", "Wine duty", "Spirits duty"))+
  facet_wrap(~Country, scales="free_y")+
  theme_custom()+
  labs(title="England buys wine, NI and Scotland cigarettes and spirits and Wales cider",
       subtitle="Proportion of total UK duty revenue coming from each UK nation, by product",
       caption="Data from UK Government and ONS | Plot by @VictimOfMaths")

dev.off()
