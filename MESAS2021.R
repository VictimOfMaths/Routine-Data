rm(list=ls())

library(tidyverse)
library(readxl)
library(extrafont)
library(curl)
library(paletteer)
library(ragg)
library(ggtext)
library(scales)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Download MESAS monitoring report data
url <- "https://www.publichealthscotland.scot/media/8092/mesas-monitoring-report-2021-alcohol-sales.xlsx"
temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

#Scotland
onvolscot <- read_excel(temp, sheet="Scotland data", range="I30:AC39")
onpricescot <- read_excel(temp, sheet="Scotland data", range="I43:AC52")
offvolscot <- read_excel(temp, sheet="Scotland data", range="AL30:BF39")
offpricescot <- read_excel(temp, sheet="Scotland data", range="AL43:BF52")

#England & Wales
onvolew <- read_excel(temp, sheet="England & Wales data", range="I30:AC39")
onpriceew <- read_excel(temp, sheet="England & Wales data", range="I43:AC52")
offvolew <- read_excel(temp, sheet="England & Wales data", range="AL30:BF39")
offpriceew <- read_excel(temp, sheet="England & Wales data", range="AL43:BF52")

#Stick together volume data
voldata <- onvolscot %>% 
  mutate(drink=c("Total", "Spirits", "RTDs", "Fortified Wines", "Wine", "Other", "Cider", 
                 "Perry", "Beer"),
         channel="On",
         country="Scotland") %>% 
  gather(year, vol, c(1:21)) %>% 
  mutate(vol=as.numeric(vol)) %>% 
  bind_rows(
    offvolscot %>% 
      mutate(drink=c("Total", "Spirits", "RTDs", "Fortified Wines", "Wine", "Other", "Cider", 
                     "Perry", "Beer"),
             channel="Off",
             country="Scotland") %>% 
      gather(year, vol, c(1:21)) %>% 
      mutate(vol=as.numeric(vol))) %>% 
  bind_rows(
    onvolew %>% 
      mutate(drink=c("Total", "Spirits", "RTDs", "Fortified Wines", "Wine", "Other", "Cider", 
                     "Perry", "Beer"),
             channel="On",
             country="England & Wales") %>% 
      gather(year, vol, c(1:21)) %>% 
      mutate(vol=as.numeric(vol))) %>% 
  bind_rows(
    offvolew %>% 
      mutate(drink=c("Total", "Spirits", "RTDs", "Fortified Wines", "Wine", "Other", "Cider", 
                     "Perry", "Beer"),
             channel="Off",
             country="England & Wales") %>% 
      gather(year, vol, c(1:21)) %>% 
      mutate(vol=as.numeric(vol)))

#Stick together price data
pricedata <- onpricescot %>% 
  mutate(drink=c("Total", "Spirits", "RTDs", "Fortified Wines", "Wine", "Other", "Cider", 
                 "Perry", "Beer"),
         channel="On",
         country="Scotland") %>% 
  gather(year, price, c(1:21)) %>% 
  mutate(price=as.numeric(price)) %>% 
  bind_rows(
    offpricescot %>% 
      mutate(drink=c("Total", "Spirits", "RTDs", "Fortified Wines", "Wine", "Other", "Cider", 
                     "Perry", "Beer"),
             channel="Off",
             country="Scotland") %>% 
      gather(year, price, c(1:21)) %>% 
      mutate(price=as.numeric(price))) %>% 
  bind_rows(
    onpriceew %>% 
      mutate(drink=c("Total", "Spirits", "RTDs", "Fortified Wines", "Wine", "Other", "Cider", 
                     "Perry", "Beer"),
             channel="On",
             country="England & Wales") %>% 
      gather(year, price, c(1:21)) %>% 
      mutate(price=as.numeric(price))) %>% 
  bind_rows(
    offpriceew %>% 
      mutate(drink=c("Total", "Spirits", "RTDs", "Fortified Wines", "Wine", "Other", "Cider", 
                     "Perry", "Beer"),
             channel="Off",
             country="England & Wales") %>% 
      gather(year, price, c(1:21)) %>% 
      mutate(price=as.numeric(price)))

#Bring in inflation index
CPIurl <- "https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/l522/mm23"
temp <- curl_download(url=CPIurl, destfile=temp, quiet=FALSE, mode="wb")

CPI <- read.csv(temp) %>% 
  set_names("year", "CPI") %>%
  mutate(year=as.numeric(year),
         CPI=as.numeric(CPI)) %>% 
  filter(year %in% c(2000:2020)) %>% 
  mutate(multiplier=CPI[year==2020]/CPI) %>% 
  select(-CPI)

#Combine everything
data <- merge(voldata, pricedata) %>% 
  mutate(spend=vol*price,
         year=as.integer(year)) %>% 
  merge(CPI) %>% 
  mutate(price_adj=price*multiplier,
           spend_adj=spend*multiplier)

agg_tiff("Outputs/MESASAlcConsxChannelGB.tiff", units="in", width=9, height=6, res=500)
data %>% filter(drink=="Total") %>% 
  ggplot(aes(x=year, y=vol, colour=channel, linetype=country))+
  geom_line()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Units of alcohol purchased per adult", limits=c(0,NA))+
  scale_colour_paletteer_d("ggthemes::Color_Blind", guide="none")+
  scale_linetype(name="Country")+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="All of Great Britain saw similar changes in alcohol purchasing in 2020",
            subtitle="Alcohol bought per adult in <span style='color:#1170aa;'>shops</span> vs. <span style='color:#fc7d0b;'>pubs/restaurants</span>",
            caption="Data from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/MESASAlcPricexChannelGB.tiff", units="in", width=9, height=6, res=500)
data %>% filter(drink=="Total") %>% 
  ggplot(aes(x=year, y=price_adj, colour=channel, linetype=country))+
  geom_line()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Units of alcohol purchased per adult", limits=c(0,NA))+
  scale_colour_paletteer_d("ggthemes::Color_Blind", guide="none")+
  scale_linetype(name="Country")+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="All of Great Britain saw similar changes in prices paid for alcohol in 2020",
       subtitle="Alcohol bought per adult in <span style='color:#1170aa;'>shops</span> vs. <span style='color:#fc7d0b;'>pubs/restaurants</span>",
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()
  
agg_tiff("Outputs/MESASAlcConsxChannelEW.tiff", units="in", width=9, height=6, res=500)
data %>% filter(drink=="Total" & country=="England & Wales") %>% 
  ggplot(aes(x=year, y=vol, fill=channel))+
  geom_col(show.legend=FALSE)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Units of alcohol purchased per adult", limits=c(0,NA))+
  scale_fill_paletteer_d("ggthemes::Color_Blind")+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="The pandemic shifted our drinking from the pub to home",
       subtitle="Alcohol bought per adult in <span style='color:#1170aa;'>shops</span> vs. <span style='color:#fc7d0b;'>pubs/restaurants</span> in England & Wales",
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/MESASAlcSpendxChannelEW.tiff", units="in", width=9, height=6, res=500)
data %>% filter(drink=="Total" & country=="England & Wales") %>% 
  ggplot(aes(x=year, y=spend_adj, fill=channel))+
  geom_col(show.legend=FALSE)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Annual spend on alcohol\n(2020 prices)", limits=c(0,NA),
                     labels=label_dollar(prefix="£"))+
  scale_fill_paletteer_d("ggthemes::Color_Blind")+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="The shift to home drinking meant we spent less on alcohol in 2020",
       subtitle="Total spend on alcohol per adult in <span style='color:#1170aa;'>shops</span> vs. <span style='color:#fc7d0b;'>pubs/restaurants</span> in England & Wales",
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/MESASAlcPricexChannelEW.tiff", units="in", width=9, height=6, res=500)
data %>% filter(drink=="Total" & country=="England & Wales") %>% 
  ggplot(aes(x=year, y=price_adj, colour=channel))+
  geom_line(show.legend=FALSE)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Mean price paid per unit of alcohol\n(2020 prices)", limits=c(0,NA),
                     labels=label_dollar(prefix="£"))+
  scale_colour_paletteer_d("ggthemes::Color_Blind")+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="The pandemic didn't see a major splurge on cheap (or expensive) booze",
       subtitle="Average price paid per unit for alcohol in <span style='color:#1170aa;'>shops</span> vs. <span style='color:#fc7d0b;'>pubs/restaurants</span> in England & Wales",
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

data_short <- data %>% 
  filter(drink!="Total") %>% 
  mutate(drinkcat=case_when(
    drink %in% c("Wine", "Fortified Wines") ~ "Wine",
    drink %in% c("Cider", "Perry") ~ "Cider/Perry",
    drink %in% c("Spirits", "RTDs", "Other") ~ "Spirits/Other",
    TRUE ~ drink)) %>% 
  group_by(drinkcat, country, channel, year) %>% 
  summarise(price_adj=weighted.mean(price_adj, vol, na.rm=TRUE), vol=sum(vol, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(spend_adj=vol*price_adj)

agg_tiff("Outputs/MESASAlcConsxChannelxDrinkEW.tiff", units="in", width=9, height=6, res=500)
data_short %>% filter(country=="England & Wales") %>% 
  ggplot(aes(x=year, y=vol, fill=channel))+
  geom_col(show.legend=FALSE)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Units of alcohol purchased per adult", limits=c(0,NA))+
  scale_fill_paletteer_d("ggthemes::Color_Blind")+
  facet_wrap(~drinkcat)+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="The pandemic had the biggest impact on beer sales",
       subtitle="Alcohol bought per adult in <span style='color:#1170aa;'>shops</span> vs. <span style='color:#fc7d0b;'>pubs/restaurants</span> in England & Wales",
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/MESASAlcSpendxChannelxDrinkEW.tiff", units="in", width=9, height=6, res=500)
data_short %>% filter(country=="England & Wales") %>%   ggplot(aes(x=year, y=spend_adj, fill=channel))+
  geom_col(show.legend=FALSE)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Annual spend on alcohol\n(2020 prices)", limits=c(0,NA),
                     labels=label_dollar(prefix="£"))+
  scale_fill_paletteer_d("ggthemes::Color_Blind")+
  facet_wrap(~drinkcat)+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="The wine market saw the smallest reduction in total value in 2020",
       subtitle="Total spend on alcohol per adult in <span style='color:#1170aa;'>shops</span> vs. <span style='color:#fc7d0b;'>pubs/restaurants</span> in England & Wales",
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/MESASAlcPricexChannelxDrinkEW.tiff", units="in", width=9, height=6, res=500)
data_short %>% filter(country=="England & Wales") %>%   ggplot(aes(x=year, y=price_adj, colour=channel))+
  geom_line(show.legend=FALSE)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Mean price paid per unit of alcohol\n(2020 prices)", limits=c(0,NA),
                     labels=label_dollar(prefix="£"))+
  scale_colour_paletteer_d("ggthemes::Color_Blind")+
  facet_wrap(~drinkcat)+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="The pandemic didn't see a major splurge on cheap (or expensive) booze",
       subtitle="Average price paid per unit for alcohol in <span style='color:#1170aa;'>shops</span> vs. <span style='color:#fc7d0b;'>pubs/restaurants</span> in England & Wales",
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

#Scotland

agg_tiff("Outputs/MESASAlcConsxChannelS.tiff", units="in", width=9, height=6, res=500)
data %>% filter(drink=="Total" & country=="Scotland") %>% 
  ggplot(aes(x=year, y=vol, fill=channel))+
  geom_col(show.legend=FALSE)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Units of alcohol purchased per adult", limits=c(0,NA))+
  scale_fill_paletteer_d("ggthemes::Color_Blind")+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="The pandemic shifted our drinking from the pub to home",
       subtitle="Alcohol bought per adult in <span style='color:#1170aa;'>shops</span> vs. <span style='color:#fc7d0b;'>pubs/restaurants</span> in Scotland",
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/MESASAlcSpendxChannelS.tiff", units="in", width=9, height=6, res=500)
data %>% filter(drink=="Total" & country=="Scotland") %>% 
  ggplot(aes(x=year, y=spend_adj, fill=channel))+
  geom_col(show.legend=FALSE)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Annual spend on alcohol\n(2020 prices)", limits=c(0,NA),
                     labels=label_dollar(prefix="£"))+
  scale_fill_paletteer_d("ggthemes::Color_Blind")+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="The shift to home drinking meant we spent less on alcohol in 2020",
       subtitle="Total spend on alcohol per adult in <span style='color:#1170aa;'>shops</span> vs. <span style='color:#fc7d0b;'>pubs/restaurants</span> in Scotland",
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/MESASAlcPricexChannelS.tiff", units="in", width=9, height=6, res=500)
data %>% filter(drink=="Total" & country=="Scotland") %>% 
  ggplot(aes(x=year, y=price_adj, colour=channel))+
  geom_line(show.legend=FALSE)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Mean price paid per unit of alcohol\n(2020 prices)", limits=c(0,NA),
                     labels=label_dollar(prefix="£"))+
  scale_colour_paletteer_d("ggthemes::Color_Blind")+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="The pandemic didn't see a major splurge on cheap (or expensive) booze",
       subtitle="Average price paid per unit for alcohol in <span style='color:#1170aa;'>shops</span> vs. <span style='color:#fc7d0b;'>pubs/restaurants</span> in Scotland",
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/MESASAlcConsxChannelxDrinkS.tiff", units="in", width=9, height=6, res=500)
data_short %>% filter(country=="Scotland") %>% 
  ggplot(aes(x=year, y=vol, fill=channel))+
  geom_col(show.legend=FALSE)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Units of alcohol purchased per adult", limits=c(0,NA))+
  scale_fill_paletteer_d("ggthemes::Color_Blind")+
  facet_wrap(~drinkcat)+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="The pandemic had the biggest impact on beer sales",
       subtitle="Alcohol bought per adult in <span style='color:#1170aa;'>shops</span> vs. <span style='color:#fc7d0b;'>pubs/restaurants</span> in Scotland",
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/MESASAlcSpendxChannelxDrinkS.tiff", units="in", width=9, height=6, res=500)
data_short %>% filter(country=="Scotland") %>%   ggplot(aes(x=year, y=spend_adj, fill=channel))+
  geom_col(show.legend=FALSE)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Annual spend on alcohol\n(2020 prices)", limits=c(0,NA),
                     labels=label_dollar(prefix="£"))+
  scale_fill_paletteer_d("ggthemes::Color_Blind")+
  facet_wrap(~drinkcat)+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="The wine market saw the smallest reduction in total value in 2020",
       subtitle="Total spend on alcohol per adult in <span style='color:#1170aa;'>shops</span> vs. <span style='color:#fc7d0b;'>pubs/restaurants</span> in Scotland",
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/MESASAlcPricexChannelxDrinkS.tiff", units="in", width=9, height=6, res=500)
data_short %>% filter(country=="Scotland") %>%   ggplot(aes(x=year, y=price_adj, colour=channel))+
  geom_line(show.legend=FALSE)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Mean price paid per unit of alcohol\n(2020 prices)", limits=c(0,NA),
                     labels=label_dollar(prefix="£"))+
  scale_colour_paletteer_d("ggthemes::Color_Blind")+
  facet_wrap(~drinkcat)+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="The pandemic didn't see a major splurge on cheap (or expensive) booze",
       subtitle="Average price paid per unit for alcohol in <span style='color:#1170aa;'>shops</span> vs. <span style='color:#fc7d0b;'>pubs/restaurants</span> in Scotland",
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()