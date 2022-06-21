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
          text=element_text(family="Lato"),
          plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
          plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),
          axis.text=element_text(colour="Grey40"),
          axis.title=element_text(colour="Grey20"),
          legend.text=element_text(colour="Grey40"),
          legend.title=element_text(colour="Grey20"))
}


#Download MESAS monitoring report data
url <- "https://publichealthscotland.scot/media/13691/mesas-monitoring-report-2022-alcohol-sales.xlsx"
temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

#Scotland
onvolscot <- read_excel(temp, sheet="Scotland data", range="I30:AD39")
onpricescot <- read_excel(temp, sheet="Scotland data", range="I43:AD52")
offvolscot <- read_excel(temp, sheet="Scotland data", range="AM30:BH39")
offpricescot <- read_excel(temp, sheet="Scotland data", range="AM43:BH52")

#England & Wales
onvolew <- read_excel(temp, sheet="England & Wales data", range="I30:AD39")
onpriceew <- read_excel(temp, sheet="England & Wales data", range="I43:AD52")
offvolew <- read_excel(temp, sheet="England & Wales data", range="AM30:BH39")
offpriceew <- read_excel(temp, sheet="England & Wales data", range="AM43:BH52")

#Stick together volume data
voldata <- onvolscot %>% 
  mutate(drink=c("Total", "Spirits", "RTDs", "Fortified Wines", "Wine", "Other", "Cider", 
                 "Perry", "Beer"),
         channel="On",
         country="Scotland") %>% 
  gather(year, vol, c(1:22)) %>% 
  mutate(vol=as.numeric(vol)) %>% 
  bind_rows(
    offvolscot %>% 
      mutate(drink=c("Total", "Spirits", "RTDs", "Fortified Wines", "Wine", "Other", "Cider", 
                     "Perry", "Beer"),
             channel="Off",
             country="Scotland") %>% 
      gather(year, vol, c(1:22)) %>% 
      mutate(vol=as.numeric(vol))) %>% 
  bind_rows(
    onvolew %>% 
      mutate(drink=c("Total", "Spirits", "RTDs", "Fortified Wines", "Wine", "Other", "Cider", 
                     "Perry", "Beer"),
             channel="On",
             country="England & Wales") %>% 
      gather(year, vol, c(1:22)) %>% 
      mutate(vol=as.numeric(vol))) %>% 
  bind_rows(
    offvolew %>% 
      mutate(drink=c("Total", "Spirits", "RTDs", "Fortified Wines", "Wine", "Other", "Cider", 
                     "Perry", "Beer"),
             channel="Off",
             country="England & Wales") %>% 
      gather(year, vol, c(1:22)) %>% 
      mutate(vol=as.numeric(vol)))

#Stick together price data
pricedata <- onpricescot %>% 
  mutate(drink=c("Total", "Spirits", "RTDs", "Fortified Wines", "Wine", "Other", "Cider", 
                 "Perry", "Beer"),
         channel="On",
         country="Scotland") %>% 
  gather(year, price, c(1:22)) %>% 
  mutate(price=as.numeric(price)) %>% 
  bind_rows(
    offpricescot %>% 
      mutate(drink=c("Total", "Spirits", "RTDs", "Fortified Wines", "Wine", "Other", "Cider", 
                     "Perry", "Beer"),
             channel="Off",
             country="Scotland") %>% 
      gather(year, price, c(1:22)) %>% 
      mutate(price=as.numeric(price))) %>% 
  bind_rows(
    onpriceew %>% 
      mutate(drink=c("Total", "Spirits", "RTDs", "Fortified Wines", "Wine", "Other", "Cider", 
                     "Perry", "Beer"),
             channel="On",
             country="England & Wales") %>% 
      gather(year, price, c(1:22)) %>% 
      mutate(price=as.numeric(price))) %>% 
  bind_rows(
    offpriceew %>% 
      mutate(drink=c("Total", "Spirits", "RTDs", "Fortified Wines", "Wine", "Other", "Cider", 
                     "Perry", "Beer"),
             channel="Off",
             country="England & Wales") %>% 
      gather(year, price, c(1:22)) %>% 
      mutate(price=as.numeric(price)))

#Bring in inflation index
CPIurl <- "https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/l522/mm23"
temp <- curl_download(url=CPIurl, destfile=temp, quiet=FALSE, mode="wb")

CPI <- read.csv(temp) %>% 
  set_names("year", "CPI") %>%
  mutate(year=as.numeric(year),
         CPI=as.numeric(CPI)) %>% 
  filter(year %in% c(2000:2021)) %>% 
  mutate(multiplier=CPI[year==2021]/CPI) %>% 
  select(-CPI)

#Combine everything
data <- merge(voldata, pricedata) %>% 
  mutate(spend=vol*price,
         year=as.integer(year)) %>% 
  merge(CPI) %>% 
  mutate(price_adj=price*multiplier,
           spend_adj=spend*multiplier)

agg_tiff("Outputs/MESASAlcConsxGB.tiff", units="in", width=9, height=6, res=500)
data %>% filter(drink=="Total") %>% 
  select(year, country, vol, channel) %>% 
  spread(channel, vol) %>% 
  mutate(combined=On+Off) %>% 
  ggplot()+
  geom_rect(xmin=2017.5, xmax=2022, ymin=0, ymax=1200, fill="Grey85")+
  geom_line(aes(x=year, y=combined, colour=country, linetype=country))+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Units of alcohol purchased per adult", limits=c(0,NA))+
  scale_colour_paletteer_d("colorblindr::OkabeIto", name="")+
  scale_linetype(name="")+
  theme_custom()+
  annotate("text", x=2019.75, y=400, family="Lato", label="MUP in place\nin Scotland")+
  labs(title="Minimum Unit Pricing has narrowed the alcohol sales gap",
       subtitle="Units of alcohol purchased annually per adult in Scotland and England & Wales",
       caption="data from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

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

#####################################################
#Price banding
#Download data from Public Health Scotland website
temp <- tempfile()
source <- "https://www.publichealthscotland.scot/media/8093/mesas-monitoring-report-2021-alcohol-price-and-affordability.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data <- read_excel(temp, sheet="E&W 2020", range="A3:R54") %>% 
  gather(price, vol, c(2:18)) %>% 
  rename("drink"="Pence per unit") %>% 
  mutate(country="England & Wales") %>% 
  bind_rows(read_excel(temp, sheet="Scotland 2020", range="A3:R54") %>% 
              gather(price, vol, c(2:18)) %>% 
              rename("drink"="Pence per unit") %>% 
              mutate(country="Scotland")) %>% 
  mutate(priceval=case_when(
    price=="up to 9" ~ 7.5, price=="10 - 14" ~ 12.5, price=="15 - 19" ~ 17.5,
    price=="20 - 24" ~ 22.5, price=="25 - 29" ~ 27.5, price=="30 - 34" ~ 32.5,
    price=="35 - 39" ~ 37.5, price=="40 - 44" ~ 42.5, price=="45 - 49" ~ 47.5,
    price=="50 - 54" ~ 52.5, price=="55 - 59" ~ 57.5, price=="60 - 64" ~ 62.5,
    price=="65 - 69" ~ 67.5, price=="70 - 74" ~ 72.5, price=="75 - 79" ~ 77.5,
    price=="80 - 84" ~ 82.5, price=="85up" ~ 87.5),
    tier1=if_else(drink %in% c("Vodka", "Blended whisky", "Gin", "Cream liqueurs", "Brandy",
                               "White rum", "Imported whisky", "Liqueurs", "Malt whisky",
                               "Dark rum", "Cognac", "Golden rum", "Speciality drinks", 
                               "Minor spirits", "RTDs", "FORTIFIED WINES", "Table wine",
                               "Sparkling wine", "Champagne", "British-made wine",
                               "Low alcohol wine", "PERRY", "Nab/Lab beer", "Commodity beer",
                               "Standard beer", "Premium beer", "Superstrength beer",
                               "Strong cider", "Regular cider"), TRUE, FALSE),
    tier2=if_else(drink %in% c("SPIRITS (all)", "RTDs", "FORTIFIED WINES", "WINE (all)",
                               "PERRY", "BEERS (all)*", "CIDER (all)"), TRUE, FALSE)) %>% 
  group_by(drink) %>% 
  mutate(meanprice=weighted.mean(priceval, vol)) %>% 
  ungroup() %>% 
  mutate(drinklabel=case_when(
    drink=="SPIRITS (all)" ~ "Spirits",
    drink=="RTDs" ~ "Pre-mixed drinks",
    drink=="FORTIFIED WINES" ~ "Foritified wines",
    drink=="WINE (all)" ~ "Wine",
    drink=="PERRY" ~ "Perry",
    drink=="BEERS (all)*" ~ "Beers",
    drink=="Nab/Lab beer" ~ "Low alcohol beer",
    drink=="CIDER (all)" ~ "Cider",
    TRUE ~ drink),
    drinklabel=fct_reorder(drinklabel, meanprice),
    price=fct_reorder(price, -priceval))

agg_png("Outputs/AlcPriceEWGrouped.png", units="in", width=10, height=6, res=500)
data %>% filter(tier2==TRUE) %>% 
  ggplot(aes(x=vol, y=drinklabel, fill=price))+
  geom_col(position="fill")+
  scale_x_continuous(name="Proportion of sales", labels=label_percent(accuracy=1))+
  scale_y_discrete(name="")+
  scale_fill_paletteer_d("colorBlindness::Blue2DarkRed18Steps", name="Price per\nunit",
                         labels=c("85p+", "80-84p", "75-79p", "70-74p", "65-69p", "60-64p",
                                  "55-59p", "50-54p", "45-49p", "40-44p", "35-39p",
                                  "30-34p", "25-29p", "20-24p", "15-19p", "10-14p", "<9p"))+
  facet_wrap(~country)+
  theme_custom()+
  labs(title="Drinkers pay the least for cider and perry",
       subtitle="Proportion of shop-bought alcohol sold in England & Wales in 2020 by price per unit of alcohol",
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

agg_png("Outputs/AlcPriceEWGroupedAlt.png", units="in", width=8, height=6, res=500)
data %>% filter(tier2==TRUE) %>% 
  ggplot(aes(x=vol, y=drinklabel, fill=price))+
  geom_col(show.legend=FALSE, position="fill")+
  scale_x_continuous(name="Proportion of sales", labels=label_percent(accuracy=1))+
  scale_y_discrete(name="")+
  scale_fill_paletteer_d("colorBlindness::Blue2DarkRed18Steps")+
  theme_custom()+
  labs(title="Drinkers pay the least for cider and perry",
       subtitle="Proportion of shop-bought alcohol sold in England & Wales in 2020 by price per unit of alcohol",
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")+
  annotate("text", x=0.59, y=7, label="85p+", colour="white", family="Lato")+
  annotate("text", x=0.515, y=5, label="50-54p", colour="black", family="Lato")+
  annotate("text", x=0.23, y=3, label="40-44p", colour="black", family="Lato")+
  annotate("text", x=0.07, y=1, label="25-29p", colour="black", family="Lato")

dev.off()


data %>% filter(tier1==TRUE) %>% 
  ggplot(aes(x=vol, y=drinklabel, fill=price))+
  geom_col(position="fill")+
  scale_x_continuous(name="Proportion of sales", labels=label_percent(accuracy=1))+
  scale_y_discrete(name="")+
  scale_fill_paletteer_d("colorBlindness::Blue2DarkRed18Steps")+
  theme_custom()
