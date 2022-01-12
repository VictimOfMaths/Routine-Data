rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(ggtext)
library(extrafont)
library(RcppRoll)
library(ragg)
library(paletteer)
library(ggrepel)

options(scipen=999999)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Download sales data from MESAS monitoring reports
#https://www.publichealthscotland.scot/publications/mesas-monitoring-report-2021/
source <- "https://www.publichealthscotland.scot/media/8093/mesas-monitoring-report-2021-alcohol-price-and-affordability.xlsx"
temp <- tempfile()

temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

Scotland17 <- read_excel(temp, sheet="Scotland 2017", range="S4:S54", col_names=FALSE) %>% 
  set_names("Vol") %>% 
  mutate(Year=2017, Country="Scotland", index=1:n())

Scotland18 <- read_excel(temp, sheet="Scotland 2018", range="S4:S54", col_names=FALSE) %>% 
  set_names("Vol") %>% 
  mutate(Year=2018, Country="Scotland", index=1:n())

Scotland19 <- read_excel(temp, sheet="Scotland 2019", range="S4:S54", col_names=FALSE) %>% 
  set_names("Vol") %>% 
  mutate(Year=2019, Country="Scotland", index=1:n())

Scotland20 <- read_excel(temp, sheet="Scotland 2020", range="S4:S54", col_names=FALSE) %>% 
  set_names("Vol") %>% 
  mutate(Year=2020, Country="Scotland", index=1:n())

EW17 <- read_excel(temp, sheet="E&W 2017", range="S4:S54", col_names=FALSE) %>% 
  set_names("Vol") %>% 
  mutate(Year=2017, Country="England&Wales", index=1:n())

EW18 <- read_excel(temp, sheet="E&W 2018", range="S4:S54", col_names=FALSE) %>% 
  set_names("Vol") %>% 
  mutate(Year=2018, Country="England&Wales", index=1:n())

EW19 <- read_excel(temp, sheet="E&W 2019", range="S4:S54", col_names=FALSE) %>% 
  set_names("Vol") %>% 
  mutate(Year=2019, Country="England&Wales", index=1:n())

EW20 <- read_excel(temp, sheet="E&W 2020", range="S4:S54", col_names=FALSE) %>% 
  set_names("Vol") %>% 
  mutate(Year=2020, Country="England&Wales", index=1:n())

data <- bind_rows(Scotland17, Scotland18, Scotland19, Scotland20, EW17, EW18, EW19, EW20) %>% 
  pivot_wider(names_from=c("Country", "Year"), names_sep="_", values_from="Vol") %>% 
  mutate(Cat1=c(NA_character_, "Vodka", "Blended Whisky", "Gin", "Cream Liqueurs", "Brandy", "White Rum",
       "Imported Whisky", "Liqueurs", "Malt Whisky", "Dark Rum", "Cognac", "Golden Rum",
       "Speciality Drinks", "Minor Spirits", "RTDs", "Fortified Wines", NA_character_, 
       "Table Wine", "Sparkling Wine", "Champagne", "British-Made Wine", "Low Alcohol Wine", "Perry",
       NA_character_, NA_character_, NA_character_, NA_character_, NA_character_,
       NA_character_, NA_character_, "Nab/Lab Lager", "Commodity Lager", "Standard Lager",
       "Premium Lager", "Super Lager", NA_character_, "Nab/Lab Ale", "Commodity Ale",
       "Standard Ale", "Premium Ale", "Super Ale", NA_character_, "Commodity Stout",
       "Standard Stout", "Premium Stout", "Super Stout", NA_character_, "Strong Cider",
       "Regular Cider", NA_character_),
Cat2=c(NA_character_, "Vodka", "Blended Whisky", "Gin", "Cream Liqueurs", "Brandy", "White Rum",
       "Imported Whisky", "Liqueurs", "Malt Whisky", "Dark Rum", "Cognac", "Golden Rum",
       "Speciality Drinks", "Minor Spirits", "RTDs", "Fortified Wines", NA_character_, 
       "Table Wine", "Sparkling Wine", "Champagne", "British-Made Wine", "Low Alcohol Wine", "Perry",
       NA_character_, "Nab/Lab Beer", "Commodity Beer", "Standard Beer", "Premium Beer",
       "Superstrength Beer", NA_character_, NA_character_, NA_character_, NA_character_,
       NA_character_, NA_character_, NA_character_, NA_character_, NA_character_,
       NA_character_, NA_character_, NA_character_, NA_character_, NA_character_,
       NA_character_, NA_character_, NA_character_, NA_character_, "Strong Cider",
       "Regular Cider", NA_character_),
Cat3=c(NA_character_, "Vodka", "Blended Whisky", "Gin", "Cream Liqueurs", "Brandy", "White Rum",
       "Imported Whisky", "Liqueurs", "Malt Whisky", "Dark Rum", "Cognac", "Golden Rum",
       "Speciality Drinks", "Minor Spirits", "RTDs", "Fortified Wines", NA_character_, 
       "Table Wine", "Sparkling Wine", "Champagne", "British-Made Wine", "Low Alcohol Wine", "Perry",
       NA_character_, NA_character_, NA_character_, NA_character_, NA_character_,
       NA_character_, "Lager", NA_character_, NA_character_, NA_character_,
       NA_character_, NA_character_, "Ale", NA_character_, NA_character_,
       NA_character_, NA_character_, NA_character_, "Stout", NA_character_,
       NA_character_, NA_character_, NA_character_, NA_character_, "Strong Cider",
       "Regular Cider", NA_character_),
Cat4=c("Spirits", NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_,
       NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_,
       NA_character_, NA_character_, "RTDs", "Fortified Wines", "Wine", 
       NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, "Perry",
       "Beer", NA_character_, NA_character_, NA_character_, NA_character_,
       NA_character_, NA_character_, NA_character_, NA_character_, NA_character_,
       NA_character_, NA_character_, NA_character_, NA_character_, NA_character_,
       NA_character_, NA_character_, NA_character_, NA_character_, NA_character_,
       NA_character_, NA_character_, NA_character_, "Cider", NA_character_,
       NA_character_, NA_character_),
Cat5=c(NA_character_, rep("Spirits", times=14), "RTDs", "Fortified Wines", NA_character_,
       rep("Wine", times=5), "Cider/Perry", NA_character_, rep("Beer", times=5), 
       rep(NA_character_, times=18), "Cider/Perry", "Cider/Perry", NA_character_)) %>% 
  pivot_longer(names_to=c("Country", "Year"), names_sep="_",cols=c(2:9), values_to="Vol")

data <- data %>% group_by(Year, Cat1, Cat2, Cat3, Cat4, Cat5) %>% 
  summarise(Vol=sum(Vol)) %>% 
  mutate(Country="GB") %>% 
  ungroup() %>% 
  bind_rows(data) %>% 
  select(-index)

spiritsdata <- data %>% filter(Cat5=="Spirits") %>% 
  mutate(Cat1=if_else(Cat1 %in% c("Speciality Drinks", "Minor Spirits"), "Other", Cat1)) %>% 
  group_by(Country, Year, Cat1) %>% 
  summarise(Vol=sum(Vol)) %>% 
  ungroup() %>% 
  mutate(cat=case_when(
    Cat1 %in% c("Blended Whisky", "Imported Whisky", "Malt Whisky") ~ "Whisky",
    Cat1 %in% c("Dark Rum", "White Rum", "Golden Rum") ~ "Rum",
    Cat1 %in% c("Brandy", "Cognac") ~ "Brandy",
    Cat1=="Gin" ~ "Gin",
    Cat1=="Vodka" ~ "Vodka",
    TRUE ~ "Other Spirits"),
    cat=factor(cat, levels=c("Other Spirits", "Brandy", "Rum", "Gin", "Vodka", "Whisky")),
    Cat1=factor(Cat1, levels=c("Blended Whisky", "Imported Whisky", "Malt Whisky", "Vodka", 
                               "Gin", "White Rum", "Golden Rum", "Dark Rum", "Brandy", "Cognac",
                               "Liqueurs", "Cream Liqueurs", "Other")),
    VolPerAdult=Vol/(4546419+48388588),
    ABV=case_when(
      Cat1 %in% c("Blended Whisky", "Imported Whisky", "Malt Whisky", "Dark Rum") ~ 0.4,
      Cat1 %in% c("Vodka", "White Rum", "Brandy", "Cognac") ~ 0.375,
      Cat1=="Golden Rum" ~ 0.35,
      Cat1 %in% c("Cream Liqueurs", "Liqueurs") ~ 0.17,
      TRUE ~ 0.375),
    ProdVolPerAdult=VolPerAdult/ABV)
      

agg_tiff("Outputs/MESASSpiritsSales.tiff", units="in", width=9, height=7, res=500)
spiritsdata %>% filter(Country=="GB" & Year==2020) %>% 
  ggplot(aes(x=VolPerAdult, y=cat, fill=Cat1))+
  geom_col(aes(alpha=Cat1), show.legend=FALSE)+
  geom_text(aes(label=Cat1), size=rel(2.5), position=position_stack(vjust=0.5), show.legend=FALSE)+
  scale_x_continuous(name="Litres of pure alcohol sold per adult")+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("#EE7733", "#EE7733", "#EE7733", "#0077BB", "#33BBEE", "#EE337F",
                             "#EE337F", "#EE337F", "#CC3311", "#CC3311", "#009988", "#009988",
                             "#009988"))+
  scale_alpha_manual(values=c(1,0.6,0.4,1,1,1,0.6,0.4,1,0.5,1,0.6,0.4))+
  theme_custom()+
  labs(title="Vodka was the biggest selling spirit in Great Britain in 2020",
       subtitle="Litre of pure alcohol sold per adult in shops in Great Britain",
       caption="Data from Nielsen via Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/MESASSpiritsSalesProdVol.tiff", units="in", width=9, height=7, res=500)
spiritsdata %>% filter(Country=="GB" & Year==2020) %>% 
  ggplot(aes(x=ProdVolPerAdult, y=cat, fill=Cat1))+
  geom_col(aes(alpha=Cat1), show.legend=FALSE)+
  geom_text(aes(label=Cat1), size=rel(2.5), position=position_stack(vjust=0.5), show.legend=FALSE)+
  scale_x_continuous(name="Litres of product sold per adult")+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("#EE7733", "#EE7733", "#EE7733", "#0077BB", "#33BBEE", "#EE337F",
                             "#EE337F", "#EE337F", "#CC3311", "#CC3311", "#009988", "#009988",
                             "#009988"))+
  scale_alpha_manual(values=c(1,0.6,0.4,1,1,1,0.6,0.4,1,0.5,1,0.6,0.4))+
  theme_custom()+
  labs(title="Vodka was the biggest selling spirit in Great Britain in 2020",
       subtitle="Litre of product sold per adult in shops in Great Britain",
       caption="Data from Nielsen via Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/MESASSpiritsSalesTime.tiff", units="in", width=9, height=7, res=500)
spiritsdata %>% filter(Country=="GB") %>%
  group_by(cat, Year) %>% 
  summarise(VolPerAdult=sum(VolPerAdult)) %>% 
  ungroup() %>% 
  mutate(cat=factor(cat, levels=c("Vodka", "Whisky",  "Gin", "Rum", "Other Spirits","Brandy"))) %>% 
ggplot(aes(x=Year, y=VolPerAdult, colour=cat, group=cat))+
  geom_line()+
  scale_y_continuous(name="Litres of pure alcohol sold per adult", limits=c(0,NA))+
  scale_colour_paletteer_d("khroma::vibrant", name="")+
  theme_custom()+
  labs(title="The gap between Vodka and Whisky sales has narrowed",
       subtitle="Litre of pure alcohol sold as spirits per adult in shops in Great Britain",
       caption="Data from Nielsen via Public Health Scotland | Plot by @VictimOfMaths")

dev.off()
