rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(extrafont)
library(ragg)
library(ggtext)

options(scipen=99999)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}


#Read in data
url <- "https://www.publichealthscotland.scot/media/7670/mup-price-distribution-appendix-a-litres-of-pure-alcohol-per-adult.xlsx"
temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

rawdata1 <- read_excel(temp, sheet="Scotland", range="C4:X28") %>% 
  mutate(country="Scotland", period=rep(c(1:3), times=8),
         drinkcat=rep(c("Total", "Beer", "Spirits", "Wine", "Cider", 
                        "Fortified Wine", "RTDs", "Perry"), each=3),
         `45-50`=`45-46`+`46-47`+`47-48`+`48-49`+`49-50`) %>%
  select(-c(`45-46`,`46-47`,`47-48`,`48-49`,`49-50`)) %>% 
  rename(Total=`...22`) %>% 
  gather(price, alcvol, c(1:17, 21))

rawdata2 <- read_excel(temp, sheet="England & Wales", range="C4:X28") %>% 
  mutate(country="England & Wales", period=rep(c(1:3), times=8),
         drinkcat=rep(c("Total", "Beer", "Spirits", "Wine", "Cider", 
                        "Fortified Wine", "RTDs", "Perry"), each=3),
         `45-50`=`45-46`+`46-47`+`47-48`+`48-49`+`49-50`) %>%
  select(-c(`45-46`,`46-47`,`47-48`,`48-49`,`49-50`)) %>% 
  rename(Total=`...22`) %>% 
  gather(price, alcvol, c(1:17, 21))

data <- bind_rows(rawdata1, rawdata2) %>% 
  mutate(midprice=case_when(
    price=="<10" ~ 7.5, price=="10-15" ~ 12.5, price=="15-20" ~ 17.5,
    price=="20-25" ~ 22.5, price=="25-30" ~ 27.5, price=="30-35" ~ 32.5,
    price=="35-40" ~ 37.5, price=="40-45" ~ 42.5, price=="45-50" ~ 47.5,
    price=="50-55" ~ 52.5, price=="55-60" ~ 57.5,
    price=="60-65" ~ 62.5, price=="65-70" ~ 67.5, price=="70-75" ~ 72.5,
    price=="75-80" ~ 77.5, price=="80-85" ~ 82.5, price=="85+" ~ 87.5),
    bevtype=case_when(
      drinkcat %in% c("Cider", "Perry") ~ "Cider/Perry",
      drinkcat %in% c("Fortified Wine", "Wine") ~ "Wine",
      drinkcat %in% c("Spirits", "RTDs") ~ "Spirits",
      TRUE ~ drinkcat),
    time=case_when(
      period==1 ~ "May 16 - Apr 17",
      period==2 ~ "Before MUP",
      TRUE ~ "After MUP")) %>% 
  group_by(time, period, price, midprice, bevtype, country) %>% 
  summarise(alcvol=sum(alcvol)) %>% 
  ungroup() %>% 
  mutate(bevtype=factor(bevtype, levels=c("Beer", "Cider/Perry", "Wine", "Spirits",
                                          "Total")),
         time=factor(time, levels=c("May 16 - Apr 17", "Before MUP",
                                    "After MUP")))

agg_tiff("Outputs/AlcPricesMUP1.tiff", units="in", width=8, height=7, res=800)
ggplot(data %>% filter(bevtype!="Total" & period>1), 
       aes(x=midprice, y=alcvol, fill=bevtype))+
  geom_col(position="stack")+
  scale_x_continuous(breaks=seq(10,80,10), 
                     name="Price per unit of alcohol (pence")+
  scale_y_continuous(name="Litres of pure alcohol sold per adult")+
  scale_fill_manual(values=c("#ffc000", "#00b050", "#7030a0", "#00b0f0"),
                    name="")+
  facet_grid(time~country)+
  theme_custom()+
  theme(legend.position="top")+
  labs(title="The introduction of Minimum Unit Pricing has removed cheap alcohol\nfrom the market in Scotland",
       subtitle="Litres of pure alcohol sold in shops per adult by drink type and price per unit of alcohol in Scotland compared to\nEngland & Wales in the 12 months before and after Scotland introduced MUP on 1st May 2018",
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/AlcPricesMUP2.tiff", units="in", width=8, height=7, res=800)
ggplot(data %>% filter(bevtype!="Total" & period>1),
       aes(x=midprice, y=alcvol, fill=time))+
  geom_area(position="identity", alpha=0.5, show.legend=FALSE)+
  scale_x_continuous(breaks=seq(10,80,10), 
                     name="Price per unit of alcohol (pence")+
  scale_y_continuous(name="Litres of pure alcohol sold per adult",
                     limits=c(0,1.5))+
  scale_fill_manual(values=c("#FFB703", "#219EBC"), name="")+
  facet_grid(bevtype~country)+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="Cider prices have seen the biggest change in Scotland",
       subtitle="Litres of pure alcohol sold in shops per adult by drink type and price per unit of alcohol in Scotland compared to<br>England & Wales in the 12 months <span style='color:#FFB703;'>before</span> and  <span style='color:#219EBC;'>after</span> Scotland introduced MUP on 1st May 2018",
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/AlcPricesMUP3.tiff", units="in", width=8, height=5, res=800)
ggplot(data %>% filter(bevtype=="Total" & period>1),
       aes(x=midprice, y=alcvol, fill=time))+
  geom_area(position="identity", alpha=0.5, show.legend=FALSE)+
  scale_x_continuous(breaks=seq(10,80,10), 
                     name="Price per unit of alcohol (pence)")+
  scale_y_continuous(name="Litres of pure alcohol sold per adult",
                     limits=c(0,4))+
  scale_fill_manual(values=c("#FFB703", "#219EBC"), name="")+
  facet_grid(~country)+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="The introduction of Minimum Unit Pricing has removed cheap alcohol\nfrom the market in Scotland",
       subtitle="Litres of pure alcohol sold in shops per adult by price paid per unit of alcohol in Scotland compared to<br>England & Wales in the 12 months <span style='color:#FFB703;'>before</span> and  <span style='color:#219EBC;'>after</span> Scotland introduced MUP on 1st May 2018",
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()