rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(scales)
library(extrafont)
library(ragg)
library(ggtext)
library(paletteer)
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

#Download ONS retail spending data for t'internet
temp <- tempfile()
inturl <- "https://www.ons.gov.uk/file?uri=%2fbusinessindustryandtrade%2fretailindustry%2fdatasets%2fretailsalesindexinternetsales%2fcurrent/internetsales.xlsx"
temp <- curl_download(url=inturl, destfile=temp, quiet=FALSE, mode="wb")

intsales <- read_excel(temp, sheet="IntValSA", range="A7:I178", col_names=FALSE) %>% 
  set_names("Date", "Total excl. fuel", "Food", "Non-food", "Non-specialist", "Clothes", 
            "Household goods", "Other", "Non-store") %>% 
  gather(Category, Spend, c(2:9)) %>% 
  mutate(Sector="Online")

#Download ONS retail spending data for t'internet
temp <- tempfile()
convurl <- "https://www.ons.gov.uk/file?uri=%2fbusinessindustryandtrade%2fretailindustry%2fdatasets%2fpoundsdatatotalretailsales%2fcurrent/poundsdata.xlsx"
temp <- curl_download(url=convurl, destfile=temp, quiet=FALSE, mode="wb")

convsales <- read_excel(temp, sheet="ValSAW", range="B8:N347", col_names=FALSE)[,-c(3,5)] %>% 
  set_names("Date", "Total", "Total excl. fuel", "Food", "Non-food", "Non-specialist",
            "Clothes", "Household goods", "Other", "Non-store", "Fuel") %>% 
  gather(Category, Spend, c(2:11)) %>% 
  mutate(Sector="Physical",
         Date=as.Date(paste(Date, "1"), format="%Y %b %d"),
         Spend=Spend/1000)
  
sales <- bind_rows(intsales, convsales)

ggplot(sales %>% filter(Category=="Total excl. fuel" & Date>as.Date("2018-01-01")), 
       aes(x=Date, y=Spend, fill=Sector)) +
  geom_area()+
  theme_custom()

agg_png("Outputs/ONSRetailSalesxSector.png", units="in", width=9, height=6, res=800)
ggplot(sales %>% filter(Category %in% c("Food", "Clothes") & Date>as.Date("2018-01-01")), 
       aes(x=Date, y=Spend, fill=Sector))+
  geom_area()+
  scale_y_continuous(name="Weekly spend (£m)")+
  scale_fill_paletteer_d("ggthemes::Color_Blind", name="")+
  facet_wrap(~Category)+
  theme_custom()+
  labs(title="Spending on clothing is at pre-pandemic levels, spending on food remains higher",
       subtitle="Seasonally-adjusted weekly retail sales for online and physical retailers in Great Britain",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()
  