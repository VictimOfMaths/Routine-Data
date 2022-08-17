rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(lubridate)
library(scales)
library(paletteer)
library(ggrepel)
library(extrafont)
library(ragg)
library(ggtext)

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

#Download inflation data from ONS
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/cpihconsistentinflationrateestimatesforukhouseholdgroupsdemocraticweighting/current/democraticreferencetables2022q2.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

#By equivalised income deciles
deciles <- read_excel(temp, sheet="Table 3", range="B5:L203") %>%
  gather(Decile, Inflation, c(2:11)) %>% 
  mutate(Period=as.Date(Period),
         Decile=as.factor(as.numeric(substr(Decile, 8,9))))

decile_labels <- deciles %>% 
  filter(Period==max(Period), Decile %in% c(1,10)) %>% 
  mutate(labels=c("Lowest incomes", "Highest incomes"))

agg_tiff("Outputs/ONSInflationxDeciles.tiff", units="in", width=8, height=6, res=500)
ggplot(deciles, aes(x=Period, y=Inflation/100, colour=Decile))+
  geom_line()+
  geom_text_repel(data=decile_labels ,aes(label=labels),
                  show.legend=FALSE, family="Lato", xlim=(c(as.Date("2022-06-01"), NA)), segment.color = NA)+
  scale_x_date(limits=c(as.Date("2020-01-01"), as.Date("2022-11-01")), name="")+
  scale_y_continuous(name="12-month inflation rate", labels=label_percent(accuracy=1),
                     position="right")+
  scale_colour_paletteer_d("dichromat::BrowntoBlue_10")+
  theme_custom()+
  theme(legend.position="none", axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.major.y=element_line(colour="Grey90"),
        plot.title=element_markdown(colour="Grey40"))+
  labs(title="The cost of living crisis is hitting <span style='color:#543005;'>lower income households</span> hardest",
       subtitle="Annual percentage growth in CPIH inflation by deciles of equivalised household disposable income",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

#By housing tenure
tenure <- read_excel(temp, sheet="Table 18", range="B5:E203") %>% 
  gather(Tenure, Inflation, c(2:4)) %>% 
  mutate(Period=as.Date(Period))

tenure_labels <- tenure %>% 
  filter(Period==max(Period)) %>% 
  mutate(labels=if_else(Tenure=="Subsidised", "Subsidised renters", Tenure))

agg_tiff("Outputs/ONSInflationxTenure.tiff", units="in", width=8, height=6, res=500)
ggplot(tenure, aes(x=Period, y=Inflation/100, colour=Tenure))+
  geom_line()+
  geom_text_repel(data=tenure_labels , aes(label=labels),
                  show.legend=FALSE, family="Lato", xlim=(c(as.Date("2022-06-01"), NA)), segment.color = NA)+
  scale_x_date(limits=c(as.Date("2020-01-01"), as.Date("2022-12-01")), name="")+
  scale_y_continuous(name="12-month inflation rate", labels=label_percent(accuracy=1),
                     position="right")+
  scale_colour_paletteer_d("tidyquant::tq_light")+
  theme_custom()+
  theme(legend.position="none", axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.major.y=element_line(colour="Grey90"),
        plot.title=element_markdown())+
  labs(title="The cost of living crisis is hitting <span style='color:#18BC9C;'>subsidised renters</span> hardest",
       subtitle="Annual percentage growth in CPIH inflation by household tenure",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

#Expenditure shares
#Income deciles
decile_exp <- read_excel(temp, sheet="Table 2", range="B6:O270") %>% 
  fill(Year) %>% 
  na.omit() %>% 
  gather(Category, Share, c(3:14)) %>% 
  mutate(Decile=as.factor(as.numeric(substr(Decile, 8,9))),
         Category=gsub(".*\\. ", "", Category),
         Share=Share/1000,
         Date=case_when(
           str_length(Year)==4 ~ as.Date(paste0(Year, "-01-01")),
           str_length(Year)==8 ~ as.Date(paste0(substr(Year, 1, 4), "-01-16")),
           TRUE ~ as.Date(paste0(substr(Year, 1, 4), "-02-16"))))

ggplot(decile_exp %>% filter(Date>as.Date("2015-01-01")), aes(x=Date, y=Share, fill=Category))+
  geom_area()+
  scale_y_continuous(name="Proportion of expenditure", labels=label_percent(accuracy=1))+
  scale_fill_paletteer_d("ggthemes::Classic_Green_Orange_12")+
  facet_grid(~Decile)+
  theme_custom()

agg_tiff("Outputs/ONSExpPropxHiLow.tiff", units="in", width=10, height=7, res=500)
ggplot(decile_exp %>% filter(Date>as.Date("2015-01-01") & Decile %in% c(1,10)) %>% 
         mutate(cat=if_else(Decile==1, "Lowest income", "Highest income")),
       aes(x=Date, y=Share, fill=Category))+
  geom_area()+
  scale_y_continuous(name="Proportion of expenditure", labels=label_percent(accuracy=1))+
  scale_fill_paletteer_d("ggthemes::Classic_Green_Orange_12")+
  facet_grid(~cat)+
  theme_custom()+
  labs(title="The lowest income households are most at risk from big increases in energy prices",
       subtitle="Proportion of household expenditure for the highest and lowest deciles of equivalised disposable household income by COICOP category\nData was last updated in February 2022, so does not reflect more recent price changes.",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/ONSExpPropxDecile.tiff", units="in", width=10, height=10, res=500)
ggplot(decile_exp %>% filter(Date>as.Date("2018-01-01")), aes(x=Date, y=Share, colour=Decile))+
  geom_line()+
  scale_y_continuous(limits=c(0,NA), name="Proportion of expenditure", labels=label_percent(accuracy=1))+
  scale_colour_paletteer_d("dichromat::BrowntoBlue_10")+
  facet_wrap(~Category, scales="free_y")+
  theme_custom()

dev.off()

#Housing tenure
tenure_exp <- read_excel(temp, sheet="Table 17", range="B6:N83") %>% 
  na.omit() %>% 
  mutate(Tenure=rep(c("Subsidised renters", "Renters", "Owner-occupiers"), each=24)) %>% 
  gather(Category, Share, c(2:13)) %>% 
  rename("Year"="...1") %>% 
  mutate(Category=gsub(".*\\. ", "", Category),
         Share=Share/1000,
         Date=case_when(
           str_length(Year)==4 ~ as.Date(paste0(Year, "-01-01")),
           str_length(Year)==8 ~ as.Date(paste0(substr(Year, 1, 4), "-01-16")),
           TRUE ~ as.Date(paste0(substr(Year, 1, 4), "-02-16"))))

agg_tiff("Outputs/ONSExpPropxTenureArea.tiff", units="in", width=9, height=7, res=500)
ggplot(tenure_exp %>% filter(Date>as.Date("2015-01-01")),
       aes(x=Date, y=Share, fill=Category))+
  geom_area()+
  scale_y_continuous(name="Proportion of expenditure", labels=label_percent(accuracy=1))+
  scale_fill_paletteer_d("ggthemes::Classic_Green_Orange_12")+
  facet_grid(~Tenure)+
  theme_custom()+
  labs(title="Subsidised renters spend less of their outgoings on rent and more on food",
       subtitle="Proportion of household expenditure by COICOP category (used to derive CPIH weights)\nData was last updated in February 2022, so does not reflect more recent price changes.",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/ONSExpPropxTenure.tiff", units="in", width=10, height=10, res=500)
ggplot(tenure_exp %>% filter(Date>as.Date("2018-01-01")), aes(x=Date, y=Share, colour=Tenure))+
  geom_line()+
  scale_y_continuous(limits=c(0,NA), name="Proportion of expenditure", labels=label_percent(accuracy=1))+
  scale_colour_paletteer_d("ggthemes::Classic_Green_Orange_12")+
  facet_wrap(~Category, scales="free_y")+
  theme_custom()

dev.off()
