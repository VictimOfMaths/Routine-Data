rm(list=ls())

library(tidyverse)
library(readxl)
library(lubridate)
library(paletteer)
library(curl)
library(ragg)
library(ggtext)
library(ggrepel)
library(scales)
library(extrafont)

#Set common font for all plots
font <- "Lato"

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          strip.clip="off",
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

#Historic data compiled from multiple sources including historic duty bulletins with assistance
#from Charles Barry. Any errors are mine and not his.
rawbeer <- read_excel("U:/Historic Alcohol Duty Rates.xlsx", sheet="Beer", range="A1:C49") %>% 
  mutate(Date=as.Date(Date)) %>% 
  set_names("Date", "Beer.Rate", "DraftBeer.Rate") %>% 
  mutate(Beer.Rate=if_else(Date<as.Date("1992-01-01"), Beer.Rate/4, Beer.Rate))

rawcider <- read_excel("U:/Historic Alcohol Duty Rates.xlsx", sheet="Cider", range="A1:C48")%>% 
  mutate(Date=as.Date(Date))%>% 
  set_names("Date", "Cider.Rate", "DraftCider.Rate")

rawwine<- read_excel("U:/Historic Alcohol Duty Rates.xlsx", sheet="Wine", range="A1:C48")%>% 
  mutate(Date=as.Date(Date))%>% 
  set_names("Date", "StillWine.Rate", "SparklingWine.Rate")

rawspirits <- read_excel("U:/Historic Alcohol Duty Rates.xlsx", sheet="Spirits", range="A1:B80")%>% 
  mutate(Date=as.Date(Date))%>% 
  set_names("Date", "Spirit.Rate")

#Set up date framework
dates <- data.frame(Date=seq.Date(from=as.Date("1950-04-19"), to=as.Date("2025-03-01"),
                                  by="days"))

WineABV <- 0.125
CiderABV <- 0.05

#Merge in rates
data <- merge(dates, rawbeer, all.x=TRUE) %>% 
  merge(rawcider, all.x=TRUE) %>% 
  merge(rawwine, all.x=TRUE) %>% 
  merge(rawspirits, all.x=TRUE) %>% 
  fill(c(Beer.Rate, DraftBeer.Rate, Cider.Rate, DraftCider.Rate, StillWine.Rate, SparklingWine.Rate, Spirit.Rate)) %>%
  #Collapse to monthly data
  mutate(year=year(Date), month=month(Date)) %>% 
  group_by(year, month) %>% 
  summarise(Beer.Rate=mean(Beer.Rate, na.rm=TRUE),
            DraftBeer.Rate=mean(DraftBeer.Rate, na.rm=TRUE),
            Cider.Rate=mean(Cider.Rate, na.rm=TRUE),
            DraftCider.Rate=mean(DraftCider.Rate, na.rm=TRUE),
            StillWine.Rate=mean(StillWine.Rate, na.rm=TRUE),
            SparklingWine.Rate=mean(SparklingWine.Rate, na.rm=TRUE),
            Spirit.Rate=mean(Spirit.Rate, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(Date=as.Date(paste0(year, "-", month, "-01"))) %>% 
  #Convert to rates per unit of alcohol
  mutate(Beer.PPU=Beer.Rate/100,
         DraftBeer.PPU=DraftBeer.Rate/100,
         Cider.PPU=if_else(Date>=as.Date("2023-08-01"), Cider.Rate/100, Cider.Rate/(CiderABV*10000)),
         DraftCider.PPU=DraftCider.Rate/100,
         StillWine.PPU=if_else(Date>=as.Date("2023-08-01"), StillWine.Rate/100, StillWine.Rate/(WineABV*10000)),
         SparklingWine.PPU=if_else(Date>=as.Date("2023-08-01"), SparklingWine.Rate/100, SparklingWine.Rate/(WineABV*10000)),
         Spirit.PPU=Spirit.Rate/100) %>% 
  select(-c(1,2)) %>% 
  #Reshape data
  pivot_longer(c(1:7, 9:15), names_to=c("drink", "metric"), names_sep="\\.", values_to="values")

#Bring in RPI data
temp <- tempfile()
url <- "https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/czeq/mm23&series=&fromMonth=01&fromYear=1950&toMonth=10&toYear=2024&frequency=months"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

RPIdata <- read.csv(temp)[-c(1:5),] 
colnames(RPIdata) <- c("Date", "RPI")

#Add in data to March 2025 based on 0.3% monthly RPI based on OBR forecasts https://obr.uk/docs/dlm_uploads/OBR_Economic_and_fiscal_outlook_Oct_2024.pdf
RPIdata <- bind_rows(RPIdata, data.frame(Date=c("2024 OCT", "2024 NOV", "2024 DEC", "2025 JAN", "2025 FEB", "2025 MAR"),
                                  RPI=rep("0.3", times=6)))

RPIdata <- RPIdata %>% 
  mutate(Date=as.Date(paste0(Date, " 1"), "%Y %b %d"),
         RPI=(as.numeric(as.character(RPI))+100)/100,
         index=cumprod(RPI),
         inflator=index[length(index)]/index)

#Merge into data and adjust
data <- data %>% 
  merge(RPIdata, all.x=TRUE) %>% 
  #Assume
  mutate(inflator=if_else(is.na(inflator), 1, inflator),
         values.adj=values*inflator,
         labels=case_when(drink=="StillWine" ~ "Still Wine",
                          drink=="SparklingWine" ~ "Sparkling Wine",
                          drink=="Spirit" ~ "Spirits",
                          drink=="DraftBeer" ~ "Draught Beer",
                          drink=="DraftCider" ~ "Draught Cider",
                          TRUE ~ drink))

agg_tiff("Outputs/HMRCDutyRateLong.tiff", units="in", width=8, height=6, res=600)
ggplot(data %>% filter(metric=="PPU" & Date>=as.Date("1979-01-01")))+ 
  geom_line(aes(x=Date, y=values, colour=drink), show.legend=FALSE)+
  scale_x_date(name="", limits=c(as.Date("1979-01-01"), as.Date("2032-12-01")))+
  scale_y_continuous(name="Average duty rate payable per unit", breaks=c(0,0.1,0.2,0.3),
                     labels=c("0", "10p", "20p", "30p"))+
  #scale_colour_manual(values=c("#F7AA14", "#2CB11B", "#FC6882","#0099D5", "#C70E7B"), name="")+
  geom_text_repel(data=data %>% filter(metric=="PPU" & Date==max(Date)),
                  aes(x=Date, y=values, color = drink, label = labels),
                  family = "Lato", fontface = "bold", direction = "y", box.padding = 0.4, hjust=0,
                  xlim = c(as.Date("2025-04-01"), NA_Date_), show.legend=FALSE, segment.color = NA)+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="Grey90"), plot.margin = unit(c(1,1,1,1), "lines"))+
  labs(title="Alcohol duty rates (in cash terms) are as high as they've ever been",
       subtitle="Mean alcohol duty payable per unit of alcohol in before adjusting for inflation",
       caption="Data from HMRC, IFS, BBPA & HMT | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/HMRCDutyRateLong.tiff", units="in", width=8, height=6, res=600)
ggplot(data %>% filter(metric=="PPU" & Date>=as.Date("1979-01-01")))+ 
  geom_line(aes(x=Date, y=values.adj, colour=drink), show.legend=FALSE)+
  scale_x_date(name="", limits=c(as.Date("1979-01-01"), as.Date("2032-12-01")))+
  scale_y_continuous(name="Average duty rate payable per unit", breaks=c(0,0.1,0.2,0.3),
                     labels=c("0", "10p", "20p", "30p"))+
  #scale_colour_manual(values=c("#F7AA14", "#2CB11B", "#FC6882","#0099D5", "#C70E7B"), name="")+
  geom_text_repel(data=data %>% filter(metric=="PPU" & Date==max(Date)),
                  aes(x=Date, y=values, color = drink, label = labels),
                  family = "Lato", fontface = "bold", direction = "y", box.padding = 0.4, hjust=0,
                  xlim = c(as.Date("2025-02-01"), NA_Date_), show.legend=FALSE, segment.color = NA)+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="Grey90"), plot.margin = unit(c(1,1,1,1), "lines"))+
  labs(title="Alcohol duty rates (in cash terms) are as high as they've ever been",
       subtitle="Mean alcohol duty payable per unit of alcohol in before adjusting for inflation",
       caption="Data from HMRC, IFS, BBPA & HMT | Plot by @VictimOfMaths")

dev.off()
