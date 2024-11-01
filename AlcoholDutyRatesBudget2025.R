rm(list=ls())

library(tidyverse)
library(readxl)
library(lubridate)
library(paletteer)
library(curl)
library(ragg)
library(ggtext)
library(ggrepel)
library(extrafont)
library(geomtextpath)
library(scales)

options(scipen=99999999)

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
          legend.title=element_text(colour="Grey20"),
          axis.line.x=element_blank(),
          panel.grid.major.y = element_line(colour="grey95"))
}

##############################################
#Current UK alcohol duty rates
dutyrates <- data.frame(ABV=seq(0,40, by=0.1)) %>% 
  mutate(Beer_Std_PreReform=case_when(
    ABV<=1.2 ~ NA_real_,
    ABV<=2.8 ~ 8.42*10/1000,
    ABV<=7.5 ~ 19.08*10/1000,
    ABV<=12.5 ~ 24.77*10/1000),
    Cider_Std_PreReform=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<6.9 ~ 40.38*10/(1000*ABV),
      ABV<=7.5 ~ 50.71*10/(1000*ABV),
      ABV<8.5 ~ 61.04*10/(1000*ABV)),
    Spirits_Std_PreReform=case_when(
      ABV<=1.2 ~ NA_real_,
      TRUE ~ 28.74*10/1000),
    Wine.Still_Std_PreReform=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<=4 ~ 91.68*10/(1000*ABV),
      ABV<=5.5 ~ 126.08*10/(1000*ABV),
      ABV<=15 ~ 297.57*10/(1000*ABV),
      ABV<=22 ~ 396.72*10/(1000*ABV)),
    Wine.Sparkling_Std_PreReform=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<=4 ~ 91.68*10/(1000*ABV),
      ABV<=5.5 ~ 126.08*10/(1000*ABV),
      ABV<=8.5 ~ 288.10*10/(1000*ABV),
      ABV<=15 ~ 381.15*10/(1000*ABV)),
    Beer_Std_PostReform=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<3.5 ~ 9.27*10/1000,
      ABV<8.5 ~ 21.01*10/1000,
      ABV<=22 ~ 28.50*10/1000,
      TRUE ~ 31.64*10/1000),
    Cider_Std_PostReform=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<3.5 ~ 9.27*10/1000,
      ABV<8.5 ~ 9.67*10/1000,
      ABV<=22 ~ 28.5*10/1000,
      TRUE ~ 31.64*10/1000),
    Wine_Std_PostReform=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<3.5 ~ 9.27*10/1000,
      ABV<8.5 ~ 24.77*10/1000,
      ABV<11.5 ~ 28.50*10/1000,
      ABV<=14.5 ~ (28.50*10/1000)*12.5/ABV, 
      ABV<=22 ~ 28.50*10/1000,
      TRUE ~ 31.64*10/1000),
    Spirits_Std_PostReform=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<3.5 ~ 9.27*10/1000,
      ABV<8.5 ~ 24.77*10/1000,
      ABV<=22 ~ 28.50*10/1000,
      TRUE ~ 31.64*10/1000),
    Beer_DR_PostReform=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<3.5 ~ 8.42*10/1000,
      ABV<8.5 ~ 19.08*10/1000,
      ABV<=22 ~ 28.50*10/1000,
      TRUE ~ 31.64*10/1000),
    Cider_DR_PostReform=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<3.5 ~ 8.42*10/1000,
      ABV<8.5 ~ 8.78*10/1000,
      ABV<=22 ~ 28.5*10/1000,
      TRUE ~ 31.64*10/1000),
    Wine_DR_PostReform=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<3.5 ~ 8.42*10/1000,
      ABV<8.5 ~ 19.08*10/1000,
      ABV<11.5 ~ 28.50*10/1000,
      ABV<=14.5 ~ (28.50*10/1000)*12.5/ABV, 
      ABV<=22 ~ 28.50*10/1000,
      TRUE ~ 31.64*10/1000),
    Beer_Std_PostBudget=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<3.5 ~ 9.61*10/1000,
      ABV<8.5 ~ 21.78*10/1000,
      ABV<=22 ~ 29.54*10/1000,
      TRUE ~ 32.79*10/1000),
    Cider_Std_PostBudget=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<3.5 ~ 9.61*10/1000,
      ABV<8.5 ~ 10.02*10/1000,
      ABV<=22 ~ 29.54*10/1000,
      TRUE ~ 32.79*10/1000),
    Wine_Std_PostBudget=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<3.5 ~ 9.61*10/1000,
      ABV<8.5 ~ 25.67*10/1000,
      ABV<=22 ~ 29.54*10/1000,
      TRUE ~ 32.79*10/1000),
    Spirits_Std_PostBudget=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<3.5 ~ 9.61*10/1000,
      ABV<8.5 ~ 25.67*10/1000,
      ABV<=22 ~ 29.54*10/1000,
      TRUE ~ 32.79*10/1000),
    Beer_DR_PostBudget=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<3.5 ~ 8.28*10/1000,
      ABV<8.5 ~ 18.76*10/1000,
      ABV<=22 ~ 29.54*10/1000,
      TRUE ~ 32.79*10/1000),
    Cider_DR_PostBudget=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<3.5 ~ 8.28*10/1000,
      ABV<8.5 ~ 8.63*10/1000,
      ABV<=22 ~ 29.54*10/1000,
      TRUE ~ 32.79*10/1000),
    Wine_DR_PostBudget=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<3.5 ~ 8.28*10/1000,
      ABV<8.5 ~ 18.76*10/1000,
      ABV<=22 ~ 29.54*10/1000,
      TRUE ~ 32.79*10/1000)) %>% 
  pivot_longer(c(2:ncol(.)), names_to=c("Drink", "Cat", "Period"), names_sep="_", values_to="DutyRate") 



agg_tiff("Outputs/HMRCUKDutyRates.tiff", units="in", width=8, height=6, res=500)
ggplot(dutyrates, aes(x=ABV/100, y=DutyRate, colour=Drink, linetype=Cat))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_line()+
  scale_x_continuous(labels=label_percent(accuracy=1), name="Alcoholic strength (ABV)", limits=c(0,0.25))+
  scale_y_continuous(limits=c(0,NA), labels=label_dollar(prefix="£"), name="Duty payable per unit of alcohol")+
  scale_colour_manual(values=c("#F7AA14", "#2CB11B", "#0099D5", "purple", "#FC6882", "#C70E7B"), name="",
                      labels=c("Beer", "Cider", "Spirits", "Wine (all)", "Sparkling Wine", "Still Wine"))+
  scale_linetype_manual(values=c(2,1), labels=c("Draught", "Standard"))+
  facet_wrap(~Period)+
  theme_custom()+
  theme(legend.position="top")+
  labs(title="The UK alcohol duty system is stupid",
       subtitle="Alcohol duty payable per unit of alcohol by alcoholic strength and product type",
       caption="Data from HMRC | Plot by @VictimOfMaths")

dev.off()

agg_png("Outputs/HMRCUKDutyRatesBudget25.png", units="in", width=8, height=6, res=500)
dutyrates %>% filter(Period!="PreReform") %>% 
  mutate(Period=if_else(Period=="PostReform", "Now", "From Feb 2025"),
         Period=factor(Period, levels=c("Now", "From Feb 2025"))) %>% 
  ggplot(aes(x=ABV/100, y=DutyRate, colour=Drink, linetype=Cat))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_line()+
  scale_x_continuous(labels=label_percent(accuracy=1), name="Alcoholic strength (ABV)", limits=c(0,0.25))+
  scale_y_continuous(limits=c(0,NA), labels=label_dollar(prefix="£"), name="Duty payable per unit of alcohol")+
  scale_colour_manual(values=c("#F7AA14", "#2CB11B", "#0099D5", "purple"), name="",
                      labels=c("Beer", "Cider", "Spirits", "Wine", "Sparkling Wine", "Still Wine"))+
  scale_linetype_manual(values=c(2,1), labels=c("Draught", "Standard"), name="")+
  facet_wrap(~Period)+
  theme_custom()+
  theme(legend.position="top")+
  labs(title="The impact of today's Budget on alcohol duties",
       subtitle="Alcohol duty payable per unit of alcohol by alcoholic strength and product type",
       caption="Data from HMRC | Plot by @VictimOfMaths")

dev.off()

#Historic data compiled from multiple sources including historic duty bulletins with assistance
#from Charles Barry. Any errors are mine and not his.
temp <- tempfile()
HistoricRatesurl <- "https://github.com/VictimOfMaths/Routine-Data/raw/refs/heads/master/Historic%20Alcohol%20Duty%20Rates.xlsx"
temp <- curl_download(url=HistoricRatesurl, destfile=temp, quiet=FALSE, mode="wb")

rawbeer <- read_excel(temp, sheet="Beer", range="A1:C49") %>% 
  mutate(Date=as.Date(Date)) %>% 
  set_names("Date", "Beer.Rate", "DraftBeer.Rate") %>% 
  mutate(Beer.Rate=if_else(Date<as.Date("1992-01-01"), Beer.Rate/4, Beer.Rate))

rawcider <- read_excel(temp, sheet="Cider", range="A1:C48")%>% 
  mutate(Date=as.Date(Date))%>% 
  set_names("Date", "Cider.Rate", "DraftCider.Rate")

rawwine<- read_excel(temp, sheet="Wine", range="A1:C48")%>% 
  mutate(Date=as.Date(Date))%>% 
  set_names("Date", "StillWine.Rate", "SparklingWine.Rate")

rawspirits <- read_excel(temp, sheet="Spirits", range="A1:B80")%>% 
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

agg_png("Outputs/HMRCDutyRateLong.png", units="in", width=8, height=6, res=600)
ggplot(data %>% filter(metric=="PPU" & Date>=as.Date("1979-01-01")))+ 
  geom_hline(yintercept=0, colour="grey30")+
  geom_line(aes(x=Date, y=values, colour=drink), show.legend=FALSE)+
  scale_x_date(name="", limits=c(as.Date("1979-01-01"), as.Date("2032-12-01")))+
  scale_y_continuous(name="Average duty rate payable per unit", breaks=c(0,0.1,0.2,0.3),
                     labels=c("0", "10p", "20p", "30p"))+
  scale_colour_manual(values=c("#F7AA14", "#2CB11B","#b35806", "#004529",  "#FC6882","#0099D5", "#C70E7B"), name="")+
  geom_text_repel(data=data %>% filter(metric=="PPU" & Date==max(Date)),
                  aes(x=Date, y=values, color = drink, label = labels),
                  family = "Lato", fontface = "bold", direction = "y", box.padding = 0.4, hjust=0,
                  xlim = c(as.Date("2025-04-01"), NA_Date_), show.legend=FALSE, segment.color = NA)+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="Grey90"), plot.margin = unit(c(1,1,1,1), "lines"),
        plot.subtitle = element_markdown())+
  labs(title="Alcohol duty rates (in cash terms) are as high as they've ever been",
       subtitle="Mean alcohol duty payable per unit of alcohol in before adjusting for inflation.<br>Since August 2023, <span style='color:#FC6882'>sparkling</span> and <span style='color:#C70E7B'>still wine</span> has been taxed at the same rate.<br>Data up to February 2025 based on announcements in the latest budget<br>",
       caption="Data from HMRC, IFS, BBPA & HMT | Plot by @VictimOfMaths")

dev.off()

agg_png("Outputs/HMRCDutyRateLongReal.png", units="in", width=8, height=6, res=600)
ggplot(data %>% filter(metric=="PPU" & Date>=as.Date("1979-01-01")))+ 
  geom_hline(yintercept=0, colour="grey30")+
  geom_line(aes(x=Date, y=values.adj, colour=drink), show.legend=FALSE)+
  scale_x_date(name="", limits=c(as.Date("1979-01-01"), as.Date("2032-12-01")))+
  scale_y_continuous(name="Average duty rate payable per unit", labels=c("0", "20p", "40p", "60p", "80p"))+
  scale_colour_manual(values=c("#F7AA14", "#2CB11B","#b35806", "#004529",  "#FC6882","#0099D5", "#C70E7B"), name="")+
  geom_text_repel(data=data %>% filter(metric=="PPU" & Date==max(Date)),
                  aes(x=Date, y=values, color = drink, label = labels),
                  family = "Lato", fontface = "bold", direction = "y", box.padding = 0.4, hjust=0,
                  xlim = c(as.Date("2025-02-01"), NA_Date_), show.legend=FALSE, segment.color = NA)+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="Grey90"), plot.margin = unit(c(1,1,1,1), "lines"),
        plot.subtitle = element_markdown())+
  labs(title="After adjusting for inflation, alcohol duties are at historically low levels",
       subtitle="Mean alcohol duty payable per unit of alcohol in adjusted to February 2025 prices using RPI inflation.<br>Since August 2023, <span style='color:#FC6882'>sparkling</span> and <span style='color:#C70E7B'>still wine</span> has been taxed at the same rate.<br>Data up to February 2025 based on announcements in the latest budget and OBR inflation forecasts<br>",
       caption="Data from HMRC, IFS, BBPA, HMT & OBR | Plot by @VictimOfMaths")

dev.off()

#Look only at changes since the coalition government scrapped the duty escalator in May 2012
#Calculate changes since 2010
Changes <- data %>% filter(metric=="PPU" & Date>=as.Date("2012-04-01")) %>% 
  group_by(Date) %>% 
  mutate(values.adj=case_when(
    is.na(values.adj) & drink=="DraftBeer" ~ values.adj[drink=="Beer"],
    is.na(values.adj) & drink=="DraftCider" ~ values.adj[drink=="Cider"],
    TRUE ~ values.adj)) %>% 
  ungroup() %>% 
  group_by(drink) %>% 
  mutate(Change=100*(values.adj-values.adj[Date==as.Date("2012-04-01")])/values.adj[Date==as.Date("2012-04-01")]) %>% 
  filter(Date==max(Date)) %>% 
  mutate(labels=paste0(labels, " (", round(Change, 1), "%)"))

agg_png("Outputs/HMRCDutyRateShortReal.png", units="in", width=8, height=6, res=600)
ggplot(data %>% filter(metric=="PPU" & Date>=as.Date("2012-04-01")))+ 
  geom_hline(yintercept=0, colour="grey30")+
  geom_line(aes(x=Date, y=values.adj, colour=drink), show.legend=FALSE)+
  scale_x_date(name="", limits=c(as.Date("2012-04-01"), as.Date("2028-12-01")))+
  scale_y_continuous(name="Average duty rate payable per unit", labels=c("0", "10p", "20p", "30p", "40p", ""))+
  scale_colour_manual(values=c("#F7AA14", "#2CB11B","#b35806", "#004529",  "#FC6882","#0099D5", "#C70E7B"), name="")+
  geom_text_repel(data=Changes,
                  aes(x=Date, y=values, color = drink, label = labels),
                  family = "Lato", fontface = "bold", direction = "y", box.padding = 0.4, hjust=0,
                  xlim = c(as.Date("2025-02-01"), NA_Date_), show.legend=FALSE, segment.color = NA)+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="Grey90"), plot.margin = unit(c(1,1,1,1), "lines"),
        plot.subtitle = element_markdown())+
  labs(title="Since 2012, alcohol duty rates have fallen substantially in real terms",
       subtitle="Mean alcohol duty payable per unit of alcohol in adjusted to February 2025 prices using RPI inflation.<br>Figures in brackets represent percentage changes since April 2012.<br>Since August 2023, <span style='color:#FC6882'>sparkling</span> and <span style='color:#C70E7B'>still wine</span> has been taxed at the same rate.<br>Data up to February 2025 based on announcements in the latest budget and OBR inflation forecasts<br>",
       caption="Data from HMRC, IFS, BBPA, HMT & OBR | Plot by @VictimOfMaths")

dev.off()
