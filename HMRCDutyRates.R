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

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Historic data compiled from multiple sources including historic duty bulletins with assistance
#from Charles Barry. Any errors are mine and not his.
rawbeer <- read_excel("Data/alcohol duty rates time series.xlsx", sheet="Beer", range="A1:B47") %>% 
  mutate(Date=as.Date(Date)) %>% 
  set_names("Date", "Beer.Rate") %>% 
  mutate(Beer.Rate=if_else(Date<as.Date("1992-01-01"), Beer.Rate/4, Beer.Rate))
rawcider <- read_excel("Data/alcohol duty rates time series.xlsx", sheet="Cider", range="A1:B46")%>% 
  mutate(Date=as.Date(Date))%>% 
  set_names("Date", "Cider.Rate")
rawwine<- read_excel("Data/alcohol duty rates time series.xlsx", sheet="Wine & Made-Wine", range="A1:I46")%>% 
  select(c(1,5,9)) %>% 
  mutate(Date=as.Date(Date))%>% 
  set_names("Date", "StillWine.Rate", "SparklingWine.Rate")
rawspirits <- read_excel("Data/alcohol duty rates time series.xlsx", sheet="Spirits", range="A1:B78")%>% 
  mutate(Date=as.Date(Date))%>% 
  set_names("Date", "Spirit.Rate")

#Set up date framework
dates <- data.frame(Date=seq.Date(from=as.Date("1950-04-19"), to=as.Date("2021-09-30"),
                                   by="days"))

WineABV <- 0.125
CiderABV <- 0.05

#Merge in rates
data <- merge(dates, rawbeer, all.x=TRUE) %>% 
  merge(rawcider, all.x=TRUE) %>% 
  merge(rawwine, all.x=TRUE) %>% 
  merge(rawspirits, all.x=TRUE) %>% 
  fill(c(Beer.Rate, Cider.Rate, StillWine.Rate, SparklingWine.Rate, Spirit.Rate)) %>%
  #Collapse to monthly data
  mutate(year=year(Date), month=month(Date)) %>% 
  group_by(year, month) %>% 
  summarise(Beer.Rate=mean(Beer.Rate, na.rm=TRUE),
            Cider.Rate=mean(Cider.Rate, na.rm=TRUE),
            StillWine.Rate=mean(StillWine.Rate, na.rm=TRUE),
            SparklingWine.Rate=mean(SparklingWine.Rate, na.rm=TRUE),
            Spirit.Rate=mean(Spirit.Rate, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(Date=as.Date(paste0(year, "-", month, "-01"))) %>% 
  #Convert to rates per unit of alcohol
  mutate(Beer.PPU=Beer.Rate/100,
         Cider.PPU=Cider.Rate/(CiderABV*10000),
         StillWine.PPU=StillWine.Rate/(WineABV*10000),
         SparklingWine.PPU=SparklingWine.Rate/(WineABV*10000),
         Spirit.PPU=Spirit.Rate/100) %>% 
  select(-c(1,2)) %>% 
  #Reshape data
  pivot_longer(c(1:5, 7:11), names_to=c("drink", "metric"), names_sep="\\.", values_to="values")

#Bring in RPI data
temp <- tempfile()
url <- "https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/czeq/mm23&series=&fromMonth=01&fromYear=1950&toMonth=09&toYear=2021&frequency=months"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

RPIdata <- read.csv(temp)[-c(1:5),] 
colnames(RPIdata) <- c("Date", "RPI")

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
                          TRUE ~ drink))

agg_tiff("Outputs/HMRCDutyRateLong.tiff", units="in", width=8, height=6, res=500)
ggplot(data %>% filter(metric=="PPU" & Date>=as.Date("1979-01-01")))+ 
  geom_line(aes(x=Date, y=values, colour=drink), show.legend=FALSE)+
  scale_x_date(name="", limits=c(as.Date("1979-01-01"), as.Date("2028-10-01")))+
  scale_y_continuous(name="Average duty rate payable per unit", breaks=c(0,0.1,0.2,0.3),
                     labels=c("0", "10p", "20p", "30p"))+
  scale_colour_manual(values=c("#F7AA14", "#2CB11B", "#FC6882","#0099D5", "#C70E7B"), name="")+
  geom_text_repel(data=data %>% filter(metric=="PPU" & Date==max(Date)),
    aes(x=Date, y=values, color = drink, label = labels),
    family = "Lato", fontface = "bold", direction = "y", box.padding = 0.4, hjust=0,
    xlim = c(as.Date("2022-02-01"), NA_Date_), show.legend=FALSE, segment.color = NA)+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="Grey90"), plot.margin = unit(c(1,1,1,1), "lines"))+
  labs(title="Alcohol duty rates are as high as they've ever been",
       subtitle="Mean alcohol duty payable per unit of alcohol in the UK before adjusting for inflation",
       caption="Data from HMRC, IFS, BBPA & HMT | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/HMRCDutyRateLongReal.tiff", units="in", width=8, height=6, res=500)
ggplot(data %>% filter(metric=="PPU" & Date>=as.Date("1979-01-01")))+ 
  geom_line(aes(x=Date, y=values.adj, colour=drink), show.legend=FALSE)+
  scale_x_date(name="", limits=c(as.Date("1979-01-01"), as.Date("2028-06-01")))+
  scale_y_continuous(name="Average duty rate payable per unit", breaks=c(0,0.1,0.2,0.3, 0.4, 0.5, 0.6),
                     labels=c("0", "10p", "20p", "30p", "40p", "50p", "60p"), limits=c(0,NA))+
  scale_colour_manual(values=c("#F7AA14", "#2CB11B", "#FC6882","#0099D5", "#C70E7B"), name="")+
  geom_text_repel(data=data %>% filter(metric=="PPU" & Date==max(Date)),
                  aes(x=Date, y=values, color = drink, label = labels),
                  family = "Lato", fontface = "bold", direction = "y", box.padding = 0.4, hjust=0,
                  xlim = c(as.Date("2022-02-01"), NA_Date_), show.legend=FALSE, segment.color = NA)+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="Grey90"), plot.margin = unit(c(1,1,1,1), "lines"))+
  labs(title="In real terms alcohol duty is at historically low rates",
       subtitle="Mean alcohol duty payable per unit of alcohol in the UK, adjusted to September 2021 prices",
       caption="Data from HMRC, IFS, BBPA & HMT | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/HMRCDutyRateShortRealHighlights.tiff", units="in", width=8, height=6, res=500)
ggplot(data %>% filter(metric=="PPU" & Date>=as.Date("2000-01-01")))+ 
  geom_rect(aes(xmin=as.Date("2008-01-01"), xmax=as.Date("2013-03-31"), ymin=0, ymax=0.4), fill="Grey80")+
  geom_line(aes(x=Date, y=values.adj, colour=drink), show.legend=FALSE)+
  scale_x_date(name="", limits=c(as.Date("2000-01-01"), as.Date("2028-06-01")))+
  scale_y_continuous(name="Average duty rate payable per unit", breaks=c(0,0.1,0.2,0.3),
                     labels=c("0", "10p", "20p", "30p"), limits=c(0,NA))+
  scale_colour_manual(values=c("#F7AA14", "#2CB11B", "#FC6882","#0099D5", "#C70E7B"), name="")+
  geom_text_repel(data=data %>% filter(metric=="PPU" & Date==max(Date)),
                  aes(x=Date, y=values, color = drink, label = labels),
                  family = "Lato", fontface = "bold", direction = "y", box.padding = 0.4, hjust=0,
                  xlim = c(as.Date("2022-02-01"), NA_Date_), show.legend=FALSE, segment.color = NA)+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="Grey90"), plot.margin = unit(c(1,1,1,1), "lines"))+
  labs(title="The 'alcohol duty escalator' increased real terms duties 2008-12",
       subtitle="Mean alcohol duty payable per unit of alcohol in the UK, adjusted to September 2021 prices",
       caption="Data from HMRC, IFS, BBPA & HMT | Plot by @VictimOfMaths")
dev.off()

##############################################
#Current UK alcohol duty rates
dutyrates <- data.frame(ABV=seq(0,40, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=1.2 ~ NA_real_,
    ABV<=2.8 ~ 8.42*10/1000,
    ABV<=7.5 ~ 19.08*10/1000,
    ABV<=12.5 ~ 24.77*10/1000),
    Cider=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<6.9 ~ 40.38*10/(1000*ABV),
      ABV<=7.5 ~ 50.71*10/(1000*ABV),
      ABV<8.5 ~ 61.04*10/(1000*ABV)),
    Spirits=case_when(
      ABV<=1.2 ~ NA_real_,
      TRUE ~ 28.74*10/1000),
    Wine.Still=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<=4 ~ 91.68*10/(1000*ABV),
      ABV<=5.5 ~ 126.08*10/(1000*ABV),
      ABV<=15 ~ 297.57*10/(1000*ABV),
      ABV<=22 ~ 396.72*10/(1000*ABV)),
    Wine.Sparkling=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<=4 ~ 91.68*10/(1000*ABV),
      ABV<=5.5 ~ 126.08*10/(1000*ABV),
      ABV<=8.5 ~ 288.10*10/(1000*ABV),
      ABV<=15 ~ 381.15*10/(1000*ABV))) %>% 
  gather(drink, rate, c(2:ncol(.)))

agg_tiff("Outputs/HMRCUKDutyRates.tiff", units="in", width=8, height=6, res=500)
ggplot(dutyrates, aes(x=ABV/100, y=rate, colour=drink))+
  geom_line()+
  scale_x_continuous(labels=label_percent(accuracy=1), name="Alcoholic strength (ABV)", limits=c(0,0.25))+
  scale_y_continuous(limits=c(0,NA), labels=label_dollar(prefix="£"), name="Duty payable per unit of alcohol")+
  scale_colour_manual(values=c("#F7AA14", "#2CB11B", "#0099D5","#FC6882", "#C70E7B"), name="",
                      labels=c("Beer", "Cider", "Spirits", "Sparkling Wine", "Still Wine"))+
  theme_custom()+
  theme(legend.position="top")+
  labs(title="The UK alcohol duty system is stupid",
       subtitle="Alcohol duty payable per unit of alcohol by alcoholic strength and product type",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/HMRCUKDutyRatesSpirits.tiff", units="in", width=8, height=6, res=500)
ggplot(dutyrates %>% filter(drink=="Spirits"), aes(x=ABV/100, y=rate))+
  geom_line(colour="#0099D5")+
  scale_x_continuous(labels=label_percent(accuracy=1), name="Alcoholic strength (ABV)", limits=c(0,0.25))+
  scale_y_continuous(limits=c(0,0.8), labels=label_dollar(prefix="£"), name="Duty payable per unit of alcohol")+
  #scale_colour_manual(values=c("#F7AA14", "#2CB11B", "#0099D5","#FC6882", "#C70E7B"), name="",
  #                    labels=c("Beer", "Cider", "Spirits", "Sparkling Wine", "Still Wine"))+
  theme_custom()+
  #theme(legend.position="top")+
  labs(title="Spirits are taxed by alcohol content",
       subtitle="Alcohol duty payable per unit in the UK of alcohol by alcoholic strength",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/HMRCUKDutyRatesBeer.tiff", units="in", width=8, height=6, res=500)
ggplot(dutyrates %>% filter(drink=="Beer"), aes(x=ABV/100, y=rate))+
  geom_line(colour="#F7AA14")+
  scale_x_continuous(labels=label_percent(accuracy=1), name="Alcoholic strength (ABV)", limits=c(0,0.25))+
  scale_y_continuous(limits=c(0,0.8), labels=label_dollar(prefix="£"), name="Duty payable per unit of alcohol")+
  #scale_colour_manual(values=c("#F7AA14", "#2CB11B", "#0099D5","#FC6882", "#C70E7B"), name="",
  #                    labels=c("Beer", "Cider", "Spirits", "Sparkling Wine", "Still Wine"))+
  theme_custom()+
  #theme(legend.position="top")+
  labs(title="Beer are taxed by alcohol content and strength",
       subtitle="Alcohol duty payable per unit in the UK of alcohol by alcoholic strength",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/HMRCUKDutyRatesWine.tiff", units="in", width=8, height=6, res=500)
ggplot(dutyrates %>% filter(drink=="Wine.Still" | drink=="Wine.Sparkling"), 
       aes(x=ABV/100, y=rate, colour=drink))+
  geom_line()+
  scale_x_continuous(labels=label_percent(accuracy=1), name="Alcoholic strength (ABV)", limits=c(0,0.25))+
  scale_y_continuous(limits=c(0,0.8), labels=label_dollar(prefix="£"), name="Duty payable per unit of alcohol")+
  scale_colour_manual(values=c("#FC6882", "#C70E7B"), name="",
                      labels=c("Sparkling Wine", "Still Wine"))+
  theme_custom()+
  theme(legend.position="top")+
  labs(title="Wine is taxed based on the volume of product",
       subtitle="Alcohol duty payable per unit in the UK of alcohol by alcoholic strength",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/HMRCUKDutyRatesCider.tiff", units="in", width=8, height=6, res=500)
ggplot(dutyrates %>% filter(drink=="Cider"), aes(x=ABV/100, y=rate))+
  geom_line(colour="#2CB11B")+
  scale_x_continuous(labels=label_percent(accuracy=1), name="Alcoholic strength (ABV)", limits=c(0,0.25))+
  scale_y_continuous(limits=c(0,0.8), labels=label_dollar(prefix="£"), name="Duty payable per unit of alcohol")+
  #scale_colour_manual(values=c("#F7AA14", "#2CB11B", "#0099D5","#FC6882", "#C70E7B"), name="",
  #                    labels=c("Beer", "Cider", "Spirits", "Sparkling Wine", "Still Wine"))+
  theme_custom()+
  #theme(legend.position="top")+
  labs(title="Cider is taxed based on the volume of product",
       subtitle="Alcohol duty payable per unit in the UK of alcohol by alcoholic strength",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

##############################
#Looking at different RPI series
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindices/current/mm23.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

RPIdata <- read.csv(temp) %>% 
  slice_tail(n=nrow(.)-523) %>% 
  #Select indices we want
  select(c(1,16,21,28, 3251, 3253, 3254, 3255, 3257, 3259)) %>% 
  set_names("date", "Alc.CPI", "NonAlc.CPI", "All.CPI", "Beer.RPI", "OnBeer.RPI", "OffBeer.RPI", "WineSpirits.RPI", 
            "OnWineSpirits.RPI", "OffWineSpirits.RPI") %>% 
  mutate(date=as.Date(paste0(date, " 1"), "%Y %b %d")) %>% 
  filter(date>=as.Date("1987-01-01")) %>% 
  pivot_longer(c(2:10), names_to=c("Metric", "Index"), names_sep="\\.", values_to="Value") %>% 
  mutate(Value=as.numeric(Value),
         labels=case_when(
           Metric=="Alc"~"Alcoholic drinks", 
           Metric=="NonAlc" ~ "Non-alcoholic drinks",
           Metric=="All" ~ "All Items", 
           Metric=="WineSpirits" ~ "Wine & Spirits",
           Metric=="OnBeer" ~ "On-trade beer",
           Metric=="OffBeer" ~ "Off-trade beer",
           Metric=="OnWineSpirits" ~ "On-trade wine & spirits",
           Metric=="OffWineSpirits" ~ "Off-trade wine & spirits",
           TRUE ~ Metric)) %>%   
  #Re-index CPI values to Jan 1988
  group_by(Metric) %>% 
  #mutate(Value2=Value/Value[date==as.Date("1988-01-01")])
  mutate(Value=if_else(Index=="CPI", Value*100/Value[date==as.Date("1988-01-01")], Value*100)) %>% 
  ungroup()

agg_tiff("Outputs/ONSAlcoholCPI.tiff", units="in", width=8, height=6, res=500)
ggplot(RPIdata %>% filter(Index=="CPI"), aes(x=date, y=Value, colour=Metric))+
  geom_line(show.legend=FALSE)+
  geom_hline(yintercept=100, colour="Grey80")+
  geom_text_repel(data=RPIdata %>% filter(Index=="CPI" & date==max(date)),
                  aes(label = labels),
                  family = "Lato", fontface = "bold", direction = "y", box.padding = 0.4, hjust=0,
                  xlim = c(as.Date("2022-02-01"), NA_Date_), show.legend=FALSE, segment.color = NA)+
  scale_x_date(name="", limits=c(as.Date("1988-01-01"), as.Date("2029-01-01")))+
  scale_y_continuous(name="CPI Inflation Jan '88 = 100")+
  scale_colour_paletteer_d("werpals::small_world")+
  theme_custom()+
  labs(title="Inflation on alcoholic drinks has been lower than on other products",
       subtitle="CPI inflation since January 1988",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()  

agg_tiff("Outputs/ONSAlcoholRPI.tiff", units="in", width=8, height=6, res=500)
ggplot(RPIdata %>% filter(Index=="RPI" & Metric %in% c("Beer", "WineSpirits")), 
       aes(x=date, y=Value/100, colour=Metric))+
  geom_line(show.legend=FALSE)+
  geom_hline(yintercept=100, colour="Grey80")+
  geom_text_repel(data=RPIdata %>% filter(Index=="RPI" & date==max(date) & Metric %in% c("Beer", "WineSpirits")),
                  aes(label = labels),
                  family = "Lato", fontface = "bold", direction = "y", box.padding = 0.4, hjust=0,
                  xlim = c(as.Date("2022-02-01"), NA_Date_), show.legend=FALSE, segment.color = NA)+
  scale_x_date(name="", limits=c(as.Date("1987-01-01"), as.Date("2027-01-01")))+
  scale_y_continuous(name="RPI Inflation Jan '87 = 100")+
  scale_colour_manual(values=c("#F7AA14", "#5D36B1"))+
  theme_custom()+
  labs(title="Wine and spirits prices have increased at a slower rate than beer prices",
       subtitle="RPI inflation since January 1987",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()  

agg_tiff("Outputs/ONSAlcoholRPIxChannel.tiff", units="in", width=8, height=6, res=500)
ggplot(RPIdata %>% filter(Index=="RPI" & !Metric %in% c("Beer", "WineSpirits")), 
       aes(x=date, y=Value/100, colour=Metric))+
  geom_line(show.legend=FALSE)+
  geom_hline(yintercept=100, colour="Grey80")+
  geom_text_repel(data=RPIdata %>% filter(Index=="RPI" & date==max(date) & !Metric %in% c("Beer", "WineSpirits")),
                  aes(label = labels),
                  family = "Lato", fontface = "bold", direction = "y", box.padding = 0.4, hjust=0,
                  xlim = c(as.Date("2022-02-01"), NA_Date_), show.legend=FALSE, segment.color = NA)+
  scale_x_date(name="", limits=c(as.Date("1987-01-01"), as.Date("2032-01-01")))+
  scale_y_continuous(name="RPI Inflation Jan '87 = 100")+
  scale_colour_manual(values=c("#F7AA14", "#795bba", "#F77914", "#331477"))+
  theme_custom()+
  labs(title="The price differential between alcohol bought in shops and pubs is growing",
       subtitle="RPI inflation since January 1987 for alcohol sold in the on-trade (pubs, bars, clubs and restaurants) and\nin the off-trade (shops)",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()  
