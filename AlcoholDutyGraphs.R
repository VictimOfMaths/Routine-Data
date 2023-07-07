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
dates <- data.frame(Date=seq.Date(from=as.Date("1950-04-19"), to=as.Date("2023-05-31"),
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
url <- "https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/czeq/mm23&series=&fromMonth=01&fromYear=1950&toMonth=05&toYear=2023&frequency=months"
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

agg_tiff("Outputs/HMRCDutyRateLong.tiff", units="in", width=8, height=6, res=600)
ggplot(data %>% filter(metric=="PPU" & Date>=as.Date("1979-01-01")))+ 
  geom_line(aes(x=Date, y=values, colour=drink), show.legend=FALSE)+
  scale_x_date(name="", limits=c(as.Date("1979-01-01"), as.Date("2032-12-01")))+
  scale_y_continuous(name="Average duty rate payable per unit", breaks=c(0,0.1,0.2,0.3),
                     labels=c("0", "10p", "20p", "30p"))+
  scale_colour_manual(values=c("#F7AA14", "#2CB11B", "#FC6882","#0099D5", "#C70E7B"), name="")+
  geom_text_repel(data=data %>% filter(metric=="PPU" & Date==max(Date)),
                  aes(x=Date, y=values, color = drink, label = labels),
                  family = "Lato", fontface = "bold", direction = "y", box.padding = 0.4, hjust=0,
                  xlim = c(as.Date("2023-04-01"), NA_Date_), show.legend=FALSE, segment.color = NA)+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="Grey90"), plot.margin = unit(c(1,1,1,1), "lines"))+
  labs(title="Alcohol duty rates (in cash terms) are as high as they've ever been",
       subtitle="Mean alcohol duty payable per unit of alcohol in before adjusting for inflation",
       caption="Data from HMRC, IFS, BBPA & HMT | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/HMRCDutyRateLongReal.tiff", units="in", width=8, height=6, res=600)
ggplot(data %>% filter(metric=="PPU" & Date>=as.Date("1979-01-01")))+ 
  geom_line(aes(x=Date, y=values.adj, colour=drink), show.legend=FALSE)+
  scale_x_date(name="", limits=c(as.Date("1979-01-01"), as.Date("2032-12-01")))+
  scale_y_continuous(name="Average duty rate payable per unit", breaks=c(0,0.1,0.2,0.3, 0.4, 0.5, 0.6),
                     labels=c("0", "10p", "20p", "30p", "40p", "50p", "60p"), limits=c(0,NA))+
  scale_colour_manual(values=c("#F7AA14", "#2CB11B", "#FC6882","#0099D5", "#C70E7B"), name="")+
  geom_text_repel(data=data %>% filter(metric=="PPU" & Date==max(Date)),
                  aes(x=Date, y=values, color = drink, label = labels),
                  family = "Lato", fontface = "bold", direction = "y", box.padding = 0.4, hjust=0,
                  xlim = c(as.Date("2023-04-01"), NA_Date_), show.legend=FALSE, segment.color = NA)+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="Grey90"), plot.margin = unit(c(1,1,1,1), "lines"))+
  labs(title="Alcohol duty rates (in real terms) are at their lowest in 40+ years",
       subtitle="Mean alcohol duty payable per unit of alcohol in the UK, adjusted to May 2023 prices",
       caption="Data from HMRC, IFS, BBPA & HMT | Plot by Colin Angus")
dev.off()

budgets <- data %>% 
  filter(metric=="PPU") %>% 
  mutate(Year=year(Date)) %>% 
  group_by(Year, drink) %>% 
  summarise(start.cash=values[month(Date)==1 & day(Date)==1], 
            start.real=values.adj[month(Date)==1 & day(Date)==1],
            end.cash=values[month(Date)==12 & day(Date)==1], 
            end.real=values.adj[month(Date)==12 & day(Date)==1],
            .groups="drop") %>% 
  mutate(cashchange.abs=end.cash-start.cash,
         cashchange.rel=cashchange.abs/start.cash,
         realchange.abs=end.real-start.real,
         realchange.rel=realchange.abs/start.real)

agg_tiff("Outputs/HMRCDutyRatechangesxYear.tiff", units="in", width=8, height=6, res=600)
ggplot(budgets %>% filter(Year>=2012 & drink!="SparklingWine"), aes(x=Year, y=cashchange.rel, fill=drink))+
  geom_hline(yintercept=0, colour="Grey30")+
  geom_col(position="dodge")+
  scale_x_continuous(breaks=c(2012, 2014, 2016, 2018, 2020, 2022))+
  scale_y_continuous(name="Change in alcohol duty\n(cash terms)", labels=label_percent(accuracy=1))+
  scale_fill_manual(values=c("#F7AA14", "#2CB11B", "#0099D5", "#C70E7B"), name="")+
  theme_custom()+
  labs(title="Duty rates haven't changed much in a decade",
       subtitle="Absolute (cash-terms) changes in rates of alcohol duty since 2012",
       caption="Data from HMRC | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/HMRCDutyRatechangesxYearRealTerms.tiff", units="in", width=8, height=6, res=600)
ggplot(budgets %>% filter(Year>=2012 & drink!="SparklingWine"), 
       aes(x=Year, y=realchange.rel, fill=drink))+
  geom_hline(yintercept=0, colour="Grey30")+
  geom_col(position="dodge")+
  scale_x_continuous(breaks=c(2012, 2014, 2016, 2018, 2020, 2022))+
  scale_y_continuous(name="Real terms change in alcohol duty", labels=label_percent(accuracy=1))+
  scale_fill_manual(values=c("#F7AA14", "#2CB11B", "#0099D5", "#C70E7B"), name="",
                    labels=c("Beer", "Cider", "Spirits", "Wine"))+  
  scale_alpha_manual(values=c(0.5,1))+
  theme_custom()+
  labs(title="The effective level of alcohol taxes has fallen substantially since 2012",
       subtitle="Annual change in inflation adjusted (real-terms) alcohol duty rates in the UK",
       caption="Data from HMRC | Plot by @VictimOfMaths")

dev.off()

#Download ONS CPI data
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindices/current/mm23.csv"
temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

RPIdata <-read.csv(temp) %>% 
  set_names(slice(., 1)) %>% 
  slice_tail(., n=nrow(.)-1) %>% 
  filter(grepl("Q", CDID, fixed=TRUE)==TRUE) %>% 
  #Select indices we want D7CA, D7BT, D7DI, D7DH, D7DG
  select(CDID, D7CA, D7BT, D7DI, D7DH, D7DG) %>% 
  set_names("date", "Alcohol", "Overall", "Beer", "Wine", "Spirits")

#Also pull out monthly time series of more detailed on/off-trade series
Channeldata <- read.csv(temp) %>% 
  set_names(slice(., 1)) %>% 
  slice_tail(., n=nrow(.)-1) %>% 
  #Select indices we want 
  select(CDID, CHAW, DOBH, DOBI, DOBJ, DOBK, DOBL, DOBM) %>% 
  set_names("date", "AllItems_Overall", "Beer_Overall", "Beer_OnTrade", "Beer_OffTrade", 
            "Wine/Spirits_Overall", "Wine/Spirits_OnTrade", "Wine/Spirits_OffTrade") %>% 
  filter(nchar(date)>7) %>% 
  slice_tail(., n=nrow(.)-3) %>% 
  mutate(date=as.Date(paste(substr(date, 1, 4), substr(date, 6, 8), "01", 
                            sep="-"), "%Y-%b-%d")) %>% 
  filter(date>=as.Date("1987-01-01")) %>% 
  pivot_longer(c(2:8), names_sep="_", names_to=c("Product", "Channel"), values_to="RPI") %>% 
  mutate(RPI=as.numeric(RPI)) %>% 
  #Rebase to 2000
  group_by(Product, Channel) %>% 
  mutate(RPI_2000=RPI/RPI[date==as.Date("2000-01-01")]) %>% 
  ungroup() %>% 
  mutate(labels=paste(case_when(Channel=="OnTrade" ~ "On-Trade",
                                Channel=="OffTrade" ~ "Off-Trade",
                                TRUE ~ Channel), Product))

#Plot on/off-trade RPI data

agg_tiff("Outputs/AlcoholRPIxProductxChannel.tiff", units="in", width=8, height=6, res=600)
Channeldata %>% filter(Channel!="Overall") %>% 
  ggplot(aes(x=date, y=RPI_2000, colour=Product))+
  geom_line(aes(linetype=Channel), show.legend=FALSE)+
  geom_line(data=Channeldata %>% filter(Product=="AllItems"), colour="black",
            linetype=2)+
  geom_text_repel(data=Channeldata %>% filter(Channel!="Overall" & date==max(date)),
                  aes(label = labels),
                  family = "Lato", fontface = "bold", direction = "y", box.padding = 0.4, hjust=0,
                  xlim = c(as.Date("2023-06-01"), NA_Date_), show.legend=FALSE, segment.color = NA)+
  scale_x_date(name="", limits=c(as.Date("2000-01-01"), as.Date("2030-12-01")))+
  scale_y_continuous(name="Relative price compared to January 2000\n(log scale)",
                     trans="log10")+
  scale_colour_manual(values=c("#F7AA14", "#7030a0"))+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="grey90"))+
  labs(title="The price gap between pubs and supermarkets keeps on widening",
       subtitle="Product-specific inflation indices (RPI) for beer and wines/spirits, in the on-trade (pubs and bars) and off-trade (shops)",
       caption="Data from ONS | Plot by @VictimOfMaths")+
  annotate("text", x=as.Date("2019-12-31"), y=1.64, colour="black",
           label="All Item RPI", family="Lato", fontface="bold")+
  annotate("text", x=as.Date("2001-01-01"), y=2.07, colour="grey80",
           label="Prices double Jan 2000 level", family="Lato", hjust=0)+
  annotate("text", x=as.Date("2001-01-01"), y=0.515, colour="grey80",
           label="Prices half Jan 2000 level", family="Lato", hjust=0)
  
dev.off()  
  
  
#Download NRJR household income data
url2 <- "https://www.ons.gov.uk/generator?format=csv&uri=/economy/grossdomesticproductgdp/timeseries/nrjr/ukea"
temp <- curl_download(url=url2, destfile=temp, quiet=FALSE, mode="wb")

Incomedata <- read_csv(temp) %>% 
  slice_tail(., n=nrow(.)-7) %>% 
  set_names("date", "Incomes") %>% 
  filter(grepl("Q", date, fixed=TRUE)==TRUE)

#Combine
Affordability <- merge(RPIdata, Incomedata) %>% 
  filter(Alcohol!="") %>% 
  #rebase
  mutate(across(c(2:7), ~as.numeric(.x)),
         across(c(2:7), ~ .x*100/.x[date=="1988 Q1"])) %>% 
  #Calculate afforadbility using NHS England approach
  mutate(RAPI_all=Alcohol*100/Overall,
         RAPI_beer=Beer*100/Overall,
         RAPI_wine=Wine*100/Overall,
         RAPI_spirits=Spirits*100/Overall,
         Affordability_all=Incomes*100/RAPI_all,
         Affordability_beer=Incomes*100/RAPI_beer,
         Affordability_wine=Incomes*100/RAPI_wine,
         Affordability_spirits=Incomes*100/RAPI_spirits,
         #Faff about with dates
         Year=as.numeric(substr(date, 1, 4)),
         time=case_when(
           substr(date, 7, 8)=="1" ~ as.Date(paste(Year, "01-01", sep="-")),
           substr(date, 7, 8)=="2" ~ as.Date(paste(Year, "04-01", sep="-")),
           substr(date, 7, 8)=="3" ~ as.Date(paste(Year, "07-01", sep="-")),
           TRUE ~ as.Date(paste(Year, "10-01", sep="-"))))

Summarydata <- Affordability %>% 
  select(time, starts_with(c("RAPI", "Affordability"))) %>% 
  pivot_longer(c(2:ncol(.)), names_to=c("Metric", "Product"), names_sep="_", values_to="Value")

#Plot variables
Affordability %>% 
  select(c("time", "Alcohol", "Overall")) %>% 
  gather(Measure, Value, c(2,3)) %>% 
  ggplot(aes(x=time, y=Value, colour=Measure))+
  geom_line(show.legend=FALSE)+
  geom_hline(yintercept=100, colour="Grey80")+
  geom_text_repel(data=. %>% filter(time==max(time)),
                  aes(label = Measure),
                  family = "Lato", fontface = "bold", direction = "y", box.padding = 0.4, hjust=0,
                  xlim = c(as.Date("2023-02-01"), NA_Date_), show.legend=FALSE, segment.color = NA)+
  scale_x_date(name="", limits=c(as.Date("1988-01-01"), as.Date("2025-01-01")))+
  scale_y_continuous(name="CPI inflation\n(1988 Q1 = 100)")+
  scale_colour_paletteer_d("colorblindr::OkabeIto")+
  theme_custom()

agg_tiff("Outputs/AlcoholAffordability.tiff", units="in", width=9, height=6, res=600)
ggplot(Summarydata %>% filter(Metric=="Affordability"), aes(x=time, y=Value, colour=Product,
                                                            linetype=Product))+
  geom_hline(yintercept=100, colour="Grey70")+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Affordability index (Q1 1988=100")+
  scale_colour_manual(values=c("Black", "#ffc000", "#00b0f0", "#7030a0"),
                      labels=c("All alcohol", "Beer", "Spirits", "Wine"), name="")+
  scale_linetype_manual(values=c(2,1,1,1),
                        labels=c("All alcohol", "Beer", "Spirits", "Wine"), name="")+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="Grey90"))+
  labs(title="The affordability of alcohol fell slightly in early 2023, but remains high",
       subtitle="Alcohol affordability in the UK since 1988 (higher = more affordable). Affordability is calculated as\nthe ratio of household disposable income (adjusted) to the relative price of alcohol vs. overall CPI inflation",
       caption="Data from the ONS")

dev.off()

#Specific focus on the cost-of-living crisis and comparing alcohol to other goods
#Download ONS CPI data
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindices/current/mm23.csv"
temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

CPIdata <-read.csv(temp) %>% 
  set_names(slice(., 1)) %>% 
  slice_tail(., n=nrow(.)-6) %>% 
  #Select indices we want
  select(CDID, D7BT, D7D5:D7FR) %>% 
  set_names("date", "Overall", "Bread & Cereals", "Meat", "Fish", 
            "Milk, Cheese & Eggs", "Oils & Fats", "Fruit", "Vegetables", "Sugar & Sweets",
            "All Food", "Tea & Coffee", "Soft Drinks", "Spirits", "Wine", "Beer", "Clothes",
            "Clothing Accessories", "Cleaning Clothes", "DIY Goods", "Maintenance/Repair Services",
            "Sewerage", "Water", "Electricity", "Gas", "Liquid Fuels", "Solid Fuels", 
            "Furniture", "Carpets", "White Goods", "Appliance Repair", "Non-Durable Goods",
            "Domestic Services", "New Cars", "Second-Hand Cars", "Bikes/Motorbikes", "Car Spares",
            "Petrol", "Car Repairs", "Other Services", "Train Tickets", "Bus Tickets",
            "Plane Tickets", "Ferry Tickets", "Phones", "TV License", "Cameras", "Computers",
            "Health Insurance", "TV Repair", "Recording Media", "Garden Plants", "Pets",
            "Restaurants", "Canteens", "Hairdressing", "Appliances for Personal Care",
            "Contents Insurance", "Car Insurance", "Overall Goods", "Overall Services",
            "Medical Products", "Drugs", "Other Medical Equipment", "Out-Patient Services", 
            "Medical Services", "Dentistry", "Hospital Services", "Major Recreation Durables",
            "Games, Toys & Hobbies", "Camping Equipment", "Sporting Services", "Cultural Services",
            "Books, Newspapers & Stationary", "Books", "Newspapers", "Stationary",
            "Package Holidays", "Jewellery & Other Personal Effects", "Jewellery", "Other Personal Effects", 
            "Other General Services") %>% 
  filter(nchar(date)>7) %>% 
  mutate(date=as.Date(paste(substr(date, 1, 4), substr(date, 6, 8), "01", 
                            sep="-"), "%Y-%b-%d")) %>% 
  filter(date>=as.Date("1988-01-01")) %>% 
  gather(Product, CPI, c(2:ncol(.))) %>% 
  mutate(CPI=as.numeric(CPI)) %>% 
  #Rebase to Jan 2021
  group_by(Product) %>% 
  mutate(CPI_2021=CPI/CPI[date==as.Date("2021-01-01")]) %>% 
  ungroup()

agg_tiff("Outputs/CPIInflationxItem.tiff", units="in", width=9, height=7, res=800)
ggplot(CPIdata %>% filter(date>=as.Date("2021-01-01") &
                            Product %in% c("Bread & Cereals", "Meat", "Fish", "Overall",
                                           "Milk, Cheese & Eggs", "Oils & Fats", "Fruit", 
                                           "Vegetables", "Sugar & Sweets","Tea & Coffee", 
                                           "Soft Drinks", "Spirits", "Wine", "Beer")),
       aes(x=date, y=CPI_2021, colour=Product))+
  geom_hline(yintercept=1, colour="grey30")+
  geom_line(data=. %>% filter(Product!="Overall"), show.legend=FALSE)+
  geom_line(data=. %>% filter(Product=="Overall"), colour="black", linetype=2)+
  geom_text_repel(data=. %>% filter(date==max(date) & Product!="Overall"),
                  aes(label = Product), size=3,
                  family = "Lato", fontface = "bold", direction = "y", box.padding = 0.3, hjust=0,
                  xlim = c(as.Date("2023-05-10"), NA_Date_), show.legend=FALSE, segment.color = NA)+
  scale_x_date(name="", limits=c(NA_Date_, as.Date("2023-10-01")))+
  scale_y_continuous(trans="log", name="CPI inflation since January 2021", 
                     breaks=c(1, 1.2, 1.4), labels=c("0%", "+20%", "+40%"))+
  scale_colour_manual(values=c("#EF7C12","#FC6882","#54BCD1", "#F4B95A", "#009F3F", "#8FDA04", 
                               "tomato","#AF6125", "#007BC3","#B25D91", "#EFC7E6", "#F4E3C7", "#C70E7B"))+
  theme_custom()+
  labs(title="Prices of alcohol have risen much slower than other food and drink items",
       subtitle="UK CPI inflation for food and drink items since January 2021. The dashed black line represents overall inflation",
       caption="Data from ONS | Plot by @VictimOfMaths")+
  theme(panel.grid.major.y=element_line(colour="grey95"))
dev.off()

