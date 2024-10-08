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
library(RcppRoll)
library(geomtextpath)
library(snakecase)

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
          axis.line.x=element_blank())
}

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
agg_tiff("Outputs/DisposableIncomes.tiff", units="in", width=9, height=6, res=600)
Affordability %>% 
  select(c("time", "Overall", "Incomes")) %>% 
  gather(Measure, Value, c(2,3)) %>%
  mutate(Measure=if_else(Measure=="Overall", "CPI prices", "Disposable incomes"),
         vjust=if_else(Measure=="CPI prices", -0.5, 1.5)) %>% 
  ggplot(aes(x=time, y=Value, colour=Measure, label=Measure, vjust=vjust))+
  geom_textline(show.legend=FALSE, hjust=0.71, straight=TRUE,)+
  geom_hline(yintercept=100, colour="Grey80")+
  scale_x_date(name="")+
  scale_y_continuous(name="Relative values\n(1988 Q1 = 100)")+
  scale_colour_paletteer_d("ggthemes::Classic_Green_Orange_6")+
  theme_custom()+
  labs(title="The UK has less money to spend and everything costs more",
       subtitle="Overall CPI prices and disposable household income relative to Q1 1988. Data up to June 2024.",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/AlcoholCPI.tiff", units="in", width=9, height=6, res=600)
Affordability %>% 
  select(c("time", "Alcohol", "Overall")) %>% 
  gather(Measure, Value, c(2,3)) %>% 
  ggplot(aes(x=time, y=Value, colour=Measure))+
  geom_line(show.legend=FALSE)+
  geom_hline(yintercept=100, colour="Grey80")+
  geom_text_repel(data=. %>% filter(time==max(time)),
                  aes(label = Measure),
                  family = "Lato", fontface = "bold", direction = "y", box.padding = 0.4, hjust=0,
                  xlim = c(as.Date("2024-08-01"), NA_Date_), show.legend=FALSE, segment.color = NA)+
  scale_x_date(name="", limits=c(as.Date("1988-01-01"), as.Date("2026-01-01")))+
  scale_y_continuous(name="CPI inflation\n(1988 Q1 = 100)")+
  scale_colour_paletteer_d("colorblindr::OkabeIto")+
  theme_custom()+
  labs(title="Alcohol prices are rising *much* slower than everything else",
       subtitle="CPI inflation for alcohol and for all products combined. Data up to June 2024.",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/AlcoholRelPrice.tiff", units="in", width=9, height=6, res=600)
ggplot(Summarydata %>% filter(Metric=="RAPI"), 
       aes(x=time, y=Value/100, colour=Product, linetype=Product))+
  geom_hline(yintercept=1, colour="Grey70")+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Relative price of alcohol\n(log scale)", 
                     trans="log")+
  scale_colour_manual(values=c("Black", "#ffc000", "#00b0f0", "#7030a0"),
                      labels=c("All alcohol", "Beer", "Spirits", "Wine"), name="")+
  scale_linetype_manual(values=c(2,1,1,1),
                        labels=c("All alcohol", "Beer", "Spirits", "Wine"), name="")+
  theme_custom()+
  labs(title="Relative to everything else, alcohol keeps getting cheaper",
       subtitle="Relative prices of alcohol and all other goods compared to their relative prices in Q1 1988.",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/AlcoholAffordability.tiff", units="in", width=9, height=6, res=600)
ggplot(Summarydata %>% filter(Metric=="Affordability"), aes(x=time, y=Value, colour=Product,
                                                            linetype=Product))+
  geom_hline(yintercept=100, colour="Grey70")+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Affordability index (Q1 1988=100", 
                     trans="log", breaks=c(100, 200, 300))+
  scale_colour_manual(values=c("Black", "#ffc000", "#00b0f0", "#7030a0"),
                      labels=c("All alcohol", "Beer", "Spirits", "Wine"), name="")+
  scale_linetype_manual(values=c(2,1,1,1),
                        labels=c("All alcohol", "Beer", "Spirits", "Wine"), name="")+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="Grey90"))+
  labs(title="Alcohol continues to get more affordable",
       subtitle="Alcohol affordability in the UK since 1988 (higher = more affordable). Affordability is calculated as\nthe ratio of household disposable income (adjusted) to the relative price of alcohol vs. overall CPI inflation",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

#Rebase data to a more recent data
base <- "2019 Q4"

#rebase
Affordability_rebase <- Affordability %>%
  select(c(1:7, 17)) %>% 
  mutate(across(c(2:7), ~ .x*100/.x[date==base])) %>% 
  #Calculate afforadbility using NHS England approach
  mutate(RAPI_all=Alcohol*100/Overall,
         RAPI_beer=Beer*100/Overall,
         RAPI_wine=Wine*100/Overall,
         RAPI_spirits=Spirits*100/Overall,
         Affordability_all=Incomes*100/RAPI_all,
         Affordability_beer=Incomes*100/RAPI_beer,
         Affordability_wine=Incomes*100/RAPI_wine,
         Affordability_spirits=Incomes*100/RAPI_spirits) %>% 
  filter(time>=time[date==base])

Summarydata_rebase <- Affordability_rebase %>% 
  select(time, starts_with(c("RAPI", "Affordability"))) %>% 
  pivot_longer(c(2:ncol(.)), names_to=c("Metric", "Product"), names_sep="_", values_to="Value")


agg_tiff("Outputs/DisposableIncomesShort.tiff", units="in", width=9, height=6, res=600)
Affordability_rebase %>% 
  select(c("time", "Overall", "Incomes")) %>% 
  gather(Measure, Value, c(2,3)) %>%
  mutate(Measure=if_else(Measure=="Overall", "CPI inflation", "Disposable incomes"),
         vjust=if_else(Measure=="CPI inflation", -0.5, 1.5)) %>% 
  ggplot(aes(x=time, y=Value, colour=Measure, label=Measure, vjust=vjust))+
  geom_textline(show.legend=FALSE, hjust=0.71, straight=TRUE,)+
  geom_hline(yintercept=100, colour="Grey80")+
  scale_x_date(name="")+
  scale_y_continuous(name=paste0("Relative values\n(", base, " = 100)"), 
                     trans="log")+
  scale_colour_paletteer_d("ggthemes::Classic_Green_Orange_6")+
  theme_custom()+
  labs(title="The UK has less money to spend and everything costs more",
       subtitle=paste0("Overall CPI inflation and disposable household income relative to ", base,". Data up to September 2023."),
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/AlcoholCPIShort.tiff", units="in", width=9, height=6, res=600)
Affordability_rebase %>% 
  select(c("time", "Alcohol", "Overall")) %>% 
  gather(Measure, Value, c(2,3)) %>% 
  ggplot(aes(x=time, y=Value, colour=Measure))+
  geom_line(show.legend=FALSE)+
  geom_hline(yintercept=100, colour="Grey80")+
  geom_text_repel(data=. %>% filter(time==max(time)),
                  aes(label = Measure),
                  family = "Lato", fontface = "bold", direction = "y", box.padding = 0.4, hjust=0,
                  xlim = c(as.Date("2024-05-01"), NA_Date_), show.legend=FALSE, segment.color = NA)+
  scale_x_date(name="", limits=c(as.Date("2019-07-01"), as.Date("2025-06-01")))+
  scale_y_continuous(name=paste0("CPI inflation\n(", base, " = 100)"), 
                     trans="log")+
  scale_colour_paletteer_d("colorblindr::OkabeIto")+
  theme_custom()+
  labs(title="Alcohol prices rises have lagged behind overall inflation",
       subtitle="CPI inflation for alcohol and for all products combined. Data up to June 2024.",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/AlcoholRelPriceShort.tiff", units="in", width=9, height=6, res=600)
ggplot(Summarydata_rebase %>% filter(Metric=="RAPI"), 
       aes(x=time, y=Value/100, colour=Product, linetype=Product))+
  geom_hline(yintercept=1, colour="Grey70")+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Relative price of alcohol\n(log scale)", 
                     trans="log")+
  scale_colour_manual(values=c("Black", "#ffc000", "#00b0f0", "#7030a0"),
                      labels=c("All alcohol", "Beer", "Spirits", "Wine"), name="")+
  scale_linetype_manual(values=c(2,1,1,1),
                        labels=c("All alcohol", "Beer", "Spirits", "Wine"), name="")+
  theme_custom()+
  labs(title="Relative to everything else, alcohol keeps getting cheaper",
       subtitle=paste0("Relative prices of alcohol and all other goods compared to their relative prices in ", base, "."),
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/AlcoholAffordabilityShort.tiff", units="in", width=9, height=6, res=600)
ggplot(Summarydata_rebase %>% filter(Metric=="Affordability"), aes(x=time, y=Value, colour=Product,
                                                                   linetype=Product))+
  geom_hline(yintercept=100, colour="Grey70")+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name=paste0("Affordability index (", base, "=100"), 
                     trans="log")+
  scale_colour_manual(values=c("Black", "#ffc000", "#00b0f0", "#7030a0"),
                      labels=c("All alcohol", "Beer", "Spirits", "Wine"), name="")+
  scale_linetype_manual(values=c(2,1,1,1),
                        labels=c("All alcohol", "Beer", "Spirits", "Wine"), name="")+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="Grey90"))+
  labs(title="Alcohol continues to get more affordable",
       subtitle=paste0("Alcohol affordability in the UK since ", base, " (higher = more affordable). Affordability is calculated as\nthe ratio of household disposable income (adjusted) to the relative price of alcohol vs. overall CPI inflation"),
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()


############################################################
#Extract on/off trade inflation indices
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindices/current/mm23.csv"
temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

RPIdata <-read.csv(temp) %>% 
  set_names(slice(., 1)) %>% 
  slice_tail(., n=nrow(.)-1) %>% 
  filter(str_length(CDID)>7) %>% 
  select(CDID, DOBI, DOBJ, DOBL, DOBM) %>% 
  set_names("date", "On-trade beer", "Off-trade beer", "On-trade wine & spirits", 
            "Off-trade wine & spirits") %>% 
  mutate(across(.cols=c(2:5), ~as.numeric(.x))) %>% 
  filter(!is.na(`On-trade beer`)) %>% 
  gather(Product, Index, c(2:5)) %>% 
  mutate(date=as.Date(paste(to_upper_camel_case(date, sep_out=" "), "01"), "%Y %b %d"))

ggplot(RPIdata, aes(x=date, y=Index, colour=Product))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="RPI index (1987=100)")+
  scale_colour_paletteer_d("lisa::MarcChagall", name="")+
  theme_custom()

#Rebase to 2013
agg_tiff("Outputs/RPIxChannel.tiff", units="in", width=8, height=6, res=600)
RPIdata %>% 
  group_by(Product) %>% 
  mutate(Index=Index*100/Index[date==as.Date("2013-01-01")]) %>% 
  filter(date>=as.Date("2013-01-01")) %>% 
  ggplot(aes(x=date, y=Index, colour=Product))+
  #geom_rect(aes(xmin=as.Date("2022-01-01"), xmax=as.Date("2023-01-01"), ymin=85,
  #              ymax=155), fill="grey90", colour="grey90")+
  geom_hline(yintercept=100, colour="grey80")+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="RPI index (2013=100)\n(log scale)", trans="log",
                     breaks=c(100, 120, 140), labels=c("No change", "+20%", "+40%"))+
  scale_colour_paletteer_d("lisa::MarcChagall", name="")+
  theme_custom()+
  labs(title="The price of alcohol in pubs has risen much more than in shops",
       subtitle="Retail Price Index data on the prices of on-trade (sold in pubs and bars) and off-trade (sold in shops) alcohol in the UK",
       caption="Data from ONS | Plot by @VictimOfMaths")+
  theme(panel.grid.major.y=element_line(colour="grey95"))

dev.off()

#Rebase to immediately prior to the pandemic

