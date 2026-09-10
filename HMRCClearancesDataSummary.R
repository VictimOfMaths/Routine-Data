rm(list=ls())

library(tidyverse)
library(curl)
library(scales)
library(readxl)
library(extrafont)
library(ragg)
library(paletteer)
library(readODS)
library(lubridate)
library(RcppRoll)
library(zoo)
library(ggtext)

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
          panel.grid.major.y=element_line(colour="grey95"))
}

#Set ABVs for historic alcohol volume assumptions
WineABV <- 0.125
CiderABV <- 0.04

#Read in data from HMRC Alcohol Bulletin https://www.gov.uk/government/statistics/alcohol-bulletin
#Start with older data from before August 2023
#National Archives website seems to have started blocking non-browser requests. 
#Old code here in case this changes

#temp <- tempfile()
#source <- "https://webarchive.nationalarchives.gov.uk/ukgwa/20240102181330mp_/https://assets.publishing.service.gov.uk/media/656718e4d6ad75000d02fc7d/Alc_Tabs_Oct_23.ods"
#temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#Read in receipts data by product type
#raw.wine <- read_ods(temp, sheet="Pre_Aug23_Wine_Tables", range="J62:K353", 
#                     col_names=FALSE) %>% 
#  set_names("Wine", "AllAlcohol")

#raw.spirits <- read_ods(temp, sheet="Pre_Aug23_Spirits_Tables", range="J61:K352", 
#                        col_names=FALSE) %>% 
#  set_names("Spirits", "AllAlcohol") 

#raw.beer <- read_ods(temp, sheet="Pre_Aug23_Beer_And_Cider_Tables", range="K62:L353", 
#                     col_names=FALSE) %>% 
#  set_names("Beer", "Cider")

#Need instead to manually download file from the url above and then read it in here
filepath <- "U:/ManWin/colin_misc_2/Data/Alc_Tables_Jan_24.ods"

raw.wine <- read_ods(filepath, sheet="Wine_(Legacy)", range="H63:J354", 
                     col_names=FALSE) %>% 
  set_names("Wine_Clearances", "Wine", "AllAlcohol")

raw.spirits <- read_ods(filepath, sheet="Spirits_(Legacy)", range="H67:J358", 
                        col_names=FALSE) %>% 
  set_names("Spirits_Clearances", "Spirits", "AllAlcohol") 

raw.beer <- read_ods(filepath, sheet="Beer_and_Cider", range="G68:K359", 
                     col_names=FALSE) %>% 
  set_names("Beer_Clearances", "Cider_Clearances", "dummy", "Beer", "Cider")

old.data <- data.frame(Wine_rev=raw.wine$Wine, Spirits_rev=raw.spirits$Spirits, Beer_rev=raw.beer$Beer,
                       Cider_rev=raw.beer$Cider, Total_rev=raw.wine$AllAlcohol, 
                       Wine_natvol=raw.wine$Wine_Clearances, Spirits_alcvol=raw.spirits$Spirits_Clearances*100,
                       Beer_alcvol=raw.beer$Beer_Clearances*100000, Cider_natvol=raw.beer$Cider_Clearances) |> 
  mutate(Wine_alcvol=Wine_natvol*WineABV*100,
         Cider_alcvol=Cider_natvol*CiderABV*100000,
         Total_alcvol=Wine_alcvol+Spirits_alcvol+Cider_alcvol+Beer_alcvol) |> 
  mutate(Index=1:n(),
         Date=as.Date("1999-04-01")%m+% months(Index-1)) |> 
  pivot_longer(cols=c(c(Wine_rev:Total_rev), Wine_alcvol, Spirits_alcvol, Beer_alcvol, Cider_alcvol, Total_alcvol), 
               names_to=c("drinkcat", "Metric"), names_sep="_", values_to="Value") |> 
  spread(Metric, Value) |> 
  dplyr::select(-c(Cider_natvol, Wine_natvol, Index))

#Add in more recent data from the latest version of the alcohol duty bulletin
#https://www.gov.uk/government/statistics/alcohol-bulletin
temp <- tempfile()
url <- "https://assets.publishing.service.gov.uk/media/6a8c1154ac563f363ba3c3f4/Alcohol_Tables_Jul26.ods"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

raw.wine.latest <- read_ods(temp, sheet="Wine", range="G22:I57", 
                            col_names=FALSE) %>% 
  set_names("Wine_Clearances", "Wine", "AllAlcohol")

raw.spirits.latest <- read_ods(temp, sheet="Spirits", range="I23:K58", 
                               col_names=FALSE) %>% 
  set_names("Spirits_Clearances", "Spirits", "AllAlcohol") 

raw.beer.latest <- read_ods(temp, sheet="Beer", range="N24:O59", 
                            col_names=FALSE) %>% 
  set_names("Beer_Clearances", "Beer")

raw.cider.latest <- read_ods(temp, sheet="Cider", range="I22:J57", 
                             col_names=FALSE) %>% 
  set_names("Cider_Clearances", "Cider")

latest.data <- data.frame(Wine_rev=raw.wine.latest$Wine, Spirits_rev=raw.spirits.latest$Spirits, 
                          Beer_rev=raw.beer.latest$Beer, Cider_rev=raw.cider.latest$Cider, 
                          Total_rev=raw.wine.latest$AllAlcohol,
                          Wine_alcvol=as.numeric(raw.wine.latest$Wine_Clearances),
                          Spirits_alcvol=as.numeric(raw.spirits.latest$Spirits_Clearances),
                          Beer_alcvol=as.numeric(raw.beer.latest$Beer_Clearances),
                          Cider_alcvol=as.numeric(raw.cider.latest$Cider_Clearances)) |> 
  mutate(Total_alcvol=Wine_alcvol+Spirits_alcvol+Cider_alcvol+Beer_alcvol,
         Index=1:n(),
         Date=as.Date("2023-08-01")%m+% months(Index-1)) |> 
  pivot_longer(cols=c(Wine_rev:Total_alcvol), 
               names_to=c("drinkcat", "Metric"), names_sep="_", values_to="Value") |> 
  spread(Metric, Value) |> 
  dplyr::select(-Index)

data <- bind_rows(old.data, latest.data) %>% 
  mutate(Month=month(Date))

#Bring in CPIH data
temp <- tempfile()
url <- "https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/l522/mm23"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

CPIdata <- read.csv(temp)[-c(1:199),] 
colnames(CPIdata) <- c("Date", "index")

CPIdata <- CPIdata %>% 
  mutate(Date=as.Date(paste0(Date, " 1"), "%Y %b %d"),
         index=as.numeric(index),
         inflator=index[length(index)]/index)

#Bring in population data
temp <- tempfile()
url <- "https://www.ons.gov.uk/generator?format=csv&uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/timeseries/ukpop/pop"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

Popdata <- read.csv(temp)[-c(1:8),] 
colnames(Popdata) <- c("Date", "Pop")
Popdata <- Popdata |> 
  mutate(Date=as.Date(paste0(Date, "-07-01")),
         Pop=as.numeric(Pop))

finaldata <- merge(data, CPIdata) %>% 
  mutate(rev_inf=rev*inflator) |> 
  group_by(drinkcat) |> 
  merge(Popdata, all.x=TRUE) |> 
  mutate(Pop=na.approx(Pop, rule=2)) |> 
  ungroup() |> 
  mutate(AlcPerCapita=alcvol/Pop,
         RevPerCapita=rev/Pop,
         RevInfPerCapita=rev_inf/Pop)

#Some descriptive graphs of the data
#Total revenue
agg_png("Outputs/HMRCDutyReceiptsOverall.png", units="in", height=5, width=8, res=800)
finaldata |> filter(drinkcat=="Total") |> 
  mutate(rev_roll=roll_mean(rev, n=12, align="center", fill=NA),
         rev_inf_roll=roll_mean(rev_inf, n=12, align="center", fill=NA)) |> 
  gather(Metric, Value, c(rev_roll, rev_inf_roll)) |> 
  ggplot(aes(x=Date, y=Value, colour=Metric))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Monthly alcohol duty receipts\n(rolling 12-month average, centered)",
                     labels=label_dollar(prefix="£", suffix="m"))+
  scale_colour_paletteer_d("palettetown::porygon", name="", labels=c("Inflation-adjusted", "Cash terms"))+
  theme_custom()+
  theme(legend.position = "inside",
        legend.position.inside = c(0.82,0.52), plot.subtitle=element_markdown())+
  labs(title="Government revenue from alcohol duties has slumped since 2021",
       subtitle="<span style='color:#F89088'>Raw (cash-terms)</span> and <span style='color:#40A0D8'>inflation-adjusted</span> (using CPIH) monthly receipts from alcohol duty in the UK.<br>",
       caption="Data from HMRC | Plot by @VictimOfMaths")

dev.off()

agg_png("Outputs/HMRCDutyClearancesOverall.png", units="in", height=6, width=8, res=800)
finaldata |> filter(drinkcat=="Total") |> 
  mutate(alc_roll=roll_mean(AlcPerCapita, n=12, align="center", fill=NA)) |> 
  ggplot(aes(x=Date, y=alc_roll))+
  geom_rect(aes(xmin=as.Date("2008-03-12"), xmax=as.Date("2013-03-20"), ymin=0, ymax=Inf), fill="grey90")+
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2021-03-08"), ymin=0, ymax=Inf), fill="grey90")+
  geom_hline(yintercept=0, colour="grey20")+
  geom_vline(xintercept=as.Date("2023-08-01"), linetype=2, colour="grey60")+
  geom_line(colour="#253494")+
  scale_x_date(name="")+
  scale_y_continuous(name="Monthly alcohol cleared for sale (litres of ethanol per capita)\n(rolling 12-month average, centered)")+
  theme_custom()+
  theme(legend.position = "inside",
        legend.position.inside = c(0.82,0.48))+
  labs(title="UK alcohol sales have been falling since Autumn 2021",
       subtitle="Total volume of pure alcohol cleared for sale in the UK by HMRC, adjusted for population size.\nData for wine and cider is estimated prior to the duty reforms from product volumes\nassuming ABVs of 12.5% and 4% respectively\n",
       caption="Data from HMRC & ONS | Plot by @VictimOfMaths")+
  annotate("text", x=as.Date("2007-11-01"), y=0.3, angle=90, family="Lato", colour="grey50", label="Alcohol duty escalator")+
  annotate("text", x=as.Date("2019-11-01"), y=0.3, angle=90, family="Lato", colour="grey50", label="COVID lockdowns")+
  annotate("text", x=as.Date("2023-04-01"), y=0.3, angle=90, family="Lato", colour="grey50", label="Duty reforms")

dev.off()

agg_png("Outputs/HMRCDutyClearancesxBev.png", units="in", height=7, width=9, res=800)
finaldata |> filter(drinkcat!="Total") |> 
  group_by(drinkcat) |> 
  mutate(alc_roll=roll_mean(AlcPerCapita, n=12, align="center", fill=NA)) |> 
  ungroup() |> 
  ggplot(aes(x=Date, y=alc_roll, colour=drinkcat))+
  geom_rect(aes(xmin=as.Date("2008-03-12"), xmax=as.Date("2013-03-20"), ymin=0, ymax=Inf), fill="grey90", colour="grey90")+
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2021-03-08"), ymin=0, ymax=Inf), fill="grey90", colour="grey90")+
  geom_hline(yintercept=0, colour="grey20")+
  geom_vline(xintercept=as.Date("2023-08-01"), linetype=2, colour="grey60")+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Monthly alcohol cleared for sale (litres of ethanol per capita)\n(rolling 12-month average, centered)")+
  scale_colour_manual(name="", values=c("#F7AA14", "#2CB11B", "#0099D5", "#C70E7B"))+
  theme_custom()+
  theme(legend.position = "inside",
        legend.position.inside = c(0.92,0.94))+
  labs(title="Sales have been falling for everything except cider",
       subtitle="Total volumes of pure alcohol cleared for sale in the UK by HMRC, adjusted for population size.\nData for wine and cider is estimated prior to the duty reforms from product volumes\nassuming ABVs of 12.5% and 4% respectively\n",
       caption="Data from HMRC & ONS | Plot by @VictimOfMaths")+
  annotate("text", x=as.Date("2007-11-01"), y=0.105, angle=90, family="Lato", colour="grey50", label="Alcohol duty escalator")+
  annotate("text", x=as.Date("2019-11-01"), y=0.1, angle=90, family="Lato", colour="grey50", label="COVID lockdowns")+
  annotate("text", x=as.Date("2023-04-01"), y=0.1, angle=90, family="Lato", colour="grey50", label="Duty reforms")

dev.off()

agg_png("Outputs/HMRCDutyRevenuesxBev.png", units="in", height=7, width=9, res=800)
finaldata |> filter(drinkcat!="Total") |> 
  group_by(drinkcat) |> 
  mutate(alc_roll=roll_mean(rev_inf, n=12, align="center", fill=NA)) |> 
  ungroup() |> 
  ggplot(aes(x=Date, y=alc_roll, colour=drinkcat))+
  geom_rect(aes(xmin=as.Date("2008-03-12"), xmax=as.Date("2013-03-20"), ymin=0, ymax=Inf), fill="grey90", colour="grey90")+
  geom_rect(aes(xmin=as.Date("2020-03-23"), xmax=as.Date("2021-03-08"), ymin=0, ymax=Inf), fill="grey90", colour="grey90")+
  geom_hline(yintercept=0, colour="grey20")+
  geom_vline(xintercept=as.Date("2023-08-01"), linetype=2, colour="grey60")+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Monthly alcohol cleared for sale (litres of ethanol per capita)\n(rolling 12-month average, centered)")+
  scale_colour_manual(name="", values=c("#F7AA14", "#2CB11B", "#0099D5", "#C70E7B"))+
  theme_custom()+
  theme(legend.position = "inside",
        legend.position.inside = c(0.92,0.92))+
  labs(title="Sales have been falling for everything except cider",
       subtitle="Total volumes of pure alcohol cleared for sale in the UK by HMRC, adjusted for population size.\nData for wine and cider is estimated prior to the duty reforms from product volumes\nassuming ABVs of 12.5% and 4% respectively\n",
       caption="Data from HMRC & ONS | Plot by @VictimOfMaths")+
  annotate("text", x=as.Date("2007-11-01"), y=0.1, angle=90, family="Lato", colour="grey50", label="Alcohol duty escalator")+
  annotate("text", x=as.Date("2019-11-01"), y=0.1, angle=90, family="Lato", colour="grey50", label="COVID lockdowns")+
  annotate("text", x=as.Date("2023-04-01"), y=0.1, angle=90, family="Lato", colour="grey50", label="Duty reforms")

dev.off()

