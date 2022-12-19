rm(list=ls())

library(tidyverse)
library(readODS)
library(curl)
library(lubridate)
library(paletteer)
library(cowplot)
library(RcppRoll)
library(ggtext)
library(ragg)

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

#Read in data from HMRC Alcohol Bulletin https://www.gov.uk/government/statistics/alcohol-bulletin
temp <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1120540/Alc_Tabs_Oct_22.ods"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

###########
#Wine data#
###########
raw.wine <- read_ods(temp, sheet="Wine_Duty_(wine)_tables", range="A56:J338", 
                     col_names=FALSE)[,c(1:4,8,9)] 

colnames(raw.wine) <- c("Month", "Still", "Sparkling", "Fortified", "Total", 
                        "Receipts")

wine <- raw.wine %>% 
  mutate(Product="Wine", Product2="Wine") %>% 
  pivot_longer(cols=c(2:5), names_to="Source", values_to="Clearances.Product") %>% 
  #Make some ABV assumptions to get to alcohol volumes
  mutate(`Clearances.Product`=if_else(`Clearances.Product`=="[x]", NA_real_, 
                                      as.numeric(`Clearances.Product`)),
    "Clearances.Alcohol"=case_when(
    Source=="Still" ~ Clearances.Product*0.125,
    Source=="Sparkling" ~ Clearances.Product*0.125,
    Source=="Fortified" ~ Clearances.Product*0.17
  ))

#Estimate total alcohol clearances
temp.wine <- wine %>% 
  filter(Source!="Total") %>% 
  group_by(Month) %>% 
  summarise(Clearances.Alcohol=sum(Clearances.Alcohol)) %>% 
  mutate(Source="Total")

wine <- merge(wine, temp.wine, by=c("Source", "Month"), all=TRUE) %>% 
  mutate(Clearances.Alcohol=coalesce(Clearances.Alcohol.x, Clearances.Alcohol.y)) %>% 
  select(-c("Clearances.Alcohol.x", "Clearances.Alcohol.y")) %>% 
  #Put into common units - receipts in £m, volumes (product and alcohol) in millions of litres
  mutate(Clearances.Product=Clearances.Product*100/1000000, Clearances.Alcohol=Clearances.Alcohol*100/1000000)

################
#Made wine data#
################
raw.madewine <- read_ods(temp, sheet="Wine_Duty_(made_wine)_tables", range="A57:K339", col_names=FALSE)[,c(1:4,9)]

colnames(raw.madewine) <- c("Month", "Low", "Still_High", "Sparkling_High", "Total")

madewine <- raw.madewine %>% 
  mutate(Product="Wine", Product2="MadeWine") %>% 
  pivot_longer(cols=c(2:5), names_to="Source", values_to="Clearances.Product") %>% 
  #Make some ABV assumptions to get to alcohol volumes
  mutate(Clearances.Product=as.numeric(Clearances.Product),
    "Clearances.Alcohol"=case_when(
    Source=="Low" ~ Clearances.Product*0.045,
    Source=="Still_High" ~ Clearances.Product*0.125,
    Source=="Sparkling_High" ~ Clearances.Product*0.125
  ))

#Estimate total alcohol clearances
temp.madewine <- madewine %>% 
  filter(Source!="Total") %>% 
  group_by(Month) %>% 
  summarise(Clearances.Alcohol=sum(Clearances.Alcohol)) %>% 
  mutate(Source="Total")

madewine <- merge(madewine, temp.madewine, by=c("Source", "Month"), all=TRUE) %>% 
  mutate(Clearances.Alcohol=coalesce(Clearances.Alcohol.x, Clearances.Alcohol.y)) %>% 
  select(-c("Clearances.Alcohol.x", "Clearances.Alcohol.y")) %>% 
  #Put into common units - receipts in £m, volumes (product and alcohol) in millions of litres
  mutate(Clearances.Product=Clearances.Product*100/1000000, Clearances.Alcohol=Clearances.Alcohol*100/1000000)

##############
#Spirits data#
##############
raw.spirits <- read_ods(temp, sheet="Spirits_Duty_tables", range="A56:J338", col_names=FALSE)[,c(1,3,4,6:9)]

colnames(raw.spirits) <- c("Month", "MaltWhisky", "OtherWhisky", "RTDs", "Other", "Total",
                          "Receipts")

spirits <- raw.spirits %>% 
  mutate(Product="Spirits", Product2="Spirits") %>% 
  pivot_longer(cols=c(2:6), names_to="Source", values_to="Clearances.Alcohol") %>% 
  #Make some ABV assumptions to get to product volumes
  mutate(`Clearances.Alcohol`=as.numeric(`Clearances.Alcohol`),
    "Clearances.Product"=case_when(
    Source=="MaltWhisky" ~ Clearances.Alcohol/0.4,
    Source=="OtherWhisky" ~ Clearances.Alcohol/0.4,
    Source=="RTDs" ~ Clearances.Alcohol/0.045,
    Source=="Other" ~ Clearances.Alcohol/0.375
  ))

#Estimate total product volumes
temp.spirits <- spirits %>% 
  filter(Source!="Total") %>% 
  group_by(Month) %>% 
  summarise(Clearances.Product=sum(Clearances.Product)) %>% 
  mutate(Source="Total")

spirits <- merge(spirits, temp.spirits, by=c("Source", "Month"), all=TRUE) %>% 
  mutate(Clearances.Product=coalesce(Clearances.Product.x, Clearances.Product.y)) %>% 
  select(-c("Clearances.Product.x", "Clearances.Product.y")) %>% 
  #Put into common units - receipts in £m, volumes (product and alcohol) in litres
  mutate(Clearances.Product=Clearances.Product*100/1000000, Clearances.Alcohol=Clearances.Alcohol*100/1000000)

###################
#Beer & Cider data#
###################
raw.beercider <- read_ods(temp, sheet="Beer_Duty_and_Cider_Duty_tables", range="A57:K339", col_names=FALSE)
beer <- raw.beercider[,c(1:3,6,7,9)]
cider <- raw.beercider[,c(1,8,10)]

colnames(beer) <- c("Month", "Production.Product", "Production.Alcohol", "Clearances.Product",
                        "Clearances.Alcohol", "Receipts")

colnames(cider) <- c("Month", "Clearances.Product", "Receipts")

beer <- beer %>% 
  mutate(across(c(2:5), ~as.numeric(.x)),
    Production.ABV=Production.Alcohol/Production.Product,
         Clearances.ABV=Clearances.Alcohol/Clearances.Product,
         Source="Total", Clearances.Product=Clearances.Product*100*1000/1000000,
         Clearances.Alcohol=Clearances.Alcohol*100*1000/1000000,
         Product="Beer", Product2="Beer")

cider <- cider %>% 
  mutate(Clearances.Product=as.numeric(Clearances.Product),
         Clearances.Alcohol=Clearances.Product*0.045,
         Source="Total", Product="Cider", Product2="Cider",
         Clearances.Product=Clearances.Product*100*1000/1000000, 
         Clearances.Alcohol=Clearances.Alcohol*100*1000/1000000)

#Stick together for analysis
data2 <- bind_rows(wine, madewine, spirits, beer, cider) %>% 
  mutate(date=as.Date(paste0("1 ",Month), format="%d %B %Y"))

#Inflate receipts
#Read in RPI data - need to edit the to and from months/years in the URL
temp2 <- tempfile()
source <- "https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/czeq/mm23&series=&fromMonth=01&fromYear=1950&toMonth=10&toYear=2022&frequency=months"
temp2 <- curl_download(url=source, destfile=temp2, quiet=FALSE, mode="wb")

RPIdata <- read.csv(temp2)[-c(1:5),]
colnames(RPIdata) <- c("date", "RPI")

RPIdata <- RPIdata %>% 
  mutate(date=as.Date(paste0(date, " 1"), "%Y %b %d"),
         RPI=(as.numeric(as.character(RPI))+100)/100,
         index=cumprod(RPI),
         inflator=index[length(index)]/index)

data2 <- merge(data2, RPIdata, by="date") %>% 
  #Inflate to Jan 2021 prices
  mutate(Receipts.Adj=Receipts*inflator)

#Compress to shorter data for main analysis
data <- data2 %>% 
  filter(Source=="Total") %>% 
  group_by(date, Product) %>% 
  summarise(Clearances.Product=sum(Clearances.Product),
            Clearances.Alcohol=sum(Clearances.Alcohol),
            Receipts=sum(Receipts, na.rm=TRUE), Receipts.Adj=sum(Receipts.Adj, na.rm=TRUE))

#Add in all alcohol
data <- data %>% 
  group_by(date) %>% 
  summarise(Clearances.Product=sum(Clearances.Product),
            Clearances.Alcohol=sum(Clearances.Alcohol),
            Receipts=sum(Receipts), Receipts.Adj=sum(Receipts.Adj)) %>% 
  mutate(Product="Total") %>% 
  bind_rows(data)

#2020 duty takes vs. 5-year ranges
AlcVol <- data %>%
  filter(date>=as.Date("2015-01-01") & date<as.Date("2020-01-01")) %>%
  group_by(date, Product) %>%
  summarise(Clearances.Product=sum(Clearances.Product, na.rm=TRUE),
            Clearances.Alcohol=sum(Clearances.Alcohol, na.rm=TRUE),
            Receipts=sum(Receipts), Receipts.Adj=sum(Receipts.Adj)) %>%
  mutate(yearmonth=month(date)) %>%
  group_by(yearmonth, Product) %>%
  summarise(alcvolmax=max(Clearances.Alcohol, na.rm=TRUE), 
            alcvolmin=min(Clearances.Alcohol, na.rm=TRUE), 
            alcvolmean=mean(Clearances.Alcohol, na.rm=TRUE),
            prodvolmax=max(Clearances.Product, na.rm=TRUE), 
            prodvolmin=min(Clearances.Product, na.rm=TRUE), 
            prodvolmean=mean(Clearances.Product, na.rm=TRUE),
            revmax=max(Receipts.Adj), revmin=min(Receipts.Adj), revmean=mean(Receipts.Adj)) %>% 
  ungroup()

AlcVol.2020 <- data %>%
  filter(date>=as.Date("2020-01-01") & date<as.Date("2021-01-01")) %>%
  mutate(yearmonth=month(date)) %>%
  group_by(yearmonth, Product) %>%
  summarise(Clearances.Product20=sum(Clearances.Product),
            Clearances.Alcohol20=sum(Clearances.Alcohol),
            Receipts20=sum(Receipts), Receipts.Adj20=sum(Receipts.Adj)) %>% 
  ungroup()

AlcVol.2021 <- data %>%
  filter(date>=as.Date("2021-01-01") & date<as.Date("2022-01-01")) %>%
  mutate(yearmonth=month(date)) %>%
  group_by(yearmonth, Product) %>%
  summarise(Clearances.Product21=sum(Clearances.Product),
            Clearances.Alcohol21=sum(Clearances.Alcohol),
            Receipts21=sum(Receipts), Receipts.Adj21=sum(Receipts.Adj)) %>% 
  ungroup()

AlcVol.2022 <- data %>%
  filter(date>=as.Date("2022-01-01") & date<as.Date("2023-01-01")) %>%
  mutate(yearmonth=month(date)) %>%
  group_by(yearmonth, Product) %>%
  summarise(Clearances.Product22=sum(Clearances.Product),
            Clearances.Alcohol22=sum(Clearances.Alcohol),
            Receipts22=sum(Receipts), Receipts.Adj22=sum(Receipts.Adj)) %>% 
  ungroup()

AlcVol <- merge(AlcVol, AlcVol.2020, by=c("yearmonth", "Product"), all.x=TRUE) %>% 
  merge(AlcVol.2021, by=c("yearmonth", "Product"), all.x=TRUE) %>% 
  merge(AlcVol.2022, by=c("yearmonth", "Product"), all.x=TRUE)


#Calculate overall changes from historic values
loss <- AlcVol %>% 
  mutate(alcvolchange20=Clearances.Alcohol20-alcvolmean,
         alcvolchange21=Clearances.Alcohol21-alcvolmean,
         alcvolchange22=Clearances.Alcohol22-alcvolmean,
         prodvolchange20=Clearances.Product20-prodvolmean,
         prodvolchange21=Clearances.Product21-prodvolmean,
         prodvolchange22=Clearances.Product22-prodvolmean,
         revchange20=Receipts.Adj20-revmean,
         revchange21=Receipts.Adj21-revmean,
         revchange22=Receipts.Adj22-revmean) %>% 
  group_by(Product) %>% 
  summarise(alcvolchange20=sum(alcvolchange20, na.rm=TRUE), 
            alcvolchange21=sum(alcvolchange21, na.rm=TRUE), 
            alcvolchange22=sum(alcvolchange22, na.rm=TRUE),alchist=sum(alcvolmean),
            alcvolprop20=alcvolchange20/alchist, alcvolprop21=alcvolchange21/alchist,
            alcvolprop22=alcvolchange22/alchist,
            prodvolchange20=sum(prodvolchange20, na.rm=TRUE),
            prodvolchange21=sum(prodvolchange21, na.rm=TRUE),
            prodvolchange22=sum(prodvolchange22, na.rm=TRUE),
            prodhist=sum(prodvolmean), prodvolprop20=prodvolchange20/prodhist,
            prodvolprop21=prodvolchange21/prodhist, prodvolprop22=prodvolchange22/prodhist,
            revchange20=sum(revchange20, na.rm=TRUE), 
            revchange21=sum(revchange21, na.rm=TRUE), revchange22=sum(revchange22, na.rm=TRUE),
            revhist=sum(revmean), revprop20=revchange20/revhist, revprop21=revchange21/revhist,
            revprop22=revchange22/revhist) %>% 
  mutate(vollabs20=if_else(alcvolchange20>0, paste0("+", round(alcvolchange20, 1), " million litres (+",
                                                round(alcvolprop20*100, 1), "%)"),
                         paste0(round(alcvolchange20, 1), " million litres (",
                                round(alcvolprop20*100, 1), "%)")),
         vollabs21=if_else(alcvolchange21>0, paste0("+", round(alcvolchange21, 1), " million litres (+",
                                                    round(alcvolprop21*100, 1), "%)"),
                           paste0(round(alcvolchange21, 1), " million litres (",
                                  round(alcvolprop21*100, 1), "%)")),
         vollabs22=if_else(alcvolchange22>0, paste0("+", round(alcvolchange22, 1), " million litres (+",
                                                    round(alcvolprop22*100, 1), "%)"),
                           paste0(round(alcvolchange22, 1), " million litres (",
                                  round(alcvolprop22*100, 1), "%)")),
         prodlabs20=if_else(prodvolchange20>0, paste0("+", round(prodvolchange20, 1), " million litres (+",
                                                  round(prodvolprop20*100, 1), "%)"),
                          paste(round(prodvolchange20, 1), " million litres (",
                                round(prodvolprop20*100, 1), "%)")),
         prodlabs21=if_else(prodvolchange21>0, paste0("+", round(prodvolchange21, 1), " million litres (+",
                                                      round(prodvolprop21*100, 1), "%)"),
                            paste(round(prodvolchange21, 1), " million litres (",
                                  round(prodvolprop21*100, 1), "%)")),
         prodlabs22=if_else(prodvolchange22>0, paste0("+", round(prodvolchange22, 1), " million litres (+",
                                                      round(prodvolprop22*100, 1), "%)"),
                            paste(round(prodvolchange22, 1), " million litres (",
                                  round(prodvolprop22*100, 1), "%)")),
         revlabs20=if_else(revchange20>0, paste0("+£", round(revchange20, 1), " million (+",
                                                 round(revprop20*100, 1), "%) revenue from alcohol duty"),
                         paste("£", round(revchange20, 1), " million (",
                               round(revprop20*100, 1), "%) revenue from alcohol duty")),
         revlabs21=if_else(revchange21>0, paste0("+£", round(revchange21, 1), " million (+",
                                                 round(revprop21*100, 1), "%) revenue from alcohol duty"),
                           paste("£", round(revchange21, 1), " million (",
                                 round(revprop21*100, 1), "%) revenue from alcohol duty")),
         revlabs22=if_else(revchange22>0, paste0("+£", round(revchange22, 1), " million (+",
                                                 round(revprop22*100, 1), "%) revenue from alcohol duty"),
                           paste("£", round(revchange22, 1), " million (",
                                 round(revprop22*100, 1), "%) revenue from alcohol duty")))

#Visualise
#Totals
tiff("Outputs/HMRCClearancesExcessBlank.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(AlcVol, Product=="Total"))+
  geom_ribbon(aes(x=yearmonth, ymin=alcvolmin, ymax=alcvolmax), fill="Skyblue2")+
  geom_line(aes(x=yearmonth, y=alcvolmean), linetype=2, colour="Grey50")+
  scale_x_continuous(name="", breaks=seq(1, 12, by=1), labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                                "Aug", "Sep", "Oct", "Nov", "Dec"))+
  scale_y_continuous(name="Estimated monthly ethanol clearances\n(millions of litres)", limits=c(0,NA))+
  theme_custom()+
  annotate("text", x=3.5, y=54, colour="Skyblue4", label="2015-19 range")+
  annotate("text", x=11, y=44, colour="Grey50", label="2015-19 mean")+
  geom_curve(aes(x=3.5, y=52.5, xend=3.2, yend=49.5), colour="Skyblue4", curvature=-0.25,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  geom_curve(aes(x=11, y=45.5, xend=10, yend=48), colour="grey30", curvature=0.15,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
 
  labs(title="Total ethanol clearances reported by HMRC",
       subtitle="Data for Cider and Wine is estimated from reported product volumes based on ABV assumptions",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/HMRCClearancesExcess2020.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(AlcVol, Product=="Total"))+
  geom_ribbon(aes(x=yearmonth, ymin=alcvolmin, ymax=alcvolmax), fill="Skyblue2")+
  geom_line(aes(x=yearmonth, y=alcvolmean), linetype=2, colour="Grey50")+
  geom_ribbon(aes(x=yearmonth, ymin=Clearances.Alcohol20, ymax=alcvolmean), fill="Red", alpha=0.2)+
  geom_line(aes(x=yearmonth, y=Clearances.Alcohol20), colour="Red")+
  scale_x_continuous(name="", breaks=seq(1, 12, by=1), labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                                "Aug", "Sep", "Oct", "Nov", "Dec"))+
  scale_y_continuous(name="Estimated monthly ethanol clearances\n(millions of litres)", limits=c(0,NA))+
  theme_custom()+
  annotate("text", x=7, y=53, colour="Red", label="2020")+
  annotate("text", x=3.5, y=54, colour="Skyblue4", label="2015-19 range")+
  annotate("text", x=11, y=44, colour="Grey50", label="2015-19 mean")+
  geom_curve(aes(x=3.5, y=52.5, xend=3.2, yend=49.5), colour="Skyblue4", curvature=-0.25,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  geom_curve(aes(x=11, y=45.5, xend=10, yend=48), colour="grey30", curvature=0.15,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  annotate("text", family="Lato", x=6, y=20, colour="Red", label=paste0("+", round(loss[4,2], 1), " million litres (+", 
                                                         round(loss[4,5]*100, 1), 
                                                         "%) of alcohol cleared in 2020\ncompared to the 2015-19 average"))+
  labs(title="Total ethanol clearances reported by HMRC",
       subtitle="Data for Cider and Wine is estimated from reported product volumes based on ABV assumptions",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/HMRCClearancesExcess2021.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(AlcVol, Product=="Total"))+
  geom_ribbon(aes(x=yearmonth, ymin=alcvolmin, ymax=alcvolmax), fill="Skyblue2")+
  geom_line(aes(x=yearmonth, y=alcvolmean), linetype=2, colour="Grey50")+
  geom_ribbon(aes(x=yearmonth, ymin=Clearances.Alcohol21, ymax=alcvolmean), fill="Purple", alpha=0.2)+
  geom_line(aes(x=yearmonth, y=Clearances.Alcohol21), colour="Purple")+
  scale_x_continuous(name="", breaks=seq(1, 12, by=1), labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                                "Aug", "Sep", "Oct", "Nov", "Dec"))+
  scale_y_continuous(name="Estimated monthly ethanol clearances\n(millions of litres)", limits=c(0,NA))+
  theme_custom()+
  annotate("text", x=7, y=53, colour="Purple", label="2021")+
  annotate("text", x=3.5, y=54, colour="Skyblue4", label="2015-19 range")+
  annotate("text", x=11, y=44, colour="Grey50", label="2015-19 mean")+
  geom_curve(aes(x=3.5, y=52.5, xend=3.2, yend=49.5), colour="Skyblue4", curvature=-0.25,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  geom_curve(aes(x=11, y=45.5, xend=10, yend=48), colour="grey30", curvature=0.15,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  annotate("text", family="Lato", x=6, y=20, colour="Purple", label=paste0("+", round(loss[4,3], 1), " million litres (+", 
                                                         round(loss[4,6]*100, 1), 
                                                         "%) of alcohol cleared in 2021\ncompared to the 2015-19 average"))+
  labs(title="Total ethanol clearances reported by HMRC",
       subtitle="Data for Cider and Wine is estimated from reported product volumes based on ABV assumptions",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/HMRCClearancesExcess2022.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(AlcVol, Product=="Total"))+
  geom_ribbon(aes(x=yearmonth, ymin=alcvolmin, ymax=alcvolmax), fill="Skyblue2")+
  geom_line(aes(x=yearmonth, y=alcvolmean), linetype=2, colour="Grey50")+
  geom_ribbon(aes(x=yearmonth, ymin=Clearances.Alcohol22, ymax=alcvolmean), fill="#3D227F", alpha=0.2)+
  geom_line(aes(x=yearmonth, y=Clearances.Alcohol22), colour="#3D227F")+
  scale_x_continuous(name="", breaks=seq(1, 12, by=1), labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                                "Aug", "Sep", "Oct", "Nov", "Dec"))+
  scale_y_continuous(name="Estimated monthly ethanol clearances\n(millions of litres)", limits=c(0,NA))+
  theme_custom()+
  annotate("text", x=1.5, y=39, colour="#21355D", label="2022")+
  annotate("text", x=3.5, y=54, colour="Skyblue4", label="2015-19 range")+
  annotate("text", x=11, y=44, colour="Grey50", label="2015-19 mean")+
  geom_curve(aes(x=3.5, y=52.5, xend=3.2, yend=49.5), colour="Skyblue4", curvature=-0.25,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  geom_curve(aes(x=11, y=45.5, xend=10, yend=48), colour="grey30", curvature=0.15,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  annotate("text", family="Lato", x=6, y=20, colour="#3D227F", label=paste0("+", round(loss[4,3], 1), " million litres (+", 
                                                                           round(loss[4,6]*100, 1), 
                                                                           "%) of alcohol cleared in 2022\ncompared to the 2015-19 average"))+
  labs(title="Total ethanol clearances reported by HMRC",
       subtitle="Data for Cider and Wine is estimated from reported product volumes based on ABV assumptions",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/HMRCClearancesExcess202020212022.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(AlcVol, Product=="Total"))+
  geom_ribbon(aes(x=yearmonth, ymin=alcvolmin, ymax=alcvolmax), fill="Skyblue2")+
  geom_line(aes(x=yearmonth, y=alcvolmean), linetype=2, colour="Grey50")+
  geom_ribbon(aes(x=yearmonth, ymin=Clearances.Alcohol22, ymax=alcvolmean), fill="Purple", alpha=0.2)+
  geom_ribbon(aes(x=yearmonth, ymin=Clearances.Alcohol21, ymax=alcvolmean), fill="Purple", alpha=0.2)+
  geom_ribbon(aes(x=yearmonth, ymin=Clearances.Alcohol20, ymax=alcvolmean), fill="Red", alpha=0.2)+
  geom_line(aes(x=yearmonth, y=Clearances.Alcohol22), colour="#3D227F")+
  geom_line(aes(x=yearmonth, y=Clearances.Alcohol21), colour="Purple")+
  geom_line(aes(x=yearmonth, y=Clearances.Alcohol20), colour="Red")+
  scale_x_continuous(name="", breaks=seq(1, 12, by=1), labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                                "Aug", "Sep", "Oct", "Nov", "Dec"))+
  scale_y_continuous(name="Estimated monthly ethanol clearances\n(millions of litres)", limits=c(0,NA))+
  theme_custom()+
  annotate("text", x=1.5, y=39, colour="#3D227F", label="2022")+
  annotate("text", x=5, y=51, colour="Purple", label="2021")+
  annotate("text", x=7, y=53, colour="Red", label="2020")+
  annotate("text", x=2, y=54, colour="Skyblue3", label="2015-19 range")+
  annotate("text", x=11, y=44, colour="Grey50", label="2015-19 mean")+
  geom_curve(aes(x=2, y=52.5, xend=2.8, yend=49.5), colour="Skyblue3", curvature=0.25,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  geom_curve(aes(x=11, y=45.5, xend=10, yend=48), colour="grey30", curvature=0.15,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  labs(title="Tax data suggests alcohol consumption has risen during the pandemic",
       subtitle="Total ethanol clearances in the UK reported by Her Majesty's Revenue and Customs (HMRC).\nData for Cider and Wine is estimated from reported product volumes based on ABV assumptions",
       caption="Data from HMRC | Plot by @VictimOfMaths")
         
dev.off()

tiff("Outputs/HMRCClearancesExcess20202021.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(AlcVol, Product=="Total"))+
  geom_ribbon(aes(x=yearmonth, ymin=alcvolmin, ymax=alcvolmax), fill="Skyblue2")+
  geom_line(aes(x=yearmonth, y=alcvolmean), linetype=2, colour="Grey50")+
  geom_ribbon(aes(x=yearmonth, ymin=Clearances.Alcohol21, ymax=alcvolmean), fill="Purple", alpha=0.2)+
  geom_ribbon(aes(x=yearmonth, ymin=Clearances.Alcohol20, ymax=alcvolmean), fill="Red", alpha=0.2)+
  geom_line(aes(x=yearmonth, y=Clearances.Alcohol21), colour="Purple")+
  geom_line(aes(x=yearmonth, y=Clearances.Alcohol20), colour="Red")+
  scale_x_continuous(name="", breaks=seq(1, 12, by=1), labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                                "Aug", "Sep", "Oct", "Nov", "Dec"))+
  scale_y_continuous(name="Estimated monthly ethanol clearances\n(millions of litres)", limits=c(0,NA))+
  theme_custom()+
  annotate("text", x=5, y=51, colour="Purple", label="2021")+
  annotate("text", x=7, y=53, colour="Red", label="2020")+
  annotate("text", x=2, y=54, colour="Skyblue3", label="2015-19 range")+
  annotate("text", x=11, y=44, colour="Grey50", label="2015-19 mean")+
  geom_curve(aes(x=2, y=52.5, xend=2.8, yend=49.5), colour="Skyblue3", curvature=0.25,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  geom_curve(aes(x=11, y=45.5, xend=10, yend=48), colour="grey30", curvature=0.15,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  labs(title="Tax data suggests alcohol consumption has risen during the pandemic",
       subtitle="Total ethanol clearances in the UK reported by Her Majesty's Revenue and Customs (HMRC).\nData for Cider and Wine is estimated from reported product volumes based on ABV assumptions",
       caption="Data from HMRC | Plot by @VictimOfMaths")

dev.off()


#This one is less meaningful since it's adding beer volumes to spirits volumes etc.
ggplot(subset(AlcVol, Product=="Total"))+
  geom_ribbon(aes(x=yearmonth, ymin=prodvolmin, ymax=prodvolmax), fill="Skyblue2")+
  geom_line(aes(x=yearmonth, y=prodvolmean), linetype=2, colour="Grey50")+
  geom_ribbon(aes(x=yearmonth, ymin=Clearances.Product20, ymax=prodvolmean), fill="Red", alpha=0.2)+
  geom_line(aes(x=yearmonth, y=Clearances.Product20), colour="Red")+
  geom_ribbon(aes(x=yearmonth, ymin=Clearances.Product21, ymax=prodvolmean), fill="Purple", alpha=0.2)+
  geom_line(aes(x=yearmonth, y=Clearances.Product21), colour="Purple")+
  scale_x_continuous(name="", breaks=seq(1, 12, by=1), labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                                "Aug", "Sep", "Oct", "Nov", "Dec"))+
  scale_y_continuous(name="Estimated monthly alcoholic product clearances\n(millions of litres)", limits=c(0,NA))+
  theme_custom()+
  annotate("text", x=7, y=650, colour="Red", label="2020")+
  annotate("text", x=2.2, y=670, colour="Skyblue4", label="2015-19 range")+
  annotate("text", x=11, y=590, colour="Grey50", label="2015-19 mean")+
  geom_curve(aes(x=2.2, y=650, xend=2.6, yend=600), colour="Skyblue4", curvature=0.25,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  geom_curve(aes(x=11, y=605, xend=10.3, yend=640), colour="grey30", curvature=0.15,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  labs(title="Total alcohol clearances (product volumes) reported by HMRC",
       subtitle="Data for Spirits is estimated based on ABV assumptions")


#tiff("Outputs/HMRCRevenueExcess.tiff", units="in", width=8, height=6, res=500)
#ggplot(subset(AlcVol, Product=="Total"))+
#  geom_ribbon(aes(x=yearmonth, ymin=revmin, ymax=revmax), fill="Skyblue2")+
#  geom_line(aes(x=yearmonth, y=revmean), linetype=2, colour="Grey50")+
#  geom_ribbon(aes(x=yearmonth, ymin=Receipts.Adj, ymax=revmean), fill="Red", alpha=0.2)+
#  geom_line(aes(x=yearmonth, y=Receipts.Adj), colour="Red")+
#  scale_x_continuous(name="", breaks=seq(1, 12, by=1), labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
#                                                                "Aug", "Sep", "Oct", "Nov", "Dec"))+
#  scale_y_continuous(name="HMRC monthly duty receipts\n(£millions)", limits=c(0,NA))+
#  theme_custom()+
#  theme(plot.title=element_text(face="bold", size=rel(1.5)))+
#  annotate("text", x=6, y=1350, colour="Red", label="2020")+
#  annotate("text", x=2.2, y=1150, colour="Skyblue4", label="2015-19 range")+
#  annotate("text", x=11, y=900, colour="Grey50", label="2015-19 mean")+
#  geom_curve(aes(x=2.2, y=1110, xend=2.5, yend=1000), colour="Skyblue4", curvature=0.25,
#             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
#  geom_curve(aes(x=11, y=980, xend=10.4, yend=1060), colour="grey30", curvature=0.15,
#             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
#  annotate("text", x=6, y=400, colour="Red", label=paste0("£", round(loss[4,8], 1), "million (", 
#                                                         round(loss[4,10]*100, 1), 
#                                                         "%) revenue from alcohol duty collected in 2020\ncompared to the 2015-19 average"))+
#  labs(title="Total HMRC revenue from alcohol duty",
#       subtitle="Adjusted to January 2021 prices using the Retail Prices Index", 
#       caption="Data from HMRC | Plot by @VictimOfMaths")
#dev.off()

#By product
tiff("Outputs/HMRCClearanceExcessxProd2020.tiff", units="in", width=10, height=8, res=500)
ggplot(subset(AlcVol, Product!="Total"))+
  geom_ribbon(aes(x=yearmonth, ymin=alcvolmin, ymax=alcvolmax), fill="Skyblue2")+
  geom_line(aes(x=yearmonth, y=alcvolmean), linetype=2, colour="Grey50")+
  geom_ribbon(aes(x=yearmonth, ymin=Clearances.Alcohol20, ymax=alcvolmean), fill="Red", alpha=0.2)+
  geom_line(aes(x=yearmonth, y=Clearances.Alcohol20), colour="Red")+
  geom_text(data=subset(loss, Product!="Total"), aes(x=c(6,6,6,6), y=c(10,7,15,9), label=vollabs20), colour="Red")+
  scale_x_continuous(name="", breaks=seq(1, 12, by=1), labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                                "Aug", "Sep", "Oct", "Nov", "Dec"))+
  scale_y_continuous(name="Estimated monthly ethanol clearances\n(millions of litres)", limits=c(0,NA))+
  facet_wrap(~Product)+
  theme_custom()+
  theme(plot.title=element_text(face="bold", size=rel(1.5)))+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Total ethanol clearances reported by HMRC in 2020",
       subtitle="Data for Cider and Wine is estimated from product volumes based on ABV assumptions",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/HMRCClearanceExcessxProd2021.tiff", units="in", width=10, height=8, res=500)
ggplot(subset(AlcVol, Product!="Total"))+
  geom_ribbon(aes(x=yearmonth, ymin=alcvolmin, ymax=alcvolmax), fill="Skyblue2")+
  geom_line(aes(x=yearmonth, y=alcvolmean), linetype=2, colour="Grey50")+
  geom_ribbon(aes(x=yearmonth, ymin=Clearances.Alcohol21, ymax=alcvolmean), fill="Purple", alpha=0.2)+
  geom_line(aes(x=yearmonth, y=Clearances.Alcohol21), colour="Purple")+
  geom_text(data=subset(loss, Product!="Total"), aes(x=c(6,6,6,6), y=c(10,7,15,9), label=vollabs21), colour="Purple")+
  scale_x_continuous(name="", breaks=seq(1, 12, by=1), labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                                "Aug", "Sep", "Oct", "Nov", "Dec"))+
  scale_y_continuous(name="Estimated monthly ethanol clearances\n(millions of litres)", limits=c(0,NA))+
  facet_wrap(~Product)+
  theme_custom()+
  theme(plot.title=element_text(face="bold", size=rel(1.5)))+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Total ethanol clearances reported by HMRC in 2021",
       subtitle="Data for Cider and Wine is estimated from product volumes based on ABV assumptions",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

#ggplot(subset(AlcVol, Product!="Total"))+
#  geom_ribbon(aes(x=yearmonth, ymin=prodvolmin, ymax=prodvolmax), fill="Skyblue2")+
#  geom_line(aes(x=yearmonth, y=prodvolmean), linetype=2, colour="Grey50")+
#  geom_ribbon(aes(x=yearmonth, ymin=Clearances.Product, ymax=prodvolmean), fill="Red", alpha=0.2)+
#  geom_line(aes(x=yearmonth, y=Clearances.Product), colour="Red")+
#  geom_text(data=subset(loss, Product!="Total"), aes(x=c(6,6,6,6), y=c(200,150,100,230), label=prodlabs), colour="Red")+
#  scale_x_continuous(name="", breaks=seq(1, 12, by=1), labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
#                                                                "Aug", "Sep", "Oct", "Nov", "Dec"))+
#  scale_y_continuous(name="Estimated monthly alcoholic product clearances\n(millions of litres)", limits=c(0,NA))+
#  facet_wrap(~Product)+
#  theme_custom()+
#  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
#  labs(title="Total alcohol clearances (product volumes) reported by HMRC",
#       subtitle="Data for Spirits is estimated based on ABV assumptions")

#tiff("Outputs/HMRCRevenueExcessxProd.tiff", units="in", width=10, height=8, res=500)
#ggplot(subset(AlcVol, Product!="Total"))+
#  geom_ribbon(aes(x=yearmonth, ymin=revmin, ymax=revmax), fill="Skyblue2")+
#  geom_line(aes(x=yearmonth, y=revmean), linetype=2, colour="Grey50")+
#  geom_ribbon(aes(x=yearmonth, ymin=Receipts.Adj, ymax=revmean), fill="Red", alpha=0.2)+
#  geom_line(aes(x=yearmonth, y=Receipts.Adj), colour="Red")+
#  geom_text(data=subset(loss, Product!="Total"), aes(x=c(6,6,6,6), y=c(600,100,460,200), label=revlabs), colour="Red")+
#  scale_x_continuous(name="", breaks=seq(1, 12, by=1), labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
#                                                                "Aug", "Sep", "Oct", "Nov", "Dec"))+
#  scale_y_continuous(name="HMRC monthly duty receipts\n(£millions)", limits=c(0,NA))+
#  facet_wrap(~Product)+
#  theme_custom()+
#  theme(plot.title=element_text(face="bold", size=rel(1.5)))+
#  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
#  labs(title="Total HMRC revenue from alcohol duty",
#       subtitle="Adjusted to January 2021 prices using the Retail Prices Index",
#       caption="Data from HMRC | Plot by @VictimOfMaths")
#dev.off()

#Generate rolling averages in long-term data
data <- data %>% 
  group_by(Product) %>% 
  mutate(Clearances.Product_roll=roll_mean(Clearances.Product, 12, align="right", fill=NA, na.rm=TRUE),
         Clearances.Alcohol_roll=roll_mean(Clearances.Alcohol, 12, align="right", fill=NA, na.rm=TRUE),
         Receipts_roll=roll_mean(Receipts, 12, align="right", fill=NA, na.rm=TRUE),
         Receipts.Adj_roll=roll_mean(Receipts.Adj, 12, align="right", fill=NA, na.rm=TRUE)) %>% 
  ungroup()

#Plot cash vs. real terms overall duty revenue
tiff("Outputs/HMRCRevenueTotal.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(data, Product=="Total"))+
  geom_line(aes(x=date, y=Receipts.Adj_roll), colour="#40A0D8")+
  geom_line(aes(x=date, y=Receipts_roll), colour="#F89088")+
  scale_x_date(name="")+
  scale_y_continuous(name="Monthly HMRC alcohol duty receipts (£m)")+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="Alcohol duty revenue has started falling sharply",
       subtitle="Rolling 12-month average of HMRC alcohol duty revenue <span style='color:#F89088;'>before</span> and <span style='color:#40A0D8;'>after</span> adjusting for inflation",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/HMRCClearancesxProd.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(data, Product!="Total" & date>=as.Date("2010-01-01")))+
  geom_line(aes(x=date, y=Clearances.Alcohol_roll, colour=Product))+
  scale_x_date(name="")+
  scale_y_continuous(name="Monthly alcohol clearances (millions of litres of ethanol)")+
  scale_colour_manual(values=c("#ffc000", "#00b050", "#00b0f0", "#7030a0"))+
  theme_custom()+
  theme(plot.title=element_text(face="bold", size=rel(1.5)))+
  labs(title="Trends in UK alcohol clearances by product",
       subtitle="Rolling 12-month average of total alcohol cleared by HMRC.\nData for cider and wine is estimated from reported product volumes assuming no change in ABVs over time",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/HMRCClearances.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(data, Product=="Total" & date>=as.Date("2010-01-01")))+
  geom_line(aes(x=date, y=Clearances.Alcohol_roll))+
  scale_x_date(name="")+
  scale_y_continuous(name="Monthly alcohol clearances (millions of litres of ethanol)",
                     limits=c(0,NA))+
  theme_custom()+
  theme(plot.title=element_text(face="bold", size=rel(1.5)))+
  labs(title="Longer-term trends in alcohol clearances by product",
       subtitle="Rolling 12-month average of total alcohol cleared by HMRC.\nData for cider and wine is estimated from reported product volumes assuming no change in ABVs over time",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

#On-off data from NHS Health Scotland 
#http://www.healthscotland.scot/media/2585/mesas-monitoring-report-2019-alcohol-sales.xlsx

split <- data.frame(prod=rep(c("Beer", "Cider", "Wine", "Spirits"), times=2), 
                    channel=rep(c("On-trade", "Off-trade"), each=4),
                    vol=c(82.4, 13.4, 18.9, 21.5, 95.2, 27.7, 126.6, 97.7),
                    prop=c("46%", "33%", "13%", "18%", "54%", "67%", "87%", "82%"))

tiff("Outputs/MESAS2018PrefVector.tiff", units="in", width=8, height=6, res=500)
ggplot(split, aes(x=prod, y=vol, fill=channel))+
  geom_bar(stat="identity", position="stack", show.legend=FALSE)+
  geom_text(aes(label=prop), position=position_stack(vjust=0.5))+
  scale_fill_paletteer_d("LaCroixColoR::PeachPear")+
  scale_x_discrete(name="")+
  scale_y_continuous(name="Millions of litres of alcohol sold per year")+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="The British drink beer in pubs, but wine and spirits at home",
       subtitle="Total alcohol sales in 2018 by volume of pure alcohol in the <span style='color:#E9A17C;'>on-trade</span> and the <span style='color:#FF3200;'>off-trade",
       caption="Data from NHS Health Scotland | Plot by @VictimOfMaths")
dev.off()

#Plot descriptive statistics
#Set up x-axis data labels
datelabs <- c("", "Jan 99", "Jan 00", "Jan 01", "Jan 02", "Jan 03", "Jan 04",
              "Jan 05", "Jan 06", "Jan 07", "Jan 08", "Jan 09",
              "Jan 10", "Jan 11", "Jan 12", "Jan 13", "Jan 14",
              "Jan 15", "Jan 16", "Jan 17", "Jan 18", "Jan 19",
              "Jan 20", "Jan 21", "Jan 22", "Jan 23", "")

agg_tiff("Outputs/HMRCAlcRev.tiff", units="in", width=8, height=6, res=500)
ggplot(data %>% filter(Product=="Total"), aes(x=date, y=Receipts))+
  geom_line(colour="Red3")+
  scale_x_date(name="", date_breaks="12 month", labels=datelabs)+
  scale_y_continuous(name="Monthly alcohol duty receipts (£m)", limits=c(0,NA))+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1),
        plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="HMRC's revenue from alcohol duty has been rising steadily",
       subtitle="Total monthly revenue reported by HMRC from alcohol duty without adjusting for inflation",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/HMRCAlcRevRoll.tiff", units="in", width=8, height=6, res=500)
ggplot(data %>% filter(Product=="Total"), aes(x=date, y=Receipts_roll))+
  geom_line(colour="Red3")+
  scale_x_date(name="", date_breaks="12 month", labels=datelabs)+
  scale_y_continuous(name="Monthly alcohol duty receipts (£m)", limits=c(0,NA))+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1),
        plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="HMRC's revenue from alcohol duty has stopped rising",
       subtitle="Rolling 12-month average revenue reported by HMRC from alcohol duty without adjusting for inflation",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/HMRCAlcRevAdj.tiff", units="in", width=8, height=6, res=500)
ggplot(data %>% filter(Product=="Total"), aes(x=date, y=Receipts.Adj))+
  geom_line(colour="Red3")+
  scale_x_date(name="", date_breaks="12 month", labels=datelabs)+
  scale_y_continuous(name="Monthly alcohol duty receipts (£m)", limits=c(0,NA))+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1),
        plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="In real terms, HMRC's revenue from alcohol duty has barely changed",
       subtitle="Total monthly revenue reported by HMRC from alcohol duty adjusted to January 2021 prices using RPI",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/HMRCAlcRevRollAdj.tiff", units="in", width=8, height=6, res=500)
ggplot(data %>% filter(Product=="Total"))+
  geom_line(aes(x=date, y=Receipts_roll), colour="#F89088")+
  geom_line(aes(x=date, y=Receipts.Adj_roll), colour="#40A0D8")+
  scale_x_date(name="", date_breaks="12 month", labels=datelabs)+
  scale_y_continuous(name="Monthly alcohol duty receipts (£m)", limits=c(0,NA))+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1),
        plot.title=element_text(face="bold", size=rel(1.2)),
        plot.subtitle=element_markdown())+
  labs(title="UK alcohol duty revenue hasn't changed much in 20 years ",
       subtitle="Rolling 12-month average of HMRC alcohol duty revenue <span style='color:#F89088;'>before</span> and <span style='color:#40A0D8;'>after</span> adjusting for inflation",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/HMRCAlcRevDrink.tiff", units="in", width=10, height=7.5, res=500)
ggplot(data %>% filter(Product!="Total"), aes(x=date, y=Receipts, colour=Product))+
  geom_line()+
  scale_x_date(name="", date_breaks="12 month", labels=datelabs)+
  scale_y_continuous(name="Monthly alcohol duty receipts (£m)", limits=c(0,NA))+
  scale_colour_manual(values=c("#ffc000", "#00b050", "#00b0f0","#7030a0"), name="")+
  facet_wrap(~Product)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1),
        plot.title=element_text(face="bold", size=rel(1.2)),
        strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Beer revenue was the most affected by the pandemic",
       subtitle="Total monthly revenue reported by HMRC from alcohol duty without adjusting for inflation",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/HMRCAlcRevDrinkRoll.tiff", units="in", width=8, height=6, res=500)
ggplot(data %>% filter(Product!="Total"), aes(x=date, y=Receipts_roll, colour=Product))+
  geom_line()+
  scale_x_date(name="", date_breaks="12 month", labels=datelabs)+
  scale_y_continuous(name="Monthly alcohol duty receipts (£m)", limits=c(0,NA))+
  scale_colour_manual(values=c("#ffc000", "#00b050", "#00b0f0","#7030a0"), name="")+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1),
        plot.title=element_text(face="bold", size=rel(1.2)),
        strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Beer revenue was the most affected by the pandemic",
       subtitle="Rolling 12-month average of HMRC alcohol duty revenue without adjusting for inflation",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/HMRCAlcRevDrinkRollAdj.tiff", units="in", width=8, height=6, res=500)
ggplot(data %>% filter(Product!="Total"), aes(x=date, y=Receipts.Adj_roll, colour=Product))+
  geom_line()+
  scale_x_date(name="", date_breaks="12 month", labels=datelabs)+
  scale_y_continuous(name="Monthly alcohol duty receipts (£m)", limits=c(0,NA))+
  scale_colour_manual(values=c("#ffc000", "#00b050", "#00b0f0","#7030a0"), name="")+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1),
        plot.title=element_markdown(face="bold", size=rel(1.2)),
        strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Duty revenue from wine and spirits is falling **fast**",
       subtitle="Rolling 12-month average of HMRC alcohol duty revenue adjusted to October 2022 prices",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/HMRCAlcVol.tiff", units="in", width=8, height=6, res=500)
ggplot(data %>% filter(Product=="Total"), aes(x=date, y=Clearances.Alcohol))+
  geom_line(colour="Red3")+
  scale_x_date(name="", date_breaks="12 month", labels=datelabs)+
  scale_y_continuous(name="Total alcohol volumes cleared for sale\n(millions of litres of ethanol)", limits=c(0,NA))+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1),
        plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="The overall volume of alcohol sold in the UK has started falling",
       subtitle="Total alcohol volumes cleared for sale by HMRC. Wine and cider data is estimated\nassuming ABVs of 12.5% & 4.5%",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/HMRCAlcVolRoll.tiff", units="in", width=8, height=6, res=500)
ggplot(data %>% filter(Product=="Total"), aes(x=date, y=Clearances.Alcohol_roll))+
  geom_line(colour="Red3")+
  scale_x_date(name="", date_breaks="12 month", labels=datelabs)+
  scale_y_continuous(name="Total alcohol volumes cleared for sale\n(millions of litres of ethanol)", limits=c(0,NA))+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1),
        plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="The overall volume of alcohol sold in the UK has started falling",
       subtitle="Rolling 12-month average of total alcohol volumes cleared for sale by HMRC. Wine and cider data is estimated\nassuming ABVs of 12.5% & 4.5%",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/HMRCAlcVolDrinkRoll.tiff", units="in", width=8, height=6, res=500)
ggplot(data %>% filter(Product!="Total"), aes(x=date, y=Clearances.Alcohol_roll, colour=Product))+
  geom_line()+
  scale_x_date(name="", date_breaks="12 month", labels=datelabs)+
  scale_y_continuous(name="Total alcohol volumes cleared for sale\n(millions of litres of ethanol)", limits=c(0,NA))+
  scale_colour_manual(values=c("#ffc000", "#00b050", "#00b0f0","#7030a0"), name="")+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1),
        plot.title=element_markdown(face="bold", size=rel(1.2)))+
  labs(title="Long-term trends in alcohol clearances by product",
       subtitle="Total alcohol volumes cleared for sale by HMRC. Wine and cider data is estimated\nassuming ABVs of 12.5% & 4.5%",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/HMRCSpiritsType.tiff", units="in", width=8, height=6, res=500)
raw.spirits %>% 
  gather(Product, AlcVol, c(2:5)) %>% 
  mutate(date=as.Date(paste0("01-",Month), format="%d-%B %Y")) %>%
  group_by(Product) %>% 
  arrange(date) %>% 
  mutate(AlcVol_roll=roll_mean(as.numeric(AlcVol), 12, align="right", fill=NA)) %>% 
  ungroup() %>% 
  mutate(Product=factor(Product, levels=c("Other", "OtherWhisky", "MaltWhisky", "RTDs"))) %>% 
  ggplot()+
  geom_line(aes(x=date, y=AlcVol_roll, colour=Product, group=Product))+
  scale_x_date(name="", date_breaks="12 month", labels=datelabs)+
  scale_y_continuous(name="Spirits volumes (of alcohol) cleared for sale (million litres)")+
  scale_colour_manual(name="", labels=c("Other Spirits", "Blended & Grain Whisky", "Malt Whisky", 
                                        "Spirit-Based Alcopops"), 
                      values=c("#FF0000", "#00A08A", "#F2AD00", "#5BBCD6"))+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1),
        plot.title=element_text(face="bold", size=rel(1)))+
  labs(title="Most spirits sold in the UK aren't whisky, and most whisky isn't malt whisky",
       subtitle="Rolling 12-month average of spirits volumes (of alcohol) cleared for sale by HMRC by type", 
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

