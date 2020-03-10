rm(list=ls())

library(tidyverse)
library(readxl)
library(curl)
library(data.table)
library(RcppRoll)

#Read in data from HMRC Alcohol Bulletin
temp <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/868899/2020_Jan_UK_Alc_Stat_Tabs.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

winedata <- read_excel(temp, sheet="T1", range="B60:O308", 
                       col_names=c("Month", "", "", "StillVol", "SparkVol", "StrongVol", "ImpVol", "WareVol", 
                                   "RegVol", "","TotalClearances", "","TotalRev", "TotalAlc"))

spiritsdata <- read_excel(temp, sheet="T3", range="B61:P309",
                          col_names=c("Month", "", "", "ProdVol", "","MaltVol", "GrainVol", "WhiskyVol", "",
                                      "SpiritRTDVol", "OtherVol", "AlcVol", "","TotalRev", "TotalAlc"))

beerdata <- read_excel(temp, sheet="T4", range="B60:R308",
                       col_names=c("Month", "", "", "BeerProdVol", "","BeerAlcProdVol", "", "RegClearances",
                                   "WareClearances", "TotalClearances", "AlcVol", "","CiderVolClearances",
                                   "","TotalRev", "TotalCiderRev", "TotalAlcRev"))

#Read in RPI data
temp2 <- tempfile()
source <- "https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/chaw/mm23"
temp2 <- curl_download(url=source, destfile=temp2, quiet=FALSE, mode="wb")

RPIdata <- fread(temp2)[-c(1:172),]
colnames(RPIdata) <- c("Month", "RPI")
RPIdata$Month <- as.Date(paste0(RPIdata$Month, " 1"), "%Y %b %d")
RPIdata$RPI <- as.numeric(RPIdata$RPI)

RPIdata$Inflator <- RPIdata$RPI[length(RPIdata$RPI)]/RPIdata$RPI

#Separate out beer and cider and tidy up data
#including rolling averages of past 12 months and inflation adjustments
ciderdata <- beerdata[,c(1,13,16)]
colnames(ciderdata) <- c("Month", "TotalClearances", "TotalRev")
ciderdata$Month <- as.Date(ciderdata$Month, origin="1899-12-30")
ciderdata$drink <- "Cider"
ciderdata$TotalClearances <- ciderdata$TotalClearances*1000
ciderdata <- merge(ciderdata, RPIdata, by="Month")
ciderdata$TotalClearances.roll <- RcppRoll::roll_sum(ciderdata$TotalClearances, 12, fill=NA, align="right")/12
ciderdata$TotalRev.roll <- RcppRoll::roll_sum(ciderdata$TotalRev, 12, fill=NA, align="right")/12
ciderdata$TotalRevAdj <- ciderdata$TotalRev*ciderdata$Inflator
ciderdata$TotalRevAdj.roll <- RcppRoll::roll_sum(ciderdata$TotalRevAdj, 12, fill=NA, align="right")/12
#Assume ABV of 4.5% to estimate total alcohol
ciderdata$AlcVol <- ciderdata$TotalClearances*0.045
ciderdata$AlcVol.roll <- ciderdata$TotalClearances.roll*0.045

beerdata <- beerdata[,-c(2,3,5,7,12,13,14,16)]
colnames(beerdata) <- c("Month", "ProdVol", "AlcProdVol", "RegClearances", "WareClearances", "TotalClearances", 
                        "AlcVol", "TotalRev", "TotalAlcRev")
beerdata$Month <- as.Date(beerdata$Month, origin="1899-12-30")
beerdata$drink <- "Beer"
beerdata[,c(2,3,4,5,6,7)] <- beerdata[,c(2,3,4,5,6,7)]*1000
beerdata <- merge(beerdata, RPIdata, by="Month")
beerdata$TotalClearances.roll <- RcppRoll::roll_sum(beerdata$TotalClearances, 12, fill=NA, align="right")/12
beerdata$TotalRev.roll <- RcppRoll::roll_sum(beerdata$TotalRev, 12, fill=NA, align="right")/12
beerdata$TotalAlcRev.roll <- RcppRoll::roll_sum(beerdata$TotalAlcRev, 12, fill=NA, align="right")/12
beerdata$TotalRevAdj <- beerdata$TotalRev*beerdata$Inflator
beerdata$TotalRevAdj.roll <- RcppRoll::roll_sum(beerdata$TotalRevAdj, 12, fill=NA, align="right")/12
beerdata$TotalAlcRevAdj <- beerdata$TotalAlcRev*beerdata$Inflator
beerdata$TotalAlcRevAdj.roll <- RcppRoll::roll_sum(beerdata$TotalAlcRevAdj, 12, fill=NA, align="right")/12
beerdata$AlcVol.roll <-  RcppRoll::roll_sum(beerdata$AlcVol, 12, fill=NA, align="right")/12

winedata <- winedata[,-c(2,3,10,12)]
winedata$Month <- as.Date(winedata$Month, origin="1899-12-30")
winedata$drink <- "Wine"
winedata <- merge(winedata, RPIdata, by="Month")
winedata$TotalClearances.roll <- RcppRoll::roll_sum(winedata$TotalClearances, 12, fill=NA, align="right")/12
winedata$TotalRev.roll <- RcppRoll::roll_sum(winedata$TotalRev, 12, fill=NA, align="right")/12
winedata$TotalRevAdj <- winedata$TotalRev*winedata$Inflator
winedata$TotalRevAdj.roll <- RcppRoll::roll_sum(winedata$TotalRevAdj, 12, fill=NA, align="right")/12
winedata$StillVol.roll <- RcppRoll::roll_sum(winedata$StillVol, 12, fill=NA, align="right")/12
winedata$SparkVol.roll <- RcppRoll::roll_sum(winedata$SparkVol, 12, fill=NA, align="right")/12
winedata$StrongVol.roll <- RcppRoll::roll_sum(winedata$StrongVol, 12, fill=NA, align="right")/12
#Assume ABV of 12.5% to estimate total product alcohol
winedata$AlcVol <- winedata$TotalClearances*0.125
winedata$AlcVol.roll <- winedata$TotalClearances.roll*0.125

spiritsdata <- spiritsdata[,-c(2,3,5,9,13)]
spiritsdata$Month <- as.Date(spiritsdata$Month, origin="1899-12-30")
spiritsdata$drink <- "Spirits"
spiritsdata <- merge(spiritsdata, RPIdata, by="Month")
spiritsdata$AlcVol.roll <- RcppRoll::roll_sum(spiritsdata$AlcVol, 12, fill=NA, align="right")/12
spiritsdata$TotalRev.roll <- RcppRoll::roll_sum(spiritsdata$TotalRev, 12, fill=NA, align="right")/12
spiritsdata$TotalRevAdj <- spiritsdata$TotalRev*spiritsdata$Inflator
spiritsdata$TotalRevAdj.roll <- RcppRoll::roll_sum(spiritsdata$TotalRevAdj, 12, fill=NA, align="right")/12
spiritsdata$MaltVol.roll <- RcppRoll::roll_sum(spiritsdata$MaltVol, 12, fill=NA, align="right")/12
spiritsdata$GrainVol.roll <- RcppRoll::roll_sum(spiritsdata$GrainVol, 12, fill=NA, align="right")/12
spiritsdata$SpiritRTDVol.roll <- RcppRoll::roll_sum(spiritsdata$SpiritRTDVol, 12, fill=NA, align="right")/12
spiritsdata$OtherVol.roll <- RcppRoll::roll_sum(spiritsdata$OtherVol, 12, fill=NA, align="right")/12

#Assume ABV of 37.5% to estimate total product volume
spiritsdata$TotalClearances <- spiritsdata$AlcVol/0.375
spiritsdata$TotalClearances.roll <- RcppRoll::roll_sum(spiritsdata$TotalClearances, 12, fill=NA, align="right")/12

#Stick data together
data <- bind_rows(ciderdata, beerdata, winedata, spiritsdata)

#Bring in alcohol duty rates data
dutyrates <- read_excel("Data/UKDutyRates.xlsx", sheet="Graphs", range="A3:O843",
                        col_names=c("Month", "RawBeer", "RawCider", "RawWine", "RawSpirits",
                                   "CashUnitBeer", "CashUnitCider", "CashUnitWine", "CashUnitSpirits",
                                   "RPIChange", "RPIIndex", "RealUnitBeer", "RealUnitCider", "RealUnitWine",
                                   "RealUnitSpirits"))

dutyrates$Month <- as.Date(dutyrates$Month, origin="1899-12-30")
dutyrates$BeerRoll <- RcppRoll::roll_sum(dutyrates$RealUnitBeer, 12, fill=NA, align="right")/12
dutyrates$CiderRoll <- RcppRoll::roll_sum(dutyrates$RealUnitCider, 12, fill=NA, align="right")/12
dutyrates$WineRoll <- RcppRoll::roll_sum(dutyrates$RealUnitWine, 12, fill=NA, align="right")/12
dutyrates$SpiritsRoll <- RcppRoll::roll_sum(dutyrates$RealUnitSpirits, 12, fill=NA, align="right")/12

data <- merge(data, dutyrates, by="Month")

data$dutyrate <- case_when(
  data$drink=="Beer" ~ data$RealUnitBeer,
  data$drink=="Cider" ~ data$RealUnitCider,
  data$drink=="Wine" ~ data$RealUnitWine,
  data$drink=="Spirits" ~ data$RealUnitSpirits,
)

data$dutyrate.roll <- case_when(
  data$drink=="Beer" ~ data$BeerRoll,
  data$drink=="Cider" ~ data$CiderRoll,
  data$drink=="Wine" ~ data$WineRoll,
  data$drink=="Spirits" ~ data$SpiritsRoll,
)

#Create long datasets for wine and spirit types and long times series duty rates
winedata_long <- gather(winedata, key=Type, value=Vol, StillVol.roll, SparkVol.roll, StrongVol.roll)
spiritsdata_long <- gather(spiritsdata, key=Type, value=Vol, MaltVol.roll, GrainVol.roll, SpiritRTDVol.roll, OtherVol.roll)
winedata_long$Type <- factor(winedata_long$Type, levels=c("StillVol.roll", "SparkVol.roll", "StrongVol.roll"))
spiritsdata_long$Type <- factor(spiritsdata_long$Type, levels=c("OtherVol.roll", "GrainVol.roll", 
                                                                "MaltVol.roll", "SpiritRTDVol.roll"))
dutyrates_long <- gather(dutyrates, key=Type, value=CashRate, CashUnitBeer, CashUnitCider, CashUnitWine, CashUnitSpirits)[,c(1,16,17)]
dutyrates_long$Type <- case_when(
  dutyrates_long$Type=="CashUnitBeer" ~ "Beer",
  dutyrates_long$Type=="CashUnitCider" ~ "Cider",
  dutyrates_long$Type=="CashUnitWine" ~ "Wine",
  dutyrates_long$Type=="CashUnitSpirits" ~ "Spirits") 

temp <- gather(dutyrates, key=Type, value=RealRate, RealUnitBeer, RealUnitCider, RealUnitWine, RealUnitSpirits)[,c(1,16,17)]
temp$Type <- case_when(
  temp$Type=="RealUnitBeer" ~ "Beer",
  temp$Type=="RealUnitCider" ~ "Cider",
  temp$Type=="RealUnitWine" ~ "Wine",
  temp$Type=="RealUnitSpirits" ~ "Spirits") 

dutyrates_long <- merge(dutyrates_long, temp, by=c("Month", "Type"))

#Set up x-axis data labels
datelabs <- c("", "Apr 1999", "Apr 2000", "Apr 2001", "Apr 2002", "Apr 2003",
              "Apr 2004", "Apr 2005", "Apr 2006", "Apr 2007", "Apr 2008",
              "Apr 2009", "Apr 2010", "Apr 2011", "Apr 2012", "Apr 2013",
              "Apr 2014", "Apr 2015", "Apr 2016", "Apr 2017", "Apr 2018",
              "Apr 2019","","")

#Graph of total duty take (cash terms)
tiff("Outputs/HMRCAlcRev.tiff", units="in", width=8, height=6, res=300)
ggplot(subset(data, drink=="Beer"))+
  geom_line(aes(x=Month, y=TotalAlcRev), colour="Red3")+
  theme_classic()+
  scale_y_continuous(limits=c(0,1500), name="Monthly alcohol duty revenue (£m)", labels=c("0", "500", "1,000", "1,500"))+
  scale_x_date(name="", date_breaks="12 month", labels=datelabs)+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))+
  labs(title="Monthly alcohol duty collected by HMRC", caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

#Graph of total duty take (real terms)
#tiff("Outputs/HMRCAlcRevAdj.tiff", units="in", width=8, height=6, res=300)
ggplot(subset(data, drink=="Beer"))+
  geom_line(aes(x=Month, y=TotalAlcRevAdj), colour="Red3")+
  theme_classic()+
  scale_y_continuous(limits=c(0,2000), name="Monthly alcohol duty revenue (£m)\nAdjusted to Dec 2019 prices",
                     labels=c("0", "500", "1,000", "1,500", "2,000"))+
  scale_x_date(name="", date_breaks="12 month", labels=datelabs)+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))
#dev.off()

#Graph of rolling average duty take (real and cash terms)
tiff("Outputs/HMRCAlcRevRoll.tiff", units="in", width=8, height=6, res=300)
ggplot(subset(data, drink=="Beer"))+
  geom_line(aes(x=Month, y=TotalAlcRev.roll), colour="Red3")+
  geom_line(aes(x=Month, y=TotalAlcRevAdj.roll), colour="Blue")+
  theme_classic()+
  scale_x_date(name="", date_breaks="12 month", labels=datelabs)+
  scale_y_continuous(name="Monthly alcohol duty revenue (£m)\n Rolling average of previous 12 months")+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))+
  annotate(geom="text", x=as.Date("2004-04-01"), y=960, label="Inflation-adjusted\n(Dec 2019 prices)",
           colour="Blue")+
  annotate(geom="text", x=as.Date("2012-04-01"), y=700, label="Unadjusted (cash terms)", colour="Red3")+
  labs(title="Changes in HMRC revenue from alcohol duty", caption="Data from HMRC & ONS | Plot by @VictimOfMaths")
dev.off()  

#Graph of drink-specific duty take (cash terms)
tiff("Outputs/HMRCAlcRevxDrink.tiff", units="in", width=10, height=8, res=300)
ggplot(data)+
  geom_line(aes(x=Month, y=TotalRev, colour=drink))+
  theme_classic()+
  scale_y_continuous(name="Monthly alcohol duty revenue (£m)")+
  scale_x_date(name="", date_breaks="12 month", labels=datelabs)+
  scale_colour_manual(values=c("#ffc000", "#00b050", "#00b0f0","#7030a0"), name="")+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)))+
  facet_wrap(~drink)+
  labs(title="HMRC duty revenue by type of alcohol", caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

#Graph of drink-specific duty take (real terms)
#tiff("Outputs/HMRCAlcRevAdjxDrink.tiff", units="in", width=10, height=8, res=300)
ggplot(data)+
  geom_line(aes(x=Month, y=TotalRevAdj, colour=drink))+
  theme_classic()+
  scale_y_continuous(name="Monthly alcohol duty revenue (£m)")+
  scale_x_date(name="", date_breaks="12 month", labels=datelabs)+
  scale_colour_manual(values=c("#ffc000", "#00b050", "#00b0f0","#7030a0"), name="")+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))+
  facet_wrap(~drink)
#dev.off()

#Graph of drink-specific duty take (cash terms)
tiff("Outputs/HMRCAlcRevxDrinkRoll.tiff", units="in", width=8, height=6, res=300)
ggplot(data)+
  geom_line(aes(x=Month, y=TotalRev.roll, colour=drink))+
  theme_classic()+
  scale_y_continuous(name="Monthly alcohol duty revenue (£m)\n Rolling average of previous 12 months")+
  scale_x_date(name="", date_breaks="12 month", labels=datelabs)+
  scale_colour_manual(values=c("#ffc000", "#00b050", "#00b0f0","#7030a0"), name="")+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="HMRC duty revenue by type of alcohol", caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

#Graph of drink-specific duty take (real terms)
tiff("Outputs/HMRCAlcRevAdjxDrinkRoll.tiff", units="in", width=8, height=6, res=300)
ggplot(data)+
  geom_line(aes(x=Month, y=TotalRevAdj.roll, colour=drink))+
  theme_classic()+
  scale_y_continuous(name="Monthly alcohol duty revenue (£m)\n Rolling average of previous 12 months\nadjusted to Dec 2019 prices")+
  scale_x_date(name="", date_breaks="12 month", labels=datelabs)+
  scale_colour_manual(values=c("#ffc000", "#00b050", "#00b0f0","#7030a0"), name="")+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="HMRC duty revenue by type of alcohol", subtitle="After adjusting for inflation",
       caption="Data from HMRC & ONS | Plot by @VictimOfMaths")
dev.off()

#Graph of product volume sales 
tiff("Outputs/HMRCClearancexDrinkRoll.tiff", units="in", width=8, height=6, res=300)
ggplot(data)+
  geom_line(aes(x=Month, y=TotalClearances.roll/10000, colour=drink))+
  theme_classic()+
  scale_y_continuous(name="Total product volumes cleared for sale (million litres)\nRolling average of previous 12 months")+
  scale_x_date(name="", date_breaks="12 month", labels=datelabs)+
  scale_colour_manual(values=c("#ffc000", "#00b050", "#00b0f0","#7030a0"), name="")+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))+
  labs(title="Total product volumes cleared for sale by HMRC by type of alcohol",
       subtitle="Spirits data estimated assuming ABV of 37.5%",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

#Graph of alcohol volume sales 
tiff("Outputs/HMRCAlcVolxDrinkRoll.tiff", units="in", width=8, height=6, res=300)
ggplot(data)+
  geom_line(aes(x=Month, y=AlcVol.roll/10000, colour=drink))+
  theme_classic()+
  scale_y_continuous(name="Total alcohol cleared for sale (million litres of ethanol)\nRolling average of previous 12 months",
                     limits=c(0,22))+
  scale_x_date(name="", date_breaks="12 month", labels=datelabs)+
  scale_colour_manual(values=c("#ffc000", "#00b050", "#00b0f0","#7030a0"), name="")+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))+
  labs(title="Total alcohol cleared for sale by HMRC by type", 
       subtitle="Wine and cider data estimated assuming ABVs of 12.5% & 4.5%",
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

#Graph of implied beer ABV
#tiff("Outputs/HMRCBeerABV.tiff", units="in", width=8, height=6, res=300)
ggplot(subset(data, drink=="Beer"))+
  geom_line(aes(x=Month, y=AlcVol/TotalClearances), colour="Navy")+
  theme_classic()+
  scale_y_continuous(name="Implied average ABV of beer cleared for sale by HMRC",
                     limits=c(0,0.05), labels=scales::percent_format(accuracy=1L))+
  scale_x_date(name="", date_breaks="12 month", labels=datelabs)+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))+
  labs(title="Trends in beer ABVs implied by HMRC data", 
       caption="Data from HMRC | Plot by @VictimOfMaths")
#dev.off()

#Graph of wine sales by type
tiff("Outputs/HMRCWineType.tiff", units="in", width=8, height=6, res=300)
ggplot(winedata_long)+
  geom_line(aes(x=Month, y=Vol*100/1000000, colour=Type))+
  theme_classic()+
  scale_y_continuous(name="Total volume of wine cleared for sale (million litres)\nRolling average of previous 12 months")+
  scale_x_date(name="", date_breaks="12 month", labels=datelabs)+
  scale_colour_manual(name="", labels=c("Still", "Sparkling", "Fortified"), values=c("#9ebcda", "#8c6bb1", "#4d004b"))+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))+
  labs(title="Wine volumes cleared for sale by HMRC by type", 
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

#Graph of spirits sales by type
tiff("Outputs/HMRCSpiritsType.tiff", units="in", width=8, height=6, res=300)
ggplot(spiritsdata_long)+
  geom_line(aes(x=Month, y=Vol*100/1000000, colour=Type))+
  theme_classic()+
  scale_y_continuous(name="Spirits volumes (of alcohol) cleared for sale (million litres)\nRolling average of previous 12 months")+
  scale_x_date(name="", date_breaks="12 month", labels=datelabs)+
  scale_colour_manual(name="", labels=c("Other Spirits", "Blended & Grain Whisky", "Malt Whisky", "Spirit-Based RTD"), 
                      values=c("#FF0000", "#00A08A", "#F2AD00", "#5BBCD6"))+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))+
  labs(title="Spirits volumes (of alcohol) cleared for sale by HMRC by type", 
       caption="Data from HMRC | Plot by @VictimOfMaths")
dev.off()

#Graph of product volume sales vs. duty
tiff("Outputs/HMRCDutyxProdVol.tiff", units="in", width=8, height=6, res=300)
ggplot(data)+
  geom_path(aes(x=TotalClearances.roll/10000, y=dutyrate.roll, colour=drink, alpha=Month))+
  scale_colour_manual(values=c("#ffc000", "#00b050", "#00b0f0","#7030a0"), name="")+
  theme_classic()+
  scale_x_continuous(name="Total product volumes cleared for sale (million litres)\nRolling average of previous 12 months")+
  scale_y_continuous(name="Duty rate levied per unit of alcohol\nRolling average of previous 12 months")+
  guides(alpha=FALSE)+
  labs(title="Association between alcohol duty rates and HMRC clearances",
       caption="Data from HMRC & ONS | Plot by @VictimOfMaths")
dev.off()

#Graph of product volume sales vs. duty
tiff("Outputs/HMRCDutyxAlcVol.tiff", units="in", width=8, height=6, res=300)
ggplot(data)+
  geom_path(aes(x=AlcVol.roll/10000, y=dutyrate.roll, colour=drink, alpha=Month))+
  scale_colour_manual(values=c("#ffc000", "#00b050", "#00b0f0","#7030a0"), name="")+
  theme_classic()+
  scale_x_continuous(name="Total volumes of alcohol cleared for sale (million litres)\nRolling average of previous 12 months")+
  scale_y_continuous(name="Duty rate levied per unit of alcohol\nRolling average of previous 12 months")+
  guides(alpha=FALSE)+
  labs(title="Association between alcohol duty rates and HMRC clearances",
       caption="Data from HMRC & ONS | Plot by @VictimOfMaths")
dev.off()

#Graph of revenue vs. duty
tiff("Outputs/HMRCDutyxRev.tiff", units="in", width=8, height=6, res=300)
ggplot(data)+
  geom_path(aes(x=TotalRevAdj.roll, y=dutyrate.roll, colour=drink, alpha=Month))+
  scale_colour_manual(values=c("#ffc000", "#00b050", "#00b0f0","#7030a0"), name="")+
  theme_classic()+
  scale_x_continuous(name="Monthly alcohol duty revenue (£m)\n Rolling average of previous 12 months\nadjusted to Dec 2019 prices")+
  scale_y_continuous(name="Duty rate levied per unit of alcohol\nRolling average of previous 12 months")+
  guides(alpha=FALSE)+
  labs(title="Association between alcohol duty rates and HMRC revenue",
       caption="Data from HMRC & ONS | Plot by @VictimOfMaths")
dev.off()

#Graph of drink-specific duty take (real terms)
tiff("Outputs/HMRCDutyRateReal.tiff", units="in", width=8, height=6, res=300)
ggplot(data)+
  geom_line(aes(x=Month, y=dutyrate.roll, colour=drink))+
  theme_classic()+
  scale_y_continuous(name="Average duty rate payable per unit\n Rolling average of previous 12 months\nadjusted to Dec 2019 prices",
                     limits=c(0,0.4), labels=c("0", "10p", "20p", "30p", "40p"))+
  scale_x_date(name="", date_breaks="12 month", labels=datelabs)+
  scale_colour_manual(values=c("#ffc000", "#00b050", "#00b0f0","#7030a0"), name="")+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))+
  labs(title="Changes in UK alcohol duty rates over time", subtitle="After adjusting for inflation",
       caption="Data from HMRC & ONS | Plot by @VictimOfMaths")
dev.off()

#Long-term graph of duty rates
tiff("Outputs/HMRCDutyRateCashLong.tiff", units="in", width=8, height=6, res=300)
ggplot(dutyrates_long, aes(x=Month, y=CashRate, colour=Type))+
  geom_line()+
  theme_classic()+
  scale_y_continuous(name="Average duty rate payable per unit", labels=c("0", "10p", "20p", "30p"))+
  scale_x_date(name="", date_breaks="36 month", labels=c("", seq(from=1949, to=2018, by=3), "", ""))+
  scale_colour_manual(values=c("#ffc000", "#00b050", "#00b0f0","#7030a0"), name="")+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))+
  labs(title="Changes in UK alcohol duty rates over time", subtitle="Without adjusting for inflation",
       caption="Data from HMRC| Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/HMRCDutyRateRealLong.tiff", units="in", width=8, height=6, res=300)
ggplot(dutyrates_long, aes(x=Month, y=RealRate, colour=Type))+
  geom_line()+
  theme_classic()+
  scale_y_continuous(name="Average duty rate payable per unit", labels=c("0", "20p", "40p", "60p", "80p"))+
  scale_x_date(name="", date_breaks="36 month", labels=c("", seq(from=1949, to=2018, by=3), "", ""))+
  scale_colour_manual(values=c("#ffc000", "#00b050", "#00b0f0","#7030a0"), name="")+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))+
  labs(title="Changes in UK alcohol duty rates over time", subtitle="After adjusting for inflation",
       caption="Data from HMRC| Plot by @VictimOfMaths")
dev.off()

#Mid-term graph
tiff("Outputs/HMRCDutyRateCash30yr.tiff", units="in", width=8, height=6, res=300)
ggplot(dutyrates_long, aes(x=Month, y=CashRate, colour=Type))+
  geom_line()+
  theme_classic()+
  scale_y_continuous(name="Average duty rate payable per unit", labels=c("0", "10p", "20p", "30p"))+
  scale_x_date(name="", limits=c(as.Date("1991-03-01"), as.Date("2020-01-01")))+
  scale_colour_manual(values=c("#ffc000", "#00b050", "#00b0f0","#7030a0"), name="")+
  labs(title="Changes in UK alcohol duty rates over time", subtitle="Without adjusting for inflation",
       caption="Data from HMRC| Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/HMRCDutyRateReal30yr.tiff", units="in", width=8, height=6, res=300)
ggplot(dutyrates_long, aes(x=Month, y=RealRate, colour=Type))+
  geom_line()+
  theme_classic()+
  scale_y_continuous(name="Average duty rate payable per unit", limits=c(0,0.45), 
                     breaks=c(0,0.1,0.2,0.3,0.4), labels=c("0", "10p", "20p", "30p", "40p"))+
  scale_x_date(name="", limits=c(as.Date("1991-03-01"), as.Date("2020-01-01")))+
  scale_colour_manual(values=c("#ffc000", "#00b050", "#00b0f0","#7030a0"), name="")+
  labs(title="Changes in UK alcohol duty rates over time", subtitle="Without adjusting for inflation",
       caption="Data from HMRC| Plot by @VictimOfMaths")
dev.off()
