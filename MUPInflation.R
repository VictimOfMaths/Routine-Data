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

#Bring in RPI data
temp <- tempfile()
url <- "https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/czeq/mm23&series=&fromMonth=01&fromYear=1950&toMonth=08&toYear=2023&frequency=months"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

RPIdata <- read.csv(temp)[-c(1:5),] 
colnames(RPIdata) <- c("Date", "RPI")

temp <- tempfile()
url <- "https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/l59c/mm23"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

CPIdata <- read.csv(temp)[-c(1:5),] 
colnames(CPIdata) <- c("Date", "CPIH")

Data <- merge(RPIdata, CPIdata) %>% 
  mutate(Date=as.Date(paste0(Date, " 1"), "%Y %b %d"),
         RPI=(as.numeric(as.character(RPI))+100)/100,
         CPIH=(as.numeric(as.character(CPIH))+100)/100,
         indexRPI=cumprod(RPI),
         indexCPIH=cumprod(CPIH),
         inflatorRPI=indexRPI[length(indexRPI)]/indexRPI,
         inflatorCPIH=indexCPIH[length(indexCPIH)]/indexCPIH)

Plotdata <- Data %>% filter(Date>=as.Date("2018-05-01")) %>% 
  mutate(valueRPI=50*inflatorRPI/inflatorRPI[Date==as.Date("2018-05-01")],
         valueCPIH=50*inflatorCPIH/inflatorCPIH[Date==as.Date("2018-05-01")])

Plotdata %>% gather(Metric, Value, c(8,9)) %>% 
ggplot(aes(x=Date, y=Value, colour=Metric))+
  geom_hline(yintercept=50, colour="grey70")+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Real-terms value of a 50p MUP", 
                     breaks=c(40, 45, 50), labels=c("40p", "45p", "50p"))+
  scale_colour_manual(values=c("darkred", "skyblue"), labels=c("CPIH", "RPI"))+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="grey95"))

