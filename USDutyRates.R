rm(list=ls())

library(readxl)
library(curl)
library(tidyverse)
library(lubridate)
library(ragg)

#Historic rates taken from: https://www.ttb.gov/tax-audit/historical-tax-rates
#Current rates taken from: https://www.ttb.gov/tax-audit/tax-and-fee-rates

beer <- data.frame(Date=as.Date(c("01/09/1862", "03/03/1863", "01/04/1864", "14/06/1898", "01/07/1901",
                          "01/07/1902", "23/10/1914", "04/10/1917", "25/02/1919", "12/01/1934",
                          "01/07/1940", "01/11/1942", "01/04/1944", "01/11/1951", "01/01/1977",
                          "01/01/1991", "01/01/2018"), format="%d/%m/%Y"),
                   Rate=c(1, 0.6, 1, 2, 1.6, 1, 1.5, 3, 6, 5, 6, 7, 8, 9, 9, 18, 16))

wine <- data.frame(Date=as.Date(c("09/09/1916", "25/02/1919", "29/06/1928", "12/01/1934", "26/06/1936",
                                  "01/07/1940", "01/10/1941", "01/11/1942", "01/04/1944", "01/11/1951",
                                  "01/01/1991", "01/01/2018"), format="%d/%m/%Y"),
                   Rate=c(0.04, 0.16, 0.04, 0.1, 0.05, 0.06, 0.08, 0.1, 0.15, 0.17, 1.07, 1.07))

spirits <- data.frame(Date=as.Date(c("01/08/1862", "07/03/1864", "01/07/1864", "01/01/1865", "20/07/1868",
                                     "06/06/1872", "03/03/1875", "27/08/1894", "04/10/1917", "25/02/1919",
                                     "01/01/1927", "01/01/1928", "12/01/1934", "01/07/1938", "01/07/1940",
                                     "01/10/1941", "01/11/1942", "01/04/1944", "01/11/1951", "01/10/1985",
                                     "01/01/1991", "01/01/2018"), format="%d/%m/%Y"),
                      Rate=c(0.2, 0.6, 1.5, 2, 0.5, 0.7, 0.9, 1.1, 2.2, 2.2, 1.65, 1.1, 2, 2.25, 3, 4,
                             6, 9, 10.5, 12.5, 13.5, 13.5))

#Set up date framework
dates <- data.frame(Date=seq.Date(from=as.Date("1862-08-01"), to=as.Date("2021-03-10"),
                                  by="days"))

BeerABV <- 0.05
WineABV <- 0.125

#Merge in rates
data <- merge(dates, beer, all.x=TRUE) %>% 
  rename(Beer.Rate=Rate) %>% 
  merge(wine, all.x=TRUE) %>% 
  rename(Wine.Rate=Rate) %>% 
  merge(spirits, all.x=TRUE) %>% 
  rename(Spirit.Rate=Rate) %>% 
  fill(c(Beer.Rate, Wine.Rate, Spirit.Rate)) %>%
  #Collapse to monthly data
  mutate(year=year(Date), month=month(Date)) %>% 
  group_by(year, month) %>% 
  summarise(Beer.Rate=mean(Beer.Rate, na.rm=TRUE),
            Wine.Rate=mean(Wine.Rate, na.rm=TRUE),
            Spirit.Rate=mean(Spirit.Rate, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(Date=as.Date(paste0(year, "-", month, "-01"))) %>% 
  #Convert to rates per US standard drink (14g or ethanol ~ 17.7ml) 
  #Beer rates are calculated as 'per barrel', where a barrel = 31 gallons
  mutate(Beer.PPU=Beer.Rate/(BeerABV*31*3.78/(17.7/1000)),
         #Wine rates are calculated as 'per wine gallon' which = 3.78 litres
         Wine.PPU=Wine.Rate/(WineABV*3.78/(17.7/1000)),
         #Spirits rates are calculated as 'per proof gallon' which = 3.78 litres @ 50% ABV,
         Spirits.PPU=Spirit.Rate/(3.78*0.5/(17.7/1000))) %>% 
  select(-c(1,2)) %>% 
  #Reshape data
  pivot_longer(c(1:3, 5:7), names_to=c("drink", "metric"), names_sep="\\.", values_to="values")

ggplot(data %>% filter(metric=="PPU"), aes(x=Date, y=values, colour=drink))+
  geom_line()

#Bring in inflation - data only available back to 1917 - available in nice format from FRED
temp <- tempfile()
url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CPIAUCNS&scale=left&cosd=1913-01-01&coed=2021-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2021-03-10&revision_date=2021-03-10&nd=1913-01-01"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

CPIdata <- read.csv(temp) %>% 
  rename(Date=1, CPI=2) %>% 
  mutate(Date=as.Date(Date),
         inflator=CPI[length(CPI)]/CPI)

data <- merge(data, CPIdata, all.x=TRUE) %>% 
  mutate(values_adj=values*inflator) %>% 
  select(-c(CPI, inflator))

agg_tiff("Outputs/USDutyCash.tiff", units="in", width=8, height=6, res=500)
ggplot(data %>% filter(metric=="PPU"))+
  geom_rect(aes(xmin=as.Date("1920-01-01"), xmax=as.Date("1933-12-31"), ymin=0, ymax=0.15),
            fill="Grey80")+
  geom_line(aes(x=Date, y=values, colour=drink))+
  scale_x_date(name="")+
  scale_y_continuous(name="Average federal tax rate per US Standard Drink (14g alcohol)",
                     breaks=c(0,0.05, 0.1,0.15), labels=c("0c", "5c", "10c", "15c"))+
  scale_colour_manual(name="", values=c("#ffc000", "#00b0f0", "#7030a0"))+
  annotate("text", x=as.Date("1927-01-01"), y=0.13, label="Prohibition", size=2.5)+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.4)))+
  labs(title="US alcohol taxes have changed very little since the 1950s",
       subtitle="Average federal tax rates, excluding relief for small brewers",
       caption="Data from Alcohol & Tobacco Tax and Trade Bureau\nPlot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/USDutyReal.tiff", units="in", width=8, height=6, res=500)
ggplot(data %>% filter(metric=="PPU"))+
  geom_rect(aes(xmin=as.Date("1920-01-01"), xmax=as.Date("1933-12-31"), ymin=0, ymax=1.3),
            fill="Grey80")+
  geom_line(aes(x=Date, y=values_adj, colour=drink))+
  scale_x_date(name="", limits=c(as.Date("1913-01-01"), NA))+
  scale_y_continuous(name="Average federal tax rate per US Standard Drink (14g alcohol)",
                     breaks=c(0,0.25, 0.5, 0.75, 1, 1.25), 
                     labels=c("0c", "25c", "50c", "75c","$1", "$1.25"))+
  scale_colour_manual(name="", values=c("#ffc000", "#00b0f0", "#7030a0"))+
  annotate("text", x=as.Date("1927-01-01"), y=1.2, label="Prohibition")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.4)))+
  labs(title="US alcohol taxes have been falling for half a century",
       subtitle="Average federal tax rates, adjusted to January 2021 prices",
       caption="Data from Alcohol & Tobacco Tax and Trade Bureau\nPlot by @VictimOfMaths")
dev.off()

