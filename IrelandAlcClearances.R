rm(list=ls())

library(curl)
library(tidyverse)
library(lubridate)
library(paletteer)

#Bring in quarterly data
temp <- tempfile()
source <- "https://www.revenue.ie/en/corporate/documents/statistics/excise/quarterly-alcohol-breakdown.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

qdata <- read.csv(temp)
colnames(qdata) <- c("Year", "Quarter", "Drink", "Clearances", "ClearanceMetric", "Revenue")

#Make ABV assumptions
WineABV <- 0.125
CiderABV <- 0.05

#Convert product volumes to ethanol volumes for wine and cider
qdata <- qdata %>% 
  mutate(Clearances=case_when(
    Drink=="Wine" ~ Clearances*WineABV,
    Drink=="Cider" ~ Clearances*CiderABV,
    TRUE ~ Clearances
  ))

#Create total category
qdata <- qdata %>% 
  group_by(Quarter, Year) %>% 
  summarise(Clearances=sum(Clearances), Revenue=sum(Revenue)) %>% 
  ungroup() %>% 
  mutate(Drink="Total") %>% 
  bind_rows(qdata)

#2020 duty takes vs. 5-year ranges
AlcVol <- qdata %>%
  filter(Year %in% 2015:2019) %>%
  group_by(Quarter, Drink) %>%
  summarise(alcvolmax=max(Clearances), alcvolmin=min(Clearances), alcvolmean=mean(Clearances),
            revmax=max(Revenue), revmin=min(Revenue), revmean=mean(Revenue)) %>% 
  ungroup()

AlcVol.2020 <- qdata %>%
  filter(Year==2020) 

AlcVol <- merge(AlcVol, AlcVol.2020, by=c("Quarter", "Drink"), all.x=TRUE)

#Calculate overall changes from historic values
loss <- AlcVol %>% 
  mutate(alcvolchange=Clearances-alcvolmean,
         revchange=Revenue-revmean) %>% 
  group_by(Drink) %>% 
  summarise(alcvolchange=sum(alcvolchange, na.rm=TRUE), alchist=sum(alcvolmean),
            alcvolprop=alcvolchange/alchist, 
            revchange=sum(revchange, na.rm=TRUE), revhist=sum(revmean),
            revprop=revchange/revhist) %>% 
  mutate(vollabs=if_else(alcvolchange>0, paste0("+", round(alcvolchange/1000000, 1), " million litres (+",
                                                round(alcvolprop*100, 1), "%)"),
                         paste0(round(alcvolchange/1000000, 1), " million litres (",
                                round(alcvolprop*100, 1), "%)")),
         revlabs=if_else(revchange>0, paste0("+€", round(revchange, 1), " million (+",
                                             round(revprop*100, 1), "%) revenue from alcohol duty"),
                         paste("-€", abs(round(revchange, 1)), " million (",
                               round(revprop*100, 1), "%) revenue from alcohol duty"))) %>% 
  ungroup()

#Visualise
#Totals
tiff("Outputs/IrelandClearancesExcess.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(AlcVol, Drink=="Total"))+
  geom_ribbon(aes(x=Quarter, ymin=alcvolmin/1000000, ymax=alcvolmax/1000000, group=Drink), 
              fill="Skyblue2")+
  geom_line(aes(x=Quarter, y=alcvolmean/1000000, group=Drink), linetype=2, colour="Grey50")+
  geom_ribbon(aes(x=Quarter, ymin=Clearances/1000000, ymax=alcvolmean/1000000, group=Drink), 
              fill="Red", alpha=0.2)+
  geom_line(aes(x=Quarter, y=Clearances/1000000, group=Drink), colour="Red")+
  scale_y_continuous(name="Estimated monthly ethanol clearances\n(millions of litres)", limits=c(0,NA))+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.5)))+
  annotate("text", x=4, y=11, colour="Red", label="2020")+
  annotate("text", x=1.7, y=8, colour="Skyblue4", label="2015-19 range")+
  annotate("text", x=1.9, y=11.5, colour="Grey50", label="2015-19 mean")+
  geom_curve(aes(x=1.6, y=8.2, xend=1.5, yend=8.85), colour="Skyblue4", curvature=0.25,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  geom_curve(aes(x=1.85, y=11.3, xend=1.8, yend=10.1), colour="grey30", curvature=0.15,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  annotate("text", x=3, y=7, colour="Red", label=paste0(loss[4,8], " compared to 2015-19"))+
  labs(title="Total alcohol sales in Ireland fell slightly in 2020",
       subtitle="Quarterly volumes of pure alcohol cleared for sale.\nData for Cider and Wine is estimated from reported product volumes based on ABV assumptions",
       caption="Data from Revenue.ie | Plot by @VictimOfMaths")
dev.off()

#By product
tiff("Outputs/IrelandClearancesExcessxDrink.tiff", units="in", width=10, height=8, res=500)
ggplot(subset(AlcVol, Drink!="Total"))+
  geom_ribbon(aes(x=Quarter, ymin=alcvolmin/1000000, ymax=alcvolmax/1000000, group=Drink), 
              fill="Skyblue2")+
  geom_line(aes(x=Quarter, y=alcvolmean/1000000, group=Drink), linetype=2, colour="Grey50")+
  geom_ribbon(aes(x=Quarter, ymin=Clearances/1000000, ymax=alcvolmean/1000000, group=Drink), 
              fill="Red", alpha=0.2)+
  geom_line(aes(x=Quarter, y=Clearances/1000000, group=Drink), colour="Red")+
  geom_text(data=subset(loss, Drink!="Total"), 
            aes(x=c(3,3,2,3), y=c(3.2,1.8,3,2), label=vollabs), colour="Red")+
  scale_y_continuous(name="Estimated monthly ethanol clearances\n(millions of litres)", 
                     limits=c(0,NA))+
  facet_wrap(~Drink)+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.5)),
        strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Beer and cider sales fell at the expense of wine and spirits",
       subtitle="Quarterly volumes of pure alcohol cleared for sale in the Republic of Ireland.\nData for Cider and Wine is estimated from reported product volumes based on ABV assumptions",
       caption="Data from Revenue.ie | Plot by @VictimOfMaths")
dev.off()
