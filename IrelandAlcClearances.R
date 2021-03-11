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

#Bring in monthly revenue data
temp <- tempfile()
source <- "https://www.revenue.ie/en/corporate/documents/statistics/excise/alcohol-and-tobacco-net-monthly-receipts.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

mdata <- read.csv(temp) %>% 
  gather(Year, Revenue, c(3:11)) %>% 
  rename(Product=1) %>% 
  mutate(Revenue=as.numeric(if_else(Revenue=="n/a", NA_character_, Revenue)),
         Year=as.integer(substr(Year, 2, 5)),
         month=factor(month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                                      "Oct", "Nov", "Dec")))

#Add total alcohol revenue
mdata <- mdata %>% 
  filter(Product!="Cigarettes") %>% 
  group_by(Year, month) %>% 
  summarise(Revenue=sum(Revenue)) %>% 
  ungroup() %>% 
  mutate(Product="TotalAlc") %>% 
  bind_rows(mdata)

#Bring in inflation
temp <- tempfile()
source <- "https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.PxAPIv1/en/28/CPIM/CPM01?query=%7B%22query%22:%5B%7B%22code%22:%22STATISTIC%22,%22selection%22:%7B%22filter%22:%22item%22,%22values%22:%5B%22CPM01C03%22%5D%7D%7D,%7B%22code%22:%22TLIST(M1)%22,%22selection%22:%7B%22filter%22:%22item%22,%22values%22:%5B%22202012%22,%22202011%22,%22202010%22,%22202009%22,%22202008%22,%22202007%22,%22202006%22,%22202005%22,%22202004%22,%22202003%22,%22202002%22,%22202001%22,%22201912%22,%22201911%22,%22201910%22,%22201909%22,%22201908%22,%22201907%22,%22201906%22,%22201905%22,%22201904%22,%22201903%22,%22201902%22,%22201901%22,%22201812%22,%22201811%22,%22201810%22,%22201809%22,%22201808%22,%22201807%22,%22201806%22,%22201805%22,%22201804%22,%22201803%22,%22201802%22,%22201801%22,%22201712%22,%22201711%22,%22201710%22,%22201709%22,%22201708%22,%22201707%22,%22201706%22,%22201705%22,%22201704%22,%22201703%22,%22201702%22,%22201701%22,%22201612%22,%22201611%22,%22201610%22,%22201609%22,%22201608%22,%22201607%22,%22201606%22,%22201605%22,%22201604%22,%22201603%22,%22201602%22,%22201601%22,%22201512%22,%22201511%22,%22201510%22,%22201509%22,%22201508%22,%22201507%22,%22201506%22,%22201505%22,%22201504%22,%22201503%22,%22201502%22,%22201501%22,%22201412%22,%22201411%22,%22201410%22,%22201409%22,%22201408%22,%22201407%22,%22201406%22,%22201405%22,%22201404%22,%22201403%22,%22201402%22,%22201401%22,%22201312%22,%22201311%22,%22201310%22,%22201309%22,%22201308%22,%22201307%22,%22201306%22,%22201305%22,%22201304%22,%22201303%22,%22201302%22,%22201301%22,%22201212%22,%22201211%22,%22201210%22,%22201209%22,%22201208%22,%22201207%22,%22201206%22,%22201205%22,%22201204%22,%22201203%22,%22201202%22,%22201201%22%5D%7D%7D,%7B%22code%22:%22C01779V03424%22,%22selection%22:%7B%22filter%22:%22item%22,%22values%22:%5B%22-%22%5D%7D%7D%5D,%22response%22:%7B%22format%22:%22csv%22,%22pivot%22:null%7D%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

CPIdata <- as.data.frame(t(read.csv(temp))) %>% 
  slice(4:n()) %>% 
  mutate(Year=rep(2012:2020, each=12),
         month=rep(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                                      "Oct", "Nov", "Dec"), each=1, times=9),
         V1=as.numeric(as.character(V1)),
         inflator=V1[length(V1)]/V1)

mdata <- merge(mdata, CPIdata) %>% 
  mutate(Revenue_adj=Revenue*inflator)

#2020 duty takes vs. 5-year ranges
AlcRev <- mdata %>%
  filter(Year %in% 2015:2019) %>%
  group_by(month, Product) %>%
  summarise(revmax=max(Revenue_adj), revmin=min(Revenue_adj), 
            revmean=mean(Revenue_adj)) %>% 
  ungroup()

AlcRev.2020 <- mdata %>%
  filter(Year==2020) 

AlcRev <- merge(AlcRev, AlcRev.2020, by=c("month", "Product"), all.x=TRUE)

#Calculate overall changes from historic values
lossRev <- AlcRev %>% 
  mutate(revchange=Revenue_adj-revmean) %>% 
  group_by(Product) %>% 
  summarise(revchange=sum(revchange, na.rm=TRUE), revhist=sum(revmean, na.rm=TRUE),
            revprop=revchange/revhist) %>% 
  mutate(revlabs=if_else(revchange>0, paste0("+€", round(revchange, 1), " million (+",
                                             round(revprop*100, 1), "%) revenue from alcohol duty"),
                         paste("-€", abs(round(revchange, 1)), " million (",
                               round(revprop*100, 1), "%) revenue from alcohol duty"))) %>% 
  ungroup()

#Visualise
#Totals
tiff("Outputs/IrelandRevenueExcess.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(AlcRev, Product=="TotalAlc"))+
  geom_ribbon(aes(x=month, ymin=revmin, ymax=revmax, group=Product), 
              fill="Skyblue2")+
  geom_line(aes(x=month, y=revmean, group=Product), linetype=2, colour="Grey50")+
  geom_ribbon(aes(x=month, ymin=Revenue_adj, ymax=revmean, group=Product), 
              fill="Red", alpha=0.2)+
  geom_line(aes(x=month, y=Revenue_adj, group=Product), colour="Red")+
  scale_x_discrete(name="")+
  scale_y_continuous(name="Alcohol duty revenue collected (€m)", limits=c(0,NA))+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.5)))+
  annotate("text", x=9, y=80, colour="Red", label="2020")+
  annotate("text", x=5, y=75, colour="Skyblue4", label="2015-19 range")+
  annotate("text", x=2.5, y=90, colour="Grey50", label="2015-19 mean")+
  geom_curve(aes(x=4.6, y=80, xend=4.4, yend=93), colour="Skyblue4", curvature=-0.25,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  geom_curve(aes(x=2.7, y=85, xend=3.2, yend=80), colour="grey30", curvature=0.15,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  annotate("text", x=6, y=135, colour="Red", label=paste0(lossRev[5,5], " compared to 2015-19"))+
  labs(title="Total government revenue from alcohol duty in Ireland fell in 2020",
       subtitle="Monthly duty receipts reported by Revenue in 2020 compared to 2015-19.\nFigures are adjusted to December 2020 prices.",
       caption="Data from Revenue.ie and CSO.ie | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/IrelandRevenueExcessxDrink.tiff", units="in", width=10, height=8, res=500)
ggplot(subset(AlcRev, !Product %in% c("TotalAlc", "Cigarettes")))+
  geom_ribbon(aes(x=month, ymin=revmin, ymax=revmax, group=Product), 
              fill="Skyblue2")+
  geom_line(aes(x=month, y=revmean, group=Product), linetype=2, colour="Grey50")+
  geom_ribbon(aes(x=month, ymin=Revenue_adj, ymax=revmean, group=Product), 
              fill="Red", alpha=0.2)+
  geom_line(aes(x=month, y=Revenue_adj, group=Product), colour="Red")+
  geom_text(data=subset(lossRev, !Product %in% c("TotalAlc", "Cigarettes")), 
            aes(x=c(6,6,6,6), y=c(15,12,10,15), label=revlabs), colour="Red")+
  scale_x_discrete(name="")+
  scale_y_continuous(name="Alcohol duty revenue collected (€m)", 
                     limits=c(0,NA))+
  facet_wrap(~Product)+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.5)),
        strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Falls in beer and cider duty revenue have been largely offset by wine and spirits",
       subtitle="Monthly duty receipts in Ireland reported by Revenue in 2020 compared to 2015-19. Figures are adjusted to December 2020 prices.",
       caption="Data from Revenue.ie and CSO.ie  | Plot by @VictimOfMaths")
dev.off()

