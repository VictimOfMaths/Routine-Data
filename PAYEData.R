rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(scales)
library(lubridate)
library(extrafont)
library(ragg)
library(ggtext)
library(paletteer)
library(scales)
library(ggrepel)
library(sf)

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

#Download ONS PAYE data
temp <- tempfile()
wardurl <- "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/realtimeinformationstatisticsreferencetableseasonallyadjusted/current/rtisajul2022.xlsx"
temp <- curl_download(url=wardurl, destfile=temp, quiet=FALSE, mode="wb")

#Pay distribution data
distdata <- read_excel(temp, sheet="5. Pay distribution (UK)", range="A6:H99") %>% 
  gather(Percentile, Pay, c(2:8)) %>% 
  mutate(Date=as.Date(paste("1", Date), format="%d %B %Y")) %>% 
  group_by(Percentile) %>% 
  mutate(Pay_Indexed=Pay/Pay[Date==as.Date("2020-01-01")]) %>% 
  ungroup()

agg_png("Outputs/PAYEDistribution.png", units="in", width=9, height=6, res=800)
ggplot(distdata %>% filter(Date>as.Date("2019-12-30")), 
       aes(x=Date, y=Pay_Indexed-1, colour=Percentile))+
  geom_line()+
  scale_x_date(name="", labels=c("", "Jan\n2020", "July\n2020", "Jan\n2021", "July\n2021", "Jan\n2022", ""))+
  scale_y_continuous(name="Change in pay since January 2020", label=label_percent(accuracy=1))+
  scale_colour_paletteer_d("rcartocolor::Geyser", name="", direction=-1)+
  theme_custom()+
  theme(panel.grid.major.y = element_line(colour="Grey93"),
        plot.title=element_markdown())+
  labs(title="Wage growth is accelerating in <span style='color:#008080;'>high earners</span> and stagnating in <span style='color:#CA562C;'>low earners</span>",
       subtitle="3-month rolling average monthly pay from PAYE data across the pay distribution, seasonally adjusted",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

annualdistdata <- distdata %>% 
  group_by(Percentile) %>% 
  mutate(paygrowth=(Pay-lag(Pay, 12))/Pay) %>% 
  ungroup()

agg_png("Outputs/PAYEGrowth.png", units="in", width=9, height=6, res=800)
ggplot(annualdistdata, aes(x=Date, y=paygrowth, colour=Percentile))+
  geom_hline(yintercept=0, colour="Grey40")+
  geom_line()+
  scale_x_date(name="", labels=c("", "Jan\n2016",  "Jan\n2018", "Jan\n2020", "Jan\n2022", ""),
               limits=c(as.Date("2015-09-01"), NA_Date_))+
  scale_y_continuous(name="Annual relative pay growth", label=label_percent(accuracy=1))+
  scale_colour_paletteer_d("rcartocolor::Geyser", name="", direction=-1)+
  theme_custom()+
  theme(panel.grid.major.y = element_line(colour="Grey93"), plot.title=element_markdown())+
  labs(title="Current wage growth is highest in <span style='color:#008080;'>high earners</span> and lowest in <span style='color:#CA562C;'>low earners</span>",
       subtitle="Annual pay growth from PAYE data across the pay distribution, seasonally adjusted",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

#long-term cumulative
longdistdata <- read_excel(temp, sheet="5. Pay distribution (UK)", range="A6:H99") %>% 
  gather(Percentile, Pay, c(2:8)) %>% 
  mutate(Date=as.Date(paste("1", Date), format="%d %B %Y")) %>% 
  group_by(Percentile) %>% 
  mutate(Pay_Indexed=Pay/Pay[Date==as.Date("2014-09-01")]) %>% 
  ungroup()

agg_png("Outputs/PAYEDistributionlong.png", units="in", width=9, height=6, res=800)
ggplot(longdistdata %>% filter(Date>as.Date("2014-08-01")), 
       aes(x=Date, y=Pay_Indexed-1, colour=Percentile))+
  geom_line()+
  scale_x_date(name="", labels=c("", "Jan\n2016",  "Jan\n2018", "Jan\n2020", "Jan\n2022", ""))+
  scale_y_continuous(name="Change in pay since September 2014", label=label_percent(accuracy=1))+
  scale_colour_paletteer_d("rcartocolor::Geyser", name="", direction=-1)+
  theme_custom()+
  theme(panel.grid.major.y = element_line(colour="Grey93"),
        plot.title=element_markdown())+
  labs(title="Cumulative wage growth since Sept 2014 is highest in <span style='color:#CA562C;'>low earners</span>,<br>but <span style='color:#008080;'>high earners</span> are catching up",
       subtitle="3-month rolling average monthly pay from PAYE data across the pay distribution, seasonally adjusted",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

#Mean pay data by industry
inddata <- read_excel(temp, sheet="25. Mean pay (Industry)", range="A7:V102") %>% 
  gather(Industry, Pay, c(2:22)) %>% 
  mutate(Date=as.Date(paste("1", Date), format="%d %B %Y")) %>% 
  group_by(Industry) %>% 
  mutate(Pay_Indexed=Pay/Pay[Date==as.Date("2020-01-01")]) %>% 
  ungroup() %>% 
  mutate(group=case_when(
    Industry %in% c("Finance and insurance", "Information and communication",
                    "Administrative and support services", "Professional, scientific and technical",
                    "Energy production and supply") ~ "Higher",
    Industry %in% c("Construction", "Water supply, sewerage and waste", "Mining and quarrying",
                    "Arts, entertainment and recreation",
                    "Accommodation and food service activities") ~ "Lower",
    Industry %in% c("Agriculture, forestry and fishing", 
                    "Public administration and defence; social security", "Education") ~ "LowerMid",
    Industry=="UK" ~ "UK", 
    TRUE ~ NA_character_),
    Industry=case_when(
      Industry=="Accommodation and food service activities" ~ "Accommodation and food service", 
      Industry=="UK" ~ "UK average",
      Industry=="Public administration and defence; social security" ~ "Public administration and defence",
      TRUE ~ Industry))

agg_png("Outputs/PAYExIndustryWinners.png", units="in", width=9, height=6, res=800)
ggplot()+
  geom_line(data=inddata %>% filter(Date>as.Date("2019-12-30")), 
            aes(x=Date, y=Pay_Indexed-1, group=Industry), colour="Grey80")+
  geom_line(data=inddata %>% filter(Date>as.Date("2019-12-30") & Industry=="UK average"), 
            aes(x=Date, y=Pay_Indexed-1, group=Industry), colour="Black", linetype=2)+
  geom_line(data=inddata %>% filter(Date>as.Date("2019-12-30") & group=="Higher"), 
            aes(x=Date, y=Pay_Indexed-1, colour=Industry), show.legend = FALSE)+
  geom_text_repel(data=inddata %>% filter(Date==max(Date) & group=="Higher"),
                  aes(x=max(Date), y=Pay_Indexed-1, label = Industry, 
                      colour=Industry),
                  family = "Lato", direction = "y", xlim = c(as.Date("2022-03-10"), NA),
                  hjust = 0, segment.color = NA, box.padding = .3, show.legend = FALSE)+
  geom_text_repel(data=inddata %>% filter(Date==max(Date) & Industry=="UK average"),
                  aes(x=max(Date), y=Pay_Indexed-1), colour="Black", label="UK average",
                  family = "Lato", direction = "y", xlim = c(as.Date("2022-03-10"), NA),
                  hjust = 0, segment.color = NA, box.padding = .3, show.legend = FALSE)+
  scale_x_date(name="", limits=c(as.Date("2020-01-01"), as.Date("2023-03-01")),
               labels=c("", "2020", "2021", "2022", "", ""))+
  scale_y_continuous(name="Change in pay since January 2020", label=label_percent(accuracy=1))+
  scale_colour_paletteer_d("ggthemes::Classic_Green_Orange_6", name="")+
  theme_custom()+
  theme(panel.grid.major.y = element_line(colour="Grey93"))+
  labs(title="Recent wage growth is highest in fields that weren't as badly hit in the early pandemic",
       subtitle="Mean monthly pay from PAYE data by industry, seasonally adjusted",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

agg_png("Outputs/PAYExIndustryLosers.png", units="in", width=9, height=6, res=800)
ggplot()+
  geom_line(data=inddata %>% filter(Date>as.Date("2019-12-30")), 
            aes(x=Date, y=Pay_Indexed-1, group=Industry), colour="Grey80")+
  geom_line(data=inddata %>% filter(Date>as.Date("2019-12-30") & Industry=="UK average"), 
            aes(x=Date, y=Pay_Indexed-1, group=Industry), colour="Black", linetype=2)+
  geom_line(data=inddata %>% filter(Date>as.Date("2019-12-30") & group=="Lower"), 
            aes(x=Date, y=Pay_Indexed-1, colour=Industry), show.legend = FALSE)+
  geom_text_repel(data=inddata %>% filter(Date==max(Date) & group  %in% c("Lower", "UK")),
                  aes(x=max(Date), y=Pay_Indexed-1, label = Industry, 
                      colour=Industry),
                  family = "Lato", direction = "y", xlim = c(as.Date("2022-03-10"), NA),
                  hjust = 0, segment.color = NA, box.padding = .3, show.legend = FALSE)+
  scale_x_date(name="", limits=c(as.Date("2020-01-01"), as.Date("2023-03-01")),
               labels=c("", "2020", "2021", "2022", "", ""))+
  scale_y_continuous(name="Change in pay since January 2020", label=label_percent(accuracy=1))+
  scale_colour_manual(values=c("#FF0066", "#107F80", "#40007F", "#AA66FF", "#000000", "#66CCFE"), name="")+
  theme_custom()+
  theme(panel.grid.major.y = element_line(colour="Grey93"))+
  labs(title="Industries hardest hit in early 2020 have seen below-average wage growth since",
       subtitle="Mean monthly pay from PAYE data by industry, seasonally adjusted",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

agg_png("Outputs/PAYExIndustryStagnators.png", units="in", width=9, height=6, res=800)
ggplot()+
  geom_line(data=inddata %>% filter(Date>as.Date("2019-12-30")), 
            aes(x=Date, y=Pay_Indexed-1, group=Industry), colour="Grey80")+
  geom_line(data=inddata %>% filter(Date>as.Date("2019-12-30") & Industry=="UK average"), 
            aes(x=Date, y=Pay_Indexed-1, group=Industry), colour="Black", linetype=2)+
  geom_line(data=inddata %>% filter(Date>as.Date("2019-12-30") & group=="LowerMid"), 
            aes(x=Date, y=Pay_Indexed-1, colour=Industry), show.legend = FALSE)+
  geom_text_repel(data=inddata %>% filter(Date==max(Date) & group  %in% c("LowerMid", "UK")),
                  aes(x=max(Date), y=Pay_Indexed-1, label = Industry, 
                      colour=Industry),
                  family = "Lato", direction = "y", xlim = c(as.Date("2022-03-10"), NA),
                  hjust = 0, segment.color = NA, box.padding = .3, show.legend = FALSE)+
  scale_x_date(name="", limits=c(as.Date("2020-01-01"), as.Date("2023-03-01")),
               labels=c("", "2020", "2021", "2022", "", ""))+
  scale_y_continuous(name="Change in pay since January 2020", label=label_percent(accuracy=1))+
  scale_colour_manual(values=c("#009ADA", "#66A64F", "#FDD10A", "Black"), name="")+
  theme_custom()+
  theme(panel.grid.major.y = element_line(colour="Grey93"))+
  labs(title="Some sectors that saw little change in wages in early 2020 have seen little change since",
       subtitle="Mean monthly pay from PAYE data by industry, seasonally adjusted",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

#Download NUTS3 region data
NUTS3data <- read_excel(temp, sheet="17. Mean pay (NUTS3)", range="A7:FX102") %>% 
  gather(NUTS3NM, Pay, c(2:180)) %>% 
  mutate(Date=as.Date(paste("1", Date), format="%d %B %Y")) %>% 
  group_by(NUTS3NM) %>% 
  mutate(Pay_Indexed=Pay/Pay[Date==as.Date("2020-01-01")]) %>% 
  ungroup() %>% 
  #Bring in NUTS3 codes
  merge(as.data.frame(t(read_excel(temp, sheet="17. Mean pay (NUTS3)", range="B6:FX7", col_names=FALSE))) %>% 
          set_names("NUTS3CD", "NUTS3NM"))

#Download NUTS3 shapefile
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/api/v3/datasets/473aefdcee19418da7e5dbfdeacf7b90_1/downloads/data?format=shp&spatialRefId=27700"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

shapefile_NUTS3 <- st_read(file.path(temp2,"NUTS_Level_3_(January_2018)_Boundaries.shp")) %>% 
  rename("NUTS3CD"="nuts318cd")

paymap <- left_join(shapefile_NUTS3, NUTS3data %>% filter(Date==max(Date)))

agg_png("Outputs/PAYEMeanPayMap.png", units="in", width=9, height=10, res=800)
ggplot(paymap, aes(geometry=geometry, fill=Pay))+
  geom_sf(colour=NA)+
  scale_fill_paletteer_c("viridis::mako", limits=c(0,NA), direction=-1)+
  theme_void()
dev.off()

paychangemap <- left_join(shapefile_NUTS3, NUTS3data %>% filter(Date==max(Date)))

agg_png("Outputs/PAYEMeanPayChangeMap.png", units="in", width=7, height=8, res=800)
ggplot(paychangemap, aes(geometry=geometry, fill=Pay_Indexed-1))+
  geom_sf(colour=NA)+
  scale_fill_paletteer_c("viridis::mako", direction=-1, limits=c(0,NA), 
                         labels=label_percent(accuracy=1), name="Change in mean\npay since\nJan 2020")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(2), hjust=0,
                                 margin=margin(0,0,5.5,0)),
         text=element_text(family="Lato"),
         plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
         plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),
         legend.text=element_text(colour="Grey40"),
         legend.title=element_text(colour="Grey20"))+
  labs(title="Wages are rising fastest in the South East",
       subtitle="Change in mean monthly PAYE pay since January 2020 by NUTS3 region,\nseasonally adjusted",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()
