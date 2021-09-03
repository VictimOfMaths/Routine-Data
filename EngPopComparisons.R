rm(list=ls())

library(tidyverse)
library(curl)
library(paletteer)
library(scales)
library(extrafont)
library(ragg)
library(readxl)
library(geofacet)

options(scipen=10000)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Download vax data
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/09/COVID-19-weekly-announced-vaccinations-02-September-2021.xlsx"
temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

vaxdata <- read_excel(temp, sheet="LTLA", range="C15:AJ321", col_names=FALSE) %>% 
  select(c(1, 4, 6:19, 21:34)) %>% 
  set_names(c("Region", "LTLACode", "<18_1st", "18-24_1st", "25-29_1st", "30-34_1st", "35-39_1st", 
              "40-44_1st", "45-49_1st", "50-54_1st", "55-59_1st", "60-64_1st", "65-69_1st", "70-74_1st", 
              "75-79_1st", "80+_1st", "<18_2nd", "18-24_2nd", "25-29_2nd", "30-34_2nd", "35-39_2nd", "40-44_2nd", 
              "45-49_2nd", "50-54_2nd", "55-59_2nd", "60-64_2nd", "65-69_2nd", "70-74_2nd", 
              "75-79_2nd", "80+_2nd")) %>% 
  pivot_longer(c(3:30), names_to=c("age", "dose"), names_sep="_", values_to="vaccinated")

#ONS 2020 denominators  
ONSpop <- read_excel(temp, sheet="Population estimates (ONS 2020)", range="L16:AA324", col_names=FALSE) %>% 
  select(-2) %>% 
  set_names(c("LTLACode", "<18", "18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
              "60-64", "65-69", "70-74", "75-79", "80+")) %>% 
  gather(age, ONSpop, c(2:15))

#MINS denominators  
NIMSpop <- read_excel(temp, sheet="Population estimates (NIMS)", range="D16:S324", col_names=FALSE) %>% 
  select(-2) %>% 
  set_names(c("LTLACode", "<18", "18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
              "60-64", "65-69", "70-74", "75-79", "80+")) %>% 
  gather(age, NIMSpop, c(2:15))

data <- vaxdata %>% 
  merge(ONSpop) %>% 
  merge(NIMSpop) %>% 
  group_by(dose, age, Region) %>% 
  summarise(vaccinated=sum(vaccinated), ONSpop=sum(ONSpop), NIMSpop=sum(NIMSpop)) %>% 
  ungroup() %>% 
  mutate(vaxpop_ONS=vaccinated/ONSpop, vaxpop_NIMS=vaccinated/NIMSpop) 

mygrid <- data.frame(name=c("North East", "North West", "Yorkshire and The Humber",
                            "West Midlands", "East Midlands", "East of England",
                            "South West", "London", "South East"),
                     row=c(1,2,2,3,3,3,4,4,4), col=c(2,1,2,1,2,3,1,2,3),
                     code=c(1:9))

agg_tiff("Outputs/ENGPopONSvsNIMSxAgexRegion.tiff", units="in", width=9, height=10, res=800)
ggplot(data %>% filter(dose=="1st"))+
  geom_segment(aes(x=vaxpop_ONS, xend=vaxpop_NIMS, y=age, yend=age, colour=age), show.legend=FALSE)+
  geom_point(aes(x=vaxpop_ONS, y=age))+
  geom_point(aes(x=vaxpop_NIMS, y=age), shape=21, fill="white")+
  scale_x_continuous(name="Proportion who have received both vaccine doses",
                     labels=label_percent(accuracy=1), breaks=seq(0, 1, by=0.2))+
  scale_y_discrete(name="Age")+
  scale_colour_viridis_d(option="mako", direction=-1)+
  facet_geo(~Region, grid=mygrid)+
  theme_custom()+
  theme(panel.grid.major.x=element_line(colour="Grey95"))+
  labs(title="Even within age groups, population size is more uncertain in London",
       subtitle="2 dose vaccine coverage by age using population estimates from NIMS (hollow circles) and ONS (filled circles)",
       caption="Data from NHS England & ONS | Plot by @VictimOfMaths")
dev.off()  

#Plot of totals by age
totaldata <- data %>% 
  group_by(dose, age) %>% 
  summarise(vaccinated=sum(vaccinated), ONSpop=sum(ONSpop), NIMSpop=sum(NIMSpop)) %>% 
  ungroup() %>% 
  mutate(vaxpop_ONS=vaccinated/ONSpop, vaxpop_NIMS=vaccinated/NIMSpop) 

totaldata2 <- totaldata %>% 
  group_by(dose) %>% 
  summarise(vaccinated=sum(vaccinated), ONSpop=sum(ONSpop), NIMSpop=sum(NIMSpop)) %>% 
  ungroup() %>% 
  mutate(vaxpop_ONS=vaccinated/ONSpop, vaxpop_NIMS=vaccinated/NIMSpop, age="Overall") %>% 
  bind_rows(totaldata)

agg_tiff("Outputs/ENGPopONSvsNIMSxAge.tiff", units="in", width=8, height=6, res=800)
ggplot(totaldata2 %>% filter(dose=="2nd"))+
  geom_segment(aes(x=vaxpop_ONS, xend=vaxpop_NIMS, y=age, yend=age, colour=age), show.legend=FALSE)+
  geom_point(aes(x=vaxpop_ONS, y=age), size=6)+
  geom_point(aes(x=vaxpop_NIMS, y=age), shape=21, fill="white", size=6)+
  geom_text(aes(x=vaxpop_ONS, y=age, label=round(vaxpop_ONS*100, 0)), colour="White", size=3)+
  geom_text(aes(x=vaxpop_NIMS, y=age, label=round(vaxpop_NIMS*100, 0)), colour="Black", size=3)+
  scale_x_continuous(name="Proportion who have received both vaccine doses",
                     labels=label_percent(accuracy=1), breaks=seq(0, 1, by=0.1))+
  scale_y_discrete(name="Age")+
  scale_colour_viridis_d(option="mako", direction=-1)+
  theme_custom()+
  theme(panel.grid.major.x=element_line(colour="Grey95"))+
  labs(title="Population data makes a big difference to estimates of vaccine coverage",
       subtitle="2 dose vaccine coverage by age using population estimates from NIMS (hollow circles) and ONS (filled circles)",
       caption="Data from NHS England & ONS| Plot by @VictimOfMaths")
dev.off()  

#Plot of totals by region
regdata <- data %>% 
  group_by(dose, Region) %>% 
  summarise(vaccinated=sum(vaccinated), ONSpop=sum(ONSpop), NIMSpop=sum(NIMSpop)) %>% 
  ungroup() %>% 
  mutate(vaxpop_ONS=vaccinated/ONSpop, vaxpop_NIMS=vaccinated/NIMSpop,
         Region=fct_reorder(Region, vaxpop_NIMS)) 

agg_tiff("Outputs/ENGPopONSvsNIMSxRegion.tiff", units="in", width=8, height=6, res=800)
ggplot(regdata %>% filter(dose=="2nd"))+
  geom_segment(aes(x=vaxpop_ONS, xend=vaxpop_NIMS, y=Region, yend=Region, colour=Region), show.legend=FALSE)+
  geom_point(aes(x=vaxpop_ONS, y=Region), size=6)+
  geom_point(aes(x=vaxpop_NIMS, y=Region), shape=21, fill="white", size=6)+
  geom_text(aes(x=vaxpop_ONS, y=Region, label=round(vaxpop_ONS*100, 0)), colour="White", size=3)+
  geom_text(aes(x=vaxpop_NIMS, y=Region, label=round(vaxpop_NIMS*100, 0)), colour="Black", size=3)+
  scale_x_continuous(name="Proportion who have received both vaccine doses",
                     labels=label_percent(accuracy=1), breaks=seq(0, 1, by=0.05))+
  scale_y_discrete(name="Age")+
  scale_colour_viridis_d(option="rocket", direction=-1)+
  theme_custom()+
  theme(panel.grid.major.x=element_line(colour="Grey95"))+
  labs(title="London's vaccination coverage is the most uncertain",
       subtitle="2 dose vaccine coverage by region using population estimates from NIMS (hollow circles) and ONS (filled circles)",
       caption="Data from NHS England & ONS| Plot by @VictimOfMaths")
dev.off()  
