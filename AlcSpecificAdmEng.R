rm(list=ls())

library(tidyverse)
library(curl)
library(paletteer)
library(sf)

#Scottish age data
temp <- tempfile()
source <- "https://files.digital.nhs.uk/A7/1EDDB8/CCG_3_14_I01983_D.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data <- read.csv(temp)

#Plot national level trend
tiff("Outputs/AlcSpecificAdmTrendEng.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(data, Level=="National"))+
  geom_ribbon(aes(x=Reporting.Period, ymin=CI.Lower, ymax=CI.Upper, group=Level), fill="Grey85")+
  geom_line(aes(x=Reporting.Period, y=Indicator.Value, group=Level), colour="Tomato")+
  scale_y_continuous(limits=c(0,NA), name="Age-standardised admissions per 100,000")+
  scale_x_discrete(name="")+
  theme_classic()+
  theme(plot.title=element_text(face="bold"))+
  labs(title="Little change in alcohol-specific hospital admissions in England",
       subtitle="Age-standardised rates of admissions to hospital with a condition caused entirely by alcohol\nwith 95% Confidence Intervals",
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

#Bring in shapefile for map
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/5252644ec26e4bffadf9d3661eef4826_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name))

#Sort out code discrepancies https://digital.nhs.uk/services/organisation-data-service/2019-changes-to-regions
map.data <- data %>% 
  filter(Reporting.Period=="2019/20" & Level!="National") %>% 
  mutate(ccg18cd=ONS.code)
 
#Devon
temp1 <- map.data %>% filter(Level=="15N")
temp2 <- temp1 %>% mutate(ccg18cd="E38000129")
temp3 <- temp1 %>% mutate(ccg18cd="E38000152")

#Derbyshire
temp4 <- map.data %>% filter(Level=="15M")
temp5 <- temp4 %>% mutate(ccg18cd="E38000058")
temp6 <- temp4 %>% mutate(ccg18cd="E38000071")
temp7 <- temp4 %>% mutate(ccg18cd="E38000115")
temp8 <- temp4 %>% mutate(ccg18cd="E38000169")

map.data <- bind_rows(map.data, temp2, temp3, temp5, temp6, temp7, temp8)

map.change <- full_join(shapefile, map.data, by="ccg18cd",
                        all.y=TRUE, all.x=TRUE)

tiff("Outputs/AlcSpecificAdmEngMap.tiff", units="in", width=8, height=8, res=500)
ggplot(subset(map.change, Level!="National"))+
  geom_sf(aes(geometry=geometry, fill=Indicator.Value), colour=NA)+
  scale_fill_paletteer_c("pals::ocean.tempo", name="Admissions\nper 100,000")+ 
  theme_classic()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title=element_text(face="bold"))+
  labs(title="Alcohol-specific hospital admissions are highest around Liverpool and Sunderland",
       subtitle="Age-standardised rates of admissions to hospital with a condition that is entirely caused by alcohol\nData from 2019/20",
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()
