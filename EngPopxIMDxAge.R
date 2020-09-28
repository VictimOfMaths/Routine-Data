rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(ggtext)
library(ggridges)
library(paletteer)

#Population data by LSOA
temp <- tempfile()
temp2 <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2flowersuperoutputareamidyearpopulationestimates%2fmid2019sape22dt2/sape22dt2mid2019lsoasyoaestimatesunformatted.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)
LSOApop <- read_excel(file.path(temp2, "SAPE22DT2-mid-2019-lsoa-syoa-estimates-unformatted.xlsx"), sheet=4,
                     range="A5:CT34758")

#Bring in IMD data (for England)
temp <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833970/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
EIMD <- read_excel(temp, sheet=2, range="A1:F32845")
colnames(EIMD) <- c("LSOAcode", "LSOAname", "LADcode", "LADname", "IMDrank", "IMDdecile")

data <- merge(LSOApop, EIMD[,c(1,6)], by.x="LSOA Code", by.y="LSOAcode")

data_long <- data %>% 
  gather(age, pop, c(8:98)) %>% 
  group_by(age, IMDdecile) %>% 
  summarise(pop=sum(pop)) 

data_long$age <- as.numeric(if_else(data_long$age=="90+", "90", data_long$age))

tiff("Outputs/EngPopxIMDxAge.tiff", units="in", width=8, height=6, res=500)
ggplot(data_long, aes(x=age, y=pop, colour=as.factor(IMDdecile)))+
  geom_line()+
  scale_colour_paletteer_d("RColorBrewer::BrBG", direction=-1, name="IMD Decile", 
                           labels=c("1 - most deprived", "2", "3", "4", "5", "6", "7", "8",
                                    "9", "10 - least deprived"))+
  scale_x_continuous(name="Age")+
  scale_y_continuous(name="Population")+
  theme_classic()+
  labs(title="Younger people in England are more likely to live in more deprived areas",
       subtitle="Age distribution of the English population by deciles of the Index of Multiple Deprivation",
       caption="Data from ONS & DHCLG | Plot by @VictimOfMaths")
dev.off()

