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
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833973/File_2_-_IoD2019_Domains_of_Deprivation.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
EIMD <- read_excel(temp, sheet=2, range="A2:T32845")[,c(1,6,8,10,12,14,16,18,20)]
colnames(EIMD) <- c("LSOAcode", "IMDdecile", "Income", "Employment", "Education, Skills & Training",
                    "Health & Disability", "Crime", "Bariers to Housing and Services", "Living Environment")

data <- merge(LSOApop, EIMD, by.x="LSOA Code", by.y="LSOAcode")

data_long <- data %>% 
  gather(age, pop, c(8:98)) %>% 
  mutate(age=as.numeric(if_else(age=="90+", "90", age)))

data_overall <- data_long %>% 
  group_by(age, IMDdecile) %>% 
  summarise(pop=sum(pop)) 

tiff("Outputs/EngPopxIMDxAge.tiff", units="in", width=8, height=6, res=500)
ggplot(data_overall, aes(x=age, y=pop, colour=as.factor(IMDdecile)))+
  geom_line()+
  scale_colour_paletteer_d("RColorBrewer::BrBG", direction=-1, name="IMD Decile", 
                           labels=c("1 - most deprived", "2", "3", "4", "5", "6", "7", "8",
                                    "9", "10 - least deprived"))+
  scale_x_continuous(name="Age")+
  scale_y_continuous(name="Population")+
  annotate("text", x=88, y=66000, label="Ages 90+ are\ngrouped together", colour="Grey50", size=rel(2.2))+
  theme_classic()+
  labs(title="Younger people in England are more likely to live in more deprived areas",
       subtitle="Age distribution of the English population by deciles of the Index of Multiple Deprivation",
       caption="Data from ONS & DHCLG | Plot by @VictimOfMaths")
dev.off()

data_income <- data_long %>% 
  group_by(age, Income) %>% 
  summarise(pop=sum(pop)) %>% 
  rename(decile=Income) %>% 
  mutate(domain="Income")

data_employment <- data_long %>% 
  group_by(age, Employment) %>% 
  summarise(pop=sum(pop)) %>% 
  rename(decile=Employment) %>% 
  mutate(domain="Employment")

data_education <- data_long %>% 
  group_by(age, `Education, Skills & Training`) %>% 
  summarise(pop=sum(pop)) %>% 
  rename(decile=`Education, Skills & Training`) %>% 
  mutate(domain="Education, Skills & Training")

data_health <- data_long %>% 
  group_by(age, `Health & Disability`) %>% 
  summarise(pop=sum(pop)) %>% 
  rename(decile=`Health & Disability`) %>% 
  mutate(domain="Health & Disability")

data_crime <- data_long %>% 
  group_by(age, Crime) %>% 
  summarise(pop=sum(pop)) %>% 
  rename(decile=Crime) %>% 
  mutate(domain="Crime")

data_housing <- data_long %>% 
  group_by(age, `Bariers to Housing and Services`) %>% 
  summarise(pop=sum(pop)) %>% 
  rename(decile=`Bariers to Housing and Services`) %>% 
  mutate(domain="Bariers to Housing and Services")

data_environment <- data_long %>% 
  group_by(age, `Living Environment`) %>% 
  summarise(pop=sum(pop)) %>% 
  rename(decile=`Living Environment`) %>% 
  mutate(domain="Living Environment")

data_domains <- bind_rows(data_income, data_employment, data_education, data_health, data_health,
                          data_crime, data_housing, data_environment)

tiff("Outputs/EngPopxIMDxAgexDomain.tiff", units="in", width=10, height=8, res=500)
ggplot(data_domains, aes(x=age, y=pop, colour=as.factor(decile)))+
  geom_line()+
  scale_colour_paletteer_d("RColorBrewer::BrBG", direction=-1, name="IMD Decile", 
                           labels=c("1 - most deprived", "2", "3", "4", "5", "6", "7", "8",
                                    "9", "10 - least deprived"))+
  scale_x_continuous(name="Age")+
  scale_y_continuous(name="Population")+
  facet_wrap(~domain)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="The age distribution of deprivation depends on how you measure deprivation",
       subtitle="English population by age across deciles of each domain of the Index of Multiple Deprivation",
       caption="Data from ONS & DHCLG | Plot by @VictimOfMaths")
dev.off()
