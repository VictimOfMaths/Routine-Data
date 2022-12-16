rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(extrafont)
library(ragg)
library(paletteer)
library(ungroup)
library(scales)
library(ggrepel)
library(imputeTS)

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

#Read in HSE data
temp <- tempfile()
url <- "https://files.digital.nhs.uk/28/149140/HSE-2021-Adults%27-health-related-behaviours-tables.xlsx"
rawfile <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

rawdata <- read_excel(rawfile, sheet="Table 15", range="A4:K304") %>% 
  mutate(Sex=rep(c("Male", "Female", "Total"), each=100)) %>% 
  filter(!row_number() %in% c(82:100, 182:200, 282:300, 1, 101, 201)) %>% 
  filter(!is.na(`...1`)) %>% 
  mutate(Age=rep(c("16-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+", "All"),
                 each=10, times=3)) %>% 
  filter(!is.na(`2011`)) %>% 
  rename("Measure"="...1") %>% 
  gather(Year, Value, c(2:11)) %>% 
  mutate(Value=as.numeric(Value), Year=as.numeric(Year))
  
rawdata %>% filter(Measure=="Mean number of units" & Sex!="Total" & Age!="All") %>% 
  ggplot(aes(x=Year, y=Value, colour=Age))+
  geom_line()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Units of alcohol per week", limits=c(0,NA))+
  scale_colour_paletteer_d("rcartocolor::Temps")+
  facet_wrap(~Sex)+
  theme_custom()

#Plot abstention rates in a sensible way
Agelabels <- rawdata %>% 
  filter(Measure=="% non drinker/did not drink in last 12 months" &  Sex!="Total" & 
           Age!="All" & Year==2021) %>% 
  mutate(label=if_else(Age=="16-24", "16-24\nyears old", Age))

agg_tiff("Outputs/HSEAbstxAge.tiff", units="in", width=8, height=6, res=600)
rawdata %>% filter(Measure=="% non drinker/did not drink in last 12 months" & 
                     Sex!="Total" & Age!="All") %>% 
  ggplot(aes(x=Year, y=Value/100, colour=Age))+
  geom_line(show.legend=FALSE)+
  geom_text_repel(data=Agelabels ,aes(label=label),
                  show.legend=FALSE, family="Lato", 
                  xlim=(c(2021.1, NA)), segment.color = NA)+
  scale_x_continuous(name="", breaks=c(2012, 2014, 2016, 2018, 2020),
                     limits=c(2011, 2023))+
  scale_y_continuous(name="Proportion of non-drinkers", limits=c(0,NA),
                     labels=label_percent(accuracy=1))+
  scale_colour_paletteer_d("rcartocolor::Temps")+
  facet_wrap(~Sex)+
  theme_custom()+
  labs(title="Young people have abandoned alcohol",
       subtitle="Proportion of respondents to the Health Survey for England who report not drinking alcohol in the previous year by age\n",
       caption="Data from NHS Digital | Plot by @VictimOfMaths")

dev.off()

#Lexis surface of abstention BECAUSE WHY NOT
absdata <- rawdata %>% 
  filter(Measure=="% non drinker/did not drink in last 12 months" & 
           Sex!="Total" & Age!="All") %>% 
  select(-Measure)

#Get population data
#2021 census
temp <- tempfile()
url2 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationandhouseholdestimatesenglandandwalescensus2021/census2021/census2021firstresultsenglandwales1.xlsx"
rawpop21 <- curl_download(url=url2, destfile=temp, quiet=FALSE, mode="wb")

pop21 <- as.data.frame(t(read_excel(rawpop21, sheet="P03", range="D10:AO10", 
                                    col_names=FALSE))) %>% 
  set_names("Pop21") %>% 
  mutate(Sex=rep(c("Female", "Male"), each=19),
         age=rep(c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
                   "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
                   "75-79", "80-84", "85-89", "90+"), times=2))

#2011 census
temp <- tempfile()
url3 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/2011censuspopulationandhouseholdestimatesforenglandandwales/tablep02w1_tcm77-270411.xls"
rawpop11 <- curl_download(url=url3, destfile=temp, quiet=FALSE, mode="wb")

pop11 <- read_excel(rawpop11, sheet="Table P02", range="A8:D87") %>% 
  bind_rows(read_excel(rawpop11, sheet="Table P02", range="F8:I75")) %>% 
  filter(`Age 2` %in% c("0 ‒ 4", "5 ‒ 9", "10 ‒ 14", "15 ‒ 19", "20 ‒ 24", "25 ‒ 29", 
                        "30 ‒ 34", "35 ‒ 39", "40 ‒ 44", "45 ‒ 49", "50 ‒ 54", 
                        "55 ‒ 59", "60 ‒ 64", "65 ‒ 69", "70 ‒ 74", "75 ‒ 79", 
                        "80 ‒ 84", "85 ‒ 89", "90 ‒ 94", "95 ‒ 99", 
                        "100 and over")) %>% 
  mutate(age=gsub(" ", "", `Age 2`),
         age=gsub("‒", "-", age),
         age=if_else(age %in% c("90-94", "95-99", "100andover"), "90+", age)) %>% 
  group_by(age) %>% 
  summarise(Male=sum(Males), Female=sum(Females), .groups="drop") %>% 
  gather(Sex, Pop11, c(2:3))

#Combine and linearly interpolate years between censuses
#necessary because until next year we don't have a single time series of the population
#of England, which is kind of bonkers.
popfull <- merge(pop21, pop11) %>% 
  gather(Year, Pop, c(3:4)) %>% 
  mutate(Year=if_else(Year=="Pop21", 2021, 2011)) %>% 
  bind_rows(data.frame(age=rep(c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", 
                             "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", 
                             "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", 
                             "90+"), times=18), 
                       Sex=rep(c("Female", "Male"), each=19, times=9),
                       Year=rep(2012:2020, each=19*2))) %>% 
  group_by(age, Sex) %>% 
  arrange(Year) %>% 
  mutate(Pop=na_interpolation(Pop)) %>% 
  ungroup()

pop <- popfull %>% 
  #Adjust 15-19 age group to exclude 15 year olds (rough approximation)
  mutate(Pop=if_else(age=="15-19", Pop*0.805, Pop),
         Age=case_when(
           age %in% c("15-19", "20-24") ~ "16-24",
           age %in% c("25-29", "30-34") ~ "25-34",
           age %in% c("35-39", "40-44") ~ "35-44",
           age %in% c("45-49", "50-54") ~ "45-54",
           age %in% c("55-59", "60-64") ~ "55-64",
           age %in% c("65-69", "70-74") ~ "65-74",
           age %in% c("75-79", "80-84", "85-89", "90+") ~ "75+")) %>% 
  group_by(Sex, Age, Year) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#Smooth age banded data to single years of age
tosmooth <- absdata %>% 
  merge(pop) %>% 
  mutate(abstainers=Value*Pop/100,
         agefrom=as.numeric(substr(Age, 1, 2)))

smoothed <- data.frame(Year=as.numeric(), Sex=as.character(), Age=as.numeric(), 
                       absrate=as.numeric())

for (i in c("Male", "Female")){
  for (j in unique(tosmooth$Year)) {
    temp <- tosmooth %>% filter(Sex==i & Year==j)
    x <- temp$agefrom
    y <- temp$abstainers
    offset <- temp$Pop
    
    model <- pclm(x, y, nlast=26, offset=offset)
    
    result <- data.frame(Year=rep(j, times=85), Sex=rep(i, times=85), 
                         Age=c(16:100), absrate=model$fitted)
    
    smoothed <- bind_rows(smoothed, result)
  }
}

#Plot the lexis surface
agg_tiff("Outputs/HSEAbstLexis.tiff", units="in", width=5, height=8, res=600)
ggplot(smoothed %>% filter(Age<=85), aes(x=Year, y=Age, fill=absrate))+
  geom_tile()+
  scale_x_continuous(breaks=c(2012, 2017, 2022), labels=c("'12", "'17", "'22"))+
  scale_fill_paletteer_c("viridis::turbo", limits=c(0,NA),
                         name="Abstention", labels=label_percent(accuracy=1))+
  coord_equal()+
  facet_wrap(~Sex)+
  theme_custom()+
  labs(title="Nondrinking is rising in young adults,\nbut falling in older ages",
       subtitle="Proportion of respondents to the Health Survey for England\nwho report not drinking alcohol in the previous year.",
       caption="\nData from NHS Digital and ONS | Plot by @VictimOfMaths")

dev.off()
