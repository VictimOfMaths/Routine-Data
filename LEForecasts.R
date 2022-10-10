rm(list=ls())

library(tidyverse)
library(readxl)
library(lubridate)
library(paletteer)
library(curl)
library(ragg)
library(ggtext)
library(ggrepel)
library(extrafont)
library(shadowtext)

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

#Updating/playing with plot originally created by @JonMinton
#https://twitter.com/JonMinton/status/1215317756817690625?s=20&t=VzQd7G2l85epykODBUAJiA

temp <- tempfile()

#2020 data
url1 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/expectationoflifeprincipalprojectionunitedkingdom/2020based/ukppp20ex.xlsx"
temp <- curl_download(url=url1, destfile=temp, quiet=FALSE, mode="wb")

raw2020_m <- as.data.frame(t(read_excel(temp, sheet="males period ex", range="B5:CM6", 
                                        col_names=FALSE))) %>% 
  set_names("Year", "e0") %>% 
  mutate(Year=as.numeric(Year), e0=as.numeric(e0), Sex="Male", YearMade=2020, 
         Type=if_else(Year>YearMade, "Forecast", "Actual"))

raw2020_f <- as.data.frame(t(read_excel(temp, sheet="females period ex", range="B5:CM6", 
                                        col_names=FALSE))) %>% 
  set_names("Year", "e0") %>% 
  mutate(Year=as.numeric(Year), e0=as.numeric(e0), Sex="Female", YearMade=2020, 
         Type=if_else(Year>YearMade, "Forecast", "Actual"))

#2018 data
url2 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/expectationoflifeprincipalprojectionunitedkingdom/2018based/ukppp18ex.xls"
temp <- curl_download(url=url2, destfile=temp, quiet=FALSE, mode="wb")

raw2018_m <- as.data.frame(t(read_excel(temp, sheet="Males period ex", range="B10:CK12", 
                                        col_names=FALSE))) %>% 
  select(-2) %>% 
  set_names("Year", "e0") %>% 
  mutate(Year=as.numeric(Year), e0=as.numeric(e0), Sex="Male", YearMade=2018, 
         Type=if_else(Year>YearMade, "Forecast", "Actual"))

raw2018_f <- as.data.frame(t(read_excel(temp, sheet="Females period ex", range="B10:CK12", 
                                        col_names=FALSE))) %>% 
  select(-2) %>% 
  set_names("Year", "e0") %>% 
  mutate(Year=as.numeric(Year), e0=as.numeric(e0), Sex="Female", YearMade=2018, 
         Type=if_else(Year>YearMade, "Forecast", "Actual"))

#2016 data
url3 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/expectationoflifeprincipalprojectionunitedkingdom/2016based/wukprincipal16.xls"
temp <- curl_download(url=url3, destfile=temp, quiet=FALSE, mode="wb")

raw2016_m <- as.data.frame(t(read_excel(temp, sheet="Males period ex", range="B10:CI12", 
                                        col_names=FALSE))) %>% 
  select(-2) %>% 
  set_names("Year", "e0") %>% 
  mutate(Year=as.numeric(Year), e0=as.numeric(e0), Sex="Male", YearMade=2016, 
         Type=if_else(Year>YearMade, "Forecast", "Actual"))

raw2016_f <- as.data.frame(t(read_excel(temp, sheet="Females period ex", range="B10:CI12", 
                                        col_names=FALSE))) %>% 
  select(-2) %>% 
  set_names("Year", "e0") %>% 
  mutate(Year=as.numeric(Year), e0=as.numeric(e0), Sex="Female", YearMade=2016, 
         Type=if_else(Year>YearMade, "Forecast", "Actual"))

#2014 data
url4 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/expectationoflifeprincipalprojectionunitedkingdom/2014based/wukprincipal14.xls"
temp <- curl_download(url=url4, destfile=temp, quiet=FALSE, mode="wb")

raw2014_m <- as.data.frame(t(read_excel(temp, sheet="Males period ex", range="B10:CI12", 
                                        col_names=FALSE))) %>% 
  select(-2) %>% 
  set_names("Year", "e0") %>% 
  mutate(Year=as.numeric(Year), e0=as.numeric(e0), Sex="Male", YearMade=2014, 
         Type=if_else(Year>YearMade, "Forecast", "Actual"))

raw2014_f <- as.data.frame(t(read_excel(temp, sheet="Females period ex", range="B10:CI12", 
                                        col_names=FALSE))) %>% 
  select(-2) %>% 
  set_names("Year", "e0") %>% 
  mutate(Year=as.numeric(Year), e0=as.numeric(e0), Sex="Female", YearMade=2014, 
         Type=if_else(Year>YearMade, "Forecast", "Actual"))

#2012 data
url5 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/expectationoflifeprincipalprojectionunitedkingdom/2012based/wukprincipal12exr.xls"
temp <- curl_download(url=url5, destfile=temp, quiet=FALSE, mode="wb")

raw2012_m <- as.data.frame(t(read_excel(temp, sheet="Males Period ex", range="B11:CE13", 
                                        col_names=FALSE))) %>% 
  select(-2) %>% 
  set_names("Year", "e0") %>% 
  mutate(Year=as.numeric(Year), e0=as.numeric(e0), Sex="Male", YearMade=2012, 
         Type=if_else(Year>YearMade, "Forecast", "Actual"))

raw2012_f <- as.data.frame(t(read_excel(temp, sheet="Females Period ex", range="B11:CE13", 
                                        col_names=FALSE))) %>% 
  select(-2) %>% 
  set_names("Year", "e0") %>% 
  mutate(Year=as.numeric(Year), e0=as.numeric(e0), Sex="Female", YearMade=2012, 
         Type=if_else(Year>YearMade, "Forecast", "Actual"))

#Older data all comes from the same place
url6 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationprojections/adhocs/006372nationalpopulationprojectionsaccuracyreportunderlyingdatauk1966to2030/eolsforfigures12and13nppaccuracyreport.xls"
temp <- curl_download(url=url6, destfile=temp, quiet=FALSE, mode="wb")

rawold_m <- read_excel(temp, sheet="Data", range="B2:BO26") %>% 
  slice_tail(n=nrow(.)-1) %>% 
  mutate(Type=if_else(`...1`=="Actual", "Actual", "Forecast"),
         YearMade=if_else(`...1`!="Actual", as.numeric(substr(`...1`, 1,4)), 1970)) %>% 
  gather(Year, e0, c(2:66)) %>% 
  mutate(Year=as.numeric(Year), Sex="Male",
         YearMade=if_else(YearMade==2012, 2011.99, YearMade)) %>% 
  group_by(Year) %>% 
  mutate(Actual=e0[Type=="Actual"]) %>% 
  ungroup() %>% 
  mutate(e0=if_else(is.na(e0) & YearMade>Year, Actual, e0)) %>% 
  filter(!is.na(e0)) %>% 
  select(-c(`...1`, Actual))

rawold_f <- read_excel(temp, sheet="Data", range="B30:BO54") %>% 
  slice_tail(n=nrow(.)-1) %>% 
  mutate(Type=if_else(`...1`=="Actual", "Actual", "Forecast"),
         YearMade=if_else(`...1`!="Actual", as.numeric(substr(`...1`, 1,4)), 1970)) %>% 
  gather(Year, e0, c(2:66)) %>% 
  mutate(Year=as.numeric(Year), Sex="Female",
         YearMade=if_else(YearMade==2012, 2011.99, YearMade)) %>% 
  group_by(Year) %>% 
  mutate(Actual=e0[Type=="Actual"]) %>% 
  ungroup() %>% 
  mutate(e0=if_else(is.na(e0) & YearMade>Year, Actual, e0)) %>% 
  filter(!is.na(e0)) %>% 
  select(-c(`...1`, Actual))

#Stitch together
data <- bind_rows(raw2020_m, raw2020_f, raw2018_m, raw2018_f, raw2016_m, raw2016_f, raw2014_m,
                  raw2014_f, raw2012_m, raw2012_f, rawold_m, rawold_f)

agg_tiff("Outputs/LEForecastsUK.tiff", units="in", width=9, height=6, res=600)
ggplot()+
  geom_line(data=data, aes(x=Year, y=e0, group=YearMade), colour="Grey80")+
  geom_line(data=data %>% filter(Type=="Actual"), aes(x=Year, y=e0), colour="#6600cc")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Life expectancy at birth")+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  facet_wrap(~Sex)+
  labs(title="ONS life expectancy forecasts were pessimistic, until they weren't",
       subtitle="Successive ONS forecasts of period life expectancy at birth for the UK compared to <span style='color:#6600cc;'>actual values",
       caption="Data from ONS | Original idea from @JonMinton | Plot by @VictimOfMaths")

dev.off()

MaxYear <- 2040

data_ends <- data %>% 
  filter(Type=="Forecast" & Year<=MaxYear) %>% 
  group_by(YearMade) %>% 
  mutate(EndYear=max(Year)) %>% 
  ungroup() %>% 
  filter(Year==EndYear)

ann_text=data.frame(Sex=c("Female", "Female", "Female"),
                    Year=c(2009, 2039, 2022),
                    e0=c(78.8, 83.8, 87.5),
                    label=c("1971 forecast", "2020\nForecast",
                            "The 2008 forecast\nwas the most optimistic"))

agg_tiff("Outputs/LEForecastsUKDots.tiff", units="in", width=9, height=6, res=800)
ggplot()+
  geom_line(data=data, aes(x=Year, y=e0, group=YearMade), colour="Grey80")+
  geom_point(data=data_ends, aes(x=Year, y=e0), shape=21, size=rel(2))+
  geom_point(data=data_ends, aes(x=Year, y=e0, colour=YearMade), show.legend=FALSE)+
  geom_line(data=data %>% filter(Type=="Actual"), aes(x=Year, y=e0), colour="Black")+
  geom_shadowtext(data=ann_text, aes(x=Year, y=e0, label=label),
            size=rel(2.7), family=font, colour="Black",
            bg.colour="White")+
  scale_x_continuous(name="", limits=c(1970, MaxYear))+
  scale_y_continuous(name="Life expectancy at birth")+
  scale_colour_paletteer_c("scico::tofino")+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  facet_wrap(~Sex)+
  labs(title="ONS life expectancy forecasts were pessimistic, until they weren't",
       subtitle="Successive ONS forecasts of period life expectancy at birth for the UK compared to <span style='color:Black;'>actual values",
       caption="Data from ONS | Original idea from @JonMinton | Plot by @VictimOfMaths")+
  coord_cartesian(clip="off")

dev.off()

#Calculate cumulative prediction error

errors <- data %>% 
  na.omit() %>% 
  filter(Type=="Forecast" & Year>=YearMade) %>% 
  merge(data %>% filter(Type=="Actual") %>% 
          rename("Actual"="e0") %>% 
          select(Year, Actual, Sex) %>%
          group_by(Year, Sex) %>% 
          slice_head(n=1)) %>% 
  distinct() %>% 
  group_by(Sex, YearMade) %>% 
  arrange(Year) %>% 
  mutate(error=cumsum(e0-Actual)) %>% 
  ungroup()

agg_tiff("Outputs/LEForecastsUKError.tiff", units="in", width=9, height=6, res=800)
ggplot(errors, aes(x=Year, y=error, group=YearMade, colour=YearMade))+
  geom_hline(yintercept=0, colour="Grey30")+
  geom_line(show.legend=FALSE)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Cumulative difference between\nforecast and actual life expectancy")+
  scale_colour_paletteer_c("scico::tofino")+
  facet_wrap(~Sex)+
  theme_custom()

dev.off()

agg_tiff("Outputs/LEForecastsUKErrorBased.tiff", units="in", width=9, height=6, res=800)
ggplot(errors, aes(x=Year-YearMade, y=error, group=YearMade, colour=YearMade))+
  geom_hline(yintercept=0, colour="Grey30")+
  geom_line()+
  scale_x_continuous(name="Years since forecast made")+
  scale_y_continuous(name="Cumulative difference between\nforecast and actual life expectancy")+
  scale_colour_paletteer_c("scico::tofino", name="Year of forecast")+
  facet_wrap(~Sex)+
  theme_custom()+
  labs(title="UK Life Expectancy forecasts have been getting more optimistic",
       subtitle="Cumulative difference between successive ONS forecasts of Life Expectancy at birth and the subsequent actual trends",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()
