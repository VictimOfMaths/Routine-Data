rm(list=ls())

library(tidyverse)
library(extrafont)
library(ggtext)
library(ragg)
library(geofacet)
library(forcats)
library(ggridges)
library(curl)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Download data from GISAH
url1 <- "https://apps.who.int/gho/athena/data/xmart.csv?target=GHO/SA_0000001400&profile=crosstable&filter=COUNTRY:*;YEAR:2020;YEAR:2019;YEAR:2018;YEAR:2017;YEAR:2016;YEAR:2015;YEAR:2014;YEAR:2013;YEAR:2012;YEAR:2011;YEAR:2010&x-sideaxis=COUNTRY;DATASOURCE;ALCOHOLTYPE&x-topaxis=GHO;YEAR"
temp1 <- tempfile()
temp1 <- curl_download(url=url1, destfile=temp1, quiet=FALSE, mode="wb")

url2 <- "https://apps.who.int/gho/athena/data/GHO/SA_0000001400?filter=COUNTRY:*;YEAR:2009;YEAR:2008;YEAR:2007;YEAR:2006;YEAR:2005;YEAR:2004;YEAR:2003;YEAR:2002;YEAR:2001;YEAR:2000&x-sideaxis=COUNTRY;DATASOURCE;ALCOHOLTYPE&x-topaxis=GHO;YEAR&profile=crosstable&format=csv"
temp2 <- tempfile()
temp2 <- curl_download(url=url2, destfile=temp2, quiet=FALSE, mode="wb")

url3 <- "https://apps.who.int/gho/athena/data/GHO/SA_0000001400?filter=COUNTRY:*;YEAR:1999;YEAR:1998;YEAR:1997;YEAR:1996;YEAR:1995;YEAR:1994;YEAR:1993;YEAR:1992;YEAR:1991;YEAR:1990;YEAR:1989;YEAR:1988;YEAR:1987;YEAR:1986;YEAR:1985;YEAR:1984;YEAR:1983;YEAR:1982;YEAR:1981;YEAR:1980&x-sideaxis=COUNTRY;DATASOURCE;ALCOHOLTYPE&x-topaxis=GHO;YEAR&profile=crosstable&format=csv"
temp3 <- tempfile()
temp3 <- curl_download(url=url3, destfile=temp3, quiet=FALSE, mode="wb")

url4 <- "https://apps.who.int/gho/athena/data/GHO/SA_0000001400?filter=COUNTRY:*;YEAR:1979;YEAR:1978;YEAR:1977;YEAR:1976;YEAR:1975;YEAR:1974;YEAR:1973;YEAR:1972;YEAR:1971;YEAR:1970;YEAR:1969;YEAR:1968;YEAR:1967;YEAR:1966;YEAR:1965;YEAR:1964;YEAR:1963;YEAR:1962;YEAR:1961;YEAR:1960&x-sideaxis=COUNTRY;DATASOURCE;ALCOHOLTYPE&x-topaxis=GHO;YEAR&profile=crosstable&format=csv"
temp4 <- tempfile()
temp4 <- curl_download(url=url4, destfile=temp4, quiet=FALSE, mode="wb")

data1 <- read.csv(temp1, skip=1)
data2 <- read.csv(temp2, skip=1)
data3 <- read.csv(temp3, skip=1)
data4 <- read.csv(temp4, skip=1)

data <- merge(data4, data3,all=T) %>% 
  merge(data2, all=T) %>% 
  merge(data1, all=T) %>%
  select(-Data.Source) %>% 
  gather(year, PCC, c(3:ncol(.))) %>% 
  mutate(year=as.numeric(substr(year, 3, 8)), 
         Country=case_when(
           Country=="United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
           TRUE ~ Country),
         Beverage.Types=factor(Beverage.Types, 
                               levels=c(" All types", " Beer", " Spirits", " Wine", " Other alcoholic beverages"))) %>% 
  distinct()

ggplot(data %>% filter(Country %in% c("France", "Germany", "United Kingdom",
                                      "Italy", "Denmark", "Sweden", "Norway", "Finland", "Netherlands", "Ireland", 
                                      "Portugal", "Spain", "Belgium", "Czech Republic", "Austria", "Slovenia",
                                      "Poland", "Slovakia", "Hungary", "Croatia", "Latvia", "Lithuania", "Estonia",
                                      "Bulgaria", "Romania", "Greece", "Malta", "Cyprus")), 
       aes(x=year, y=PCC, group=Country))+
  geom_line()+
  facet_geo(~Country, grid="eu_grid1", scales="free_y")+
  theme_custom()
  #facet_wrap(~Country)

tiles <- data %>% 
  filter(Beverage.Types==" All types") %>% 
  group_by(Country) %>% 
  mutate(peak=max(PCC, na.rm=TRUE),
         peakyear=year[which(peak==PCC)][1], 
         peakprop=PCC/peak) %>% 
  ungroup()

#Ordered heatmap of peak years
agg_tiff("Outputs/GISAHOECDHeatmap.tiff", units="in", width=9, height=8, res=800)
ggplot(tiles %>% filter(Country %in% c("Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia",
                                       "Costa Rica", "Czechia", "Denmark", "Estonia", "Finland", 
                                       "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel",
                                       "Italy", "Japan", "Republic of Korea", "Latvia", "Lithuania", "Luxembourg",
                                       "Mexico", "Netherlands", "New Zealand", "Norway", "Poland",
                                       "Portugal", "Slovakia", "Slovenia", "Spain", "Sweden",
                                       "Switzerland", "Turkey", "United Kingdom", 
                                       "United States of America")), 
       aes(x=year, y=fct_reorder(Country, peakyear), fill=peakprop))+
  geom_tile(show.legend=FALSE)+
  scale_x_continuous(breaks=seq(1960, 2020, by=10), name="")+
  scale_y_discrete(name="")+
  scale_fill_distiller(palette="Spectral", na.value="White")+
  theme_custom()+
  theme(plot.title=element_text(size=rel(1.5)))+
  labs(title="When did alcohol consumption peak?",
       subtitle="Per capita alcohol consumption as a proportion of each country's 1960-2019 peak for OECD member states.\nYears with missing data appear in white.",
       caption="Data from WHO GISAH database | Plot by @VictimOfMaths")
dev.off()

#Slightly bonkers ridgeplot
ggplot(tiles %>% filter(Country %in% c("Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia",
                                       "Costa Rica", "Czech Republic", "Denmark", "Estonia", "Finland", 
                                       "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel",
                                       "Italy", "Japan", "Korea", "Latvia", "Lithuania", "Luxembourg",
                                       "Mexico", "Netherlands", "New Zealand", "Norway", "Poland",
                                       "Portugal", "Slovakia", "Slovenia", "Spain", "Sweden",
                                       "Switzerland", "Turkey", "United Kingdom", 
                                       "United States of America")), 
       aes(x=year, y=fct_reorder(Country, peakyear), height=PCC, fill=PCC))+
  geom_ridgeline_gradient(show.legend=FALSE, alpha=0.5, scale=1)+
  scale_fill_distiller(palette="Spectral")+
  theme_custom()

#Faceted plot of OECD countries
agg_tiff("Outputs/GISAHOECDFacets.tiff", units="in", width=14, height=8, res=800)
ggplot(tiles %>% mutate(Country=case_when(
  Country=="Czechia" ~ "Czech Republic",
  Country=="United States of America" ~ "United States",
  TRUE ~ Country)), aes(x=year, y=PCC, fill=PCC))+
  geom_col(show.legend=FALSE)+
  geom_line(colour="Grey40")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Per capita alcohol consumption (litres of alcohol per year)")+
  scale_fill_distiller(palette="Spectral")+
  facet_geo(~Country, grid="oecd_grid1")+
  theme_custom()+
  theme(strip.text=element_text(size=rel(0.8)), axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  labs(title="Trends in alcohol consumption vary widely across OECD nations",
       subtitle="Per capita alcohol consumption 1960-2019",
       caption="Data from WHO GISAH database | Plot by @VictimOfMaths")
dev.off()

#Version by beverage type
agg_tiff("Outputs/GISAHOECDFacetsxBevtype.tiff", units="in", width=14, height=8, res=800)
ggplot(data %>% mutate(Country=case_when(
  Country=="Czechia" ~ "Czech Republic",
  Country=="United States of America" ~ "United States",
  TRUE ~ Country)) %>%  filter(Beverage.Types!=" All types"), aes(x=year, y=PCC, fill=Beverage.Types))+
  geom_area()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Per capita alcohol consumption (litres of alcohol per year)")+
  scale_fill_manual(values=c("#ffc000", "#00b0f0", "#7030a0", "Grey70"), name="")+
  facet_geo(~Country, grid="oecd_grid1")+
  theme_custom()+
  theme(strip.text=element_text(size=rel(0.8)), axis.text.x=element_text(angle=45, hjust=1, vjust=1),
        legend.position="top")+
  labs(title="Trends in alcohol consumption vary widely across OECD nations",
       subtitle="Per capita alcohol consumption by beverage type 1960-2019",
       caption="Data from WHO GISAH database | Plot by @VictimOfMaths")
dev.off()
