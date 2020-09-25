rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(forcats)
library(paletteer)
library(sf)
library(rmapshaper)
library(lubridate)
library(ggtext)

temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fhealthandlifeexpectancies%2fdatasets%2flifeexpectancyestimatesallagesuk%2f2001to2003to2017to2019/lepivottableupdated.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data <- read_excel(temp, sheet=2, range="A6:BP17526", col_names=TRUE)[,c(14:68)]

data <- data %>% 
  fill(c(1:3), .direction="down")

colnames(data) <- c("name", "code", "sex", "age", "central_2001-03", "lowerCI_2001-03", "upperCI_2001-03",
             "central_2002-04", "lowerCI_2002-04", "upperCI_2002-04",
             "central_2003-05", "lowerCI_2003-05", "upperCI_2003-05",
             "central_2004-06", "lowerCI_2004-06", "upperCI_2004-06",
             "central_2005-07", "lowerCI_2005-07", "upperCI_2005-07",
             "central_2006-08", "lowerCI_2006-08", "upperCI_2006-08",
             "central_2007-09", "lowerCI_2007-09", "upperCI_2007-09",
             "central_2008-10", "lowerCI_2008-10", "upperCI_2008-10",
             "central_2009-11", "lowerCI_2009-11", "upperCI_2009-11",
             "central_2010-12", "lowerCI_2010-12", "upperCI_2010-12",
             "central_2011-13", "lowerCI_2011-13", "upperCI_2011-13",
             "central_2012-14", "lowerCI_2012-14", "upperCI_2012-14",
             "central_2013-15", "lowerCI_2013-15", "upperCI_2013-15",
             "central_2014-16", "lowerCI_2014-16", "upperCI_2014-16",
             "central_2015-17", "lowerCI_2015-17", "upperCI_2015-17",
             "central_2016-18", "lowerCI_2016-18", "upperCI_2016-18",
             "central_2017-19", "lowerCI_2017-19", "upperCI_2017-19")

data_long <- pivot_longer(data, c(5:ncol(data)), names_to=c("metric", "year"), names_sep="_",
                          values_to="LE")

data_long$age <- factor(data_long$age, levels=c("<1", "01-04", "05-09", "10-14", "15-19", "20-24",
                                                "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
                                                "55-59", "60-64", "65-69", "70-74", "75-79", "80-84",
                                                "85-89", "90+"))

data_long$year <- as.numeric(substr(data_long$year, 1, 4))+1

data_long$age_cont <- case_when(
  data_long$age=="<1" ~ 0.5,
  data_long$age=="01-04" ~ 3,
  data_long$age=="05-09" ~ 7.5,
  TRUE ~ as.numeric(substr(data_long$age,1,2))+2.5
)

data_long$deathage <- data_long$LE+data_long$age_cont

tiff("Outputs/LEtrendsxageEng.tiff", units="in", width=10, height=6, res=500)
data_long %>% 
  filter(name=="England" & metric=="central") %>% 
ggplot()+
  geom_line(aes(x=year, y=deathage, colour=fct_rev(age)))+
  scale_x_continuous(name="Year")+
  scale_y_continuous(name="Expected age at death")+
  scale_colour_paletteer_d(pal="pals::stepped", name="Age")+
  facet_wrap(~sex)+
  theme_classic()+
  theme(strip.background = element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Life expectancy improvements in England have stalled across all age groups",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

#Bring in shapefile
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/1d78d47c87df4212b79fe2323aae8e08_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name))

names(shapefile)[names(shapefile) == "lad19cd"] <- "code"

#Sort out Buckinghamshire to match hex template
shapefile$code <- if_else(shapefile$code %in% c("E07000004", "E07000005", "E07000006", "E07000007"),  "E06000060", as.character(shapefile$code))

simplemap <- ms_simplify(shapefile, keep=0.2, keep_shapes = TRUE)

map.data <- data_long %>% 
  filter(metric=="central" & age=="<1") %>% 
  group_by(name, sex, code) %>% 
  summarise(change0411=LE[year==2011]-LE[year==2004], change1811=LE[year==2018]-LE[year==2011],
            LE=LE[year==2018]) %>% 
  gather(period, change, c("change0411", "change1811")) %>% 
  ungroup()

map.cases <- full_join(simplemap, map.data, by="code", all.y=TRUE)

ggplot(subset(map.cases, sex=="Male"))+
  geom_sf(aes(geometry=geometry, fill=change), colour=NA)+
  scale_fill_paletteer_c("viridis::magma", name="")+
  facet_wrap(~period)+
  theme_classic()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank())

tiff("Outputs/LEcorr.tiff", units="in", width=8, height=6, res=500)
data_long %>% 
  filter(metric=="central" & age=="<1") %>% 
  group_by(name, sex, code) %>% 
  summarise(change0411=LE[year==2011]-LE[year==2004], change1811=LE[year==2018]-LE[year==2011]) %>%
ggplot(aes(x=change0411, y=change1811, colour=sex))+
  geom_point(show.legend=FALSE)+
  scale_x_continuous(name="Change in Life Expectancy at birth 2004-2011")+
  scale_y_continuous(name="Change in Life Expectancy at birth 2011-2018")+
  scale_colour_manual(values=c("#00cc99", "#6600cc"))+
  facet_wrap(~sex)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Improvements in Life Expectancy before 2011 don't seem to predict subsequent changes",
       subtitle="Changes in period Life Expectancy at birth in Local Authorities across the UK",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

#Bring in IMD data for England
temp <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833995/File_10_-_IoD2019_Local_Authority_District_Summaries__lower-tier__.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
IMDdata <- read_excel(temp, sheet="IMD", range="A1:D318", col_names=TRUE)
colnames(IMDdata) <-  c("code", "name", "IMDrank", "IMDorder")
IMDdata$quintile <- ntile(IMDdata$IMDrank, 5)
IMDdata$tertile <- ntile(IMDdata$IMDrank, 3)

#Sort out Buckinghamshire
IMDdata$code <- if_else(IMDdata$code %in% c("E07000004", "E07000005", "E07000006", "E07000007"),  "E06000060", as.character(IMDdata$code))

#Combine
IMDmap <- full_join(map.cases, IMDdata[,c(1,4,5,6)], by="code")

#calculate tertiles for changes in LE in each period
IMDmap <- IMDmap %>% 
  group_by(sex,period) %>% 
  mutate(tertilechange=ntile(change,3))

#Highlight areas with -ve LE growth
IMDmap$negflag <- if_else(IMDmap$change<0, TRUE, FALSE)

#Set up bivariate colour scheme
IMDmap$col <- case_when(
  IMDmap$tertilechange==1 & IMDmap$tertile==1 ~ "#e8e8e8",
  IMDmap$tertilechange==1 & IMDmap$tertile==2 ~ "#b5c0da",
  IMDmap$tertilechange==1 & IMDmap$tertile==3 ~ "#6c83b5",
  IMDmap$tertilechange==2 & IMDmap$tertile==1 ~ "#b8d6be",
  IMDmap$tertilechange==2 & IMDmap$tertile==2 ~ "#90b2b3",
  IMDmap$tertilechange==2 & IMDmap$tertile==3 ~ "#567994",
  IMDmap$tertilechange==3 & IMDmap$tertile==1 ~ "#73ae80",
  IMDmap$tertilechange==3 & IMDmap$tertile==2 ~ "#5a9178",
  IMDmap$tertilechange==3 & IMDmap$tertile==3 ~ "#2a5a5b"
)

#Bivariate map
ggplot(subset(IMDmap, !is.na(quintile) & !is.na(period) & !is.na(sex)))+
  geom_sf(aes(geometry=geometry, fill=col), colour=NA)+
  scale_fill_identity()+
  facet_grid(sex~period)+
  theme_classic()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank())

labels <- as_labeller(c(`change0411`="2004-11", `change1811`="2011-18", `Male`="Male", `Female`="Female"))

tiff("Outputs/LEchangexIMD.tiff", units="in", width=9, height=6, res=500)
ggplot(subset(IMDmap, !is.na(quintile) & !is.na(period) & !is.na(sex)),
       aes(x=IMDorder, y=change))+
  geom_point(aes(colour=negflag), show.legend=FALSE)+
  geom_smooth(method="lm", formula=y~x, colour="Black")+
  scale_x_continuous(name="Index of Multiple Deprivation rank")+
  scale_y_continuous(name="Change in Life Expectancy at Birth")+
  scale_colour_manual(values=c("Grey60", "tomato"))+
  facet_grid(sex~period, labeller=labels)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.subtitle=element_markdown())+
  labs(title="Since 2011, Life Expectancy has risen more slowly in the most deprived Local Authorities",
       subtitle="Changes in Life Expectancy at birth in English Local Authorities in the 7 years before/after 2011.<br> Authorities ordered by the Index of Multiple Deprivation, lower ranking = more deprived.<br>Areas where Life Expectancy has fallen are highlighted in <span style='color:tomato;'>red</span>.",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()
