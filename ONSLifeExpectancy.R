rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(forcats)
library(paletteer)

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

data_long %>% 
  filter(name=="England") %>% 
ggplot()+
  geom_line(aes(x=year, y=deathage, colour=fct_rev(age)))+
  scale_x_continuous(name="Year")+
  scale_y_continuous(name="Expected age at death")+
  scale_colour_paletteer_d(pal="ggthemes::Classic Cyclic")+
  facet_wrap(~sex)+
  theme_classic()