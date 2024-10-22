rm(list=ls())

library(tidyverse)
library(curl)
library(scales)
library(readxl)
library(paletteer)
library(gtools)
library(extrafont)
library(ragg)

options(scipen=999999999)

#Set common font for all plots
font <- "Lato"

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          strip.clip="off",
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"),
          plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
          plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),
          axis.text=element_text(colour="Grey40"),
          axis.title=element_text(colour="Grey20"),
          legend.text=element_text(colour="Grey40"),
          legend.title=element_text(colour="Grey20"),
          axis.line.x=element_blank(),
          panel.grid.major.y=element_line(colour="grey95"))
}

#Download ONS population estimates

#2023 data isn't yet available at LSOA level - it should appear here:
#https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates

#2021-22
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2021andmid2022/sapelsoasyoatablefinal.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

Pop2022 <- read_excel(temp, sheet="Mid-2022 LSOA 2021", range="C4:GE35676") %>% 
  gather(Group, Pop, c(4:185)) %>% 
  mutate(Sex=if_else(substr(Group, 1, 1)=="F", "Female", "Male"),
         Age=as.numeric(substr(Group, 2, 3))) %>% 
  group_by(`LSOA 2021 Code`, `LSOA 2021 Name`, Sex, Age) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

Pop2021 <- read_excel(temp, sheet="Mid-2021 LSOA 2021", range="C4:GE35676") %>% 
  gather(Group, Pop, c(4:185)) %>% 
  mutate(Sex=if_else(substr(Group, 1, 1)=="F", "Female", "Male"),
         Age=as.numeric(substr(Group, 2, 3))) %>% 
  group_by(`LSOA 2021 Code`, `LSOA 2021 Name`, Sex, Age) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2020
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2020sape23dt2/sape23dt2mid2020lsoasyoaestimatesunformatted.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

Pop2020 <- bind_rows(read_excel(temp, sheet="Mid-2020 Males", range="A5:CT34758") %>% 
                       gather(Age, Pop, c(8:98)) %>% 
                       mutate(Sex="Male"),
                     read_excel(temp, sheet="Mid-2020 Females", range="A5:CT34758") %>% 
                       gather(Age, Pop, c(8:98)) %>% 
                       mutate(Sex="Female")) %>% 
  mutate(Age=gsub("\\+", "", Age),
         Age=as.numeric(Age)) %>% 
  group_by(`LSOA Code`, `LSOA Name`, Sex, Age) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2019
temp <- tempfile()
temp2 <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2019sape22dt2/sape22dt2mid2019lsoasyoaestimatesunformatted.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

Pop2019 <- bind_rows(read_excel(file.path(temp2, "SAPE22DT2-mid-2019-lsoa-syoa-estimates-unformatted.xlsx"), 
                                sheet="Mid-2019 Males", range="A5:CT34758") %>% 
                       gather(Age, Pop, c(8:98)) %>% 
                       mutate(Sex="Male"),
                     read_excel(file.path(temp2, "SAPE22DT2-mid-2019-lsoa-syoa-estimates-unformatted.xlsx"), 
                                sheet="Mid-2019 Females", range="A5:CT34758") %>% 
                       gather(Age, Pop, c(8:98)) %>% 
                       mutate(Sex="Female")) %>% 
  mutate(Age=gsub("\\+", "", Age),
         Age=as.numeric(Age)) %>% 
  group_by(`LSOA Code`, `LSOA Name`, Sex, Age) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2018
temp <- tempfile()
temp2 <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2018sape21dt1a/sape21dt1amid2018on2019lalsoasyoaestimatesformatted.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

Pop2018 <- bind_rows(read_excel(file.path(temp2, "SAPE21DT1a-mid-2018-on-2019-LA-lsoa-syoa-estimates-formatted.xlsx"), 
                                sheet="Mid-2018 Males", range="A5:CQ35097") %>% 
                       gather(Age, Pop, c(5:95)) %>% 
                       mutate(Sex="Male"),
                     read_excel(file.path(temp2, "SAPE21DT1a-mid-2018-on-2019-LA-lsoa-syoa-estimates-formatted.xlsx"), 
                                sheet="Mid-2018 Females", range="A5:CQ35097") %>% 
                       gather(Age, Pop, c(5:95)) %>% 
                       mutate(Sex="Female")) %>% 
  mutate(Age=gsub("\\+", "", Age),
         Age=as.numeric(Age)) %>% 
  group_by(`Area Codes`, `LSOA`, Sex, Age) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2017
temp <- tempfile()
temp2 <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2017/sape20dt1mid2017lsoasyoaestimatesformatted.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

Pop2017 <- bind_rows(read_excel(file.path(temp2, "SAPE20DT1-mid-2017-lsoa-syoa-estimates-formatted.XLS"), 
                                sheet="Mid-2017 Males", range="A5:CQ35106") %>% 
                       gather(Age, Pop, c(5:95)) %>% 
                       mutate(Sex="Male"),
                     read_excel(file.path(temp2, "SAPE20DT1-mid-2017-lsoa-syoa-estimates-formatted.XLS"), 
                                sheet="Mid-2017 Females", range="A5:CQ35106") %>% 
                       gather(Age, Pop, c(5:95)) %>% 
                       mutate(Sex="Female")) %>% 
  mutate(Age=gsub("\\+", "", Age),
         Age=as.numeric(Age)) %>% 
  group_by(`Area Codes`, `...3`, Sex, Age) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2016
temp <- tempfile()
temp2 <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2016/sape20dt1mid2016lsoasyoaestimatesformatted.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

Pop2016 <- bind_rows(read_excel(file.path(temp2, "SAPE20DT1-mid-2016-lsoa-syoa-estimates-formatted.xls"), 
                                sheet="Mid-2016 Males", range="A5:CQ35106") %>% 
                       gather(Age, Pop, c(5:95)) %>% 
                       mutate(Sex="Male"),
                     read_excel(file.path(temp2, "SAPE20DT1-mid-2016-lsoa-syoa-estimates-formatted.xls"), 
                                sheet="Mid-2016 Females", range="A5:CQ35106") %>% 
                       gather(Age, Pop, c(5:95)) %>% 
                       mutate(Sex="Female")) %>% 
  mutate(Age=gsub("\\+", "", Age),
         Age=as.numeric(Age)) %>% 
  group_by(`Area Codes`, `...3`, Sex, Age) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2015
temp <- tempfile()
temp2 <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2015/sape20dt1mid2015lsoasyoaestimatesformatted.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

Pop2015 <- bind_rows(read_excel(file.path(temp2, "SAPE20DT1-mid-2015-lsoa-syoa-estimates-formatted.xls"), 
                                sheet="Mid-2015 Males", range="A5:CQ35106") %>% 
                       gather(Age, Pop, c(5:95)) %>% 
                       mutate(Sex="Male"),
                     read_excel(file.path(temp2, "SAPE20DT1-mid-2015-lsoa-syoa-estimates-formatted.xls"), 
                                sheet="Mid-2015 Females", range="A5:CQ35106") %>% 
                       gather(Age, Pop, c(5:95)) %>% 
                       mutate(Sex="Female")) %>% 
  mutate(Age=gsub("\\+", "", Age),
         Age=as.numeric(Age)) %>% 
  group_by(`Area Codes`, `...3`, Sex, Age) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2014
temp <- tempfile()
temp2 <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2014/sape20dt1mid2014lsoasyoaestimatesformatted.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

Pop2014 <- bind_rows(read_excel(file.path(temp2, "SAPE20DT1-mid-2014-lsoa-syoa-estimates-formatted.xls"), 
                                sheet="Mid-2014 Males", range="A5:CQ35106") %>% 
                       gather(Age, Pop, c(5:95)) %>% 
                       mutate(Sex="Male"),
                     read_excel(file.path(temp2, "SAPE20DT1-mid-2014-lsoa-syoa-estimates-formatted.xls"), 
                                sheet="Mid-2014 Females", range="A5:CQ35106") %>% 
                       gather(Age, Pop, c(5:95)) %>% 
                       mutate(Sex="Female")) %>% 
  mutate(Age=gsub("\\+", "", Age),
         Age=as.numeric(Age)) %>% 
  group_by(`Area Codes`, `...3`, Sex, Age) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2013
temp <- tempfile()
temp2 <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2013/sape20dt1mid2013lsoasyoaestimatesformatted.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

Pop2013 <- bind_rows(read_excel(file.path(temp2, "SAPE20DT1-mid-2013-lsoa-syoa-estimates-formatted.xls"), 
                                sheet="Mid-2013 Males", range="A5:CQ35106") %>% 
                       gather(Age, Pop, c(5:95)) %>% 
                       mutate(Sex="Male"),
                     read_excel(file.path(temp2, "SAPE20DT1-mid-2013-lsoa-syoa-estimates-formatted.xls"), 
                                sheet="Mid-2013 Females", range="A5:CQ35106") %>% 
                       gather(Age, Pop, c(5:95)) %>% 
                       mutate(Sex="Female")) %>% 
  mutate(Age=gsub("\\+", "", Age),
         Age=as.numeric(Age)) %>% 
  group_by(`Area Codes`, `...3`, Sex, Age) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2012
temp <- tempfile()
temp2 <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2012/sape20dt1mid2012lsoasyoaestimatesformatted.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

Pop2012 <- bind_rows(read_excel(file.path(temp2, "SAPE20DT1-mid-2012-lsoa-syoa-estimates-formatted.xls"), 
                                sheet="Mid-2012 Males", range="A5:CQ35106") %>% 
                       gather(Age, Pop, c(5:95)) %>% 
                       mutate(Sex="Male"),
                     read_excel(file.path(temp2, "SAPE20DT1-mid-2012-lsoa-syoa-estimates-formatted.xls"), 
                                sheet="Mid-2012 Females", range="A5:CQ35106") %>% 
                       gather(Age, Pop, c(5:95)) %>% 
                       mutate(Sex="Female")) %>% 
  mutate(Age=gsub("\\+", "", Age),
         Age=as.numeric(Age)) %>% 
  group_by(`Area Codes`, `...3`, Sex, Age) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2002-11 data is combined in a single file for each sex
#Males
temp <- tempfile()
temp2 <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2002tomid2011males/rftlsoaunformattedtablemales.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

Pop2011m <- read_excel(file.path(temp2, "SAPE8DT2b-LSOA-syoa-unformatted-males-mid2007-to-mid2010.xls"), 
                       sheet="Mid-2011", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Male")

Pop2010m <- read_excel(file.path(temp2, "SAPE8DT2b-LSOA-syoa-unformatted-males-mid2007-to-mid2010.xls"), 
                       sheet="Mid-2010", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Male")

Pop2009m <- read_excel(file.path(temp2, "SAPE8DT2b-LSOA-syoa-unformatted-males-mid2007-to-mid2010.xls"), 
                       sheet="Mid-2009", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Male")

Pop2008m <- read_excel(file.path(temp2, "SAPE8DT2b-LSOA-syoa-unformatted-males-mid2007-to-mid2010.xls"), 
                       sheet="Mid-2008", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Male")

Pop2007m <- read_excel(file.path(temp2, "SAPE8DT2b-LSOA-syoa-unformatted-males-mid2007-to-mid2010.xls"), 
                       sheet="Mid-2007", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Male")

Pop2006m <- read_excel(file.path(temp2, "SAPE8DT2a-LSOA-syoa-unformatted-males-mid2002-to-mid2006.xls"), 
                       sheet="Mid-2006", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Male") 

Pop2005m <- read_excel(file.path(temp2, "SAPE8DT2a-LSOA-syoa-unformatted-males-mid2002-to-mid2006.xls"), 
                       sheet="Mid-2005", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Male") 

Pop2004m <- read_excel(file.path(temp2, "SAPE8DT2a-LSOA-syoa-unformatted-males-mid2002-to-mid2006.xls"), 
                       sheet="Mid-2004", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Male") 

Pop2003m <- read_excel(file.path(temp2, "SAPE8DT2a-LSOA-syoa-unformatted-males-mid2002-to-mid2006.xls"), 
                       sheet="Mid-2003", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Male") 

Pop2002m <- read_excel(file.path(temp2, "SAPE8DT2a-LSOA-syoa-unformatted-males-mid2002-to-mid2006.xls"), 
                       sheet="Mid-2002", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Male") 

#Females
temp <- tempfile()
temp2 <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2002tomid2011females/rftlsoaunformattedtablefemales.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

Pop2011f <- read_excel(file.path(temp2, "SAPE8DT3b-LSOA-syoa-unformatted-females-mid2007-to-mid2010.xls"), 
                       sheet="Mid-2011", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Female")

Pop2010f <- read_excel(file.path(temp2, "SAPE8DT3b-LSOA-syoa-unformatted-females-mid2007-to-mid2010.xls"), 
                       sheet="Mid-2010", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Female")

Pop2009f <- read_excel(file.path(temp2, "SAPE8DT3b-LSOA-syoa-unformatted-females-mid2007-to-mid2010.xls"), 
                       sheet="Mid-2009", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Female")

Pop2008f <- read_excel(file.path(temp2, "SAPE8DT3b-LSOA-syoa-unformatted-females-mid2007-to-mid2010.xls"), 
                       sheet="Mid-2008", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Female")

Pop2007f <- read_excel(file.path(temp2, "SAPE8DT3b-LSOA-syoa-unformatted-females-mid2007-to-mid2010.xls"), 
                       sheet="Mid-2007", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Female")

Pop2006f <- read_excel(file.path(temp2, "SAPE8DT3a-LSOA-syoa-unformatted-females-mid2002-to-mid2006.xls"), 
                       sheet="Mid-2006", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Female") 

Pop2005f <- read_excel(file.path(temp2, "SAPE8DT3a-LSOA-syoa-unformatted-females-mid2002-to-mid2006.xls"), 
                       sheet="Mid-2005", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Female") 

Pop2004f <- read_excel(file.path(temp2, "SAPE8DT3a-LSOA-syoa-unformatted-females-mid2002-to-mid2006.xls"), 
                       sheet="Mid-2004", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Female") 

Pop2003f <- read_excel(file.path(temp2, "SAPE8DT3a-LSOA-syoa-unformatted-females-mid2002-to-mid2006.xls"), 
                       sheet="Mid-2003", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Female") 

Pop2002f <- read_excel(file.path(temp2, "SAPE8DT3a-LSOA-syoa-unformatted-females-mid2002-to-mid2006.xls"), 
                       sheet="Mid-2002", range="A1:CQ34754") %>% 
  gather(Age, Pop, c(5:95)) %>% 
  mutate(Sex="Female") 

#Bring sexes together and tidy up
Pop2011 <- bind_rows(Pop2011m, Pop2011f) %>% 
  mutate(Age=gsub("m", "", Age),
         Age=gsub("f", "", Age),
         Age=gsub("plus", "", Age),
         Age=as.numeric(Age)) %>% 
  group_by(LSOA11CD, Sex, Age) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

Pop2010 <- bind_rows(Pop2010m, Pop2010f) %>% 
  mutate(Age=gsub("m", "", Age),
         Age=gsub("f", "", Age),
         Age=gsub("plus", "", Age),
         Age=as.numeric(Age)) %>% 
  group_by(LSOA11CD, Sex, Age) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

Pop2009 <- bind_rows(Pop2009m, Pop2009f) %>% 
  mutate(Age=gsub("m", "", Age),
         Age=gsub("f", "", Age),
         Age=gsub("plus", "", Age),
         Age=as.numeric(Age)) %>% 
  group_by(LSOA11CD, Sex, Age) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

Pop2008 <- bind_rows(Pop2008m, Pop2008f) %>% 
  mutate(Age=gsub("m", "", Age),
         Age=gsub("f", "", Age),
         Age=gsub("plus", "", Age),
         Age=as.numeric(Age)) %>% 
  group_by(LSOA11CD, Sex, Age) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

Pop2007 <- bind_rows(Pop2007m, Pop2007f) %>% 
  mutate(Age=gsub("m", "", Age),
         Age=gsub("f", "", Age),
         Age=gsub("plus", "", Age),
         Age=as.numeric(Age)) %>% 
  group_by(LSOA11CD, Sex, Age) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

Pop2006 <- bind_rows(Pop2006m, Pop2006f) %>% 
  mutate(Age=gsub("m", "", Age),
         Age=gsub("f", "", Age),
         Age=gsub("plus", "", Age),
         Age=as.numeric(Age)) %>% 
  group_by(LSOA11CD, Sex, Age) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

Pop2005 <- bind_rows(Pop2005m, Pop2005f) %>% 
  mutate(Age=gsub("m", "", Age),
         Age=gsub("f", "", Age),
         Age=gsub("plus", "", Age),
         Age=as.numeric(Age)) %>% 
  group_by(LSOA11CD, Sex, Age) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

Pop2004 <- bind_rows(Pop2004m, Pop2004f) %>% 
  mutate(Age=gsub("m", "", Age),
         Age=gsub("f", "", Age),
         Age=gsub("plus", "", Age),
         Age=as.numeric(Age)) %>% 
  group_by(LSOA11CD, Sex, Age) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

Pop2003 <- bind_rows(Pop2003m, Pop2003f) %>% 
  mutate(Age=gsub("m", "", Age),
         Age=gsub("f", "", Age),
         Age=gsub("plus", "", Age),
         Age=as.numeric(Age)) %>% 
  group_by(LSOA11CD, Sex, Age) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

Pop2002 <- bind_rows(Pop2002m, Pop2002f) %>% 
  mutate(Age=gsub("m", "", Age),
         Age=gsub("f", "", Age),
         Age=gsub("plus", "", Age),
         Age=as.numeric(Age)) %>% 
  group_by(LSOA11CD, Sex, Age) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

####################################################################
#Download IMD data for all available years (updated every 3-4 years) 

#2019
temp <- tempfile()
url <- "https://assets.publishing.service.gov.uk/media/5d8b3abded915d0373d3540f/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

IMD19 <- read_excel(temp, sheet="IMD2019", range="A1:F32845") %>% 
  mutate(IMDq=case_when(
    `Index of Multiple Deprivation (IMD) Decile` %in% c(1,2) ~ "Q5 (most deprived)",
    `Index of Multiple Deprivation (IMD) Decile` %in% c(3,4) ~ "Q4",
    `Index of Multiple Deprivation (IMD) Decile` %in% c(5,6) ~ "Q3",
    `Index of Multiple Deprivation (IMD) Decile` %in% c(7,8) ~ "Q2",
    `Index of Multiple Deprivation (IMD) Decile` %in% c(9,10) ~ "Q1 (least deprived)"
  ))

#2015
temp <- tempfile()
url <- "https://assets.publishing.service.gov.uk/media/5a805f96ed915d74e33fa0df/File_1_ID_2015_Index_of_Multiple_Deprivation.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

IMD15 <- read_excel(temp, sheet="IMD 2015", range="A1:F32845") %>% 
  mutate(IMDq=case_when(
    `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)` %in% c(1,2) ~ "Q5 (most deprived)",
    `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)` %in% c(3,4) ~ "Q4",
    `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)` %in% c(5,6) ~ "Q3",
    `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)` %in% c(7,8) ~ "Q2",
    `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)` %in% c(9,10) ~ "Q1 (least deprived)"
  ))

#2010
temp <- tempfile()
url <- "https://assets.publishing.service.gov.uk/media/5a79b8c6ed915d042206a8e2/1871524.xls"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

IMD10 <- read_excel(temp, sheet="IMD 2010", range="A1:G32483") %>% 
  mutate(quintile=quantcut(`RANK OF IMD SCORE (where 1 is most deprived)`, 5))

IMD10 <- IMD10 %>% 
  mutate(IMDq=case_when(
    quintile==levels(IMD10$quintile)[1] ~ "Q5 (most deprived)",
    quintile==levels(IMD10$quintile)[2] ~ "Q4",           
    quintile==levels(IMD10$quintile)[3] ~ "Q3",           
    quintile==levels(IMD10$quintile)[4] ~ "Q2",           
    quintile==levels(IMD10$quintile)[5] ~ "Q1 (least deprived)"
    ))

#2007
temp <- tempfile()
temp2 <- tempfile()
source <- "https://webarchive.nationalarchives.gov.uk/ukgwa/20100411141238mp_/http://www.communities.gov.uk/documents/communities/zip/indices2007.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

IMD07 <- read_excel(file.path(temp2, "IMD 2007 for DCLG 4 dec.xls"),
                    sheet="IMD 2007", range=c("A1:G32483")) %>% 
  mutate(quintile=quantcut(`RANK OF IMD (where 1 is most deprived)`, 5))

IMD07 <- IMD07 %>% 
  mutate(IMDq=case_when(
    quintile==levels(IMD07$quintile)[1] ~ "Q5 (most deprived)",
    quintile==levels(IMD07$quintile)[2] ~ "Q4",           
    quintile==levels(IMD07$quintile)[3] ~ "Q3",           
    quintile==levels(IMD07$quintile)[4] ~ "Q2",           
    quintile==levels(IMD07$quintile)[5] ~ "Q1 (least deprived)"
    ))

#2004
temp <- tempfile()
temp2 <- tempfile()
source <- "https://webarchive.nationalarchives.gov.uk/ukgwa/20100407164233mp_/http://www.communities.gov.uk/documents/communities/zip/soalevelid.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

IMD04 <- read_excel(file.path(temp2, "SOA levelid2004.xls"),
                    sheet="IMD 2004", range=c("A1:G32483")) %>% 
  mutate(quintile=quantcut(`RANK OF IMD (where 1 is most deprived)`, 5))

IMD04 <- IMD04 %>% 
  mutate(IMDq=case_when(
    quintile==levels(IMD04$quintile)[1] ~ "Q5 (most deprived)",
    quintile==levels(IMD04$quintile)[2] ~ "Q4",           
    quintile==levels(IMD04$quintile)[3] ~ "Q3",           
    quintile==levels(IMD04$quintile)[4] ~ "Q2",           
    quintile==levels(IMD04$quintile)[5] ~ "Q1 (least deprived)"
    ))

#Read in LSOA2011 - LSOA2021 code lookup
temp <- tempfile()
url <- "https://hub.arcgis.com/api/v3/datasets/b14d449ba10a48508bd05cd4a9775e2b_0/downloads/data?format=csv&spatialRefId=3857&where=1%3D1"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

LSOA1121Lookup <- read.csv(temp) %>% 
  select(LSOA11CD, LSOA21CD)

#Read in LSOA2001 - LSOA2011 code lookup
temp <- tempfile()
url <- "https://opendata.arcgis.com/api/v3/datasets/3dd1bc5dd053426aa84a068c7afbb3b2_0/downloads/data?format=csv&spatialRefId=4326&where=1%3D1"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

LSOA0111Lookup <- read.csv(temp) %>% 
  select(LSOA01CD, LSOA11CD)

############################################################################
#Pull together to get IMD quintile, age and sex-specific populations for each year
#2022 and IMD2019
Pop22 <- Pop2022 %>% 
  merge(LSOA1121Lookup, by.x="LSOA 2021 Code", by.y="LSOA21CD") %>% 
  merge(IMD19, by.x="LSOA11CD", by.y="LSOA code (2011)") %>% 
  group_by(Age, Sex, IMDq) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2021 and IMD2019
Pop21 <- Pop2021 %>% 
  merge(LSOA1121Lookup, by.x="LSOA 2021 Code", by.y="LSOA21CD") %>% 
  merge(IMD19, by.x="LSOA11CD", by.y="LSOA code (2011)") %>% 
  group_by(Age, Sex, IMDq) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2020 and IMD2019
Pop20 <- Pop2020 %>% 
  merge(IMD19, by.x="LSOA Code", by.y="LSOA code (2011)") %>% 
  group_by(Age, Sex, IMDq) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2019 and IMD2019
Pop19 <- Pop2019 %>% 
  merge(IMD19, by.x="LSOA Code", by.y="LSOA code (2011)") %>% 
  group_by(Age, Sex, IMDq) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2018 and IMD2019
Pop18 <- Pop2018 %>% 
  merge(IMD19, by.x="Area Codes", by.y="LSOA code (2011)") %>% 
  group_by(Age, Sex, IMDq) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2017 and IMD2015
Pop17 <- Pop2017 %>% 
  merge(IMD15, by.x="Area Codes", by.y="LSOA code (2011)") %>% 
  group_by(Age, Sex, IMDq) %>% 
  summarise(Pop=sum(Pop), .groups="drop") 

#2016 and IMD2015
Pop16 <- Pop2016 %>% 
  merge(IMD15, by.x="Area Codes", by.y="LSOA code (2011)") %>% 
  group_by(Age, Sex, IMDq) %>% 
  summarise(Pop=sum(Pop), .groups="drop") 

#2015 and IMD2015
Pop15 <- Pop2015 %>% 
  merge(IMD15, by.x="Area Codes", by.y="LSOA code (2011)") %>% 
  group_by(Age, Sex, IMDq) %>% 
  summarise(Pop=sum(Pop), .groups="drop") 

#2014 and IMD2015
Pop14 <- Pop2014 %>% 
  merge(IMD15, by.x="Area Codes", by.y="LSOA code (2011)") %>% 
  group_by(Age, Sex, IMDq) %>% 
  summarise(Pop=sum(Pop), .groups="drop") 

#2013 and IMD2015
Pop13 <- Pop2013 %>% 
  merge(IMD15, by.x="Area Codes", by.y="LSOA code (2011)") %>% 
  group_by(Age, Sex, IMDq) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2012 and IMD2010
Pop12 <- Pop2012 %>% 
  merge(LSOA0111Lookup, by.x="Area Codes", by.y="LSOA11CD") %>% 
  merge(IMD10, by.x="LSOA01CD", by.y="LSOA CODE") %>% 
  group_by(Age, Sex, IMDq) %>% 
  summarise(Pop=sum(Pop), .groups="drop") 

#2011 and IMD2010
Pop11 <- Pop2011 %>% 
  merge(LSOA0111Lookup, by="LSOA11CD") %>% 
  merge(IMD10, by.x="LSOA01CD", by.y="LSOA CODE") %>% 
  group_by(Age, Sex, IMDq) %>% 
  summarise(Pop=sum(Pop), .groups="drop") 

#2010 and IMD2010
Pop10 <- Pop2010 %>% 
  merge(LSOA0111Lookup, by="LSOA11CD") %>% 
  merge(IMD10, by.x="LSOA01CD", by.y="LSOA CODE") %>% 
  group_by(Age, Sex, IMDq) %>% 
  summarise(Pop=sum(Pop), .groups="drop")

#2009 and IMD2010
Pop09 <- Pop2009 %>% 
  merge(LSOA0111Lookup, by="LSOA11CD") %>% 
  merge(IMD10, by.x="LSOA01CD", by.y="LSOA CODE") %>% 
  group_by(Age, Sex, IMDq) %>% 
  summarise(Pop=sum(Pop), .groups="drop") 

#2008 and IMD2007
Pop08 <- Pop2008 %>% 
  merge(LSOA0111Lookup, by="LSOA11CD") %>% 
  merge(IMD07, by.x="LSOA01CD", by.y="LSOA") %>% 
  group_by(Age, Sex, IMDq) %>% 
  summarise(Pop=sum(Pop), .groups="drop") 

#2007 and IMD2007
Pop07 <- Pop2007 %>% 
  merge(LSOA0111Lookup, by="LSOA11CD") %>% 
  merge(IMD07, by.x="LSOA01CD", by.y="LSOA") %>% 
  group_by(Age, Sex, IMDq) %>% 
  summarise(Pop=sum(Pop), .groups="drop") 

#2006 and IMD2007
Pop06 <- Pop2006 %>% 
  merge(LSOA0111Lookup, by="LSOA11CD") %>% 
  merge(IMD07, by.x="LSOA01CD", by.y="LSOA") %>% 
  group_by(Age, Sex, IMDq) %>% 
  summarise(Pop=sum(Pop), .groups="drop") 

#2005 and IMD2004
Pop05 <- Pop2005 %>% 
  merge(LSOA0111Lookup, by="LSOA11CD") %>% 
  merge(IMD04, by.x="LSOA01CD", by.y="SOA") %>% 
  group_by(Age, Sex, IMDq) %>% 
  summarise(Pop=sum(Pop), .groups="drop") 

#2004 and IMD2004
Pop04 <- Pop2004 %>% 
  merge(LSOA0111Lookup, by="LSOA11CD") %>% 
  merge(IMD04, by.x="LSOA01CD", by.y="SOA") %>% 
  group_by(Age, Sex, IMDq) %>% 
  summarise(Pop=sum(Pop), .groups="drop") 

#2003 and IMD2004
Pop03 <- Pop2003 %>% 
  merge(LSOA0111Lookup, by="LSOA11CD") %>% 
  merge(IMD04, by.x="LSOA01CD", by.y="SOA") %>% 
  group_by(Age, Sex, IMDq) %>% 
  summarise(Pop=sum(Pop), .groups="drop") 

#2002 and IMD2004
Pop02 <- Pop2002 %>% 
  merge(LSOA0111Lookup, by="LSOA11CD") %>% 
  merge(IMD04, by.x="LSOA01CD", by.y="SOA") %>% 
  group_by(Age, Sex, IMDq) %>% 
  summarise(Pop=sum(Pop), .groups="drop") 

#Stick all the populations together
PopFinal <- bind_rows(Pop22 %>% mutate(Year=2022),
                      Pop21 %>% mutate(Year=2021),
                      Pop20 %>% mutate(Year=2020),
                      Pop19 %>% mutate(Year=2019),
                      Pop18 %>% mutate(Year=2018),
                      Pop17 %>% mutate(Year=2017),
                      Pop16 %>% mutate(Year=2016),
                      Pop15 %>% mutate(Year=2015),
                      Pop14 %>% mutate(Year=2014),
                      Pop13 %>% mutate(Year=2013),
                      Pop12 %>% mutate(Year=2012),
                      Pop11 %>% mutate(Year=2011),
                      Pop10 %>% mutate(Year=2010),
                      Pop09 %>% mutate(Year=2009),
                      Pop08 %>% mutate(Year=2008),
                      Pop07 %>% mutate(Year=2007),
                      Pop06 %>% mutate(Year=2006),
                      Pop05 %>% mutate(Year=2005),
                      Pop04 %>% mutate(Year=2004),
                      Pop03 %>% mutate(Year=2003),
                      Pop02 %>% mutate(Year=2002)) %>% 
  set_names("Age", "Sex", "IMDq", "Pop", "Year") 

#Plot to check everything looks sensible
ggplot(PopFinal, aes(x=Year, y=Age, fill=Pop))+
  geom_tile()+
  scale_x_continuous(name="")+
  scale_y_discrete(name="Age")+
  scale_fill_paletteer_c("viridis::turbo")+
  facet_grid(Sex~IMDq)+
  theme_classic()+
  coord_equal()+
  theme(strip.background = element_blank(), strip.clip = "none")

#Write out population data
write.csv(PopFinal, "Outputs/EngPopxSexxAgexIMD2002-2022.csv")

#Combine sexes for summary plot
agg_png("Outputs/ENGPopLexisxIMDq.png", units="in", width=9, height=6, res=600)
PopFinal %>% group_by(Age, IMDq, Year) %>% 
  summarise(Pop=sum(Pop), .groups="drop") %>% 
  filter(Age<90) %>% 
  ggplot(aes(x=Year, y=Age, fill=Pop))+
  geom_tile()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age")+
  scale_fill_paletteer_c("viridis::turbo", name="Population", limits=c(0,NA))+
  theme_custom()+
  facet_wrap(~~IMDq, nrow=1)+
  coord_equal()+
  theme(axis.text.x=element_text(angle=45, hjust=1),
        axis.line.y=element_blank())+
  labs(title="Young people live in more deprived areas in England",
       subtitle="England's changing population structure 2002-2022 by quintiles of the Index of Multiple Deprivation\n",
       caption="Data from ONS and MHCLG | Analysis and plot by @VictimOfMaths")

dev.off()