rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)

temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/weekly-monthly-births-deaths-data/2020/mar/weekly-march-20.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data04 <- read_excel(temp, sheet="2004", range="A6:D57", col_names=FALSE)
data05 <- read_excel(temp, sheet="2005", range="A6:D58", col_names=FALSE)
data06 <- read_excel(temp, sheet="2006", range="A6:D57", col_names=FALSE)
data07 <- read_excel(temp, sheet="2007", range="A6:D57", col_names=FALSE)
data08 <- read_excel(temp, sheet="2008", range="A6:D57", col_names=FALSE)
data09 <- read_excel(temp, sheet="2009", range="A6:D57", col_names=FALSE)
data10 <- read_excel(temp, sheet="2010", range="A6:D57", col_names=FALSE)
data11 <- read_excel(temp, sheet="2011", range="A6:D58", col_names=FALSE)
data12 <- read_excel(temp, sheet="2012", range="A6:D57", col_names=FALSE)
data13 <- read_excel(temp, sheet="2013", range="A6:D57", col_names=FALSE)
data14 <- read_excel(temp, sheet="2014", range="A6:D57", col_names=FALSE)
data15 <- read_excel(temp, sheet="2015 ", range="A6:D58", col_names=FALSE)
data16 <- read_excel(temp, sheet="2016", range="E6:G57", col_names=FALSE)
data17 <- read_excel(temp, sheet="2017", range="E6:G57", col_names=FALSE)
data18 <- read_excel(temp, sheet="2018", range="E6:G57", col_names=FALSE)
data19 <- read_excel(temp, sheet="2019", range="E6:G57", col_names=FALSE)
data20 <- read_excel(temp, sheet="2020", range="E6:G19", col_names=FALSE)

data04$year <- "2004"
data05$year <- "2005"
data06$year <- "2006"
data07$year <- "2007"
data08$year <- "2008"
data09$year <- "2009"
data10$year <- "2010"
data11$year <- "2011"
data12$year <- "2012"
data13$year <- "2013"
data14$year <- "2014"
data15$year <- "2015"
data16$year <- "2016"
data17$year <- "2017"
data18$year <- "2018"
data19$year <- "2019"
data20$year <- "2020"

#Stick together 2004-15 which share the same structure
data0415 <- bind_rows(data04, data05, data06, data07, data08, data09, data10, data11, data12, data13, data14, data15)
colnames(data0415) <- c("weekno", "date", "births", "deaths", "year")

#Then 2016-20 data
data1620 <- bind_rows(data16, data17, data18, data19, data20)
colnames(data1620) <- c("weekno", "date", "deaths", "year")

data <- bind_rows(data0415, data1620)

#Extract max/min values
#split off 2020 data
data_new <- subset(data, year=="2020")
data_old <- subset(data, year!="2020")

data_old <- data_old %>%
  group_by(weekno) %>%
  summarise(max=max(deaths), min=min(deaths))

tiff("Outputs/NRSWeeklyDeaths_reg.tiff", units="in", width=10, height=8, res=300)
ggplot()+
  geom_ribbon(data=data_old, aes(x=weekno, ymin=min, ymax=max), fill="Skyblue2")+
  geom_line(data=data_new, aes(x=weekno, y=deaths), colour="Red")+
  theme_classic()+
  scale_x_continuous(name="Week number")+
  scale_y_continuous(name="Deaths registered")+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold"))+
  labs(title="Deaths from all causes have risen sharply in Scotland",
       subtitle="Weekly deaths in 2020 compared to the range in 2004-19",
       caption="Data from NRS | Plot by @VictimOfMaths")+
  annotate(geom="text", x=15.5, y=1740, label="2020", colour="Red")+
  annotate(geom="text", x=30, y=1110, label="Historic Maximum", colour="deepskyblue4")+
  annotate(geom="text", x=30, y=830, label="Historic Minimum", colour="deepskyblue4")
dev.off()  
