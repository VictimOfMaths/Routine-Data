rm(list=ls())

library(tidyverse)
library(paletteer)
library(curl)
library(readxl)
library(lubridate)

#To do
#Finish exploring BH variation
#Look at gender variation for EW & Scotland

#Import individual years of data, with inevitable fuckery because ONS keep subtly changing the spreadsheets
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2020/publishedweek152020.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2020 <- read_excel(temp, sheet="Weekly figures 2020", range="B22:Q41", col_names=FALSE)
colnames(data2020) <- c("Age", format(seq.Date(from=as.Date("2020-01-03"), by="7 days", length.out=ncol(data2020)-1), "%d/%m/%y"))

#match 2020 agebands to other years
data2020$age <- case_when(
  data2020$Age=="<1" ~ "Under 1 year",
  data2020$Age %in% c("1-4", "5-9", "10-14") ~ "01-14",
  data2020$Age %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44") ~ "15-44",
  data2020$Age %in% c("45-49", "50-54", "55-59", "60-64") ~ "45-64",
  data2020$Age %in% c("65-69", "70-74") ~ "65-74",
  data2020$Age %in% c("75-79", "80-84") ~ "75-84",
  TRUE ~ "85+"
)

data2020 <- data2020 %>%
  group_by(age) %>%
  summarise_at(c(2:(ncol(data2020)-1)), sum)

data2020$age <- factor(data2020$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", "65-74", "75-84", "85+"))
data2020 <- arrange(data2020, age)

temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2019/publishedweek522019.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2019 <- read_excel(temp, sheet="Weekly figures 2019", range="B16:BB22", col_names=FALSE)
colnames(data2019) <- c("age", format(seq.Date(from=as.Date("2019-01-04"), by="7 days", length.out=ncol(data2019)-1)), "%d/%m/%y")
data2019$age <- factor(data2019$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", "65-74", "75-84", "85+"))
data2019 <- arrange(data2019, age)

temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2018/publishedweek522018withupdatedrespiratoryrow.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2018 <- read_excel(temp, sheet="Weekly figures 2018", range="B16:BB22", col_names=FALSE)
colnames(data2018) <- c("age", format(seq.Date(from=as.Date("2018-01-05"), by="7 days", length.out=ncol(data2018)-1)), "%d/%m/%y")
data2018$age <- factor(data2018$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", "65-74", "75-84", "85+"))
data2018 <- arrange(data2018, age)

temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2017/publishedweek522017.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2017 <- read_excel(temp, sheet="Weekly figures 2017", range="B16:BB22", col_names=FALSE)
colnames(data2017) <- c("age", format(seq.Date(from=as.Date("2017-01-06"), by="7 days", length.out=ncol(data2017)-1)), "%d/%m/%y")
data2017$age <- factor(data2017$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", "65-74", "75-84", "85+"))
data2017 <- arrange(data2017, age)

temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2016/publishedweek522016.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2016 <- read_excel(temp, sheet="Weekly figures 2016", range="B16:BB22", col_names=FALSE)
colnames(data2016) <- c("age", format(seq.Date(from=as.Date("2016-01-08"), by="7 days", length.out=ncol(data2016)-1)), "%d/%m/%y")
data2016$age <- factor(data2016$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", "65-74", "75-84", "85+"))
data2016 <- arrange(data2016, age)

temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2015/publishedweek2015.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2015 <- read_excel(temp, sheet="Weekly Figures 2015", range="A16:BB22", col_names=FALSE)
colnames(data2015) <- c("age", format(seq.Date(from=as.Date("2015-01-02"), by="7 days", length.out=ncol(data2015)-1)), "%d/%m/%y")
data2015$age <- factor(data2015$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", "65-74", "75-84", "85+"))
data2015 <- arrange(data2015, age)

temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2014/publishedweek2014.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2014 <- read_excel(temp, sheet="Weekly Figures 2014", range="A16:BA22", col_names=FALSE)
colnames(data2014) <- c("age", format(seq.Date(from=as.Date("2014-01-03"), by="7 days", length.out=ncol(data2014)-1)), "%d/%m/%y")
data2014$age <- factor(data2014$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", "65-74", "75-84", "85+"))
data2014 <- arrange(data2014, age)

temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2013/publishedweek2013.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2013 <- read_excel(temp, sheet="Weekly Figures 2013", range="A16:BA22", col_names=FALSE)
colnames(data2013) <- c("age", format(seq.Date(from=as.Date("2013-01-04"), by="7 days", length.out=ncol(data2013)-1)), "%d/%m/%y")
data2013$age <- factor(data2013$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", "65-74", "75-84", "85+"))
data2013 <- arrange(data2013, age)

temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2012/publishedweek2012.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2012 <- read_excel(temp, sheet="Weekly Figures 2012", range="A16:BA22", col_names=FALSE)
colnames(data2012) <- c("age", format(seq.Date(from=as.Date("2012-01-06"), by="7 days", length.out=ncol(data2012)-1)), "%d/%m/%y")
data2012$age <- factor(data2012$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", "65-74", "75-84", "85+"))
data2012 <- arrange(data2012, age)

temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2011/publishedweek2011.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2011 <- read_excel(temp, sheet="Weekly Figures 2011", range="A17:BA23", col_names=FALSE)
colnames(data2011) <- c("age", format(seq.Date(from=as.Date("2011-01-07"), by="7 days", length.out=ncol(data2011)-1)), "%d/%m/%y")
data2011$age <- factor(data2011$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", "65-74", "75-84", "85+"))
data2011 <- arrange(data2011, age)

temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2010/publishedweek2010.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2010 <- read_excel(temp, sheet="Weekly Figures 2010", range="A16:BA22", col_names=FALSE)
colnames(data2010) <- c("age", format(seq.Date(from=as.Date("2010-01-08"), by="7 days", length.out=ncol(data2010)-1)), "%d/%m/%y")
data2010$age <- factor(data2010$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", "65-74", "75-84", "85+"))
data2010 <- arrange(data2010, age)

data_wide <- bind_cols(data2010, data2011, data2012, data2013, data2014, data2015, data2016, data2017, data2018, data2019, data2020)
data <- gather(data_wide, week, deaths, c(2:ncol(data_wide)))
data <- subset(data, substr(data$week,1,3)!="age")
data$deaths <- as.numeric(data$deaths)
data$week <- as.Date(data$week)

ggplot(data, aes(x=week, y=deaths,colour=age))+
  geom_line()+
  theme_classic()+
  scale_x_date()+
  facet_wrap(~age)

#Counts in all groups <44 are low, so collapse these groups
data$age2 <- case_when(
  data$age %in% c("Under 1 year", "01-14", "15-44") ~ "<45",
  TRUE ~ as.character(data$age))

data2 <- data %>%
  group_by(week, age2) %>%
  summarise(deaths=sum(deaths)) %>%
  ungroup

data2$year <- as.numeric(format(data2$week, "%Y"))
data2$weekmonth <- yday(data2$week)
data2$weekno <- week(data2$week)

#Extract max/min values
#split off 2020 data
datanew <- subset(data2, year==2020)
dataold <- subset(data2, year<2020)

dataold <- dataold %>%
  group_by(weekno, age2) %>%
  summarise(max=max(deaths), min=min(deaths))

#Generate filled area for total excess deaths vs. previous 10-year maximum
datanew <- merge(datanew, dataold, by=c("weekno", "age2"))
datanew <- datanew %>%
  mutate(ymax=pmax(deaths, max))

ann_text <- data.frame(weekno=c(16, 32, 32), deaths=c(6000, 3900,2700), 
                       age2=factor(rep("85+", times=3), levels=c("<45", "45-64", "65-74", "75-84", "85+")))

tiff("Outputs/ONSWeeklyDeaths.tiff", units="in", width=12, height=8, res=300)
ggplot()+
  geom_ribbon(data=datanew, aes(x=weekno, ymin=max, ymax=ymax), fill="Red", alpha=0.2)+
  geom_ribbon(data=dataold, aes(x=weekno, ymin=min, ymax=max), fill="Skyblue2")+
  geom_line(data=datanew, aes(x=weekno, y=deaths), colour="Red")+
  theme_classic()+
  facet_wrap(~age2)+
  scale_x_continuous(name="Week number")+
  scale_y_continuous(name="Deaths registered")+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold"))+
  labs(title="Deaths from all causes have risen sharply in England & Wales across all age groups over 45",
       subtitle="Weekly deaths in 2020 compared to the range in 2010-19. Data up to 10th April",
       caption="Data from ONS | Plot by @VictimOfMaths")+
  geom_text(data=ann_text, aes(x=weekno, y=deaths), label=c("Unprecedented excess deaths\nin 2020", "Max", "Min"), size=3, 
            colour=c("Red", "deepskyblue4", "deepskyblue4"), hjust=0)
dev.off()  

#Move on to regional variation
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2020/publishedweek152020.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2020 <- read_excel(temp, sheet="Weekly figures 2020", range="B87:Q96", col_names=FALSE)
colnames(data2020) <- c("reg", format(seq.Date(from=as.Date("2020-01-03"), by="7 days", length.out=ncol(data2020)-1)), "%d/%m/%y")

temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2019/publishedweek522019.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2019 <- read_excel(temp, sheet="Weekly figures 2019", range="B43:BB52", col_names=FALSE)
colnames(data2019) <- c("reg", format(seq.Date(from=as.Date("2019-01-04"), by="7 days", length.out=ncol(data2019)-1)), "%d/%m/%y")

temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2018/publishedweek522018withupdatedrespiratoryrow.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2018 <- read_excel(temp, sheet="Weekly figures 2018", range="B43:BB52", col_names=FALSE)
colnames(data2018) <- c("reg", format(seq.Date(from=as.Date("2018-01-05"), by="7 days", length.out=ncol(data2018)-1)), "%d/%m/%y")

temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2017/publishedweek522017.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2017 <- read_excel(temp, sheet="Weekly figures 2017", range="B43:BB52", col_names=FALSE)
colnames(data2017) <- c("reg", format(seq.Date(from=as.Date("2017-01-06"), by="7 days", length.out=ncol(data2017)-1)), "%d/%m/%y")

temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2016/publishedweek522016.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2016 <- read_excel(temp, sheet="Weekly figures 2016", range="B43:BB52", col_names=FALSE)
colnames(data2016) <- c("reg", format(seq.Date(from=as.Date("2016-01-08"), by="7 days", length.out=ncol(data2016)-1)), "%d/%m/%y")

temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2015/publishedweek2015.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2015 <- read_excel(temp, sheet="Weekly Figures 2015", range="A43:BB52", col_names=FALSE)
colnames(data2015) <- c("reg", format(seq.Date(from=as.Date("2015-01-02"), by="7 days", length.out=ncol(data2015)-1)), "%d/%m/%y")

temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2014/publishedweek2014.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2014 <- read_excel(temp, sheet="Weekly Figures 2014", range="A43:BA52", col_names=FALSE)
colnames(data2014) <- c("reg", format(seq.Date(from=as.Date("2014-01-03"), by="7 days", length.out=ncol(data2014)-1)), "%d/%m/%y")

temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2013/publishedweek2013.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2013 <- read_excel(temp, sheet="Weekly Figures 2013", range="A43:BA52", col_names=FALSE)
colnames(data2013) <- c("reg", format(seq.Date(from=as.Date("2013-01-04"), by="7 days", length.out=ncol(data2013)-1)), "%d/%m/%y")

temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2012/publishedweek2012.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2012 <- read_excel(temp, sheet="Weekly Figures 2012", range="A43:BA52", col_names=FALSE)
colnames(data2012) <- c("reg", format(seq.Date(from=as.Date("2012-01-06"), by="7 days", length.out=ncol(data2012)-1)), "%d/%m/%y")

temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2011/publishedweek2011.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2011 <- read_excel(temp, sheet="Weekly Figures 2011", range="A44:BA53", col_names=FALSE)
colnames(data2011) <- c("reg", format(seq.Date(from=as.Date("2011-01-07"), by="7 days", length.out=ncol(data2011)-1)), "%d/%m/%y")

temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2010/publishedweek2010.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2010 <- read_excel(temp, sheet="Weekly Figures 2010", range="A43:BA52", col_names=FALSE)
colnames(data2010) <- c("reg", format(seq.Date(from=as.Date("2010-01-08"), by="7 days", length.out=ncol(data2010)-1)), "%d/%m/%y")

data_wide_r <- bind_cols(data2010, data2011, data2012, data2013, data2014, data2015, data2016, data2017, data2018, data2019, data2020)
data_r <- gather(data_wide_r, week, deaths, c(2:ncol(data_wide_r)))
data_r <- subset(data_r, substr(data_r$week,1,3)!="reg")
data_r$deaths <- as.numeric(data_r$deaths)
data_r$week <- as.Date(data_r$week)

data_r$year <- as.numeric(format(data_r$week, "%Y"))
data_r$weekno <- week(data_r$week)

#Extract max/min values
#split off 2020 data
data_rnew <- subset(data_r, year==2020)
data_rold <- subset(data_r, year<2020)

data_rold <- data_rold %>%
  group_by(weekno, reg) %>%
  summarise(max=max(deaths), min=min(deaths))

#Generate filled area for total excess deaths vs. previous 10-year maximum
data_rnew <- merge(data_rnew, data_rold, by=c("weekno", "reg"))
data_rnew <- data_rnew %>%
  mutate(ymax=pmax(deaths, max))

ann_text2 <- data.frame(weekno=c(16, 32, 32), deaths=c(1700,1200,700), 
                       reg=rep("East", times=3))

tiff("Outputs/ONSWeeklyDeaths_reg.tiff", units="in", width=12, height=8, res=300)
ggplot()+
  geom_ribbon(data=data_rnew, aes(x=weekno, ymin=max, ymax=ymax), fill="Red", alpha=0.2)+
  geom_ribbon(data=data_rold, aes(x=weekno, ymin=min, ymax=max), fill="Skyblue2")+
  geom_line(data=data_rnew, aes(x=weekno, y=deaths), colour="Red")+
  theme_classic()+
  facet_wrap(~reg)+
  scale_x_continuous(name="Week number")+
  scale_y_continuous(name="Deaths registered")+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold"))+
  labs(title="Deaths from all causes have risen sharply across England & Wales",
       subtitle="Weekly deaths in 2020 compared to the range in 2010-19. Data up to 10th April",
       caption="Data from ONS | Plot by @VictimOfMaths")+
  geom_text(data=ann_text2, aes(x=weekno, y=deaths), label=c("Unprecedented excess deaths\nin 2020","Max", "Min"), size=3, 
            colour=c("Red", "deepskyblue4", "deepskyblue4"), hjust=0)
dev.off()  

#################################
#Bring in Scottish data from NRS#
#################################

temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/weekly-monthly-births-deaths-data/2020/mar/weekly-march-20.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
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
#data20 <- read_excel(temp, sheet="2020", range="E6:G19", col_names=FALSE)

#Take 2020 data from dedicated COVID-19 page, which is updated more regularly
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/covid19/covid-deaths-data-week-15.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data20 <- data.frame(t(read_excel(temp, sheet="Table 2 - All deaths", range="C6:Q7", col_names=FALSE))[,c(2)])
date <- data.frame(date=format(seq.Date(from=as.Date("2019-12-30"), by="7 days", length.out=nrow(data20)), "%d/%m/%y"))
data20 <- cbind(date, data20)
colnames(data20) <- c("date", "deaths")
data20$date <- as.Date(data20$date, "%d/%m/%y")
data20$weekno <- week(data20$date)

#Stick together 2004-15 which share the same structure
data1015 <- bind_rows(data10, data11, data12, data13, data14, data15)
colnames(data1015) <- c("weekno", "date", "births", "deaths")
data1015$date <- as.Date(data1015$date)

#Then 2016-19 data
data1619 <- bind_rows(data16, data17, data18, data19)
colnames(data1619) <- c("weekno", "date", "deaths")
data1619$date <- as.Date(data1619$date)

data <- bind_rows(data1015, data1619, data20)

#Recalculate dates to align with ONS data (which uses week to, not w/c)
data$date <- data$date+days(6)

data$year <- as.character(year(data$date))
data$weekno <- week(data$date)

#Extract max/min values
#split off 2020 data
data_new <- subset(data, year=="2020")
data_old <- subset(data, year!="2020")

data_old <- data_old %>%
  group_by(weekno) %>%
  summarise(max=max(deaths), min=min(deaths))

data_old$reg <- "Scotland"
data_new$reg <- "Scotland"
data_new$year <- as.numeric(data_new$year)

#Generate filled area for total excess deaths vs. previous 10-year maximum
data_new <- merge(data_new, data_old, by=c("weekno", "reg"))
data_new <- data_new %>%
  mutate(ymax=pmax(deaths, max))

data_rold <- bind_rows(data_rold, data_old)
data_rnew <- bind_rows(data_rnew, data_new)

ann_text3 <- data.frame(weekno=c(18.2, 32, 32), deaths=c(2000, 1200,700), lab=c("2020", "Max", "Min"), 
                        reg=rep("East", times=3))

tiff("Outputs/ONSNRSWeeklyDeaths_reg.tiff", units="in", width=12, height=8, res=300)
ggplot()+
  geom_ribbon(data=data_rnew, aes(x=weekno, ymin=max, ymax=ymax), fill="Red", alpha=0.2)+
  geom_ribbon(data=data_rold, aes(x=weekno, ymin=min, ymax=max), fill="Skyblue2")+
  geom_line(data=data_rnew, aes(x=weekno, y=deaths), colour="Red")+
  theme_classic()+
  facet_wrap(~reg)+
  scale_x_continuous(name="Week number")+
  scale_y_continuous(name="Deaths registered")+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold"))+
  labs(title="Deaths from all causes have risen sharply across Great Britain",
       subtitle="Weekly deaths in 2020 compared to the range in 2010-19\nEngland & Wales data to April 10th\nScotland data to April 12th",
       caption="Data from ONS & NRS | Plot by @VictimOfMaths")+
  geom_text(data=ann_text3, aes(x=weekno, y=deaths), label=c("2020", "Max", "Min"), size=3, 
            colour=c("Red", "deepskyblue4", "deepskyblue4"))
dev.off()  

#########################################
#Bring in Northern Irish data from NISRA#
#########################################
temp <- tempfile()
source <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Weekly_Deaths.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2020 <- read_excel(temp, sheet="Weekly Deaths_2020", range="B5:C18", col_names=FALSE)
colnames(data2020) <- c("date", "deaths")

temp <- tempfile()
source <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Weekly_Deaths%20-%20Historical.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2019 <- read_excel(temp, sheet="Weekly Deaths_2019", range="C5:D56", col_names=FALSE)
colnames(data2019) <- c("date", "deaths")

data2018 <- read_excel(temp, sheet="Weekly Deaths_2018", range="C5:D56", col_names=FALSE)
colnames(data2018) <- c("date", "deaths")

data2017 <- read_excel(temp, sheet="Weekly Deaths_2017", range="C5:D57", col_names=FALSE)
colnames(data2017) <- c("date", "deaths")

data2016 <- read_excel(temp, sheet="Weekly Deaths_2016", range="C5:D56", col_names=FALSE)
colnames(data2016) <- c("date", "deaths")

data2015 <- read_excel(temp, sheet="Weekly Deaths_2015", range="C5:D57", col_names=FALSE)
colnames(data2015) <- c("date", "deaths")

data2014 <- read_excel(temp, sheet="Weekly Deaths_2014", range="C5:D56", col_names=FALSE)
colnames(data2014) <- c("date", "deaths")

data2013 <- read_excel(temp, sheet="Weekly Deaths_2013", range="C5:D56", col_names=FALSE)
colnames(data2013) <- c("date", "deaths")

data2012 <- read_excel(temp, sheet="Weekly Deaths_2012", range="C5:D56", col_names=FALSE)
colnames(data2012) <- c("date", "deaths")

data2011 <- read_excel(temp, sheet="Weekly Deaths_2011", range="C5:D56", col_names=FALSE)
colnames(data2011) <- c("date", "deaths")

data2010 <- read_excel(temp, sheet="Weekly Deaths_2010", range="C5:D57", col_names=FALSE)
colnames(data2010) <- c("date", "deaths")

datani <- bind_rows(data2010, data2011, data2012, data2013, data2014, data2015, data2016, data2017, data2018, data2019, data2020)
datani$week <- as.Date(datani$date)

datani$weekno <- week(datani$week)
datani$year <- year(datani$week)

#Extract max/min values
#split off 2020 data
data_new <- subset(datani, year=="2020")
data_old <- subset(datani, year!="2020")

data_old <- data_old %>%
  group_by(weekno) %>%
  summarise(max=max(deaths), min=min(deaths))

data_old$reg <- "Northern Ireland"
data_new$reg <- "Northern Ireland"
data_new$year <- as.numeric(data_new$year)

#Generate filled area for total excess deaths vs. previous 10-year maximum
data_new <- merge(data_new, data_old, by=c("weekno", "reg"))
data_new <- data_new %>%
  mutate(ymax=pmax(deaths, max))

data_new$date <- as.Date(data_new$date)

data_rold <- bind_rows(data_rold, data_old)
data_rnew <- bind_rows(data_rnew, data_new)

ann_text4 <- data.frame(weekno=c(16.5, 28, 28), deaths=c(1700, 1200,700), reg=rep("East", times=3))

tiff("Outputs/ONSNRSNISRAWeeklyDeaths_reg.tiff", units="in", width=12, height=8, res=300)
ggplot()+
  geom_ribbon(data=data_rnew, aes(x=weekno, ymin=max, ymax=ymax), fill="Red", alpha=0.2)+
  geom_ribbon(data=data_rold, aes(x=weekno, ymin=min, ymax=max), fill="Skyblue2")+
  geom_line(data=data_rnew, aes(x=weekno, y=deaths), colour="Red")+
  theme_classic()+
  facet_wrap(~reg)+
  scale_x_continuous(name="Week number")+
  scale_y_continuous(name="Deaths registered")+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold"))+
  labs(title="Deaths from all causes have risen sharply across the UK",
       subtitle="Weekly deaths in 2020 compared to the range in 2010-19\nEngland, Wales & Northern Ireland data to April 10th\nScotland data to April 12th",
       caption="Data from ONS, NRS & NISRA | Plot by @VictimOfMaths")+
  geom_text(data=ann_text4, aes(x=weekno, y=deaths), label=c("Unprecedented excess deaths\nin 2020","Max", "Min"), size=3, 
            colour=c("Red", "deepskyblue4", "deepskyblue4"), hjust=0)
dev.off()  

#Tidy up data to stick it all together
data$reg <- "Scotland"
datani$reg <- "Northern Ireland"
data$year <- as.numeric(data$year)
datani$year <- as.numeric(datani$year)
colnames(data)[colnames(data)=="date"] <- "week"
data$week <- as.Date(data$week)
data_r$week <- as.Date(data_r$week)
datani$week <- as.Date(datani$week)

data_all <- bind_rows(data_r, data[,-c(3)], datani[,-c(1)])

#Generate cumulative death counts by year
data_all <- data_all %>%
  group_by(reg, year) %>%
  mutate(cumul_deaths=cumsum(deaths))

ann_text5 <- data.frame(weekno=15, cumul_deaths=25000, reg="London")

tiff("Outputs/ONSNRSNISRAWeeklyCumulDeaths_reg.tiff", units="in", width=12, height=8, res=300)
ggplot()+
  geom_line(data=subset(data_all, year!=2020), aes(x=weekno, y=cumul_deaths, group=as.factor(year)), colour="Grey80")+
  geom_line(data=subset(data_all, year==2020), aes(x=weekno, y=cumul_deaths), colour="Red")+
  theme_classic()+
  facet_wrap(~reg)+
  facet_wrap(~reg)+
  scale_x_continuous(name="Week number")+
  scale_y_continuous(name="Deaths registered")+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold"))+
  labs(title="Cumulative deaths in the UK from all causes in 2020 vs. 2010-2019",
       caption="Data from ONS, NRS & NISRA | Plot by @VictimOfMaths")+
  geom_text(data=ann_text5, aes(x=weekno, y=cumul_deaths), label=c("2020"), size=3, colour="Red")
dev.off()
