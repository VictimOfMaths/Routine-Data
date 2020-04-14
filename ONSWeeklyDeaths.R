rm(list=ls())

library(tidyverse)
library(paletteer)
library(curl)
library(readxl)
library(lubridate)

#Import individual years of data, with inevitable fuckery because ONS keep subtly changing the spreadsheets
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2020/referencetablesweek142020.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2020 <- read_excel(temp, sheet="Weekly figures 2020", range="B18:P37", col_names=FALSE)
colnames(data2020) <- c("Age", format(seq.Date(from=as.Date("2020-01-03"), by="7 days", length.out=ncol(data2020)-1)), "%d/%m/%y")

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
  summarise_at(c(2:15), sum)

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

ann_text <- data.frame(weekno=c(16.5, 32, 32), deaths=c(6500, 3900,2700), lab=c("2020", "Max", "Min"), age2=factor(c("85+", "85+", "85+"), levels=c("<45", "45-64", "65-74", "75-84", "85+")))

tiff("Outputs/ONSWeeklyDeaths.tiff", units="in", width=12, height=8, res=300)
ggplot()+
  geom_ribbon(data=dataold, aes(x=weekno, ymin=min, ymax=max), fill="Skyblue2")+
  geom_line(data=datanew, aes(x=weekno, y=deaths), colour="Red")+
  theme_classic()+
  facet_wrap(~age2)+
  scale_x_continuous(name="Week number")+
  scale_y_continuous(name="Deaths registered")+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold"))+
  labs(title="Deaths from all causes have risen sharply in all age groups over 45",
       subtitle="Weekly deaths in 2020 compared to the range in 2010-19",
       caption="Data from ONS | Plot by @VictimOfMaths")+
  geom_text(data=ann_text, aes(x=weekno, y=deaths), label=c("2020", "Max", "Min"), size=3, 
            colour=c("Red", "deepskyblue4", "deepskyblue4"))
dev.off()  
