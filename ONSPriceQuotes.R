rm(list=ls())

library(tidyverse)
library(curl)
library(extrafont)
library(forcats)
library(scales)
library(paletteer)
library(lubridate)
library(ragg)
library(ggridges)
library(RcppRoll)
library(ggrepel)
library(ggtext)

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

#Create list of non-alcohol products to include
comparators <- c(220107, 220318)

#Glossary for datasets can be found here: https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fglossary/glossaryrevised.xls

#Download ONS price quotes - credit to Peter Donaghy (@peterdonaghy) for bringing this data to my attention
#February 2023
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesfebruary2023/upload-pricequotes202302.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2302 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#January 2023
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesjanuary2023/upload-pricequotes202301.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2301 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#December 2022
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesdecember2022/upload-pricequotes202212.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2212 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#November 2022
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesnovember2022/upload-pricequotes202211.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2211 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#October 2022
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesoctober2022/upload-pricequotes202210.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2210 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))


#September 2022
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesseptember2022/upload-pricequotes202209.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2209 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#August 2022
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesaugust2022/upload-pricequotes202208.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2208 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#July 2022
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesjuly2022/upload-pricequotes202207.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2207 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#June 2022
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesjune2022/upload-pricequotes202206.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2206 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#May 2022
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotesmay2022/upload-pricequotes202205.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2205 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

################
#April 2022
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesapril2022/upload-pricequotes202204.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2204 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#March2022
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesmarch2022/upload-pricequotes202203.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2203 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Feb 2022
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesfebruary2022/upload-pricequotes202202.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2202 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Jan 2022
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesjanuary2022/upload-pricequotes202201.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2201 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Dec 2021
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesdecember2021/upload-pricequotes202112.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2112 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Nov 2021
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesnovember2021/upload-pricequotes202111.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2111 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Oct 2021
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesoctober2021/upload-pricequotes202110.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2110 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Sep 2021
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesseptember2021/upload-pricequotes202109.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2109 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Aug 2021
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesaugust2021/upload-pricequotes202108.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2108 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Jul 2021
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesjuly2021/upload-pricequotes202107.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2107 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Jun 2021
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesjune2021/upload-pricequotes202106.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2106 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#May 2021
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesmay2021/upload-pricequotes2021051.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2105 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Apr 2021
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesapril2021/upload-pricequotes202104.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2104 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Mar 2021
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesmarch2021/upload-pricequotes202103.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2103 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Feb 2021
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesfebruary2021/upload-pricequotes202102.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2102 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Jan 2021
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesjanuary2021/upload-pricequotes202101.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2101 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Dec 2020
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesdecember2020/upload-pricequotes202012.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2012 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Nov 2020
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesnovember2020/upload-pricequotes202011.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2011 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Oct 2020
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesoctober2020/upload-pricequotes202010.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2010 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Sep 2020
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesseptember2020/upload-pricequotes202009.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2009 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Aug 2020
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesaugust2020/upload-pricequotes202008.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2008 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Jul 2020
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricesquotesjuly2020/upload-pricequotes202007.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2007 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Jun 2020
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesjune2020/upload-pricequotes202006.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2006 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))
#May 2020
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricesquotesmay2020/upload-202005pricequotes.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2005 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Apr 2020
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricesquotesapril2020/upload-202004pricequotes.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2004 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Mar 2020
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricesquotesmarch2020/upload-pricequotes202003.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2003 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Feb 2020
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricesquotesfebruary2020/upload-pricequotes202002.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2002 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Jan 2020
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricesquotesjanuary2020/upload-pricequotes202001.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data2001 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Dec 2019
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesdecember2019/upload-pricequotes201912v1.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data1912 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Nov 2019
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesnovember2019/upload-pricequotes201911.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data1911 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Oct 2019
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesoctober2019/upload-pricequotes201910.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data1910 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Sep 2019
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesseptember2019/upload-pricequotes201909.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data1909 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Aug 2019
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotes2019/upload-pricequotes201908.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data1908 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Jul 2019
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotes2019/previous/v7/upload-pricequotes201907.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data1907 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Jun 2019
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotes2019/previous/v6/upload-pricequotes201906.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data1906 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#May 2019
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotes2019/previous/v5/upload-pricequote201905.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data1905 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Apr 2019
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotes2019/previous/v4/upload-pricequote201904.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data1904 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Mar 2019
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotes2019/previous/v3/upload-pricequotes201903.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data1903 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Feb 2019
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotes2019/previous/v2/upload-pricequotes201902.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data1902 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Jan 2019
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotes2019/previous/v1/upload-pricequotes201901.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data1901 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Dec 2018
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesdecember2018/upload-pricequotes201812.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data1812 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Nov 2018 - files provided zipped from here on back
temp <- tempfile()
temp2 <- tempfile()

url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotenovember2018/pricequote201811.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

data1811 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Oct 2018
temp <- tempfile()
temp2 <- tempfile()

url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequoteoctober2018/pricequote201810.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)


data1810 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Sep 2018
temp <- tempfile()
temp2 <- tempfile()

url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequoteseptember2018/pricequote201809.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)


data1809 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Aug 2018
temp <- tempfile()
temp2 <- tempfile()

url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequoteaugust2018/pricequote201808.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

data1808 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Jul 2018
temp <- tempfile()
temp2 <- tempfile()

url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotejuly2018/pricequote201807.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)


data1807 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Jun 2018
temp <- tempfile()
temp2 <- tempfile()

url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotejune2018/pricequote201806.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)


data1806 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#May 2018
temp <- tempfile()
temp2 <- tempfile()

url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotemay2018/pricequote201805.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)


data1805 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Apr 2018
temp <- tempfile()
temp2 <- tempfile()

url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequoteapril2018/pricequote201804.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)


data1804 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Mar 2018
temp <- tempfile()
temp2 <- tempfile()

url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotemarch2018/pricequote201803.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)


data1803 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Feb 2018
temp <- tempfile()
temp2 <- tempfile()

url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotefebruary2018/pricequote201802.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)


data1802 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Jan 2018
temp <- tempfile()
temp2 <- tempfile()

url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotejanuary2018/pricequote201801.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

data1801 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Dec 2017
temp <- tempfile()
temp2 <- tempfile()

url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesdecember2017/pricequote201712.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

data1712 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

##################################
#Mar-Nov 2017 all zipped together
temp <- tempfile()
temp2 <- tempfile()

url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotes2017/pricequote201703to201711.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#Nov 2017
data1711 <- read_csv(file.path(temp2, "price_quote_201711.zip")) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Oct 2017
data1710 <- read_csv(file.path(temp2, "price_quote_201710.zip")) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Sep 2017
data1709 <- read_csv(file.path(temp2, "price_quote_201709.zip")) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Aug 2017
data1708 <- read_csv(file.path(temp2, "price_quote_201708.zip")) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Jul 2017
data1707 <- read_csv(file.path(temp2, "price_quote_201707.zip")) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Jun 2017
data1706 <- read_csv(file.path(temp2, "price_quote_201706.zip")) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#May 2017
data1705 <- read_csv(file.path(temp2, "price_quote_201705.zip")) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Apr 2017
data1704 <- read_csv(file.path(temp2, "price_quote_201704.zip")) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Mar 2017
data1703 <- read_csv(file.path(temp2, "price_quote_201703.zip")) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Feb 2017
temp <- tempfile()
temp2 <- tempfile()

url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotes2017/previous/v2/pricequote201702.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

data1702 <- read_csv(temp) %>% 
  set_names(toupper(colnames(.))) %>% 
  rename("STRATUM_WEIGHT"="STRATUM_WEI") %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDI %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Jan 2017
temp <- tempfile()
temp2 <- tempfile()

url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes/pricequotes2017/previous/v1/pricequote201701.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

data1701 <- read_csv(temp) %>% 
  set_names(toupper(colnames(.))) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))


##########
#From here on back it's quarterly
#Q4 2016
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesquarter42016/upload-pricequote2016q4.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data16q4 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Q3 2016
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotequarter32016/upload-pricequote2016q3.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data16q3 <- read_csv(temp) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Q2 2016
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotequarter22016/upload-pricequote2016q2.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data16q2 <- read_csv(temp) %>% 
  set_names(toupper(colnames(.))) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Q1 2016
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotequarter12016/upload-pricequote2016q1.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data16q1 <- read_csv(temp) %>% 
  set_names(toupper(colnames(.))) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Q4 2015
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotequarter42015/upload-pricequote2015q4.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data15q4 <- read_csv(temp) %>% 
  set_names(toupper(colnames(.))) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Q3 2015
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesquarter32015/upload-pricequote2015q3.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data15q3 <- read_csv(temp) %>% 
  set_names(toupper(colnames(.))) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Q2 2015
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesquarter22015/upload-pricequote2015q2.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data15q2 <- read_csv(temp) %>% 
  set_names(toupper(colnames(.))) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Q1 2015
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotesquarter12015/upload-pricequote2015q1.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data15q1 <- read_csv(temp) %>% 
  set_names(toupper(colnames(.))) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#2014 data zipped together
temp <- tempfile()
temp2 <- tempfile()

url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotes2014/pricequote2014.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

data14q4 <- read_csv(file.path(temp2, "price_quote_2014_q4.csv")) %>% 
  set_names(toupper(colnames(.))) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

data14q3 <- read_csv(file.path(temp2, "price_quote_2014_q3.csv")) %>% 
  set_names(toupper(colnames(.))) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

data14q2 <- read_csv(file.path(temp2, "price_quote_2014_q2.csv")) %>% 
  set_names(toupper(colnames(.))) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

data14q1 <- read_csv(file.path(temp2, "price_quote_2014_q1.csv")) %>% 
  set_names(toupper(colnames(.))) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#2013 data zipped together
temp <- tempfile()
temp2 <- tempfile()

url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricesquote2013/pricequote2013.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

data13q4 <- read_csv(file.path(temp2, "price_quote_2013_q4.csv")) %>% 
  set_names(toupper(colnames(.))) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

data13q3 <- read_csv(file.path(temp2, "price_quote_2013_q3.csv")) %>% 
  set_names(toupper(colnames(.))) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

data13q2 <- read_csv(file.path(temp2, "price_quote_2013_q2.csv")) %>% 
  set_names(toupper(colnames(.))) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

data13q1 <- read_csv(file.path(temp2, "price_quote_2013_q1.csv")) %>% 
  set_names(toupper(colnames(.))) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#2012 data zipped together
temp <- tempfile()
temp2 <- tempfile()

url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotes2012/pricequote2012.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

data12q4 <- read_csv(file.path(temp2, "price_quote_2012_q4.csv")) %>% 
  set_names(toupper(colnames(.))) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

data12q3 <- read_csv(file.path(temp2, "price_quote_2012_q3.csv")) %>% 
  set_names(toupper(colnames(.))) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

data12q2 <- read_csv(file.path(temp2, "price_quote_2012_q2.csv")) %>% 
  set_names(toupper(colnames(.))) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

data12q1 <- read_csv(file.path(temp2, "price_quote_2012_q1.csv")) %>% 
  set_names(toupper(colnames(.))) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#2011 data zipped together
temp <- tempfile()
temp2 <- tempfile()

url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotes2011/pricequote2011.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

data11q4 <- read_csv(file.path(temp2, "price_quote_2011_q4.csv")) %>% 
  set_names(toupper(colnames(.))) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

data11q3 <- read_csv(file.path(temp2, "price_quote_2011_q3.csv")) %>% 
  set_names(toupper(colnames(.))) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

data11q2 <- read_csv(file.path(temp2, "price_quote_2011_q2.csv")) %>% 
  set_names(toupper(colnames(.))) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

data11q1 <- read_csv(file.path(temp2, "price_quote_2011_q1.csv")) %>% 
  set_names(toupper(colnames(.))) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#2010 data zipped together
temp <- tempfile()
temp2 <- tempfile()

url <- "https://www.ons.gov.uk/file?uri=%2feconomy%2finflationandpriceindices%2fdatasets%2fconsumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes%2fpricequotes2010/pricequote2010.zip"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

data10q4 <- read_csv(file.path(temp2, "price_quote_2010_q4.csv")) %>% 
  set_names(toupper(colnames(.))) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

data10q3 <- read_csv(file.path(temp2, "price_quote_2010_q3.csv")) %>% 
  set_names(toupper(colnames(.))) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

data10q2 <- read_csv(file.path(temp2, "price_quote_2010_q2.csv")) %>% 
  set_names(toupper(colnames(.))) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

data10q1 <- read_csv(file.path(temp2, "price_quote_2010_q1.csv")) %>% 
  set_names(toupper(colnames(.))) %>% 
  filter((ITEM_ID %in% comparators | substr(ITEM_ID, 1, 3) %in% c("310", "320")) & 
           VALIDITY %in% c(3,4)) %>% 
  mutate(date=as.Date(paste0(QUOTE_DATE, "01"), format="%Y%m%d"))

#Stick it all together
#2012 and older data includes item *codes* only, not descriptions and some item descriptions have changed
#while the item codes have remained the same

#Get code to description lookup
lookup <- bind_rows(data2302, data2301, data2212, data2211, data2210,
  data2209, data2208, data2207, data2206, data2205, data2204, data2203, data2202, 
                    data2201, data2112, data2111, data2110, data2109,
                    data2108, data2107, data2106, data2105, data2104, data2103, data2102, data2101,
                    data2012, data2011, data2010, data2009, data2008, data2007, data2006, data2005,
                    data2004, data2003, data2002, data2001, data1912, data1911, data1910, data1909,
                    data1908, data1907, data1906, data1905, data1904, data1903, data1902, data1901,
                    data1812, data1811, data1810, data1809, data1808, data1807, data1806, data1805, 
                    data1804, data1803, data1802, data1801, data1712, data1711, data1710, data1709, 
                    data1708, data1707, data1706, data1705, data1704, data1703, data1702, data1701, 
                    data16q4, data16q3, data16q2, data16q1, data15q4, data15q3, data15q2, data15q1, 
                    data14q4, data14q3, data14q2, data14q1, data13q4, data13q3, data13q2, data13q1) %>% 
  group_by(ITEM_ID, ITEM_DESC, date) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  #For codes where description has changed, take the most recent
  arrange(date) %>% 
  group_by(ITEM_ID) %>% 
  slice_tail(n=1) %>% 
  ungroup() %>% 
  select(-c(count, date))

fulldata <- bind_rows(data2302, data2301, data2212, data2211, data2210,
                      data2209, data2208, data2207, data2206, data2205, data2204, data2203, data2202, data2201, data2112, data2111, data2110, data2109,
                      data2108, data2107, data2106, data2105, data2104, data2103, data2102, data2101,
                      data2012, data2011, data2010, data2009, data2008, data2007, data2006, data2005,
                      data2004, data2003, data2002, data2001, data1912, data1911, data1910, data1909,
                      data1908, data1907, data1906, data1905, data1904, data1903, data1902, data1901,
                      data1812, data1811, data1810, data1809, data1808, data1807, data1806, data1805, 
                      data1804, data1803, data1802, data1801, data1712, data1711, data1710, data1709, 
                      data1708, data1707, data1706, data1705, data1704, data1703, data1702, data1701, 
                      data16q4, data16q3, data16q2, data16q1, data15q4, data15q3, data15q2, data15q1, 
                      data14q4, data14q3, data14q2, data14q1, data13q4, data13q3, data13q2, data13q1, 
                      data12q4, data12q3, data12q2, data12q1, data11q4, data11q3, data11q2, data11q1,
                      data10q4, data10q3, data10q2, data10q1) %>% 
  select(-ITEM_DESC) %>% 
  merge(lookup) %>% 
  mutate(SHOP_TYPE=if_else(SHOP_TYPE==1, "Multiple", "Independent"),
         product_cat=case_when(
           ITEM_DESC %in% c("20 FILTER - OTHER BRAND", "5 CIGARS: SPECIFY BRAND",
                            "CIGARETTES 12", "CIGARETTES 15", "CIGARETTES18", "CIGARETTES 20", 
                            "CIGARETTES 21", "CIGARETTES 22",
                            "CIGARETTES 8", "HAND ROLLING TOBACCO PACK 30GM",
                            "E-CIG REFILL BOTTL/CART 2-10ML") ~ "Tobacco",
           ITEM_ID %in% comparators | ITEM_DESC=="BOTTLE OF MIXER 125-200ML" ~ "Other",
           TRUE ~ "Alcohol"),
         product_cat_detail=case_when(
           ITEM_DESC %in% c("20 FILTER - OTHER BRAND", "CIGARETTES 12", "CIGARETTES 15", "CIGARETTES18", 
                            "CIGARETTES 20", "CIGARETTES 21", "CIGARETTES 22", "CIGARETTES 8") ~ "Cigarettes",
           ITEM_DESC=="5 CIGARS: SPECIFY BRAND" ~ "Cigars",
           ITEM_DESC=="HAND ROLLING TOBACCO PACK 30GM" ~ "RYO Tobacco",
           ITEM_DESC=="E-CIG REFILL BOTTL/CART 2-10ML" ~ "E-cigarettes",
           ITEM_DESC %in% c("APPLE CIDER 4 CAN PK 440-500ML", "APPLE CIDER 500-750ML 4.5-5.5%",
                            "CIDER-PER PINT OR 500-568ML", "CIDER FLAVOURED BOTT 500-568ML",
                            "CIDER 4.5%-5.5% ABV PINT/BOTTL") ~ "Cider",
           ITEM_DESC %in% c("BITTER-4CANS-440-500ML", "BOTTLE OF LAGER IN NIGHTCLUB",
                            "LAGER 10-24 BOTTLES 250-330ML",
                            "LAGER 10 - 24 CANS (440-500ML)", "LAGER 4 BOTTLES- PREMIUM",
                            "SPEC'Y BEER BOTT 500ML 4-5.5", "STOUT - 4 CAN PACK",
                            "BOTTLED PREMIUM LAGER 4.3-7.5%","DRAUGHT BITTER (PER PINT)",
                            "DRAUGHT STOUT PER PINT") ~ "Beer",
           ITEM_DESC %in% c("FORTIFIED WINE  (70-75CL)", "RED WINE- EUROPEAN 75CL",
                            "RED WINE- NEW WORLD 75CL", "ROSE WINE-75CL BOTTLE",
                            "SPARKLING WINE 75CL MIN 11%ABV", "WHITE WINE- EUROPEAN 75CL", 
                            "WHITE WINE- NEW WORLD 75CL", "BOTTLE OF CHAMPAGNE 75 CL",
                            "BOTTLE OF WINE 70-75CL", "BOTTLE OF CHAMPAGNE") ~ "Wine",
           ITEM_DESC %in% c("BRANDY 70CL BOTTLE", "CREAM LIQUER 70CL-1LT 14-20%",
                            "GIN BOTTLE 70CL", "PRE MIXED SPIRIT 250-330ML",
                            "RUM WHITE EG BACARDI 70CL BOTT", "VODKA-70 CL BOTTLE", "WHISKY-70 CL BOTTLE",
                            "GIN PER NIP", "LIQUEUR PER NIP   SPECIFY ML", "SPIRIT BASED DRINK 250-330MLS",
                            "SPIRIT BASED DRINK 275ML", "VODKA (PER NIP) SPECIFY ML",
                            "WHISKY (PER NIP) SPECIFY ML") ~ "Spirits",
           TRUE ~ "Other"),
         #Separate out on- and off-trade (currently done on the basis of inspecting prices)
         channel=case_when(
           ITEM_DESC %in% c("APPLE CIDER 4 CAN PK 440-500ML", "BITTER-4CANS-440-500ML", 
                            "BRANDY 70CL BOTTLE", "CIDER FLAVOURED BOTT 500-568ML",
                            "CREAM LIQUER 70CL-1LT 14-20%", "FORTIFIED WINE  (70-75CL)",
                            "GIN BOTTLE 70CL", "LAGER 10-24 BOTTLES 250-330ML", 
                            "LAGER 10 - 24 CANS (440-500ML)", "LAGER 4 BOTTLES- PREMIUM",
                            "PRE MIXED SPIRIT 250-330ML", "RED WINE- EUROPEAN 75CL",
                            "RED WINE- NEW WORLD 75CL", "ROSE WINE-75CL BOTTLE",
                            "RUM WHITE EG BACARDI 70CL BOTT", "SPARKLING WINE 75CL MIN 11%ABV",
                            "SPEC'Y BEER BOTT 500ML 4-5.5", "STOUT - 4 CAN PACK",
                            "VODKA-70 CL BOTTLE", "WHISKY-70 CL BOTTLE",
                            "WHITE WINE- EUROPEAN 75CL", "WHITE WINE- NEW WORLD 75CL",
                            "APPLE CIDER 500-750ML 4.5-5.5%") ~ "Off-trade",
           ITEM_DESC %in% c("BOTTLE OF CHAMPAGNE 75 CL", "BOTTLE OF CHAMPAGNE", "BOTTLE OF MIXER 125-200ML",
                            "BOTTLE OF WINE 70-75CL", "BOTTLED PREMIUM LAGER 4.3-7.5%",
                            "CIDER 4.5%-5.5% ABV PINT/BOTTL", "DRAUGHT BITTER (PER PINT)",
                            "DRAUGHT STOUT PER PINT", "GIN PER NIP", "LAGER - PINT 3.4-4.2%",
                            "LIQUEUR PER NIP   SPECIFY ML", "PREMIUM LAGER - PINT 4.3-7.5%",
                            "SPIRIT BASED DRINK 275ML", "VODKA (PER NIP) SPECIFY ML",
                            "WHISKY (PER NIP) SPECIFY ML", "WINE, PER 175 - 250 ML SERVING",
                            "CIDER-PER PINT OR 500-568ML", "SPIRIT BASED DRINK 250-330MLS") ~ "On-trade",
           TRUE ~ "N/A"),
         region=case_when(
           REGION==2 ~ "London", REGION==3 ~ "South East England", REGION==4 ~ "South West England", 
           REGION==5 ~ "East Anglia", REGION==6 ~ "East Midlands", REGION==7 ~ "West Midlands",
           REGION==8 ~ "Yorkshire & Humberside", REGION==9 ~ "North West England", 
           REGION==10 ~ "North East England", REGION==11 ~ "Wales", REGION==12 ~ "Scotland",
           REGION==13 ~ "Northern Ireland")) %>% 
  select(ITEM_ID, ITEM_DESC, SHOP_CODE, PRICE, STRATUM_WEIGHT, STRATUM_TYPE, SHOP_TYPE,
         SHOP_WEIGHT, date, product_cat, product_cat_detail, channel, region)

#Write data out
write.csv(fulldata, "X:/ScHARR/SARG_SAPM_3_5/General/Data/ONS Price Quotes/Fulldata.csv")

#Show which items are included at each time point
agg_png("Outputs/ONSPriceQuotesTable.png", units="in", width=12, height=7, res=500)
fulldata %>% 
  group_by(ITEM_DESC, date) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  filter(!ITEM_DESC %in% c("INDIAN TAKEAWAY")) %>% 
  ggplot(aes(x=date, y=fct_rev(ITEM_DESC), fill=count))+
  geom_tile()+
  scale_fill_paletteer_c("viridis::mako", direction=-1, limits=c(0,NA), name="Number of\nobservations")+
  scale_x_date(name="Month")+
  scale_y_discrete(name="Product description")+
  theme_custom()
dev.off()

#GRAPHS
#Regional variation by individual item
regmeans <- fulldata %>% 
  group_by(ITEM_DESC, date, region) %>% 
  summarise(meanprice=weighted.mean(PRICE, STRATUM_WEIGHT)) %>% 
  ungroup() %>% 
  #calculate rolling averages
  group_by(ITEM_DESC, region) %>% 
  mutate(roll_meanprice=roll_mean(meanprice, n=6, align="center", fill=NA)) %>% 
  ungroup()

agg_tiff("Outputs/ONSPriceQuotesLagerxReg.tiff", units="in", width=8, height=6, res=500)
ggplot(regmeans %>% filter(ITEM_DESC=="LAGER - PINT 3.4-4.2%"),
       aes(x=date, y=roll_meanprice, colour=region))+
  geom_line(show.legend=FALSE)+
  geom_text_repel(data=regmeans %>% filter(ITEM_DESC=="LAGER - PINT 3.4-4.2%" & 
                                             date==max(date[!is.na(roll_meanprice)])),
                  aes(x=max(date[!is.na(roll_meanprice)]), y=roll_meanprice, label = region, 
                      colour=region),
                  family = "Lato", direction = "y", xlim = c(as.Date("2023-02-01"), as.Date("2025-06-01")),
                  hjust = 0, segment.color = NA, box.padding = .1, show.legend = FALSE, size=rel(2.5))+
  scale_x_date(name="", limits=c(as.Date("2010-01-01"), as.Date("2025-06-01")),
               labels=c("", "2010", "2015", "2020", "", ""))+
  scale_y_continuous(name="Mean price observed", labels=dollar_format(prefix="£"), limits=c(0,NA))+
  scale_colour_manual(values=c("#0e3724", "#008c5c", "#33b983", "#0050ae", "#9b54f3", "#bf8cfc",
                               "#551153", "#ac0000", "#c85b00", "#f98517", "grey10", "grey70"))+
  theme_custom()+
  theme(plot.title=element_markdown())+
  labs(title="The price of a pint is highest in <span style='color:#33b983;'>London</span> and <span style='color:#bf8cfc;'>Northern Ireland",
       subtitle="Average observed price for a pint of lager (3.4-4.2% ABV), rolling 5-month average\n",
       caption="Data from ONS price quotes | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/ONSPriceQuotesBitterxReg.tiff", units="in", width=8, height=6, res=500)
ggplot(regmeans %>% filter(ITEM_DESC=="DRAUGHT BITTER (PER PINT)"),
       aes(x=date, y=roll_meanprice, colour=region))+
  geom_line(show.legend=FALSE)+
  geom_text_repel(data=regmeans %>% filter(ITEM_DESC=="DRAUGHT BITTER (PER PINT)" & 
                                             date==max(date[!is.na(roll_meanprice)])),
                  aes(x=max(date[!is.na(roll_meanprice)]), y=roll_meanprice, label = region, 
                      colour=region),
                  family = "Lato", direction = "y", xlim = c(as.Date("2023-02-01"), as.Date("2025-06-01")),
                  hjust = 0, segment.color = NA, box.padding = .1, show.legend = FALSE, size=rel(2.5))+
  scale_x_date(name="", limits=c(as.Date("2010-01-01"), as.Date("2025-06-01")),
               labels=c("", "2010", "2015", "2020", "", ""))+
  scale_y_continuous(name="Mean price observed", labels=dollar_format(prefix="£"), limits=c(0,NA))+
  scale_colour_manual(values=c("#0e3724", "#008c5c", "#33b983", "#0050ae", "#9b54f3", "#bf8cfc",
                               "#551153", "#ac0000", "#c85b00", "#f98517", "Grey10", "Grey70"))+
  theme_custom()+
  theme(plot.title=element_markdown())+
  labs(title="The price of a pint is highest in <span style='color:#33b983;'>London</span> and <span style='color:#bf8cfc;'>Northern Ireland",
       subtitle="Average observed price for a pint of bitter, rolling 5-month average\n",
       caption="Data from ONS price quotes | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/ONSPriceQuotesPremLagerxReg.tiff", units="in", width=8, height=6, res=500)
ggplot(regmeans %>% filter(ITEM_DESC=="PREMIUM LAGER - PINT 4.3-7.5%"),
       aes(x=date, y=roll_meanprice, colour=region))+
  geom_line(show.legend=FALSE)+
  geom_text_repel(data=regmeans %>% filter(ITEM_DESC=="PREMIUM LAGER - PINT 4.3-7.5%" & 
                                             date==max(date[!is.na(roll_meanprice)])),
                  aes(x=max(date[!is.na(roll_meanprice)]), y=roll_meanprice, label = region, 
                      colour=region),
                  family = "Lato", direction = "y", xlim = c(as.Date("2023-02-01"), as.Date("2025-06-01")),
                  hjust = 0, segment.color = NA, box.padding = .1, show.legend = FALSE, size=rel(2.5))+
  scale_x_date(name="", limits=c(as.Date("2010-01-01"), as.Date("2025-06-01")),
               labels=c("", "2010", "2015", "2020", "", ""))+
  scale_y_continuous(name="Mean price observed", labels=dollar_format(prefix="£"), limits=c(0,NA))+
  scale_colour_manual(values=c("#0e3724", "#008c5c", "#33b983", "#0050ae", "#9b54f3", "#bf8cfc",
                               "#551153", "#ac0000", "#c85b00", "#f98517", "Grey10", "Grey70"))+
  theme_custom()+
  theme(plot.title=element_markdown())+
  labs(title="The price of a pint is highest in <span style='color:#33b983;'>London</span> and <span style='color:#bf8cfc;'>Northern Ireland",
       subtitle="Average observed price for a pint of premium lager(4.3-7.5% ABV), rolling 5-month average\n",
       caption="Data from ONS price quotes | Plot by @VictimOfMaths")

dev.off()

#Calculate product means
prodmeans <- fulldata %>% 
  group_by(ITEM_DESC, date) %>% 
  summarise(meanprice=weighted.mean(PRICE, STRATUM_WEIGHT)) %>% 
  ungroup() %>% 
  #calculate rolling averages
  group_by(ITEM_DESC) %>% 
  mutate(roll_meanprice=roll_mean(meanprice, n=6, align="center", fill=NA)) %>% 
  ungroup()

labels1 <- prodmeans %>% 
  filter(ITEM_DESC %in% c("WHITE WINE- EUROPEAN 75CL",
                          "RED WINE- EUROPEAN 75CL",
                          "ROSE WINE-75CL BOTTLE",
                          "SPARKLING WINE 75CL MIN 11%ABV")) %>% 
  filter(date==max(date)-months(3)) %>% 
  mutate(label=c("Red wine", "Rose wine", "Sparkling wine", "White wine"))

agg_tiff("Outputs/ONSPriceQuotesOffWine.tiff", units="in", width=8, height=6, res=500)
prodmeans %>% 
  filter(ITEM_DESC %in% c("WHITE WINE- EUROPEAN 75CL",
                          "RED WINE- EUROPEAN 75CL",
                          "ROSE WINE-75CL BOTTLE",
                          "SPARKLING WINE 75CL MIN 11%ABV")) %>% 
  ggplot(aes(x=date, y=roll_meanprice, colour=ITEM_DESC))+
  geom_line(show.legend=FALSE)+
  geom_text_repel(data=labels1, aes(x=date, y=roll_meanprice, label=label, colour=ITEM_DESC), 
                  family = "Lato", direction = "y", xlim = c(as.Date("2023-02-10"), NA),
                  hjust = 0, segment.color = NA, box.padding = .3, show.legend = FALSE)+
  scale_x_date(name="", limits=c(NA_Date_, as.Date("2025-06-01")),
               breaks=as.Date(c("2010-01-01", "2015-01-01", "2020-01-01")),
               labels=c("2010", "2015", "2020"))+
  scale_y_continuous(name="Average price per bottle", labels=label_dollar(prefix="£"))+
  scale_colour_manual(values=c("#BE294C", "Pink", "#0FB2D3", "#EEEDC4"))+
  theme_custom()+
  labs(title="Wine prices have been rising recently",
       subtitle="Rolling 6-month average price for a 75cl bottle of wine based ONS' price quotes data",
       caption="Data from ONS | Plot from @VictimOfMaths")

dev.off()

labels2 <- prodmeans %>% 
  filter(ITEM_DESC %in% c("CIDER 4.5%-5.5% ABV PINT/BOTTL", "DRAUGHT BITTER (PER PINT)",
                          "LAGER - PINT 3.4-4.2%", "PREMIUM LAGER - PINT 4.3-7.5%")) %>% 
  filter(date==max(date)-months(3)) %>% 
  mutate(label=c("Cider", "Bitter", "Lager", "Premium Lager"))

agg_tiff("Outputs/ONSPriceQuotesOnBeer.tiff", units="in", width=8, height=6, res=500)
prodmeans %>% 
  filter(ITEM_DESC %in% c("CIDER-PER PINT OR 500-568ML", "CIDER 4.5%-5.5% ABV PINT/BOTTL", 
                          "DRAUGHT BITTER (PER PINT)",
                          "LAGER - PINT 3.4-4.2%", "PREMIUM LAGER - PINT 4.3-7.5%")) %>% 
  mutate(ITEM_DESC=if_else(ITEM_DESC=="CIDER-PER PINT OR 500-568ML",
                           "CIDER 4.5%-5.5% ABV PINT/BOTTL", ITEM_DESC)) %>% 
  ggplot(aes(x=date, y=roll_meanprice, colour=ITEM_DESC))+
  geom_line(show.legend=FALSE)+
  geom_text_repel(data=labels2, aes(x=date, y=roll_meanprice, label=label, colour=ITEM_DESC), 
                  family = "Lato", direction = "y", xlim = c(as.Date("2022-07-10"), NA),
                  hjust = 0, segment.color = NA, box.padding = .3, show.legend = FALSE)+
  scale_x_date(name="", limits=c(NA_Date_, as.Date("2024-06-01")),
               breaks=as.Date(c("2010-01-01", "2015-01-01", "2020-01-01")),
               labels=c("2010", "2015", "2020"))+
  scale_y_continuous(name="Average price per pint", labels=label_dollar(prefix="£"))+
  scale_colour_paletteer_d("awtools::mpalette")+
  theme_custom()+
  labs(title="The price of a pint has risen sharply",
       subtitle="Rolling 6-month average price for a pint of cider/beer bought for consumption on the premises",
       caption="Data from ONS Price Quotes | Plot from @VictimOfMaths")

dev.off()


labels3 <- prodmeans %>% 
  filter(ITEM_DESC %in% c("LAGER - PINT 3.4-4.2%",
                          "WINE, PER 175 - 250 ML SERVING",
                          "VODKA (PER NIP) SPECIFY ML")) %>% 
  filter(date==max(date)-months(3)) %>% 
  mutate(label=c("Pint of lager", "Shot of vodka", "Glass of wine"))

agg_tiff("Outputs/ONSPriceQuotesOnSelect.tiff", units="in", width=8, height=6, res=500)
prodmeans %>% 
  filter(ITEM_DESC %in% c("LAGER - PINT 3.4-4.2%",
                          "WINE, PER 175 - 250 ML SERVING",
                          "VODKA (PER NIP) SPECIFY ML")) %>% 
  ggplot(aes(x=date, y=roll_meanprice, colour=ITEM_DESC))+
  geom_line(show.legend=FALSE)+
  geom_text_repel(data=labels3, aes(x=date, y=roll_meanprice, label=label, colour=ITEM_DESC), 
                  family = "Lato", direction = "y", xlim = c(as.Date("2022-07-10"), NA),
                  hjust = 0, segment.color = NA, box.padding = .3, show.legend = FALSE)+
  scale_x_date(name="", limits=c(NA_Date_, as.Date("2024-06-01")),
               breaks=as.Date(c("2010-01-01", "2015-01-01", "2020-01-01")),
               labels=c("2010", "2015", "2020"))+
  scale_y_continuous(name="Average price per bottle", labels=label_dollar(prefix="£"))+
  scale_colour_manual(values=c("Orange", "#0FB2D3", "#BE294C"))+
  theme_custom()+
  labs(title="The price of drinking in the pub has risen sharply",
       subtitle="Rolling 6-month average prices of selected items bought for consumption on the premises",
       caption="Data from ONS' Price Quotes | Plot from @VictimOfMaths")

dev.off()


labels4 <- prodmeans %>% 
  filter(ITEM_DESC %in% c("BITTER-4CANS-440-500ML", "LAGER 10 - 24 CANS (440-500ML)",
                          "RED WINE- EUROPEAN 75CL", "APPLE CIDER 4 CAN PK 440-500ML",
                          "VODKA-70 CL BOTTLE", "WHISKY-70 CL BOTTLE")) %>% 
  filter(date==max(date)-months(3)) %>% 
  mutate(label=c("4 cans of cider", "4 cans of bitter", "Slab of lager", 
                 "Bottle of red wine", "Bottle of vodka", "Bottle of whisky"))

agg_tiff("Outputs/ONSPriceQuotesOffSelect.tiff", units="in", width=8, height=6, res=500)
prodmeans %>% 
  filter(ITEM_DESC %in% c("BITTER-4CANS-440-500ML", "LAGER 10 - 24 CANS (440-500ML)",
                          "WHITE WINE- EUROPEAN 75CL", "APPLE CIDER 4 CAN PK 440-500ML",
                          "VODKA-70 CL BOTTLE", "WHISKY-70 CL BOTTLE")) %>% 
  ggplot(aes(x=date, y=roll_meanprice, colour=ITEM_DESC))+
  geom_line(show.legend=FALSE)+
  geom_text_repel(data=labels4, aes(x=date, y=roll_meanprice, label=label, colour=ITEM_DESC), 
                  family = "Lato", direction = "y", xlim = c(as.Date("2022-07-10"), NA),
                  hjust = 0, segment.color = NA, box.padding = .3, show.legend = FALSE)+
  scale_x_date(name="", limits=c(NA_Date_, as.Date("2024-06-01")),
               breaks=as.Date(c("2010-01-01", "2015-01-01", "2020-01-01")),
               labels=c("2010", "2015", "2020"))+
  scale_y_continuous(name="Average price per bottle", labels=label_dollar(prefix="£"))+
  #scale_colour_manual(values=c("Orange", "#0FB2D3", "#BE294C"))+
  theme_custom()+
  labs(title="The price of drinking in the pub has risen sharply",
       subtitle="Rolling 6-month average prices of selected items bought for consumption on the premises",
       caption="Data from ONS' Price Quotes | Plot from @VictimOfMaths")

dev.off()
