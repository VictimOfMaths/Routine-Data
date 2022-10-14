rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(scales)
library(lubridate)
library(extrafont)
library(ragg)
library(ggtext)
library(paletteer)
library(scales)
library(ggrepel)
library(sf)

options(scipen=99999)

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

#Download ONS data on %of people in UK aged 16-64 who are economically inactive
#Max range
actmax <- 627
temp <- tempfile()
wardurl <- "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/employmentunemploymentandeconomicinactivityforpeopleaged16andoverandagedfrom16to64seasonallyadjusteda02sa/current/a02saoct2022.xls"
temp <- curl_download(url=wardurl, destfile=temp, quiet=FALSE, mode="wb")

activity_p <- read_excel(temp, sheet="People", range=paste0("A8:S", actmax)) %>% 
  na.omit() %>% 
  select(`Dataset identifier code`, LF2G, LF2I, LF2M) %>% 
  set_names("Date", "Employed", "Unemployed", "Inactive") %>% 
  gather(Status, Count, c(2:4)) %>% 
  mutate(Sex="People")

activity_m <- read_excel(temp, sheet="Men", range=paste0("A8:S", actmax)) %>% 
  na.omit() %>% 
  select(`Dataset identifier code`, YBSF, YBSI, YBSO) %>% 
  set_names("Date", "Employed", "Unemployed", "Inactive") %>% 
  gather(Status, Count, c(2:4)) %>% 
  mutate(Sex="Male")

activity_f <- read_excel(temp, sheet="Women", range=paste0("A8:S", actmax)) %>% 
  na.omit() %>% 
  select(`Dataset identifier code`, LF2H, LF2J, LF2N) %>% 
  set_names("Date", "Employed", "Unemployed", "Inactive") %>% 
  gather(Status, Count, c(2:4)) %>% 
  mutate(Sex="Female")

activity <- bind_rows(activity_p, activity_m, activity_f) %>% 
  mutate(Level1=TRUE, Level2a=if_else(Status=="Inactive", FALSE, TRUE), 
         Level2b=if_else(Status=="Inactive", FALSE, TRUE), 
         Level3=if_else(Status=="Inactive", FALSE, TRUE))

#Download ONS data on reasons for economic inactivity
inactmax <- 373

temp <- tempfile()
wardurl <- "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peoplenotinwork/economicinactivity/datasets/economicinactivitybyreasonseasonallyadjustedinac01sa/current/inac01saoct2022.xls"
temp <- curl_download(url=wardurl, destfile=temp, quiet=FALSE, mode="wb")

inactive_p <- read_excel(temp, sheet="People", range=paste0("A8:AP", inactmax)) %>% 
  na.omit() %>% 
  select(`Dataset identifier code`, LF63, LF65, LF67, LF69, LFL8, LF6B, LF6D, `LFL9...10`, 
         `LFM2...11`, LF6F, LFP7, LF6H, LFP8, LF8B, LF6J, LF6L, LF6N, LF6P, LF6R, LF8E, LF6T) %>%
  rename("Date"="Dataset identifier code") %>% 
  gather(Code, Count, c(2:ncol(.))) %>% 
  mutate(Count=as.numeric(Count),
         Status=case_when(
           Code %in% c("LF63", "LF6F", "LF6L") ~ "Student",
           Code %in% c("LF65", "LFP7", "LF6N") ~ "Looking after family/home",
           Code %in% c("LF67", "LF6H", "LF6P") ~ "Temp. sick",
           Code %in% c("LF69", "LFP8", "LF6R") ~ "Long-term sick",
           Code %in% c("LFL8", "LF8B") ~ "Discouraged",
           Code %in% c("LF6B", "LF8E") ~ "Retired",
           Code %in% c("LF6D", "LF6J", "LF6T") ~ "Other",
           Code=="LFL9...10" ~ "Wants a job",
           Code=="LFM2...11" ~ "Doesn't want a job"),
         Level2a=if_else(Code %in% c("LF63", "LF65", "LF67", "LF69", "LFL8", "LF6B", "LF6D"), TRUE,
                         FALSE),
         Level2b=if_else(Code %in% c("LFL9...10", "LFM2...11"), TRUE, FALSE),
         Level3=if_else(Code %in% c("LF6F", "LF6L", "LFP7", "LF6N", "LF6H", "LF6P", "LFP8", 
                                    "LF6R", "LF8B", "LF8E", "LF6J", "LF6T"), TRUE, FALSE),
         Sex="People") 

inactive_m <- read_excel(temp, sheet="Men", range=paste0("A8:AP", inactmax)) %>% 
  na.omit() %>% 
  select(`Dataset identifier code`, BEEX, BEAQ, BEDI, BEDL, YCFP, BEDR, BEDU, `YBWA...10`, 
         `YBWD...11`, BEAT, YCFV, BEGX, YCFS, LF8C, BEHG, BEIV, BEIY, BEJB, BEJE, LF8F, BEJK) %>%
  rename("Date"="Dataset identifier code") %>% 
  gather(Code, Count, c(2:ncol(.))) %>% 
  mutate(Count=as.numeric(Count),
         Status=case_when(
           Code %in% c("BEEX", "BEAT", "BEIV") ~ "Student",
           Code %in% c("BEAQ", "YCFV", "BEIY") ~ "Looking after family/home",
           Code %in% c("BEDI", "BEGX", "BEJB") ~ "Temp. sick",
           Code %in% c("BEDL", "YCFS", "BEJE") ~ "Long-term sick",
           Code %in% c("YCFP", "LF8C") ~ "Discouraged",
           Code %in% c("BEDR", "LF8F") ~ "Retired",
           Code %in% c("BEDU", "BEHG", "BEJK") ~ "Other",
           Code=="LFL9...10" ~ "Wants a job",
           Code=="LFM2...11" ~ "Doesn't want a job"),
         Level2a=if_else(Code %in% c("BEEX", "BEAQ", "BEDI", "BEDL", "YCFP", "BEDR", "BEDU"), TRUE,
                         FALSE),
         Level2b=if_else(Code %in% c("YBWA...10", "YBWD...11"), TRUE, FALSE),
         Level3=if_else(Code %in% c("BEAT", "BEIV", "YCFV", "BEIY", "BEGX", "BEJB", "YCFS",
                                    "BEJE", "LF8C", "LF8F", "BEHG", "BEJK"), TRUE, FALSE),
         Sex="Male") 

inactive_f <- read_excel(temp, sheet="Women", range=paste0("A8:AP", inactmax)) %>% 
  na.omit() %>% 
  select(`Dataset identifier code`, LF64, LF66, LF68, LF6A, LFM3, LF6C, LF6E, `LFM4...10`, 
         `LFM5...11`, LF6G, LFP9, LF6I, LFQ2, LF8D, LF6K, LF6M, LF6O, LF6Q, LF6S, LF8G, LF6U) %>%
  rename("Date"="Dataset identifier code") %>% 
  gather(Code, Count, c(2:ncol(.))) %>% 
  mutate(Count=as.numeric(Count),
         Status=case_when(
           Code %in% c("LF64", "LF6G", "LF6M") ~ "Student",
           Code %in% c("LF66", "LFP9", "LF6O") ~ "Looking after family/home",
           Code %in% c("LF68", "LF6I", "LF6Q") ~ "Temp. sick",
           Code %in% c("LF6A", "LFQ2", "LF6S") ~ "Long-term sick",
           Code %in% c("LFM3", "LF8D") ~ "Discouraged",
           Code %in% c("LF6C", "LF8G") ~ "Retired",
           Code %in% c("LF6E", "LF6K", "LF6U") ~ "Other",
           Code=="LFM4...10" ~ "Wants a job",
           Code=="LFM5...11" ~ "Doesn't want a job"),
         Level2a=if_else(Code %in% c("LF64", "LF66", "LF68", "LF6A", "LFM3", "LF6C", "LF6E"), TRUE,
                         FALSE),
         Level2b=if_else(Code %in% c("LFL9...10", "LFM2...11"), TRUE, FALSE),
         Level3=if_else(Code %in% c("LF6G", "LF6M", "LFP9", "LF6O", "LF6I", "LF6Q", "LFQ2", 
                                    "LF6S", "LF8D", "LF8G", "LF6K", "LF6U"), TRUE, FALSE),
         Sex="Female") 

inactive <- bind_rows(inactive_p, inactive_m, inactive_f) %>% 
  mutate(Level1=FALSE) %>% 
  select(-Code)

data <- bind_rows(activity, inactive) %>% 
  mutate(Year=as.numeric(substr(Date, nchar(Date)-4, nchar(Date))),
         Month=substr(Date, 1, 3),
         Year=if_else(Month %in% c("Nov", "Dec"), Year-1, Year),
         date=as.Date(paste(Year, Month, "15", sep="-"), format="%Y-%b-%d"))

agg_tiff("Outputs/EconInactiveStatus.tiff", units="in", width=9, height=6, res=600)
  data %>% filter(Level2a==TRUE & Sex=="People" & date>=as.Date("1993-04-01") &
                  !Status %in% c("Employed", "Unemployed")) %>% 
  mutate(Status=factor(Status, levels=c("Long-term sick", "Student", "Looking after family/home",
                                        "Retired", "Other", "Temp. sick", "Discouraged"))) %>% 
  ggplot(aes(x=date, y=Count/1000000, colour=Status))+
    geom_rect(aes(xmin=as.Date("2020-02-01"), xmax=as.Date("2022-07-01"), ymin=0, ymax=Inf), 
              fill="Grey90", colour="Grey90")+
    geom_line()+
    scale_x_date(name="")+
    scale_y_continuous(name="Millions of people")+
    scale_colour_paletteer_d("colorblindr::OkabeIto", name="")+
    theme_custom()+
    theme(panel.grid.major.y=element_line(colour="Grey90"))+
    labs(title="The pandemic has seen a big increase in long-term sickness",
         subtitle="Self-reported reasons for economic inactivity - people who are out of work and not actively looking for a job",
         caption="Data from ONS | Plot by @VictimOfMaths")

  dev.off()
  
  agg_tiff("Outputs/EconInactiveStatusxSex.tiff", units="in", width=11, height=6, res=600)
  data %>% filter(Level2a==TRUE & Sex!="People" & date>=as.Date("1993-04-01") &
                    !Status %in% c("Employed", "Unemployed")) %>% 
    mutate(Status=factor(Status, levels=c("Long-term sick", "Student", "Looking after family/home",
                                          "Retired", "Other", "Temp. sick", "Discouraged"))) %>% 
    ggplot(aes(x=date, y=Count/1000000, colour=Status))+
    geom_rect(aes(xmin=as.Date("2020-02-01"), xmax=as.Date("2022-07-01"), ymin=0, ymax=Inf), 
              fill="Grey90", colour="Grey90")+
    geom_line()+
    scale_x_date(name="")+
    scale_y_continuous(name="Millions of people")+
    scale_colour_paletteer_d("colorblindr::OkabeIto", name="")+
    facet_wrap(~Sex)+
    theme_custom()+
    theme(panel.grid.major.y=element_line(colour="Grey90"))+
    labs(title="There are big gender differences in reasons for economic inactivity",
         subtitle="Self-reported reasons for economic inactivity - people who are out of work and not actively looking for a job",
         caption="Data from ONS | Plot by @VictimOfMaths")
  
  dev.off()
