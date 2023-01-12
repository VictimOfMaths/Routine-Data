rm(list=ls())

library(tidyverse)
library(curl)
library(ragg)
library(paletteer)
library(extrafont)
library(lubridate)
library(ggrepel)

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

#Download ambulance data
temp <- tempfile()
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/01/AmbSYS-for-December-2022.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data <- read.csv(temp) %>% 
  mutate(date=as.Date(paste0(Year, "-", Month, "-1")))

#Graph of call wait times

agg_tiff("Outputs/AmbulanceCallAnswerTimes.tiff", units="in", width=8, height=6, res=600)
data %>% 
  filter(Region=="Eng") %>% 
  select(date, A3, A4, A114, A5, A6) %>% 
  set_names("date", "Mean", "Median", "90th percentile", "95th percentile", "99th percentile") %>% 
  gather(Metric, Value, c(2:6)) %>% 
  mutate(Value=as.numeric(Value)) %>% 
  ggplot(aes(x=date, y=Value/60, colour=Metric))+
  geom_line()+
  geom_text_repel(data=. %>% filter(date==as.Date("2022-12-01")),
    aes(label = Metric), direction = "y", xlim = c(as.Date("2023-01-01"), NA_Date_),
    hjust = 0, family="Lato")+  
  scale_x_date(name="", limits=c(as.Date("2017-07-01"), as.Date("2023-12-01")),
               breaks=c(as.Date("2018-01-01"), as.Date("2020-01-01"),
                        as.Date("2022-01-01")),
               labels=c("2018", "2020", "2022"))+
  scale_y_continuous(name="Call answer times (minutes)", breaks=c(0,2,4,6,8))+
  scale_colour_manual(values=c("#FFAD0A", "#EE6100", "#D72000", "#1BB6AF", "#132157"))+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="grey90"))+
  guides(color = "none")+
  labs(title="However you measure it, ambulance calls are taking a long time to answer",
       subtitle="Time taken to answer phone calls to the emergency ambulance services in England",
       caption="Data from NHS Digital | Plot by @VictimOfMaths")

  dev.off()
