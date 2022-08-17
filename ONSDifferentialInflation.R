rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(lubridate)
library(scales)
library(paletteer)
library(ggrepel)
library(extrafont)
library(ragg)
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

#Download inflation data from ONS
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/cpihconsistentinflationrateestimatesforukhouseholdgroupsdemocraticweighting/current/democraticreferencetables2022q2.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

deciles <- read_excel(temp, sheet="Table 3", range="B5:L203") %>%
  gather(Decile, Inflation, c(2:11)) %>% 
  mutate(Period=as.Date(Period),
         Decile=as.factor(as.numeric(substr(Decile, 8,9))))

decile_labels <- deciles %>% 
  filter(Period==max(Period), Decile %in% c(1,10)) %>% 
  mutate(labels=c("Lowest incomes", "Highest incomes"))

agg_tiff("Outputs/ONSInflationxDeciles.tiff", units="in", width=8, height=6, res=500)
ggplot(deciles, aes(x=Period, y=Inflation/100, colour=Decile))+
  geom_line()+
  geom_text_repel(data=decile_labels ,aes(label=labels),
                  show.legend=FALSE, family="Lato", xlim=(c(as.Date("2022-06-01"), NA)), segment.color = NA)+
  scale_x_date(limits=c(as.Date("2020-01-01"), as.Date("2022-11-01")), name="")+
  scale_y_continuous(name="12-month inflation rate", labels=label_percent(accuracy=1),
                     position="right")+
  scale_colour_paletteer_d("dichromat::BrowntoBlue_10")+
  theme_custom()+
  theme(legend.position="none", axis.line.y=element_blank(), panel.grid.major.y=element_line(colour="Grey90"),
        plot.title=element_markdown(colour="Grey40"))+
  labs(title="The cost of living crisis is hitting <span style='color:#543005;'>lower income households</span> hardest",
       subtitle="Annual percentage growth in CPIH inflation by deciles of equivalised household disposable income",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()
