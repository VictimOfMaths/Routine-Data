rm(list=ls())

#Need the GitHub version to fix error in pulling sub-UK data
remotes::install_github("timriffe/TR1/TR1/HMDHFDplus")

library(tidyverse)
library(HMDHFDplus)
library(extrafont)
library(paletteer)
library(keyring)
library(scales)
library(ragg)
library(ggtext)

options(scipen=999999)

#Set common font for all plots
font <- "Lato"

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

#Read in life tables for selected countries by sex
data <- tibble(Year=as.integer(), Age=as.integer(), mx=as.double(), qx=as.double(),
               ax=as.double(), lx=as.integer(), dx=as.double(), Lx=as.integer(),
               Tx=as.integer(), ex=as.double(), OpenInterval=as.logical(),
               Country=as.character(), Sex=as.character())

for(i in c("m", "f", "b")){
  for(j in c("USA", "GBR_SCO", "GBRTENW", "FRATNP", "DEUTNP", "CAN", "AUS",
             "AUT", "JPN", "NLD", "SWE", "CHE")){
    data <- readHMDweb(CNTRY=j, paste0(i, "ltper_1x1"), key_list("mortality.org")[1,2], 
               key_get("mortality.org", key_list("mortality.org")[1,2])) %>% 
      mutate(Country=j, Sex=i) %>% 
      bind_rows(data)
  }
}

data2 <- data %>% 
  mutate(lx_p=lx/100000, phix=qx*lx_p,
         Sex=case_when(
           Sex=="m" ~ "Male", Sex=="f" ~ "Female", Sex=="b" ~ "Overall")) %>% 
  group_by(Year, Country, Sex) %>% 
  mutate(Ps5=1-Lx/Lx[Age==5],
         Pd5=1-Ps5) %>%
  ungroup() %>% 
  group_by(Country) %>% 
  mutate(maxyear=max(Year)) %>% 
  ungroup()

data2 %>% filter(Year==2020) %>% 
  ggplot(aes(x=Age, y=mx*100000, colour=Country))+
  geom_line()+
  scale_y_continuous(trans="log10")+
  theme_custom()+
  facet_wrap(~Sex)

data2 %>% filter(Year==2020) %>% 
  ggplot(aes(x=Age, y=phix, colour=Country))+
  geom_line()+
  scale_y_continuous()+
  theme_custom()+
  facet_wrap(~Sex)

data2 %>% filter(Age==0 & Year>1980) %>% 
  ggplot(aes(x=Year, y=ex, colour=Country))+
  geom_line()+
  theme_custom()+
  facet_wrap(~Sex)

ggplot(data2 %>% filter(Age>=5 & Year==maxyear & Age<=40),
       aes(x=Age, y=Ps5, colour=Country))+
  geom_line(show.legend=FALSE)+
  scale_y_continuous(name="Probability of a 5 year old dying before reaching...",
                     labels=label_percent(accuracy=1))+
  scale_colour_manual(values=c("#78c8e5", "#78c8e5", "gold", "#78c8e5",
                               "#78c8e5", "#78c8e5", "navyblue", "black",
                               "#78c8e5", "#78c8e5", "#78c8e5", "#d15f67"))+
  facet_wrap(~Sex)+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="grey95"))

agg_tiff("Outputs/SurvivalAge5.tiff", units="in", width=8, height=6, res=800)
ggplot(data2 %>% filter(Age>=5 & Year==maxyear & Age<=40 & Sex=="Male"),
       aes(x=Age, y=Ps5, colour=Country))+
  geom_line(show.legend=FALSE)+
  scale_x_continuous(breaks=c(10, 20, 30, 40), name="",
                     labels=c("Age 10", "20", "30", "40"))+
  scale_y_continuous(name="Probability of a 5 year old dying before reaching...",
                     labels=label_percent(accuracy=1),
                     breaks=c(0,0.01, 0.02, 0.03, 0.04, 0.05))+
  scale_colour_manual(values=c("#78c8e5", "#78c8e5", "gold", "#78c8e5",
                               "#78c8e5", "#78c8e5", "navyblue", "#342f2c",
                               "#78c8e5", "#78c8e5", "#78c8e5", "#d15f67"))+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="grey95"),
        plot.caption=element_markdown())+
  annotate("text", x=34, y=0.042, colour="#d15f67", family="Lato", label="US")+
  annotate("text", x=38, y=0.022, colour="gold", family="Lato", label="Canada")+
  annotate("text", x=37, y=0.03, colour="navyblue", family="Lato", label="Scotland")+
  annotate("text", x=41, y=0.018, colour="#342f2c", family="Lato", label="E&W")+
  annotate("text", x=34, y=0.0045, colour="#78c8e5", family="Lato", label="Other peer\ncountries*")+
  coord_cartesian(clip="off")+
  labs(title="Young male deaths in the USA far exceed other developed nations",
       subtitle="Probability of a 5-year old dying before reaching each age",
       caption="Data from Mortality.org | Plot idea by @jburnmurdoch | Plot by @VictimOfMaths<br><br><span style='color:#78c8e5;'>*Austria, Australia, France, Germany, Japan, Netherlands, Sweden, Switzerland ")
  
dev.off()
