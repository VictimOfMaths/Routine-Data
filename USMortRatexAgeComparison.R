rm(list=ls())

library(tidyverse)
library(HMDHFDplus)
library(paletteer)
library(ragg)
library(extrafont)
library(scales)
library(ggtext)
library(keyring)
library(RcppRoll)
library(forcats)

#Set common font for all plots
font <- "Lato"

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family=font),
          plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
          plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),
          axis.text=element_text(colour="Grey40"),
          axis.title=element_text(colour="Grey20"),
          legend.text=element_text(colour="Grey40"),
          legend.title=element_text(colour="Grey20"))
}


options(scipen=10000)

#Extract life tables
HMDLE<- function(countryabbrevs, countrynames) {
  lifetables <- data.frame(Year=integer(), Age=integer(), mx=double(), qx=double(), ax=double(),
                           lx=integer(), dx=integer(), Lx=integer(), Tx=integer(), ex=double(), 
                           Sex=character(), Country=character())
  
  for (i in 1:length(countryabbrevs)) {
    lifetables <- bind_rows(readHMDweb(CNTRY=countryabbrevs[i], "fltper_1x1", key_list("mortality.org")[1,2], 
                                       key_get("mortality.org", key_list("mortality.org")[1,2])) %>% 
                              mutate(Sex="Female"),
                            readHMDweb(CNTRY=countryabbrevs[i], "mltper_1x1", key_list("mortality.org")[1,2], 
                                       key_get("mortality.org", key_list("mortality.org")[1,2])) %>% 
                              mutate(Sex="Male"),
                            readHMDweb(CNTRY=countryabbrevs[i], "bltper_1x1", key_list("mortality.org")[1,2], 
                                       key_get("mortality.org", key_list("mortality.org")[1,2])) %>% 
                              mutate(Sex="Total")) %>% 
      mutate(Country=countrynames[i]) %>% 
      bind_rows(lifetables)
    
  }
  
  return(lifetables)
  
}

lifetables <- HMDLE(c("GBRTENW", "USA", "AUS", "CAN", "FRATNP", "AUT", "NLD", "JPN", "SWE", "CHE"), 
                    c("UK", "USA", "Australia", "Canada", "France", "Austria", "Netherlands",
                      "Japan", "Sweden", "Switzerland"))  

agebands <- lifetables %>%
  mutate(ageband=case_when(Age==0 ~ "0", Age<5 ~ "1-4", Age<10 ~ "5-9", Age<15 ~ "10-14", Age<20 ~ "15-19",
                           Age<25 ~ "20-24", Age<25 ~ "25-29", Age<30 ~ "30-34", Age<35 ~ "35-39",
                           Age<40 ~ "40-44", Age<45 ~ "45-49", Age<50 ~ "50-54", Age<55 ~ "55-59",
                           Age<60 ~ "60-64", Age<65 ~ "65-69", Age<70 ~ "70-74", Age<75 ~ "75-79",
                           Age<80 ~ "80-84", TRUE ~ "85+")) %>% 
  group_by(Country, Sex, ageband, Year) %>% 
  summarise(dx=sum(dx), lx=sum(lx), .groups="drop") %>% 
  mutate(mx=dx/lx,
         ageband=factor(ageband, levels=c("0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", 
                                          "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", 
                                          "70-74", "75-79", "80-84", "85+")))

ggplot(agebands %>% filter(Sex=="Total" & Year>=1980), aes(x=Year, y=mx*100000, colour=Country))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_line()+
  facet_wrap(~ageband, scales="free_y")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Deaths per 100,000")+
  scale_colour_manual(values=c("#bc80bd", "#8dd3c7", "#d9d9d9", "#bebada", "#80b1d3", "#fdb462", "#b3de69", "#fccde5",
                               "#fb8072", "darkred"))+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="grey95"),
        axis.line.x=element_blank())

agebands10 <- lifetables %>% 
  mutate(ageband=case_when(Age<10 ~ "0-9", Age<20 ~ "10-19", Age<30 ~ "20-29", Age<40 ~ "30-39",
                           Age<50 ~ "40-49", Age<60 ~ "50-59", Age<70 ~ "60-69", Age<80 ~ "70-79",
                           TRUE ~ "80+"))%>% 
  group_by(Country, Sex, ageband, Year) %>% 
  summarise(dx=sum(dx), lx=sum(lx), .groups="drop") %>% 
  mutate(mx=dx/lx)

agg_png("Outputs/MortRateTrendsxCountryxAge.png", units="in", width=10, height=9, res=800)
ggplot(agebands10 %>% filter(Sex=="Total" & Year>=1980), aes(x=Year, y=mx*100000, colour=Country))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_line()+
  facet_wrap(~ageband, scales="free_y")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Deaths per 100,000")+
  scale_colour_manual(values=c("#bc80bd", "#8dd3c7", "#d9d9d9", "#bebada", "#80b1d3", "#fdb462", "#b3de69", "#fccde5",
                               "#fb8072", "darkred"))+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="grey95"),
        axis.line.x=element_blank(), plot.title=element_markdown(),
        plot.subtitle=element_markdown())+
  labs(title="The <span style = 'color:darkred;'>US</span> has higher mortality rates in every age group than its peers,<br>and the gap is widening",
       subtitle="Annual all-cause mortality rates since 1980 in the <span style = 'color:darkred;'>USA</span> and selected high income countries<br>",
       caption="Data from mortality.org | Plot by @VictimOfMaths")

dev.off()
