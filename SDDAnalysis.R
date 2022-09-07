rm(list=ls())

library(tidyverse)
library(extrafont)
library(scales)
library(curl)
library(readxl)
library(paletteer)
library(ragg)
library(geomtextpath)
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

#Read in SDD data from NHS digital website
#https://digital.nhs.uk/data-and-information/publications/statistical/smoking-drinking-and-drug-use-among-young-people-in-england/2021/data-tables

#Smoking
smokeurl <- "https://files.digital.nhs.uk/0F/1DD277/sdd_2021_tab1.xlsx"
smokedata <- tempfile()
smokedata <- curl_download(url=smokeurl, destfile=smokedata, quiet=FALSE, mode="wb")

#Smoking status
smokestatus <- read_excel(smokedata, sheet="Table 1.1", range="A26:AD30", col_names=FALSE) %>% 
  set_names(c("Smoking status", as.character(c(1982, 1984, 1986, 1988, 1990, 1992, 1993, 1994, 1996, 1998, 
                                               1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 
                                               2009, 2010, 2011, 2012, 2013, 2014, 2016, 2018, 2021)))) %>% 
  gather(Year, Prop, c(2:ncol(.))) %>% 
  mutate(Year=as.numeric(Year),
         SmokingShort=case_when(
           `Smoking status` %in% c("Occasional smoker", "Regular smoker") ~ "Current smoker",
           `Smoking status`=="Never smoked" ~ "Never smoked",
           TRUE ~ "Tried smoking")) %>% 
  group_by(Year, SmokingShort) %>% 
  summarise(Prop=sum(Prop), .groups="drop")

agg_tiff("Outputs/SDDSmokingStatus.tiff", units="in", width=8, height=6, res=600)
ggplot(smokestatus, aes(x=Year, y=Prop/100, colour=SmokingShort, label=SmokingShort))+
  geom_textpath(show.legend=FALSE, hjust=0.15)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Proportion of 11-15 year olds", labels=label_percent(accuracy=1), limits=c(0,1))+
  scale_colour_paletteer_d("colorblindr::OkabeIto")+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="Grey90"),
        plot.title=element_text(size=rel(2)))+
  labs(title="Young people aren't smoking any more",
       subtitle="Proportions of 11-15 year old schoolchildren in England reporting current, ever or never smoking.\nData from the Smoking, Drinking and Drug Use among Young People in England survey",
       caption="Data from NHS Digital | Plot by @VictimOfMaths")
 
dev.off() 

#Smoking status by age
smokeage <- read_excel(smokedata, sheet="Table 1.3", range="A24:AD28", col_names=FALSE) %>% 
  set_names(c("Age", as.character(c(1982, 1984, 1986, 1988, 1990, 1992, 1993, 1994, 1996, 1998, 
                                    1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 
                                    2009, 2010, 2011, 2012, 2013, 2014, 2016, 2018, 2021)))) %>% 
  gather(Year, Prop, c(2:ncol(.))) %>% 
  mutate(Year=as.numeric(Year))

agg_tiff("Outputs/SDDSmokingxAge.tiff", units="in", width=8, height=6, res=600)
ggplot(smokeage, aes(x=Year, y=Prop/100, colour=Age, label=substr(Age,1,2)))+
  geom_textpath(show.legend=FALSE, hjust=0.58, straight=TRUE)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Proportion of age group who smoke regularly", labels=label_percent(accuracy=1))+
  scale_colour_paletteer_d("LaCroixColoR::Orange")+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="Grey90"),
        plot.title=element_text(size=rel(2)))+
  labs(title="All age groups are smoking less",
       subtitle="Proportions of 11-15 year old schoolchildren in England who report smoking regularly.\nData from the Smoking, Drinking and Drug Use among Young People in England survey",
       caption="Data from NHS Digital | Plot by @VictimOfMaths")

dev.off()

#Median cigarettes
cigarettes <- read_excel(smokedata, sheet="Table 1.6", range="A31:P31", col_names=FALSE) %>% 
  set_names(c("Cigarettes", as.character(c(2003, 2004, 2005, 2006, 2007, 2008, 
                                    2009, 2010, 2011, 2012, 2013, 2014, 2016, 2018, 2021)))) %>% 
  gather(Year, Median, c(2:ncol(.))) %>% 
  mutate(Year=as.numeric(Year))

agg_tiff("Outputs/SDDCigarettes.tiff", units="in", width=8, height=6, res=600)
ggplot(cigarettes, aes(x=Year, y=Median))+
  geom_line(colour="Tomato")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Cigarettes per week", limits=c(0,NA))+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="Grey90"),
        plot.title=element_text(size=rel(2)))+
  labs(title="Even the smokers are smoking less",
       subtitle="Median number of cigarettes smoked in the past week among current smokers aged 11-15.\nData from the Smoking, Drinking and Drug Use among Young People in England survey",
       caption="Data from NHS Digital | Plot by @VictimOfMaths")

dev.off()

#e-cigarettes
ecigurl <- "https://files.digital.nhs.uk/8C/6331D3/sdd_2021_tab4.xlsx"
ecigdata <- tempfile()
ecigdata <- curl_download(url=ecigurl, destfile=ecigdata, quiet=FALSE, mode="wb")

ecigs <- read_excel(ecigdata, sheet="Table 4.2", range="A26:E30", col_names=FALSE) %>% 
  set_names(c("Status", as.character(c(2014, 2016, 2018, 2021)))) %>% 
  gather(Year, Prop, c(2:ncol(.))) %>% 
  mutate(Year=as.numeric(Year),
         StatusShort=case_when(
           Status %in% c("Occasional e-cigarette user", "Regular e-cigarette user") ~ "Current e-cigarette user",
           Status=="Never used e-cigarettes" ~ "Never used e-cigarettes",
           TRUE ~ "Tried using e-cigarettes")) %>% 
  group_by(Year, StatusShort) %>% 
  summarise(Prop=sum(Prop), .groups="drop")

agg_tiff("Outputs/SDDEcigStatus.tiff", units="in", width=8, height=6, res=600)
ggplot(ecigs, aes(x=Year, y=Prop/100, colour=StatusShort, label=StatusShort))+
  geom_textpath(show.legend=FALSE)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Proportion of 11-15 year olds", labels=label_percent(accuracy=1), limits=c(0,1))+
  scale_colour_paletteer_d("colorblindr::OkabeIto")+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="Grey90"),
        plot.title=element_text(size=rel(2)))+
  labs(title="E-cigarette use in young people is low, but rising",
       subtitle="Proportions of 11-15 year old schoolchildren in England reporting current, ever or never having used e-cigarettes\nData from the Smoking, Drinking and Drug Use among Young People in England survey",
       caption="Data from NHS Digital | Plot by @VictimOfMaths")

dev.off()

#Ecigs vs. cigarettes
ecigvscig <- read_excel(ecigdata, sheet="Table 4.4", range="A9:F13", col_names=FALSE) %>% 
  set_names("E-cigarette use", "Regular smokers", "Occasional smokers", "Ex-smokers", "Tried smoking",
            "Never smokers") %>% 
  gather(`Cigarette use`, Prop, c(2:ncol(.))) %>% 
  mutate(`E-cigarette use`=factor(`E-cigarette use`, levels=c("Never used e-cigarettes",
                                                              "Tried using e-cigarettes",
                                                              "Ex e-cigarette user",
                                                              "Occasional e-cigarette user",
                                                              "Regular e-cigarette user")),
         `Cigarette use`=factor(`Cigarette use`, levels=c("Never smokers", "Tried smoking",
                                                          "Ex-smokers", "Occasional smokers",
                                                          "Regular smokers")))

agg_tiff("Outputs/SDDEcigsvsCigs.tiff", units="in", width=9, height=8, res=600)
ggplot(ecigvscig, aes(y=`Cigarette use`, x=`E-cigarette use`, label=paste0(round(Prop, 0), "%"), 
                      fill=Prop/100))+
  geom_tile(show.legend=FALSE)+
  geom_text()+
  scale_y_discrete(name="Proportion of...")+
  scale_x_discrete(name="Who...\n", labels=c("Have never\nused\ne-cigs", "Have only\ntried e-cigs",
                                             "Are former\ne-cig users", "Are occasional\ne-cig users",
                                             "Are regular\ne-cig users"))+
  scale_fill_paletteer_c("pals::ocean.amp", limits=c(0,1))+
  coord_equal()+
  theme_custom()+
  theme(plot.title=element_text(size=rel(2)))+
  labs(title="Young smokers are more likely to be using e-cigarettes",
       subtitle="E-cigarette use by smoking status among 11-15 year old schoolchildren in England.\nData from the Smoking, Drinking and Drug Use among Young People in England survey",
       caption="Data from NHS Digital | Plot by @VictimOfMaths")

dev.off()

#Alcohol
alcurl <- "https://files.digital.nhs.uk/56/EE0535/sdd_2021_tab5.xlsx"
alcdata <- tempfile()
alcdata <- curl_download(url=alcurl, destfile=alcdata, quiet=FALSE, mode="wb")

alcage <- read_excel(alcdata, sheet="Table 5.5", range="A24:Z28", col_names=FALSE) %>% 
  set_names("Age", as.character(c(1988, 1990, 1992, 1994, 1996, 1998:2014 , 2016, 2018, 2021))) %>% 
  gather(Year, Prop, c(2:ncol(.))) %>% 
  mutate(Year=as.numeric(Year))

agg_tiff("Outputs/SDDAlcxAge.tiff", units="in", width=8, height=6, res=600)
ggplot()+
  geom_textpath(data=alcage %>% filter(Year<2015), aes(x=Year, y=Prop/100, colour=Age, label=substr(Age,1,2)), show.legend=FALSE, 
                hjust=0.58, straight=TRUE)+
  geom_line(data=alcage %>% filter(Year>2015), aes(x=Year, y=Prop/100, colour=Age))+
  geom_vline(xintercept=2015, colour="Grey40", linetype=2)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Proportion of age group who drank in the last week", labels=label_percent(accuracy=1))+
  scale_colour_paletteer_d("LaCroixColoR::Orange")+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="Grey90"),
        plot.title=element_text(size=rel(2)),
        legend.position="none")+
  labs(title="All age groups are drinking less",
       subtitle="Proportions of 11-15 year old schoolchildren in England reporting drinking alcohol in the past week.\nData from the Smoking, Drinking and Drug Use among Young People in England survey",
       caption="Data from NHS Digital | Plot by @VictimOfMaths")+
  annotate("text", x=2015.2, y=0.45, label="Wording of the\nquestions was\nchanged", family=font, hjust=0,
           colour="Grey40")

dev.off()

#Drugs
drugurl <- "https://files.digital.nhs.uk/51/6249FE/sdd_2021_tab8.xlsx"
drugdata <- tempfile()
drugdata <- curl_download(url=drugurl, destfile=drugdata, quiet=FALSE, mode="wb")

#By age
drugage <- read_excel(drugdata, sheet="Table 8.2", range="A24:R28", col_names=FALSE) %>% 
  set_names(c("Age", as.character(c(2001:2014, 2016, 2018, 2020)))) %>% 
  gather(Year, Prop, c(2:ncol(.))) %>% 
  mutate(Year=as.numeric(Year))

agg_tiff("Outputs/SDDDrugsxAge.tiff", units="in", width=8, height=6, res=600)
ggplot()+
  geom_textpath(data=drugage %>% filter(Year<2015), aes(x=Year, y=Prop/100, colour=Age, label=substr(Age,1,2)), show.legend=FALSE, 
                hjust=0.58, straight=TRUE)+
  geom_line(data=drugage %>% filter(Year>2015), aes(x=Year, y=Prop/100, colour=Age))+
  geom_vline(xintercept=2015, colour="Grey40", linetype=2)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Proportion of age group who took drugs in the last year", 
                     labels=label_percent(accuracy=1), limits=c(0,NA))+
  scale_colour_paletteer_d("LaCroixColoR::Orange")+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="Grey90"),
        plot.title=element_text(size=rel(2)),
        legend.position="none")+
  labs(title="Drug use has fallen across all age groups",
       subtitle="Proportions of 11-15 year old schoolchildren in England reporting taking any drug in the past year\nData from the Smoking, Drinking and Drug Use among Young People in England survey",
       caption="Data from NHS Digital | Plot by @VictimOfMaths")+
  annotate("text", x=2015.2, y=0.35, label="Wording of the\nquestions was\nchanged", family=font, hjust=0,
           colour="Grey40")

dev.off()

#By substance
drugs <- read_excel(drugdata, sheet="Table 8.6c", range="A9:H29", col_names=FALSE) %>% 
  set_names("Drug", as.character(c(2011:2014, 2016, 2018, 2021))) %>% 
  mutate(Drug=case_when(
    Drug=="Any stimulants3" ~ "Any stimulants",
    Drug=="Mephedrone4" ~ "Mephedrone",
    Drug=="Any psychoactive substances5" ~ "Any psychoactive substances",
    Drug=="New psychoactive substances (previously known as legal highs)" ~ "New psychoactive substances",
    Drug=="Glue, gas, aerosols or solvents (volatile substances)" ~ "Glue, gas, aerosols or solvents",
    TRUE ~ Drug)) %>% 
  gather(Year, Prop, c(2:ncol(.))) %>% 
  mutate(Year=as.numeric(Year),
         Prop=as.numeric(if_else(Prop==":", "NA", Prop)),
         Upper=if_else(startsWith(Drug, "Any") | Drug %in% c("Cannabis", 
                                                             "Glue, gas, aerosols or solvents",
                                                             "Tranquillisers", "Other drugs"), 1, 0),
         Lower=if_else(startsWith(Drug, "Any"), 0, 1),
         Drug=factor(Drug, levels=c("Cannabis", "Glue, gas, aerosols or solvents", 
                                    "Any psychoactive substances", "Nitrous oxide", "Any stimulants",
                                    "Any psychedelics", "New psychoactive substances", "Magic mushrooms",
                                    "Cocaine", "Other drugs", "Ecstasy", "LSD", "Amphetamines",
                                    "Ketamine", "Tranquillisers", "Any opiates", "Heroin", "Poppers",
                                    "Crack", "Methadone", "Mephedrone")))

agg_tiff("Outputs/SDDDrugsxSubstance.tiff", units="in", width=8, height=8, res=600)
ggplot(drugs %>% filter(Upper==1), aes(x=Year, y=Prop/100, label=Drug, colour=Drug))+
  geom_line(show.legend=FALSE)+
  geom_point(show.legend=FALSE)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Proportion of 11-15 year olds who have ever taken...", 
                     labels=label_percent(accuracy=1))+
  scale_colour_paletteer_d("RSkittleBrewer::smarties")+
  facet_wrap(~Drug)+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="Grey90"))+
  labs(title="Use of the most common drugs fell between 2018 and 2021",
       subtitle="Proportions of 11-15 year old schoolchildren in England reporting ever taking each substance\nData from the Smoking, Drinking and Drug Use among Young People in England survey\n",
       caption="Data from NHS Digital | Plot by @VictimOfMaths")

dev.off()

newpal <- c(palettes_d$awtools$bpalette, "Black")

agg_tiff("Outputs/SDDDrugsxSubstanceDetail.tiff", units="in", width=12, height=8, res=600)
ggplot(drugs %>% filter(Lower==1), aes(x=Year, y=Prop/100, label=Drug, colour=Drug))+
  geom_line(show.legend=FALSE)+
  geom_point(show.legend=FALSE)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Proportion of 11-15 year olds who have ever taken...", 
                     labels=label_percent(accuracy=1))+
  scale_colour_manual(values=newpal)+
  facet_wrap(~Drug)+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="Grey90"))+
  labs(title="Use of the most common drugs fell between 2018 and 2021",
       subtitle="Proportions of 11-15 year old schoolchildren in England reporting ever taking each substance\nData from the Smoking, Drinking and Drug Use among Young People in England survey\n",
       caption="Data from NHS Digital | Plot by @VictimOfMaths")

dev.off()

#Comparison of ever tried/taken
drugcomp <- read_excel(drugdata, sheet="Table 8.6c", range="B30:H30", col_names=FALSE) %>% 
  set_names(as.character(c(2011:2014, 2016, 2018, 2021))) %>% 
  mutate(Substance="Class A drugs") %>% 
  gather(Year, Prop, c(1:ncol(.)-1)) %>% 
  mutate(Year=as.numeric(Year))

smokecomp <- smokestatus %>% 
  filter(SmokingShort!="Never smoked") %>% 
  group_by(Year) %>% 
  summarise(Prop=sum(Prop), .groups="drop") %>% 
  mutate(Substance="Cigarettes")

alccomp <- read_excel(alcdata, sheet="Table 5.1", range="B11:Z11", col_names=FALSE) %>% 
  set_names(as.character(c(1988, 1990, 1992, 1994, 1996, 1998:2014 , 2016, 2018, 2021))) %>% 
  mutate(Substance="Alcohol") %>% 
  gather(Year, Prop, c(1:ncol(.)-1)) %>% 
  mutate(Substance=if_else(Year<2015, "Alcohol (old measure)", "Alcohol (new measure)"),
         Year=as.numeric(Year))

ecigcomp <- ecigs %>% 
  filter(StatusShort!="Never used e-cigarettes") %>% 
  group_by(Year) %>% 
  summarise(Prop=sum(Prop), .groups="drop") %>% 
  mutate(Substance="E-cigarettes")

compdata <- bind_rows(drugcomp, smokecomp, alccomp, ecigcomp) %>% 
  mutate(Substance=factor(Substance, levels=c("Alcohol (old measure)", "Alcohol (new measure)",
                                              "E-cigarettes", "Cigarettes", "Class A drugs")))

agg_tiff("Outputs/SDDEverTriedComparison.tiff", units="in", width=9, height=7, res=600)
ggplot(compdata %>% filter(Year>=2000), aes(x=Year, y=Prop/100, colour=Substance))+
  geom_line()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Proportion of 11-15 year olds who have ever tried", 
                     labels=label_percent(accuracy=1))+
  scale_colour_manual(values=c("#203b7e", "#0FB2D3", "#cf152d", "#fea30c", "#43B629"), name="")+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="Grey90"), plot.title=element_text(size=rel(2)))+
  labs(title="Generation(s) sensible?",
       subtitle="Proportions of 11-15 year old schoolchildren in England reporting ever trying each substance\nData from the Smoking, Drinking and Drug Use among Young People in England survey\n",
       caption="Data from NHS Digital | Plot by @VictimOfMaths")

dev.off()
