rm(list=ls())

library(tidyverse)
library(scales)
library(curl)
library(extrafont)
library(ragg)
library(ggtext)
library(readxl)
library(paletteer)
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

#Download ONS life-expectancy data for England
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fhealthinequalities%2fdatasets%2fhealthstatelifeexpectanciesbynationaldeprivationdecilesengland2018to2020%2f2018to2020/reftablehsleimd1820.xlsx"
temp <- tempfile()
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

rawdata <- read_excel(temp, sheet="Table 1", range="A3:M3203")

summary <- rawdata %>% 
  filter(Period=="2018-2020", Ageband==1) %>% 
  select(c("IMD Decile", "Sex", "Life expectancy (LE)", "Healthy life expectancy (HLE)")) %>% 
  set_names(c("IMD", "Sex", "LE", "HLE")) %>% 
  mutate(Diff=LE-HLE) %>% 
  gather(Metric, Value, c(Diff,HLE))

agg_png("Outputs/HLEIneqEnglandxSex.png", units="in", width=8, height=6, res=500)
ggplot(summary, aes(x=Value, y=as.factor(IMD), fill=Metric, label=round(Value, 1)))+
  geom_col(position="stack")+
  geom_text(aes(colour=Metric),position=position_stack(vjust=0.5), show.legend=FALSE, size=3)+
  scale_x_continuous(name="Years of life", breaks=seq(0,90, by=10))+
  scale_y_discrete(labels=c("Most deprived", rep("", times=8), "Least deprived"), name="IMD decile")+
  scale_fill_manual(name="", values=c("#009f92", "#03312e"), labels=c("Years lived in poor health",
                                                                      "Years lived in good health"))+
  scale_colour_manual(values=c("Black", "White"))+
  facet_grid(Sex~., switch="y")+
  theme_custom()+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(), legend.position="top",
        panel.grid.major.x=element_line(colour="Grey90"))+
  guides(fill=guide_legend(reverse=TRUE))+
  labs(title="Inequalities in healthy lifespan are larger than in overall lifespan",
       subtitle="Average years lived in self-rated 'good' or 'very good' health compared to overall Life Expectancy in England\nby sex and decile of the Index of Multiple Deprivation",
       caption="Data from ONS for 2018-20 | Plot by @VictimOfMaths")
dev.off()

changes <- rawdata %>% filter(Ageband==1 & Period %in% c("2017-2019", "2018-2020")) %>% 
  select(c("Period", "IMD Decile", "Sex", "Life expectancy (LE)", "Healthy life expectancy (HLE)")) %>% 
  gather(Metric, Value, c("Life expectancy (LE)", "Healthy life expectancy (HLE)")) %>% 
  group_by(Metric, Sex, `IMD Decile`) %>% 
  mutate(diff=Value-Value[Period=="2017-2019"],
         change=diff/Value[Period=="2017-2019"]) %>% 
  ungroup()

ggplot(changes %>% filter(Period=="2018-2020"), aes(x=as.factor(`IMD Decile`), y=change, fill=Metric))+
  geom_col(position="Dodge")+
  facet_grid(~ Sex)+
  theme_custom()

agg_png("Outputs/LEEnglandxIMDxSex.png", units="in", width=8, height=6, res=500)
ggplot()+
  geom_segment(data=changes %>% filter(Metric=="Life expectancy (LE)") %>% 
              select(-c("diff", "change")) %>% 
              spread(Period, Value), aes(x=`2017-2019`, xend=`2018-2020`, y=as.factor(`IMD Decile`),
                                         yend=as.factor(`IMD Decile`)), colour="Grey40")+ 
  geom_point(data=changes %>% filter(Metric=="Life expectancy (LE)"), 
             aes(x=Value, y=as.factor(`IMD Decile`)),
             fill="White", colour="transparent", shape=21, size=3)+
  geom_point(data=changes %>% filter(Metric=="Life expectancy (LE)"), 
             aes(x=Value, y=as.factor(`IMD Decile`), fill=Sex, colour=Period, alpha=Period),
             shape=21, size=3)+
  scale_x_continuous(name="Life expectancy at birth")+
  scale_y_discrete(name="IMD Decile", labels=c("1 - most deprived", "2", "3", "4", "5", "6", "7", 
                                               "8", "9", "10 - least deprived"))+
  scale_colour_manual(values=c("transparent", "Black"))+
  scale_fill_manual(values=c("#00cc99", "#6600cc"))+
  scale_alpha_manual(values=c(0.6,1))+
  theme_custom()+
  theme(legend.position="none", plot.subtitle=element_markdown())+
  labs(title="Deprived areas have seen the biggest losses in life expectancy",
       subtitle="Changes in life expectancy at birth for <span style='color:#6600cc;'>men</span> and <span style='color:#00cc99;'>women</span> between 2017-19 and <span style='color:Black;'>2018-20</span> in England by deciles<br>of the Index of Multiple Deprivation (IMD)",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

agg_png("Outputs/HLEEnglandxIMDxSex.png", units="in", width=8, height=6, res=500)
ggplot()+
  geom_segment(data=changes %>% filter(Metric=="Healthy life expectancy (HLE)") %>% 
                 select(-c("diff", "change")) %>% 
                 spread(Period, Value), aes(x=`2017-2019`, xend=`2018-2020`, y=as.factor(`IMD Decile`),
                                            yend=as.factor(`IMD Decile`)), colour="Grey40")+ 
  geom_point(data=changes %>% filter(Metric=="Healthy life expectancy (HLE)"), 
             aes(x=Value, y=as.factor(`IMD Decile`)),
             fill="White", colour="transparent", shape=21, size=3)+
  geom_point(data=changes %>% filter(Metric=="Healthy life expectancy (HLE)"), 
             aes(x=Value, y=as.factor(`IMD Decile`), fill=Sex, colour=Period, alpha=Period),
             shape=21, size=3)+
  scale_x_continuous(name="Healthy Life Expectancy at birth")+
  scale_y_discrete(name="IMD Decile", labels=c("1 - most deprived", "2", "3", "4", "5", "6", "7", 
                                               "8", "9", "10 - least deprived"))+
  scale_colour_manual(values=c("transparent", "Black"))+
  scale_fill_manual(values=c("#00cc99", "#6600cc"))+
  scale_alpha_manual(values=c(0.6,1))+
  theme_custom()+
  theme(legend.position="none", plot.subtitle=element_markdown())+
  labs(title="Meanwhile Healthy Life Expectancy has risen for most groups",
       subtitle="Changes in healthy life expectancy at birth for <span style='color:#6600cc;'>men</span> and <span style='color:#00cc99;'>women</span> between 2017-19 and <span style='color:Black;'>2018-20</span> in England by deciles<br>of the Index of Multiple Deprivation (IMD)",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

agechanges <- rawdata %>% filter(Period %in% c("2017-2019", "2018-2020")) %>% 
  select(c("Age group", "Period", "IMD Decile", "Sex", "Life expectancy (LE)", "Healthy life expectancy (HLE)")) %>% 
  gather(Metric, Value, c("Life expectancy (LE)", "Healthy life expectancy (HLE)")) %>% 
  group_by(`Age group`, Metric, Sex, `IMD Decile`) %>% 
  mutate(diff=Value-Value[Period=="2017-2019"],
         change=diff/Value[Period=="2017-2019"]) %>% 
  ungroup()

ggplot(agechanges %>% filter(Period=="2018-2020" & Metric=="Life expectancy (LE)"),
       aes(x=`Age group`, y=diff, fill=Sex))+
  geom_col(position="dodge")+
  facet_wrap(~`IMD Decile`)+
  theme_custom()

agg_png("Outputs/HLETrendsEngxIMDxSex.png", units="in", width=8, height=6, res=500)
rawdata %>% filter(Ageband==1 & Period!="2018-2020") %>% 
  ggplot(aes(x=Period, y=`Healthy life expectancy (HLE)`, colour=as.factor(`IMD Decile`), group=as.factor(`IMD Decile`)))+
  geom_line()+
  scale_x_discrete(name="", labels=c("2011\n-2013", "", "2013\n-2015", "", "2015\n-2017", "",
                                     "2017\n-2019"))+
  scale_colour_paletteer_d("dichromat::BrowntoBlue_10", name="IMD Decile", labels=c("1 - most deprived", "2", 
                                                                             "3", "4", "5", "6", "7", 
                                                                             "8", "9", 
                                                                             "10 - least deprived"),
                           guide = guide_legend(reverse = TRUE))+
  facet_wrap(~Sex)+
  theme_custom()+
  labs(title="Healthy Life Expectancy has changed little in recent years",
       subtitle="Expected number of years at birth living in `good` or `very good` health in England",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

agg_png("Outputs/LETrendsEngxIMDxSex.png", units="in", width=8, height=6, res=500)
rawdata %>% filter(Ageband==1 & Period!="2018-2020") %>% 
  ggplot(aes(x=Period, y=`Life expectancy (LE)`, colour=as.factor(`IMD Decile`), group=as.factor(`IMD Decile`)))+
  geom_line()+
  scale_x_discrete(name="", labels=c("2011\n-2013", "", "2013\n-2015", "", "2015\n-2017", "",
                                     "2017\n-2019"))+
  scale_colour_paletteer_d("dichromat::BrowntoBlue_10", name="IMD Decile", labels=c("1 - most deprived", "2", 
                                                                             "3", "4", "5", "6", "7", 
                                                                             "8", "9", 
                                                                             "10 - least deprived"),
                           guide = guide_legend(reverse = TRUE))+
  facet_wrap(~Sex)+
  theme_custom()+
  labs(title="Life Expectancy has become more unequal",
       subtitle="Life Expectancy at birth in England by deciles of the Index of Multiple Deprivation (IMD)",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

changes <- rawdata %>% 
  filter(Ageband==1 & Period %in% c("2011-2013", "2017-2019")) %>% 
  group_by(`IMD Decile`, Sex) %>% 
  mutate(LEchange=`Life expectancy (LE)`[Period=="2017-2019"]-
           `Life expectancy (LE)`[Period=="2011-2013"],
         HLEchange=`Healthy life expectancy (HLE)`[Period=="2017-2019"]-
           `Healthy life expectancy (HLE)`[Period=="2011-2013"]) %>% 
  ungroup() %>% 
  filter(Period=="2011-2013") %>% 
  gather(Metric, Change, c(LEchange, HLEchange)) %>% 
  mutate(Metric=if_else(Metric=="LEchange", "Life Expectancy", "Healthy Life Expectancy"),
         Metric=factor(Metric, levels=c("Life Expectancy", "Healthy Life Expectancy")))

agg_png("Outputs/LEHLEChangesEngxIMDxSex.png", units="in", width=10, height=6, res=500)
ggplot(changes, aes(x=as.factor(`IMD Decile`), y=Change, fill=as.factor(`IMD Decile`)))+
  geom_col(show.legend=FALSE)+
  geom_hline(yintercept=0, colour="Black")+
  scale_x_discrete(name="IMD Decile", labels=c("1\nmost\ndeprived", "2", "3", "4", "5", "6", "7", "8", 
                                               "9", "10\nleast\ndeprived"))+
  scale_y_continuous(name="Change between 2011-13 & 2017-19\n(years)")+
  scale_fill_paletteer_d("dichromat::BrowntoBlue_10")+
  facet_grid(Metric~Sex)+
  theme_custom()+
  labs(title="England is becoming more unequal",
       subtitle="Changes in Life Expectancy and Healthy Life Expectancy at birth between 2011-13 and 2017-19\nby sex and deciles of the Index of Multiple Deprivation",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

#Longer-term context from ONS
#https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/articles/ourpopulationwherearewehowdidwegetherewherearewegoing/2020-03-27
source <- "https://www.ons.gov.uk/visualisations/dvc775/fig4/line_chart1/datadownload.csv"
temp <- tempfile()
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

LEtrends <- read.csv(temp) %>% 
  set_names("Year", "Male", "Female") %>% 
  gather(Sex, LE, c(Male, Female))

agg_png("Outputs/LETrendsLongTermxSex.png", units="in", width=8, height=6, res=500)
ggplot(LEtrends %>% filter(Year>1951), aes(x=Year, y=LE, colour=Sex))+
  geom_line(show.legend=FALSE)+
  geom_text_repel(data=LEtrends %>% filter(Year==max(Year)),
                  aes(x=max(Year), y=LE, label = Sex, 
                      colour=Sex),
                  family = "Calibri", direction = "y", xlim = c(2019, NA),
                  hjust = 0, segment.color = NA, box.padding = .3, show.legend = FALSE)+
  scale_x_continuous(name="", limits=c(1951, 2022))+
  scale_y_continuous(name="Life Expectancy")+
  scale_colour_manual(values=c("#00cc99", "#6600cc"))+
  theme_custom()+
  labs(title="Life Expectancy gains stagnated in the 2010s",
       subtitle="Life Expectancy at birth in England by Sex in the last 70 years",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()
