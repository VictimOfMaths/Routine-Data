rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(extrafont)
library(ragg)

temp <- tempfile()
url <- "https://www.nrscotland.gov.uk/files//statistics/healthy-life-expectancy/17-19/healthy-life-expectancy-17-19-data.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

rawdata <- read_excel(temp, sheet="Figure6 Data", range="C7:G27", col_names=FALSE) %>% 
  mutate(Sex=c(rep("Male", times=10), NA, rep("Female", times=10))) %>% 
  filter(!is.na(Sex)) %>% 
  mutate(SIMD=rep(c("Most deprived", rep("", times=8), "Least deprived"), times=2),
         index=rep(1:10, times=2)) %>% 
  rename(HLE=`...1`, LE=`...5`) %>% 
  mutate(ULE=LE-HLE) %>% 
  gather(Measure, Years, c(HLE, ULE)) %>%
  mutate(Measure=factor(Measure, levels=c("ULE", "HLE")))

agg_tiff("Outputs/HLEIneqScotlandxSex.tiff", units="in", width=8, height=6, res=500)
ggplot(rawdata, aes(x=Years, y=as.factor(index), fill=Measure, label=round(Years, 1)))+
  geom_col(position="stack")+
  geom_text(aes(colour=Measure),position=position_stack(vjust=0.5), show.legend=FALSE, size=3)+
  scale_x_continuous(name="Years of life", breaks=seq(0,90, by=10))+
  scale_y_discrete(labels=c("Most deprived", rep("", times=8), "Least deprived"), name="SIMD decile")+
  scale_fill_manual(name="", values=c("#009f92", "#03312e"), labels=c("Years lived in poor health",
                                                                      "Years lived in good health"))+
  scale_colour_manual(values=c("Black", "White"))+
  facet_grid(Sex~., switch="y")+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        axis.ticks.y=element_blank(), text=element_text(family="Lato"), legend.position="top",
        axis.line.y=element_blank(), plot.title=element_text(face="bold", size=rel(1.6)),
        plot.title.position="plot", panel.grid.major.x=element_line())+
  guides(fill=guide_legend(reverse=TRUE))+
  labs(title="Inequalities in healthy lifespan are larger than in overall lifespan",
       subtitle="Average years lived in self-rated 'good' or 'very good' health compared to overall Life Expectancy in Scotland\nby sex and decile of the Scottish Index of Multiple Deprivation",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")
dev.off()