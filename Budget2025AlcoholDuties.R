rm(list=ls())

library(tidyverse)
library(readxl)
library(lubridate)
library(paletteer)
library(curl)
library(ragg)
library(ggtext)
library(ggrepel)
library(extrafont)
library(geomtextpath)

options(scipen=99999999)

#Set common font for all plots
font <- "Lato"

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          strip.clip="off",
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"),
          plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
          plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),
          axis.text=element_text(colour="Grey40"),
          axis.title=element_text(colour="Grey20"),
          legend.text=element_text(colour="Grey40"),
          legend.title=element_text(colour="Grey20"),
          axis.line.x=element_blank(),
          panel.grid.major.y = element_line(colour="grey95"))
}

##############################################
#Current UK alcohol duty rates
dutyrates <- data.frame(ABV=seq(0,40, by=0.1)) %>% 
  mutate(Beer_Std_PreReform=case_when(
    ABV<=1.2 ~ NA_real_,
    ABV<=2.8 ~ 8.42*10/1000,
    ABV<=7.5 ~ 19.08*10/1000,
    ABV<=12.5 ~ 24.77*10/1000),
    Cider_Std_PreReform=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<6.9 ~ 40.38*10/(1000*ABV),
      ABV<=7.5 ~ 50.71*10/(1000*ABV),
      ABV<8.5 ~ 61.04*10/(1000*ABV)),
    Spirits_Std_PreReform=case_when(
      ABV<=1.2 ~ NA_real_,
      TRUE ~ 28.74*10/1000),
    Wine.Still_Std_PreReform=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<=4 ~ 91.68*10/(1000*ABV),
      ABV<=5.5 ~ 126.08*10/(1000*ABV),
      ABV<=15 ~ 297.57*10/(1000*ABV),
      ABV<=22 ~ 396.72*10/(1000*ABV)),
    Wine.Sparkling_Std_PreReform=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<=4 ~ 91.68*10/(1000*ABV),
      ABV<=5.5 ~ 126.08*10/(1000*ABV),
      ABV<=8.5 ~ 288.10*10/(1000*ABV),
      ABV<=15 ~ 381.15*10/(1000*ABV)),
    Beer_Std_PostReform=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<3.5 ~ 9.27*10/1000,
      ABV<8.5 ~ 21.01*10/1000,
      ABV<=22 ~ 28.50*10/1000,
      TRUE ~ 31.64*10/1000),
    Cider_Std_PostReform=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<3.5 ~ 9.27*10/1000,
      ABV<8.5 ~ 9.67*10/1000,
      ABV<=22 ~ 28.5*10/1000,
      TRUE ~ 31.64*10/1000),
    Wine_Std_PostReform=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<3.5 ~ 9.27*10/1000,
      ABV<8.5 ~ 24.77*10/1000,
      ABV<11.5 ~ 28.50*10/1000,
      ABV<=14.5 ~ (28.50*10/1000)*12.5/ABV, 
      ABV<=22 ~ 28.50*10/1000,
      TRUE ~ 31.64*10/1000),
    Spirits_Std_PostReform=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<3.5 ~ 9.27*10/1000,
      ABV<8.5 ~ 24.77*10/1000,
      ABV<=22 ~ 28.50*10/1000,
      TRUE ~ 31.64*10/1000),
    Beer_DR_PostReform=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<3.5 ~ 8.42*10/1000,
      ABV<8.5 ~ 19.08*10/1000,
      ABV<=22 ~ 28.50*10/1000,
      TRUE ~ 31.64*10/1000),
    Cider_DR_PostReform=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<3.5 ~ 8.42*10/1000,
      ABV<8.5 ~ 8.78*10/1000,
      ABV<=22 ~ 28.5*10/1000,
      TRUE ~ 31.64*10/1000),
    Wine_DR_PostReform=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<3.5 ~ 8.42*10/1000,
      ABV<8.5 ~ 19.08*10/1000,
      ABV<11.5 ~ 28.50*10/1000,
      ABV<=14.5 ~ (28.50*10/1000)*12.5/ABV, 
      ABV<=22 ~ 28.50*10/1000,
      TRUE ~ 31.64*10/1000),
    Beer_Std_PostBudget=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<3.5 ~ 9.61*10/1000,
      ABV<8.5 ~ 21.78*10/1000,
      ABV<=22 ~ 29.54*10/1000,
      TRUE ~ 32.79*10/1000),
    Cider_Std_PostBudget=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<3.5 ~ 9.61*10/1000,
      ABV<8.5 ~ 10.02*10/1000,
      ABV<=22 ~ 29.54*10/1000,
      TRUE ~ 32.79*10/1000),
    Wine_Std_PostBudget=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<3.5 ~ 9.61*10/1000,
      ABV<8.5 ~ 25.67*10/1000,
      ABV<=22 ~ 29.54*10/1000,
      TRUE ~ 32.79*10/1000),
    Spirits_Std_PostBudget=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<3.5 ~ 9.61*10/1000,
      ABV<8.5 ~ 25.67*10/1000,
      ABV<=22 ~ 29.54*10/1000,
      TRUE ~ 32.79*10/1000),
    Beer_DR_PostBudget=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<3.5 ~ 8.28*10/1000,
      ABV<8.5 ~ 18.76*10/1000,
      ABV<=22 ~ 29.54*10/1000,
      TRUE ~ 32.79*10/1000),
    Cider_DR_PostBudget=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<3.5 ~ 8.28*10/1000,
      ABV<8.5 ~ 8.63*10/1000,
      ABV<=22 ~ 29.54*10/1000,
      TRUE ~ 32.79*10/1000),
    Wine_DR_PostBudget=case_when(
      ABV<=1.2 ~ NA_real_,
      ABV<3.5 ~ 8.28*10/1000,
      ABV<8.5 ~ 18.76*10/1000,
      ABV<=22 ~ 29.54*10/1000,
      TRUE ~ 32.79*10/1000)) %>% 
  pivot_longer(c(2:ncol(.)), names_to=c("Drink", "Cat", "Period"), names_sep="_", values_to="DutyRate") 



agg_tiff("Outputs/HMRCUKDutyRates.tiff", units="in", width=8, height=6, res=500)
ggplot(dutyrates, aes(x=ABV/100, y=DutyRate, colour=Drink, linetype=Cat))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_line()+
  scale_x_continuous(labels=label_percent(accuracy=1), name="Alcoholic strength (ABV)", limits=c(0,0.25))+
  scale_y_continuous(limits=c(0,NA), labels=label_dollar(prefix="£"), name="Duty payable per unit of alcohol")+
  scale_colour_manual(values=c("#F7AA14", "#2CB11B", "#0099D5", "purple", "#FC6882", "#C70E7B"), name="",
                      labels=c("Beer", "Cider", "Spirits", "Wine (all)", "Sparkling Wine", "Still Wine"))+
  scale_linetype_manual(values=c(2,1), labels=c("Draught", "Standard"))+
  facet_wrap(~Period)+
  theme_custom()+
  theme(legend.position="top")+
  labs(title="The UK alcohol duty system is stupid",
       subtitle="Alcohol duty payable per unit of alcohol by alcoholic strength and product type",
       caption="Data from HMRC | Plot by @VictimOfMaths")

dev.off()

agg_png("Outputs/HMRCUKDutyRatesBudget25.png", units="in", width=8, height=6, res=500)
dutyrates %>% filter(Period!="PreReform") %>% 
  mutate(Period=if_else(Period=="PostReform", "Now", "From Feb 2025"),
         Period=factor(Period, levels=c("Now", "From Feb 2025"))) %>% 
  ggplot(aes(x=ABV/100, y=DutyRate, colour=Drink, linetype=Cat))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_line()+
  scale_x_continuous(labels=label_percent(accuracy=1), name="Alcoholic strength (ABV)", limits=c(0,0.25))+
  scale_y_continuous(limits=c(0,NA), labels=label_dollar(prefix="£"), name="Duty payable per unit of alcohol")+
  scale_colour_manual(values=c("#F7AA14", "#2CB11B", "#0099D5", "purple"), name="",
                      labels=c("Beer", "Cider", "Spirits", "Wine", "Sparkling Wine", "Still Wine"))+
  scale_linetype_manual(values=c(2,1), labels=c("Draught", "Standard"), name="")+
  facet_wrap(~Period)+
  theme_custom()+
  theme(legend.position="top")+
  labs(title="The impact of today's Budget on alcohol duties",
       subtitle="Alcohol duty payable per unit of alcohol by alcoholic strength and product type",
       caption="Data from HMRC | Plot by @VictimOfMaths")

dev.off()

