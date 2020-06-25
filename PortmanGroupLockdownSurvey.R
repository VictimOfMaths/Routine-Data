rm(list=ls())

library(tidyverse)
library(paletteer)

#Data estimated from information in:
#https://www.portmangroup.org.uk/wp-content/uploads/2020/06/YouGov-Portman-Group-survey-on-alcohol-consumption-during-the-COVID-19-lockdown-2.pdf

#Proportions of people within each of the 3 categories (drink less, the same or more) were in each
#baseline (i.e. pre-lockdown) consumption category
data <- data.frame(drinkcat=c("Less than 14 units", "15-21 units", "22-28 units", "29-35 units",
                              "36-49 units", "50+ units"),
                   less=c(81,6,4,2,0,1),
                   same=c(72,10,6,4,3,2),
                   more=c(44,22,13,8,6,4))

#27% of drinkers reported they had cut down, so:
data$less.all <- data$less*0.27/100

#38% of drinkers reported no change
data$same.all <- data$same*0.38/100

#35% of drinkers reported drinking more
data$more.all <- data$more*0.35/100

data_long <- gather(data, group, perc, c(5:7))

data_long$drinkcat <- factor(data_long$drinkcat, levels=c("Less than 14 units", "15-21 units", "22-28 units", "29-35 units",
                                                          "36-49 units", "50+ units"))
data_long$group <- factor(data_long$group, levels=c("less.all", "same.all", "more.all"))

data_long <- data_long %>%
  group_by(drinkcat) %>% 
  mutate(prop=sum(perc)) %>%
  ungroup()

data_long$groupperc <- data_long$perc/data_long$prop

tiff("Outputs/PGlockdowndrinking100.tiff", units="in", width=8, height=6, res=500)
ggplot(data_long, aes(x=groupperc, y=drinkcat, fill=group))+
  geom_col(position="stack")+
  scale_x_continuous(name="Proportion of drinker group", breaks=c(0,0.25,0.5,0.75,1),
                     labels=c("0%", "25%", "50%", "75%", "100%"))+
  scale_y_discrete(name="Weekly alcohol consumption")+
  scale_fill_paletteer_d("Redmonder::qPBI", name="Reported change in\ndrinking during\nlockdown",
                         labels=c("Drinking less", "Drinking the same", "Drinking more"))+
  theme_classic()+
  labs(title="Heavier drinkers are drinking more during lockdown",
       subtitle="Self-reported change in alcohol consumption compared to pre-pandemic levels",
       caption="Data from YouGov/Portman Group | plot by @VictimOfMaths")
dev.off()

tiff("Outputs/PGlockdowndrinking.tiff", units="in", width=8, height=6, res=500)
ggplot(data_long, aes(x=perc, y=drinkcat, fill=group))+
  geom_col(position="stack")+
  scale_x_continuous(name="Proportion of drinkers in sample", breaks=c(0,20,40,60),
                     labels=c("0%", "20%", "40%", "60%"))+
  scale_y_discrete(name="Weekly alcohol consumption")+
  scale_fill_paletteer_d("Redmonder::qPBI", name="Reported change in\ndrinking during\nlockdown",
                         labels=c("Drinking less", "Drinking the same", "Drinking more"))+
  theme_classic()+
  labs(title="Heavier drinkers are drinking more during lockdown",
       subtitle="Self-reported change in alcohol consumption compared to pre-pandemic levels",
       caption="Data from YouGov/Portman Group | plot by @VictimOfMaths")
dev.off()
