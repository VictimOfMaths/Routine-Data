rm(list=ls())

library(tidyverse)
library(readxl)
library(systemfonts)
library(ggtext)
library(sf)
library(scales)
library(forcats)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Download data from ONS website
url <- "https://www.ons.gov.uk/file?uri=%2femploymentandlabourmarket%2fpeopleinwork%2fearningsandworkinghours%2fdatasets%2frealtimeinformationstatisticsreferencetableseasonallyadjusted%2fcurrent/rtistatisticsreferencetableseasonallyadjusted2.xlsx"
temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

#Employees by LA
employees <- read_excel(temp, sheet="19. Employees (LA)", range="A6:NZ92") %>% 
  filter(Date %in% c("May 2021", "May 2020", "May 2019", "August 2019", "August 2021")) %>% 
  gather(LA, employees, c(2:ncol(.))) %>% 
  #Address weird LA anomalies in the ONS data
  mutate(LA=case_when(
    LA %in% c("Argyll and Bute Islands", "Argyll and Bute Mainland",
              "Helensburgh and Lomond", "Lochaber") ~ "Argyll and Bute",
    LA %in% c("Arran and Cumbrae", "North Ayrshire mainland") ~ "North Ayrshire",
    LA %in% c("North East Moray", "West Moray") ~ "Moray",
    LA %in% c("Badenoch and Strathspey", "Caithness and Sutherland", "Inverness and Nairn",
              "Ross and Cromarty") ~ "Highland",
    TRUE ~ LA)) %>% 
  group_by(LA, Date) %>%
  summarise(employees=sum(employees)) %>% 
  ungroup() %>% 
  spread(Date, employees) %>% 
  mutate(employees_2021_abs=`May 2021`-`May 2020`,
         employees_1920_abs=`May 2020`-`May 2019`,
         employees_1921_abs=`August 2021`-`August 2019`,
         employees_2021_rel=employees_2021_abs/`May 2020`,
         employees_1920_rel=employees_1920_abs/`May 2019`,
         employees_1921_rel=employees_1921_abs/`August 2019`) %>% 
  select(-c(`May 2019`, `May 2020`, `May 2021`, `August 2019`, `August 2021`))

#Median pay by LA
pay <- read_excel(temp, sheet="20. Median pay (LA)", range="A6:NZ92") %>% 
  filter(Date %in% c("May 2021", "May 2020", "May 2019", "August 2019", "August 2021")) %>% 
  gather(LA, pay, c(2:ncol(.))) %>% 
  #Address weird LA anomalies in the ONS data
  mutate(LA=case_when(
    LA %in% c("Argyll and Bute Islands", "Argyll and Bute Mainland",
              "Helensburgh and Lomond", "Lochaber") ~ "Argyll and Bute",
    LA %in% c("Arran and Cumbrae", "North Ayrshire mainland") ~ "North Ayrshire",
    LA %in% c("North East Moray", "West Moray") ~ "Moray",
    LA %in% c("Badenoch and Strathspey", "Caithness and Sutherland", "Inverness and Nairn",
              "Ross and Cromarty") ~ "Highland",
    TRUE ~ LA)) %>% 
  group_by(LA, Date) %>%
  summarise(pay=sum(pay)) %>% 
  ungroup() %>% 
  spread(Date, pay) %>% 
  mutate(pay_2021_abs=`May 2021`-`May 2020`,
         pay_1920_abs=`May 2020`-`May 2019`,
         pay_1921_abs=`August 2021`-`August 2019`,
         pay_2021_rel=pay_2021_abs/`May 2020`,
         pay_1920_rel=pay_1920_abs/`May 2019`,
         pay_1921_rel=pay_1921_abs/`August 2019`) %>% 
  select(-c(`May 2019`, `May 2020`, `May 2021`, `August 2019`, `August 2021`))

LAdata <- merge(employees, pay) %>% 
  rename(Laname=LA) %>% 
  #Fix boundary changes to align with cartogram
  mutate(Laname=case_when(
    Laname=="Bournemouth, Christchurch and Poole" ~ "Bournemouth, Christchurch & Poole",
    Laname=="Somerset West and Taunton" ~ "Somerset West & Taunton",
    TRUE ~ Laname
  )) %>% 
  filter(Laname!="UK") %>% 
  pivot_longer(cols=c(2:13), names_to=c("metric", "date", "type"), values_to="change", names_sep="_")

#Scatter plots (not much of interest here to be honest)
LAdata %>% filter(metric=="employees" & type=="abs") %>% 
  spread(date, change) %>%
  ggplot(aes(x=`1920`, y=`2021`))+
  geom_point(shape=21)+
  theme_custom()

LAdata %>% filter(metric=="employees" & type=="rel") %>% 
  spread(date, change) %>%
  ggplot(aes(x=`1920`, y=`2021`))+  
  geom_point(shape=21)+
  theme_custom()

LAdata %>% filter(metric=="pay" & type=="abs") %>% 
  spread(date, change) %>%
  ggplot(aes(x=`1920`, y=`2021`))+  
  geom_point(shape=21)+
  theme_custom()

LAdata %>% filter(metric=="pay" & type=="rel") %>% 
  spread(date, change) %>%
  ggplot(aes(x=`1920`, y=`2021`))+  
  geom_point(shape=21)+
  theme_custom()

#Maps
#Download Car Baker's lovely hex cartogram
ltla <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/LocalAuthorities-lowertier.gpkg")
ltla <- curl_download(url=source, destfile=ltla, quiet=FALSE, mode="wb")

Background <- st_read(ltla, layer="7 Background")

ltladata <- st_read(ltla, layer="5 LTLA-2020") %>% 
  left_join(LAdata, by="Laname", all=TRUE)

Groups <- st_read(ltla, layer="2 Groups")

Group_labels <- st_read(ltla, layer="1 Group labels") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))

plot1data <- ltladata %>% filter(metric=="pay" & type=="rel" & date!="1921") %>% 
  mutate(labs=if_else(date=="1920", "May '19 to May '20", "May '20 to May '21"))

plot1 <- ggplot()+
  geom_sf(data=Background, aes(geometry=geom), fill="White")+
  geom_sf(data=plot1data, aes(geometry=geom, fill=change), colour="Black", size=0.1)+
  geom_sf(data=Groups, aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels, aes(geometry=geom, label=Group.labe,
                                      hjust=just), size=rel(2.4), colour="Black")+
  #scale_fill_paletteer_c("pals::warmcool", limit=c(-1,1)*max(abs(plot1data$change)), 
   #                      name="Change in number\nof people employed\nbetween August 2019\nand August 2020",
  #                       na.value="transparent")+
  scale_fill_steps2(limits=c(-1,1)*max(abs(plot1data$change)), n.breaks=12, labels=percent_format(accuracy=1),
                    name="Change in median wage")+
  facet_wrap(~labs)+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.4)),
        text=element_text(family="Lato"), legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="The pandemic stalled wage growth across the UK, but this has now picked up",
       subtitle="Change in median wages for individuals paid through PAYE",
       caption="Data from ONS, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/UKMedianWages.tiff", units="in", width=12, height=9, res=800)
plot1
dev.off()

#Look at data by industry
ind_employ <- read_excel(temp, sheet="23. Employees (Industry)", range="A6:W92") %>% 
  filter(Date %in% c("May 2021", "May 2020", "May 2019", "August 2019", "August 2021")) %>% 
  gather(industry, employees, c(2:ncol(.))) %>% 
  spread(Date, employees) %>% 
  rowwise() %>% 
  mutate(employees_2021_abs=`May 2021`-`May 2020`,
         employees_1920_abs=`May 2020`-`May 2019`,
         employees_1921_abs=`August 2021`-`August 2019`,
         employees_2021_rel=employees_2021_abs/`May 2020`,
         employees_1920_rel=employees_1920_abs/`May 2019`,
         employees_1921_rel=employees_1921_abs/`August 2019`,
         max=max(`May 2019`, `May 2020`, `May 2021`),
         labvalue=paste0(if_else((employees_1920_abs+employees_2021_abs)/`May 2019`>0, "+", ""),
           round(100*(employees_1920_abs+employees_2021_abs)/`May 2019`, 1), "%"))

agg_tiff("Outputs/UKEmployeesxIndustry.tiff", units="in", width=10, height=7, res=800)
ggplot(ind_employ %>% filter(industry!="UK"), aes(y=fct_reorder(industry, `May 2019`)))+
  geom_segment(aes(x=`May 2019`, xend=`May 2020`, yend=industry), colour="Grey70")+
  geom_segment(aes(x=`May 2021`, xend=`May 2020`, yend=industry), colour="Grey70")+
  geom_point(aes(x=`May 2019`), colour="#c7e94b", size=rel(3))+
  geom_point(aes(x=`May 2020`), colour="#41b6c4", size=rel(3))+
  geom_point(aes(x=`May 2021`), colour="#253494", size=rel(3))+
  geom_text(aes(x=max, label=labvalue), size=rel(2.5), hjust=-0.5, colour="Grey30")+
  scale_x_continuous(name="Total employees (millions)", labels=c(0,1,2,3,4),
                     breaks=c(0,1000000,2000000,3000000,4000000), limits=c(0,4800000))+
  scale_y_discrete(name="Industry")+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="Retail, hospitality, manufacturing and the arts have seen big job loses during the pandemic",
       subtitle="Total numbers of employees paid through PAYE by sector in May <span style='color:#c7e94b;'>2019</span>, <span style='color:#41b6c4;'>2020</span> and <span style='color:#253494;'>2021</span>.<br>Figures represent the overall change between May 2019 and May 2021.",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

ind_pay <- read_excel(temp, sheet="24. Median pay (Industry)", range="A6:W92") %>% 
  filter(Date %in% c("May 2021", "May 2020", "May 2019", "August 2019", "August 2021")) %>% 
  gather(industry, pay, c(2:ncol(.))) %>% 
  spread(Date, pay) %>% 
  rowwise() %>% 
  mutate(pay_2021_abs=`May 2021`-`May 2020`,
         pay_1920_abs=`May 2020`-`May 2019`,
         pay_1921_abs=`August 2021`-`August 2019`,
         pay_2021_rel=pay_2021_abs/`May 2020`,
         pay_1920_rel=pay_1920_abs/`May 2019`,
         pay_1921_rel=pay_1921_abs/`August 2019`,
         max=max(`May 2019`, `May 2020`, `May 2021`),
         labvalue=paste0(if_else((pay_1920_abs+pay_2021_abs)/`May 2019`>0, "+", ""),
                         round(100*(pay_1920_abs+pay_2021_abs)/`May 2019`, 1), "%"))

agg_tiff("Outputs/UKPayxIndustry.tiff", units="in", width=10, height=7, res=800)
ggplot(ind_pay %>% filter(industry!="UK"), aes(y=fct_reorder(industry, `May 2019`)))+
  geom_segment(aes(x=`May 2019`, xend=`May 2020`, yend=industry), colour="Grey70")+
  geom_segment(aes(x=`May 2021`, xend=`May 2020`, yend=industry), colour="Grey70")+
  geom_point(aes(x=`May 2019`), colour="#c7e94b", size=rel(3))+
  geom_point(aes(x=`May 2020`), colour="#41b6c4", size=rel(3))+
  geom_point(aes(x=`May 2021`), colour="#253494", size=rel(3))+
  geom_text(aes(x=max, label=labvalue), size=rel(2.5), hjust=-0.5, colour="Grey30")+
  scale_x_continuous(name="Median monthly wage (seasonally adjusted)", labels=dollar_format(prefix="£"), limits=c(0,4200))+
  scale_y_discrete(name="Industry")+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="Many sectors saw wages fall at the start of the pandemic, but these have now recovered",
       subtitle="Median wages of employees paid through PAYE by sector in May <span style='color:#c7e94b;'>2019</span>, <span style='color:#41b6c4;'>2020</span> and <span style='color:#253494;'>2021</span>.<br>Figures represent the overall change between May 2019 and May 2021.",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

plot3data <- merge(ind_employ %>% mutate(emp_abs=`May 2021`- `May 2019`,
                                         emp_rel=emp_abs/`May 2019`) %>% 
                     select(industry, emp_abs, emp_rel), 
                   ind_pay %>% mutate(pay_abs=`May 2021`- `May 2019`,
                                      pay_rel=pay_abs/`May 2019`) %>% 
                     select(industry, pay_abs, pay_rel))

ggplot(plot3data, aes(x=emp_rel, y=pay_rel))+
  geom_point(shape=21)+
  theme_custom()

#By age
ageemp <- read_excel(temp, sheet="28. Employees (Age)", range="A6:G92") %>% 
  mutate(month=seq.Date(from=as.Date("2014-07-01"), to=as.Date("2021-08-01"), by="months")) %>% 
  gather(age, employees, c(2:7)) %>% 
  group_by(age) %>% 
  mutate(ref=employees[Date=="January 2020"],
         emp_ind=employees/ref)
  
agg_tiff("Outputs/UKEmploymentxAge.tiff", units="in", width=9, height=6, res=800)
ggplot(ageemp, aes(x=month, y=emp_ind*100, colour=age, group=age))+
  geom_segment(x=as.Date("2018-01-01"), xend=as.Date("2021-08-01"), y=100, yend=100, colour="Grey80")+
  geom_line()+
  scale_x_date(limits=c(as.Date("2018-01-01"), NA), name="")+
  scale_y_continuous(name="Total number of employees\n(indexed to January 2020)")+
  scale_colour_paletteer_d("awtools::a_palette", name="Age")+
  theme_custom()+
  theme(legend.position="top")+
  guides(colour=guide_legend(nrow=1))+
  labs(title="COVID has disproportionately impacted the employment prospects of the young",
       subtitle="Total number of employees paid through PAYE by age group, indexed to January 2020 levels. Seasonally adjusted.",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()  

agepay <- read_excel(temp, sheet="29. Median pay (Age)", range="A6:G92") %>% 
  mutate(month=seq.Date(from=as.Date("2014-07-01"), to=as.Date("2021-08-01"), by="months")) %>% 
  gather(age, pay, c(2:7)) %>% 
  group_by(age) %>% 
  mutate(ref=pay[Date=="January 2020"],
         pay_ind=pay/ref)

agg_tiff("Outputs/UKPayxAge.tiff", units="in", width=9, height=6, res=800)
ggplot(agepay, aes(x=month, y=pay_ind*100, colour=age, group=age))+
  geom_segment(x=as.Date("2018-01-01"), xend=as.Date("2021-08-01"), y=100, yend=100, colour="Grey80")+
  geom_line()+
  scale_x_date(limits=c(as.Date("2018-01-01"), NA), name="")+
  scale_y_continuous(name="Median pay\n(indexed to January 2020)")+
  scale_colour_paletteer_d("awtools::a_palette", name="Age")+
  theme_custom()+
  theme(legend.position="top")+
  guides(colour=guide_legend(nrow=1))+
  labs(title="18-24 year olds have seen the biggest fall in their pay during the pandemic",
       subtitle="Median monthly pay of employees paid through PAYE by age group, indexed to January 2020 levels. Seasonally adjusted.",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()  
  