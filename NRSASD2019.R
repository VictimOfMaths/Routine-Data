rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(ggtext)
library(ggdraw)
library(cowplot)
library(geofacet)
library(paletteer)

#Download Alcohol-specific deaths data from NRS
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/alcohol-deaths/2019/alcohol-specific-deaths-19-all-tabs.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#Read in headline figures
main <- read_excel(temp, sheet=2, range="A10:F50", col_names=FALSE)[,-c(3,4)]
colnames(main) <- c("Year", "All", "Male", "Female")

main_long <- gather(main, Sex, deaths, c(2:4)) %>% 
  group_by(Sex) %>% 
  mutate(change=deaths-lag(deaths, 1, order_by=Year),
         percchange=change/lag(deaths, 1, order_by=Year),
         percchange.lab=case_when(
           percchange>0 ~ paste0("+", round(percchange*100,1),"%"),
           percchange<0 ~ paste0(round(percchange*100,1),"%"),
           TRUE ~ "0%"
         ),
         vjust=if_else(percchange<0,1.4,-0.8))

#Plot overall trend
tiff("Outputs/ASDNRS2019_Overall.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(main_long, Sex=="All" & Year>=1990), aes(x=Year, y=deaths))+
  geom_line(colour="#00b0f0")+
  scale_y_continuous(limits=c(0,NA), name="Alcohol-specific deaths")+
  geom_text(aes(label=percchange.lab, vjust=vjust), size=rel(2))+
  theme_classic()+
  labs(title="Alcohol-specific deaths in Scotland fell in 2019",
       subtitle="Annual deaths from alcohol-specific causes and the annual % change",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")
dev.off()

#Plot trend by sex
tiff("Outputs/ASDNRS2019_Sex.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(main_long, Sex!="All" & Year>1990), aes(x=Year, y=deaths))+
  geom_line(aes(colour=Sex), show.legend=FALSE)+
  scale_colour_manual(values=c("#00cc99", "#6600cc"))+
  scale_y_continuous(limits=c(0,NA), name="Alcohol-specific deaths")+
  theme_classic()+
  theme(plot.subtitle=element_markdown())+
  labs(title="The sharpest fall in deaths came among men",
       subtitle="Annual deaths from alcohol-specific causes in Scotland for <span style='color:#6600cc;'>men</span> and <span style='color:#00cc99;'>women",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")
dev.off()

#Read in data by cause
cause <- as.data.frame(t(read_excel(temp, sheet=4, range=c("B3:U19"), col_names=FALSE))) %>% 
  mutate(Other=V3+V5+V6+V7+V8+V9+V11+V12+V13+V14+V15+V16+V17) %>% 
  select(c(1,4,10,18)) %>%
  rename(Year=V1, `Mental & behavioural disorders`=V4, `Alcoholic liver disease`=V10) %>% 
  gather(cause, deaths, c(2:4))

tiff("Outputs/ASDNRS2019_Cause.tiff", units="in", width=8, height=6, res=500)
ggplot(cause)+
  geom_line(aes(x=Year, y=deaths, colour=cause))+
  scale_colour_manual(values=c("#d55e00", "#e69f00", "#56b4e9"), name="Cause")+
  scale_y_continuous(limits=c(0,NA), name="Alcohol-specific deaths")+
  theme_classic()+
  labs(title="Alcoholic liver disease deaths have fallen 10.6% since 2017",
       subtitle="Alcohol-specific deaths in Scotland by cause",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")
dev.off()

#Read in data by age
age <- read_excel(temp, sheet=6, range=c("A8:U48"), col_names=FALSE) %>% 
  select(-c(2))

colnames(age) <- c("Year", "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
                   "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
                   "70-74", "75-79", "80-84", "85-89", "90+")

age <- gather(age, ageband, deaths, c(2:20))

#Generate groups path plot
#Group ages
age_grouped <- age %>% 
  mutate(ageband2=case_when(
    ageband %in% c("0-4", "5-9", "10-14", "15-19", "20-24") ~ "Under 25",
    ageband %in% c("25-29", "30-34") ~ "25-34",
    ageband %in% c("35-39", "40-44") ~ "35-44",
    ageband %in% c("45-49", "50-54") ~ "45-54",
    ageband %in% c("55-59", "60-64") ~ "55-64",
    ageband %in% c("65-69", "70-74") ~ "65-74",
    TRUE ~ "Over 75")) %>% 
  group_by(Year, ageband2) %>% 
  summarise(deaths=sum(deaths)) %>% 
  filter(Year>=2004) %>% 
  mutate(ageband2=factor(ageband2, levels=c("Under 25", "25-34", "35-44", "45-54",
                                            "55-64", "65-74", "75+"))) %>% 
  arrange(ageband2, Year)

#Define groupedpath function
groupedpath <- function(data, group, time, outcome, fill, xlabel, ylabel, title, caption){
  #data = the data frame with the original data in to graph
  #group = the grouping variable (e.g. age group) in data, must be an ordered factor in quote marks
  ########################################################
  #NB the data frame needs to be ordered on this variable
  #will try and remove this requirement
  ########################################################
  #time = the time variable in data, must be continuous in quote marks
  #outcome = the y-axis variable, must be continuous in quote marks
  #fill = the colour to fill the polygons
  #xlabel = the x-axis label for the graph (in quote marks)
  #ylabel = the y-axis label for the graph (in quote marks)
  #title = the plot title, use empty quotes if not required
  #caption = the plot caption, use empty quotes if not required
  
  require(ggplot2, cowplot)
  
  #size data to scale polygons
  #This next line doesn't seem to preserve the factor ordering - to fix
  group_names <- unique(data[[group]])
  times <- unique(data[[time]])
  n_groups <- length(group_names)
  n_time <- length(times)
  
  #sort the data and add index variable for plotting
  data <- data[order(data[[group]], data[[time]]),]
  data$index <- c(1:(n_groups*n_time))
  
  #set up y-axis values for each age group
  for (i in 1:n_groups) {
    name <- paste0("x",i)
    assign(name, 0)
    
    for (j in 1:n_time) {
      assign(name, rbind(eval(as.symbol(name)), data[[outcome]][data[[group]]==group_names[i] & data[[time]]==times[j]]))
      
    }
    assign(name, rbind(eval(as.symbol(name)), 0))
  }
  
  #generate plot - start with bottom group
  plot <- ggplot()+
    geom_polygon(aes(x=c(1,1:n_time,n_time), y=x1), fill=fill)
  
  #loop over remaining groups, adding them to the graph
  for (i in 2:n_groups) {
    #have to specify it this way as defining the data in the aes call only plots the last group
    loopdata <- data.frame(x=c(1+n_time*(i-1), (1+n_time*(i-1)):(n_time*i), n_time*i), y=eval(as.symbol(paste0("x",i))))
    
    plot <- plot+
      geom_polygon(data=loopdata, aes(x,y), fill=fill)
  }
  
  #add path to highlight trends and tidy up graph
  plot <- plot+
    geom_path(data=data,aes(x=index, y=data[[outcome]], group=data[[group]]), arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
    theme_classic()+
    scale_x_continuous(name=xlabel, breaks=seq(n_time/2, n_groups*n_time-n_time/2, by=n_time), labels=group_names)+
    #rescale y-axis to ensure that inset key fits
    scale_y_continuous(name=ylabel, limits=c(0,max(data[[outcome]]*1.4)))+
    labs(title=title, caption=caption)
  
  #generate inset graph
  insetvalues <- runif(n_time, 0.5,1)
  insetvalues2 <- c(0,insetvalues,0)
  
  inset <- ggplot()+
    geom_polygon(aes(x=c(1,1:n_time,n_time), y=insetvalues2), fill=fill)+
    geom_line(aes(x=c(1:n_time), y=insetvalues), arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
    theme_classic()+
    #Remove all clutter
    theme(axis.line=element_blank(), axis.text=element_blank(),axis.ticks=element_blank(),
          axis.title=element_blank())
  
  
  finalplot <- ggdraw()+
    draw_plot(plot)+
    draw_plot(inset, x=0.85, y=0.75, width=0.1, height=0.2)+
    draw_label(min(times), x=0.87, y=0.76, size=10)+
    draw_label(max(times), x=0.94, y=0.76, size=10)+
    draw_label("Key", x=0.88, y=0.95, size=10)
  
  return(finalplot)
}
    
tiff("Outputs/ASDNRS2019_Age.tiff", units="in", width=8, height=6, res=500)
groupedpath(data=age_grouped, group="ageband2", time="Year", outcome="deaths",
            fill="#87ceeb", xlabel="Age", ylabel="Annual alcohol-specific deaths",
            title="Age patterns in alcohol-specific deaths in Scotland over the last 15 years", 
            caption="Data from National Records of Scotland | Plot by @VictimOfMaths")    
dev.off()
    
    
#Bring in population for rates
#Download shapefile of LSOA boundaries
temp1 <- tempfile()
temp2 <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2001tomid2019detailedtimeseries/ukdetailedtimeseries20012019.zip"
temp1 <- curl_download(url=source, destfile=temp1, quiet=FALSE, mode="wb")
unzip(zipfile=temp1, exdir=temp2)

pop <- read_csv(file.path(temp2, "MYEB1_detailed_population_estimates_series_UK_(2019).csv")) %>% 
  filter(country=="S") %>% 
  gather(Year, pop, c(6:24)) %>% 
  mutate(Year=as.numeric(substr(Year, 12,15))) %>% 
  group_by(laname19, age, Year) %>% 
  summarise(pop=sum(pop))

#Calculate overall LA populations
LApop <- pop %>% 
  group_by(Year, laname19) %>% 
  summarise(pop=sum(pop))

#Read in LA level data
LAdeaths <- read_excel(temp, sheet=10, range=c("A6:AH46"), col_names=FALSE)[,-c(2)]
colnames(LAdeaths) <- c("Year", unique(LApop$laname19))

LAdeaths2 <- gather(LAdeaths, laname19, deaths, c(2:33)) %>% 
  filter(Year>=2001) %>% 
  merge(LApop, by=c("laname19", "Year"), all=TRUE) %>% 
  mutate(deaths=as.numeric(deaths), mortrate=deaths*100000/pop,
         laname19=case_when(
           laname19=="Na h-Eileanan Siar" ~ "Eilean Siar",
           laname19=="Argyll and Bute" ~ "Argyll & Bute",
           laname19=="Perth and Kinross" ~ "Perth & Kinross",
           laname19=="Dumfries and Galloway" ~ "Dumfries & Galloway",
           laname19=="City of Edinburgh" ~ "Edinburgh",
           TRUE ~ laname19)) %>% 
  group_by(laname19) %>% 
  mutate(change2019=(deaths[Year==2019]-deaths[Year==2018])/deaths[Year==2018])

tiff("Outputs/ASDNRS2019_LA.tiff", units="in", width=11, height=13, res=500)
ggplot(LAdeaths2)+
  geom_line(aes(x=Year, y=mortrate), colour="#00cc99")+
  #geom_line(aes(x=Year, y=mortrate, colour=change2019))+
  #scale_colour_paletteer_c("scico::roma", limit=c(-1,1)*max(abs(LAdeaths2$change2019)), 
  #                       name="Change between\n2018 and 2019", direction=-1, 
  #                       breaks=c(-0.5,0,0.5), labels=c("-50%", "No change", "+50%"))+
  scale_y_continuous(name="Alcohol-specific deaths per 100,000", limits=c(0,NA))+
  facet_geo(~laname19, grid="scotland_local_authority_grid1")+
  theme_classic()+
  theme(strip.text=element_text(face="bold"), strip.background=element_blank(),
        plot.title=element_text(face="bold"))+
  labs(title="There are substantial local variations in the level and trends in alcohol-specific deaths",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")
dev.off()

#age standardise
#Collapse population to national level
ASMR <- pop %>% 
  group_by(age, Year) %>% 
  summarise(pop=sum(pop)) %>% 
  #merge agebands to ASD data
  mutate(ageband=case_when(
    age<5 ~ "0-4", age<10 ~ "5-9", age<15 ~ "10-14", age<20 ~ "15-19",
    age<25 ~ "20-24", age<30 ~ "25-29",  age<35 ~ "30-34", age<40 ~ "35-39",
    age<45 ~ "40-44", age<50 ~ "45-49", age<55 ~ "50-54", age<60 ~ "55-59",
    age<65 ~ "60-64", age<70 ~ "65-69", age<75 ~ "70-74", age<80 ~ "75-79",
    age<85 ~ "80-84", age<90 ~ "85-89",
    TRUE ~ "90+")) %>% 
  group_by(Year, ageband) %>% 
  summarise(pop=sum(pop)) %>% 
  #merge into age-specific deaths data
  merge(age) %>% 
  mutate(mortrate=deaths*100000/pop) %>% 
  #bring in ESP %>% 
  mutate(ESP=case_when(
    ageband %in% c("0-4", "70-74") ~ 5000,
    ageband %in% c("5-9", "10-14", "15-19", "65-69") ~ 5500,
    ageband %in% c("20-24", "25-29", "60-64") ~ 6000,
    ageband %in% c("30-34", "55-59") ~ 6500,
    ageband %in% c("35-39", "40-44", "45-49", "50-54") ~ 7000,
    ageband=="75-79" ~ 4000,
    ageband=="80-84" ~ 2500,
    ageband=="85-89" ~ 1500,
    ageband=="90+" ~ 1000)) %>% 
  #Calculate ASMR
  group_by(Year) %>% 
  summarise(ASMR=weighted.mean(mortrate, ESP)) %>% 
  mutate(change=ASMR-lag(ASMR, 1, order_by=Year),
         changeperc=change/lag(ASMR, 1, order_by=Year),
         changeperc.lab=case_when(
           Year==2001 ~ "",
           changeperc>0 ~ paste0("+", round(changeperc*100,1),"%"),
           changeperc<0 ~ paste0(round(changeperc*100,1),"%"),
           TRUE ~ "0%"
         ),
         vjust=if_else(changeperc<0,1.4,-0.8))

tiff("Outputs/ASDNRS2019_ASMR.tiff", units="in", width=8, height=6, res=500)
ggplot(ASMR, aes(x=Year, y=ASMR))+
  geom_line(colour="#00b0f0")+
  scale_y_continuous(limits=c(0,NA), name="Age-standardised deaths per 100,000")+
  geom_text(aes(label=changeperc.lab, vjust=vjust), size=rel(2.5))+
  theme_classic()+
  labs(title="Alcohol-specific deaths in Scotland fell in 2019",
       subtitle="Annual deaths from alcohol-specific causes and the annual % change",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")
dev.off()
