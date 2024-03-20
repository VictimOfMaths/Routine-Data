rm(list=ls())

library(tidyverse)
library(readxl)
library(lubridate)
library(paletteer)
library(curl)
library(ragg)
library(ggtext)
library(ggrepel)
library(scales)
library(extrafont)

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
          legend.title=element_text(colour="Grey20"))
}

#Download ONS CPI data
url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindices/current/mm23.csv"
temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

CPIdata <-read.csv(temp) %>% 
  set_names(slice(., 1)) %>% 
  slice_tail(., n=nrow(.)-6) %>% 
  #Select indices we want
  select(CDID, D7BT, D7D5:D7FR) %>% 
  set_names("date", "Overall", "Bread & Cereals", "Meat", "Fish", 
            "Milk, Cheese & Eggs", "Oils & Fats", "Fruit", "Vegetables", "Sugar & Sweets",
            "All Food", "Tea & Coffee", "Soft Drinks", "Spirits", "Wine", "Beer", "Clothes",
            "Clothing Accessories", "Cleaning Clothes", "DIY Goods", "Maintenance/Repair Services",
            "Sewerage", "Water", "Electricity", "Gas", "Liquid Fuels", "Solid Fuels", 
            "Furniture", "Carpets", "White Goods", "Appliance Repair", "Non-Durable Goods",
            "Domestic Services", "New Cars", "Second-Hand Cars", "Bikes/Motorbikes", "Car Spares",
            "Petrol", "Car Repairs", "Other Services", "Train Tickets", "Bus Tickets",
            "Plane Tickets", "Ferry Tickets", "Phones", "TV License", "Cameras", "Computers",
            "Health Insurance", "TV Repair", "Recording Media", "Garden Plants", "Pets",
            "Restaurants", "Canteens", "Hairdressing", "Appliances for Personal Care",
            "Contents Insurance", "Car Insurance", "Overall Goods", "Overall Services",
            "Medical Products", "Drugs", "Other Medical Equipment", "Out-Patient Services", 
            "Medical Services", "Dentistry", "Hospital Services", "Major Recreation Durables",
            "Games, Toys & Hobbies", "Camping Equipment", "Sporting Services", "Cultural Services",
            "Books, Newspapers & Stationary", "Books", "Newspapers", "Stationary",
            "Package Holidays", "Jewellery & Other Personal Effects", "Jewellery", "Other Personal Effects", 
            "Other General Services") %>% 
  filter(nchar(date)>7) %>% 
  mutate(date=as.Date(paste(substr(date, 1, 4), substr(date, 6, 8), "01", 
                            sep="-"), "%Y-%b-%d")) %>% 
  filter(date>=as.Date("1988-01-01")) %>% 
  gather(Product, CPI, c(2:ncol(.))) %>% 
  mutate(CPI=as.numeric(CPI)) %>% 
  #Rebase to Jan 2021
  group_by(Product) %>% 
  mutate(CPI_2021=CPI/CPI[date==as.Date("2021-01-01")]) %>% 
  ungroup()

agg_png("Outputs/CPIInflationxItem.png", units="in", width=9, height=7, res=800)
ggplot(CPIdata %>% filter(date>=as.Date("2021-01-01") &
                            Product %in% c("Bread & Cereals", "Meat", "Fish", "Overall",
                                           "Milk, Cheese & Eggs", "Oils & Fats", "Fruit", 
                                           "Vegetables", "Sugar & Sweets","Tea & Coffee", 
                                           "Soft Drinks", "Spirits", "Wine", "Beer")),
       aes(x=date, y=CPI_2021, colour=Product))+
  geom_hline(yintercept=1, colour="grey30")+
  geom_line(data=. %>% filter(Product!="Overall"), show.legend=FALSE)+
  geom_line(data=. %>% filter(Product=="Overall"), colour="black", linetype=2)+
  geom_text_repel(data=. %>% filter(date==max(date) & Product!="Overall"),
                  aes(label = Product), size=3,
                  family = "Lato", fontface = "bold", direction = "y", box.padding = 0.3, hjust=0,
                  xlim = c(as.Date("2024-02-10"), NA_Date_), show.legend=FALSE, segment.color = NA)+
  scale_x_date(name="", limits=c(NA_Date_, as.Date("2024-09-01")))+
  scale_y_continuous(trans="log", name="CPI inflation since January 2021", 
                     breaks=c(1, 1.2, 1.4), labels=c("0%", "+20%", "+40%"))+
  scale_colour_manual(values=c("#EF7C12","#FC6882","#54BCD1", "#F4B95A", "#009F3F", "#8FDA04", 
                               "tomato","#AF6125", "#007BC3","#B25D91", "#EFC7E6", "#F4E3C7", "#C70E7B"))+
  theme_custom()+
  labs(title="Prices of alcohol have risen much slower than other food and drink items",
       subtitle="UK CPI inflation for food and drink items since January 2021. The dashed black line represents overall inflation",
       caption="Data from ONS | Plot by @VictimOfMaths")+
  theme(panel.grid.major.y=element_line(colour="grey95"),
        axis.line.x=element_blank())
dev.off()


