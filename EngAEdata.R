rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(paletteer)
library(lubridate)
library(ggtext)

#Download A&E data https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/ae-attendances-and-emergency-admissions-2020-21/
temp <- tempfile()
source <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/10/Adjusted-Monthly-AE-Time-Series-September-2020.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#Read in and tidy
rawdata1 <- read_excel(temp, sheet="Activity", range="C19:M140")
colnames(rawdata1) <- c("Period", "Attendance_Major", "Attendance_Specialty", "Attendance_Other", 
                        "Attendance_Total", "Admission_Major", "Admission_Specialty", "Admission_Other",
                        "Admission_Total", "Admission_NonAE", "Admissions_Overall")
rawdata2 <- read_excel(temp, sheet="Performance", range="B19:N137")[,c(1,10:13)]
colnames(rawdata2) <- c("Period", "Total", "Major", "Specialty", "Other")

#Convert to long
rawdata1_long <- pivot_longer(rawdata1, c(2:11), names_to=c("Measure", "Location"),
                              names_sep="_", values_to="value")

rawdata2_long <- pivot_longer(rawdata2, c(2:5), names_to="Location", values_to="value")
rawdata2_long$Measure="Target"

data <- bind_rows(rawdata1_long, rawdata2_long)

data$year <- year(data$Period)
data$month <- month(data$Period)

#Plot attendance
tiff("Outputs/AandEAttendance.tiff", units="in", width=9, height=6, res=500)
ggplot()+
  geom_line(data=subset(data, Measure=="Attendance" & Location=="Total" & year>2010 & year<2020),
            aes(x=month, y=value, colour=as.factor(year)), show.legend=FALSE)+
  geom_line(data=subset(data, Measure=="Attendance" & Location=="Total" & year==2020),
            aes(x=month, y=value), colour="Red")+
  scale_x_continuous(breaks=c(1:12), labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                              "Aug", "Sep", "Oct", "Nov", "Dec"), name="")+
  scale_y_continuous(limits=c(0,NA), name="Monthly A&E attendances")+
  scale_colour_paletteer_d("ggsci::blue_material")+
  theme_classic()+
  labs(title="A&E attendance in England is still below normal levels",
       subtitle="Total number of individual A&E presentations in <span style='color:red;'>2020</span> compared to <span style='color:#42A5F5;'>2011-19 </span>(more recent years are darker blue)",
       caption="Data from NHS England | Plot by @VictimOfMaths")+
  theme(plot.subtitle=element_markdown())
dev.off()

#Plot admissions
tiff("Outputs/AandEAdmissions.tiff", units="in", width=10, height=6, res=500)
ggplot()+
  geom_line(data=subset(data, Measure=="Admission" & Location=="Total" & year>2010 & year<2020),
            aes(x=month, y=value, colour=as.factor(year)), show.legend=FALSE)+
  geom_line(data=subset(data, Measure=="Admission" & Location=="Total" & year==2020),
            aes(x=month, y=value), colour="Red")+
  scale_x_continuous(breaks=c(1:12), labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                              "Aug", "Sep", "Oct", "Nov", "Dec"), name="")+
  scale_y_continuous(limits=c(0,NA), name="Monthly emergency admissions from A&E")+
  scale_colour_paletteer_d("ggsci::blue_material")+
  theme_classic()+
  labs(title="Emergency hospital admissions from A&E are slightly below normal levels",
       subtitle="Total number of emergency inpatient admissions via A&E in England in <span style='color:red;'>2020</span> compared to <span style='color:#42A5F5;'>2011-19 </span>(more recent years are darker blue)",
       caption="Data from NHS England | Plot by @VictimOfMaths")+
  theme(plot.subtitle=element_markdown())
dev.off()

#Plot % attendances converted to admissions
tiff("Outputs/AandEAdmissionsProp.tiff", units="in", width=10, height=6, res=500)
data %>% 
  filter(Location=="Total" & Measure!="Target" & year>2010) %>% 
  spread(Measure, value) %>% 
  mutate(admprop=Admission/Attendance) %>% 
  ggplot()+
  geom_line(aes(x=month, y=admprop, colour=as.factor(year)), show.legend=FALSE)+
  scale_x_continuous(breaks=c(1:12), labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                              "Aug", "Sep", "Oct", "Nov", "Dec"), name="")+
  scale_y_continuous(limits=c(0,NA), name="A&E attendances leading to emergency admission",
                     labels = scales::percent_format(accuracy = 1))+
  scale_colour_manual(values=c(palettes_d$ggsci$blue_material[1:9], "Red"))+
  theme_classic()+
  labs(title="People attending A&E in England are more likely to be admitted to hospital",
       subtitle="Proportion of A&E attendances which result in an emergency admission in <span style='color:red;'>2020</span> compared to <span style='color:#42A5F5;'>2011-19 </span>(more recent years are darker blue)",
       caption="Data from NHS England | Plot by @VictimOfMaths")+
  theme(plot.subtitle=element_markdown())
dev.off()

#Plot % hitting 4 hour target
tiff("Outputs/AandE4hrTarget.tiff", units="in", width=10, height=6, res=500)
ggplot()+
  geom_line(data=subset(data, Measure=="Target" & Location=="Total" & year>2010 & year<2020),
            aes(x=month, y=value, colour=as.factor(year)), show.legend=FALSE)+
  geom_line(data=subset(data, Measure=="Target" & Location=="Total" & year==2020),
            aes(x=month, y=value), colour="Red")+
  scale_x_continuous(breaks=c(1:12), labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                              "Aug", "Sep", "Oct", "Nov", "Dec"), name="")+
  scale_y_continuous(limits=c(0,1), name="Proportion of patients seen within 4 hours",
                     labels = scales::percent_format(accuracy = 1))+
  scale_colour_paletteer_d("ggsci::blue_material")+
  theme_classic()+
  labs(title="Patients in A&E in England are being seen quicker",
       subtitle="Proportion of A&E attendances being admitted, transferred or discharges within 4 hours<br>in <span style='color:red;'>2020</span> compared to <span style='color:#42A5F5;'>2011-19 </span>(more recent years are darker blue)",
       caption="Data from NHS England | Plot by @VictimOfMaths")+
  theme(plot.subtitle=element_markdown())
dev.off()

