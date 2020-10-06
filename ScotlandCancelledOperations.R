rm(list=ls())

library(tidyverse)
library(curl)
library(paletteer)

temp <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/479848ef-41f8-44c5-bfb5-666e0df8f574/resource/df65826d-0017-455b-b312-828e47df325b/download/cancellations_scotland_august_2020.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data <- read.csv(temp)[,c(1,3,5,7,9,11,13)]

data$OperationWentAhead <- data$TotalOperations-data$TotalCancelled

data_long <- gather(data, status, count, c(4:8))[,c(1,4,5)]

data_long$date <- as.Date(paste0(as.character(data_long$Month),"01"), "%Y%m%d")
data_long$status <- case_when(
  data_long$status=="CancelledByPatientReason" ~ "Cancelled by patient",
  data_long$status=="ClinicalReason" ~ "Cancelled for clinical reason",
  data_long$status=="NonClinicalCapacityReason" ~ "Cancelled for capacity/non-clinical reason",
  data_long$status=="OperationWentAhead" ~ "Operation went ahead",
  TRUE ~ "Cancelled for other reason"
)

data_long$status <- factor(data_long$status, levels=c("Operation went ahead",
                                                      "Cancelled by patient",
                                                      "Cancelled for clinical reason",
                                                      "Cancelled for capacity/non-clinical reason",
                                                      "Cancelled for other reason"))

tiff("Outputs/ScotlandOperationCancellations.tiff", units="in", width=10, height=6, res=500)
ggplot(subset(data_long, date>as.Date("2019-05-01")))+
  geom_area(aes(x=date, y=count, fill=status))+
  scale_x_date(name="Scheduled operation date")+
  scale_y_continuous(name="Number of operations")+
  scale_fill_paletteer_d("fishualize::Scarus_quoyi",name="")+
  theme_classic()+
  labs(title="Planned operations in Scotland are still well below usual levels",
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()
