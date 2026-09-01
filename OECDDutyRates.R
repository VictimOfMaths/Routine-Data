rm(list=ls())

options(scipen=99999)

library(tidyverse)
library(ragg)
library(extrafont)
library(paletteer)
library(scales)

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
          panel.grid.major.y=element_line(colour="grey95"))
}

#Generate dataset of alcohol duty rates for ever OECD country with data
#OCED countries: https://www.oecd.org/en/about/members-partners.html

#Define common unit size (in ml of alcohol)
unit <- 10

#Define Plato -> ABV conversion
Plato <- 0.4
PlatoUnit <- unit/Plato

#All rates calculated in domestic currencies for conversion later

#Australia 
#Data from https://www.ato.gov.au/businesses-and-organisations/gst-excise-and-indirect-taxes/excise-on-alcohol/excise-duty-rates-for-alcohol
#Wine is taxed on a purely ad-valorem basis https://www.ato.gov.au/businesses-and-organisations/gst-excise-and-indirect-taxes/wine-equalisation-tax/how-much-to-pay/retail-sales-and-own-use
Australia <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=1.15 ~ 0,
    ABV<=3 ~ 55.83/(1000/unit),
    ABV<=3.5 ~ 65.03/(1000/unit),
    ABV<=15 ~ 65.03/(1000/unit),
    TRUE ~ NA_real_),
    Spirits=case_when(
      ABV<=1.15 ~ 0,
      ABV<=10 ~ 110.15/(1000/unit),
      TRUE ~ 110.15/(1000/unit)),
    Country="Australia")

#Austria
#Data from https://ec.europa.eu/taxation_customs/tedb/#/tax-details?taxId=4045&versionDate=1782856800&isEuro=true&taxType=EDU_ALCOHOL
Austria <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=0.5 ~ 0,
    ABV<=15 ~ 2*PlatoUnit/1000,
    TRUE ~ NA_real_),
    Wine=if_else(ABV<=22, 0, NA_real_),
    Spirits=case_when(
      ABV<=1.2 ~ 0,
      TRUE ~ 1200/(100*1000/unit)),
    Country="Austria")
  

#Belgium
#Data from https://ec.europa.eu/taxation_customs/tedb/#/tax-details?taxId=4205&versionDate=1782856800&isEuro=true&taxType=EDU_ALCOHOL
Belgium <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=0.5 ~ 0,
    ABV<=15 ~ 2.0043*PlatoUnit/1000,
    TRUE ~ NA_real_),
    Wine=case_when(
      ABV<=1.2 ~ 0,
      ABV<=8.5 ~ 23.9119/(1000*ABV/unit),
      ABV<=22 ~ 74.9086/(1000*ABV/unit),
      TRUE ~ NA_real_),
    Spirits=case_when(
      ABV<=1.2 ~ 0,
      TRUE ~ 2992.7928/(100*1000/unit)),
    Country="Belgium")

#Canada
#Data from https://www.canada.ca/en/revenue-agency/services/tax/technical-information/excise-duty/rates.html#Regular_rates_of_excise_duty
Canada <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=0.5 ~ 0,
    ABV<=1.2 ~ 3.128/(ABV*1000/unit),
    ABV<=2.5 ~ 18.85/(ABV*1000/unit),
    ABV<=15 ~ 37.69/(ABV*1000/unit),
    TRUE ~ NA_real_),
    Wine=case_when(
      ABV<=0.5 ~ 0,
      ABV<=1.2 ~ 0.022*unit/(10*ABV),
      ABV<=7 ~ 0.358*unit/(10*ABV),
      ABV<=22 ~ 0.745*unit/(10*ABV),
      TRUE ~ NA_real_),
    Spirits=case_when(
      ABV<=0.5 ~ 0,
      ABV<=7 ~ 0.358/(10*ABV/unit),
      TRUE ~ 14.117/(1000/unit)),
    Country="Canada")

#Chile
#All alcohol taxed on an ad valorem basis, although with higher rates for spirits
#https://doi.org/10.1111/add.15208

#Colombia
#Can't source any data

#Costa Rica
#All alcohol taxed on an ad valorem basis, although with higher rates for spirits
#https://www.ttb.gov/itd/international-affairs-resources-for-costa-rica#TARIFFS

#Czechia
#Data from https://ec.europa.eu/taxation_customs/tedb/#/tax-details?taxId=4057&versionDate=1782856800&isEuro=false&taxType=EDU_ALCOHOL
Czechia <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=0.5 ~ 0,
    ABV<=15 ~ 32*PlatoUnit/1000,
    TRUE ~ NA_real_),
    Wine=if_else(ABV<=22, 0, NA_real_),
    Spirits=case_when(
      ABV<=1.2 ~ 0,
      TRUE ~ 41050/(100*1000/unit)),
    Country="Czechia")

#Denmark
#Data from https://ec.europa.eu/taxation_customs/tedb/#/tax-details?taxId=4065&versionDate=1782856800&isEuro=false&taxType=EDU_ALCOHOL
Denmark <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=2.8 ~ 0,
    ABV<=15 ~ 48.74/(1000/unit),
    TRUE ~ NA_real_),
    Wine=case_when(
      ABV<=1.2 ~ 0,
      ABV<=6 ~ 518/(1000*ABV/unit),
      ABV<=15 ~ 1126/(1000*ABV/unit),
      ABV<=22 ~ 1508/(1000*ABV/unit),
      TRUE ~ NA_real_),
    Spirits=case_when(
      ABV<=1.2 ~ 0,
      TRUE ~ 15000/(100*1000/unit)),
    Country="Denmark")

#Estonia
#Data from https://ec.europa.eu/taxation_customs/tedb/#/tax-details?taxId=4068&versionDate=1782856800&isEuro=true&taxType=EDU_ALCOHOL
Estonia <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=0.5 ~ 0,
    ABV<=15 ~ 15.4/(1000/unit),
    TRUE ~ NA_real_),
    Wine=case_when(
      ABV<=1.2 ~ 0,
      ABV<=6 ~ 76.82/(1000*ABV/unit),
      ABV<=22 ~ 179.27/(1000*ABV/unit),
      TRUE ~ NA_real_),
    Spirits=case_when(
      ABV<=1.2 ~ 0,
      TRUE ~ 2281/(100*1000/unit)),
    Country="Estonia")

#Finland
#Data from https://ec.europa.eu/taxation_customs/tedb/#/tax-details?taxId=4076&versionDate=1782856800&isEuro=true&taxType=EDU_ALCOHOL
Finland <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=0.5 ~ 0,
    ABV<=2.8 ~ 28.75/(1000/unit),
    ABV<=15 ~ 36.71/(1000/unit),
    TRUE ~ NA_real_),
    Wine=case_when(
      ABV<=1.2 ~ 0,
      ABV<=2.8 ~ 36/(1000*ABV/unit),
      ABV<=5.5 ~ 219.02/(1000*ABV/unit),
      ABV<=8.5 ~ 340.7/(1000*ABV/unit),
      ABV<=22 ~ 504.97/(1000*ABV/unit),
      TRUE ~ NA_real_),
    Spirits=case_when(
      ABV<=1.2 ~ 0,
      ABV<=2.8 ~ 3133/(100*1000/unit),
      ABV<=10 ~ 5557/(100*1000/unit),
      TRUE ~ 5628/(100*1000/unit)),
    Country="Finland")

#France
#Data from https://ec.europa.eu/taxation_customs/tedb/#/tax-details?taxId=4079&versionDate=1782856800&isEuro=true&taxType=EDU_ALCOHOL
France <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=0.5 ~ 0,
    ABV<=2.8 ~ 4.12/(1000/unit),
    ABV<=15 ~ 8.24/(1000/unit),
    TRUE ~ NA_real_),
    Wine=case_when(
      ABV<=1.2 ~ 0,
      ABV<=22 ~ 4.19/(1000*ABV/unit),
      TRUE ~ NA_real_),
    Spirits=case_when(
      ABV<=1.2 ~ 0,
      TRUE ~ 1932.42/(100*1000/unit)),
    Country="France")

#Germany
#Data from https://ec.europa.eu/taxation_customs/tedb/#/tax-details?taxId=4061&versionDate=1782856800&isEuro=true&taxType=EDU_ALCOHOL
Germany <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=0.5 ~ 0,
    ABV<=15 ~ 0.787*PlatoUnit/1000,
    TRUE ~ NA_real_),
    Wine=case_when(
      ABV<=22 ~ 0,
      TRUE ~ NA_real_),
    Spirits=case_when(
      ABV<=1.2 ~ 0,
      TRUE ~ 1303/(100*1000/unit)),
    Country="Germany")

#Greece
#Data from https://www.gov.gr/en/sdg/taxes/excise-duties/excise-duties/ethyl-alcohol-and-alcoholic-products
Greece <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=0.5 ~ 0,
    ABV<=15 ~ 5*PlatoUnit/1000,
    TRUE ~ NA_real_),
    Wine=if_else(ABV<=22, 0, NA_real_),
    Spirits=case_when(
      ABV<=1.2 ~ 0,
      TRUE ~ 2450/(100*1000/unit)),
    Country="Greece")

#Hungary
#Data from https://ec.europa.eu/taxation_customs/tedb/#/tax-details?taxId=4085&versionDate=1782856800&isEuro=false&taxType=EDU_ALCOHOL
Hungary <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=0.5 ~ 0,
    ABV<=15 ~ 1950/(1000/unit),
    TRUE ~ NA_real_),
    Wine=case_when(
      ABV<=22 ~ 0,
      TRUE ~ NA_real_),
    Spirits=case_when(
      ABV<=1.2 ~ 0,
      TRUE ~ 614370/(100*1000/unit)),
    Country="Hungary")

#Iceland
#Data from https://www.reddit.com/r/VisitingIceland/comments/1szv4ov/buying_alcohol_in_iceland_what_tourists_should/
#Basis of taxation is confusing, original legislation is here: https://www.althingi.is/lagas/nuna/1995096.html
Iceland <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=2.25 ~ 0,
    ABV<=15 ~ 156.45*(ABV-2.25)/(ABV*10/unit),
    TRUE ~ NA_real_),
    Wine=case_when(
      ABV<=2.25 ~ 0,
      ABV<=15 ~ 142.50*(ABV-2.25)/(ABV*10/unit),
      ABV<=22 ~ 192.85*ABV/(ABV*10/unit),
      TRUE ~ NA_real_),
    Spirits=case_when(
      ABV<=2.25 ~ 0,
      TRUE ~ 192.85*ABV/(ABV*10/unit)),
    Country="Iceland")

#Ireland
#Data from https://ec.europa.eu/taxation_customs/tedb/#/tax-details?taxId=4091&versionDate=1782856800&isEuro=true&taxType=EDU_ALCOHOL
Ireland <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=0.5 ~ 0,
    ABV<=2.8 ~ 11.27/(1000/unit),
    ABV<=15 ~ 22.55/(1000/unit),
    TRUE ~ NA_real_),
    Wine=case_when(
      ABV<=1.2 ~ 0,
      ABV<=5.5 ~ 141.57/(1000*ABV/unit),
      ABV<=15 ~ 424.84/(1000*ABV/unit),
      ABV<=22 ~ 616.45/(1000*ABV/unit),
      TRUE ~ NA_real_),
    Spirits=case_when(
      ABV<=1.2 ~ 0,
      TRUE ~ 4257/(100*1000/unit)),
    Country="Ireland")

#Israel
#Data from this very janky website https://shaarolami-query.customs.mof.gov.il/CustomspilotWeb/en/CustomsBook/Import/ImportCustomsItemDetails?customsItemId=15879&isDiscountCode=False
Israel <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=2 ~ 0,
    ABV<=15 ~ 2.76/(ABV*10/unit)),
    Wine=case_when(
      ABV<=22 ~ 0,
      TRUE ~ NA_real_),
    Spirits=case_when(
      ABV<=2 ~ 0,
      TRUE ~ 100.24/(1000/unit)),
    Country="Israel")

#Italy
#Data from https://ec.europa.eu/taxation_customs/tedb/#/tax-details?taxId=4095&versionDate=1770128073&isEuro=true&taxType=EDU_ALCOHOL
Italy <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=0.5 ~ 0,
    ABV<=15 ~ 2.98*PlatoUnit/1000,
    TRUE ~ NA_real_),
    Wine=if_else(ABV<=22, 0, NA_real_),
    Spirits=case_when(
      ABV<=1.2 ~ 0,
      TRUE ~ 1035.52/(100*1000/unit)),
    Country="Italy")

#Japan
#Data from https://apps.fas.usda.gov/newgainapi/api/Report/DownloadReportByFileName?fileName=Update%20to%20Japan%27s%20Liquor%20Tax_Tokyo%20ATO_Japan_JA2025-0013
#Spirits taxes from https://ebc-jp.com/digital-white-paper/issues/consumer-goods/liquor/liquor-taxes/
Japan <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=0.5 ~ 0,
    ABV<=15 ~ 155/(ABV*10/unit),
    TRUE ~ NA_real_),
    Wine=case_when(
      ABV<=1.2 ~ 0,
      ABV<=22 ~ 100/(ABV*10/unit),
      TRUE ~ NA_real_),
    Spirits=case_when(
      ABV<=1.2 ~ 0,
      ABV<=37 ~ 370/(ABV*10/unit),
      TRUE ~ (370+10*(ABV-37))/(ABV*10/unit)),
    Country="Japan")

#Korea
#https://apps.fas.usda.gov/newgainapi/api/Report/DownloadReportByFileName?fileName=South%20Korea%20Adopts%20Volume-based%20Liquor%20Tax%20for%20Beer_Seoul%20ATO_Korea%20-%20Republic%20of_01-04-2020
#https://elaw.klri.re.kr/eng_service/jomunPrint.do?hseq=3419&cseq=447451
Korea <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=0.5 ~ 0,
    ABV<=15 ~ 885.7/(ABV*10/unit),
    TRUE ~ NA_real_),
    Wine=case_when(
      ABV<=22 ~ 0,
      TRUE ~ NA_real_),
    Spirits=case_when(
      ABV<=1.2 ~ 0,
      TRUE ~ 57/(ABV*10/unit)),
    Country="Korea")

#Latvia
#Data from https://ec.europa.eu/taxation_customs/tedb/#/tax-details?taxId=4098&versionDate=1782856800&isEuro=true&taxType=EDU_ALCOHOL
Latvia <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=0.5 ~ 0,
    ABV<=15 ~ 10.58/(1000/unit),
    TRUE ~ NA_real_),
    Wine=case_when(
      ABV<=1.2 ~ 0,
      ABV<=18 ~ 74/(1000*ABV/unit),
      ABV<=22 ~ 148/(1000*ABV/unit),
      TRUE ~ NA_real_),
    Spirits=case_when(
      ABV<=1.2 ~ 0,
      TRUE ~ 2084/(100*1000/unit)),
    Country="Latvia")

#Lithuania
#Data from https://ec.europa.eu/taxation_customs/tedb/#/tax-details?taxId=4101&versionDate=1782856800&isEuro=true&taxType=EDU_ALCOHOL
Lithuania <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=0.5 ~ 0,
    ABV<=15 ~ 12.74/(1000/unit),
    TRUE ~ NA_real_),
    Wine=case_when(
      ABV<=1.2 ~ 0,
      ABV<=8.5 ~ 148/(1000*ABV/unit),
      ABV<=22 ~ 296/(1000*ABV/unit),
      TRUE ~ NA_real_),
    Spirits=case_when(
      ABV<=1.2 ~ 0,
      TRUE ~ 3130/(100*1000/unit)),
    Country="Lithuania")

#Luxembourg
#Data from https://ec.europa.eu/taxation_customs/tedb/#/tax-details?taxId=4102&versionDate=1782856800&isEuro=true&taxType=EDU_ALCOHOL
Luxembourg <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=0.5 ~ 0,
    ABV<=15 ~ 0.7933*PlatoUnit/1000,
    TRUE ~ NA_real_),
    Wine=case_when(
      ABV<=22 ~ 0,
      TRUE ~ NA_real_),
    Spirits=case_when(
      ABV<=1.2 ~ 0,
      TRUE ~ 1123.1042/(100*1000/unit)),
    Country="Luxembourg")

#Mexico
#Mexico applies only ad-valorem taxation to alcohol
#https://www.fas.usda.gov/data/gain-report/2025/04/Tax%20on%20Imported%20and%20Domestic%20Alcoholic%20Beverages_Guadalajara_Mexico_MX2025-0021.pdf

#Netherlands
#Data from https://ec.europa.eu/taxation_customs/tedb/#/tax-details?taxId=4135&versionDate=1782856800&isEuro=true&taxType=EDU_ALCOHOL
Netherlands <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=0.5 ~ 0,
    ABV<=1.2 ~ 26.13/(1000/unit),
    ABV<=15 ~ 8.12/(1000/unit),
    TRUE ~ NA_real_),
    Wine=case_when(
      ABV<=1.2 ~ 0,
      ABV<=8.5 ~ 47.95/(1000*ABV/unit),
      ABV<=22 ~ 95.69/(1000*ABV/unit),
      TRUE ~ NA_real_),
    Spirits=case_when(
      ABV<=1.2 ~ 0,
      TRUE ~ 1827/(100*1000/unit)),
    Country="Netherlands")

#New Zealand
#Data from https://www.customs.govt.nz/about-us/news/important-notices/new-excise-duty-rates-for-alcohol-from-1-july-2026
NewZealand <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=1.15 ~ 0,
    ABV<=2.5 ~ 0.56747/(ABV*10/unit),
    ABV<=6 ~ 37.836/(1000/unit),
    ABV<=9 ~ 3.0268/(ABV*10/unit),
    ABV<=14 ~ 3.7836/(ABV*10/unit),
    ABV<=15 ~ 68.915/(1000/unit),
    TRUE ~ NA_real_),
    Wine=case_when(
      ABV<=1.15 ~ 0,
      ABV<=2.5 ~ 0.56747/(ABV*10/unit),
      ABV<=6 ~ 37.836/(1000/unit),
      ABV<=9 ~ 3.0268/(ABV*10/unit),
      ABV<=14 ~ 3.7836/(ABV*10/unit),
      ABV<=22 ~ 68.915/(1000/unit),
      TRUE ~ NA_real_),
    Spirits=case_when(
      ABV<=1.15 ~ 0,
      ABV<=2.5 ~ 0.56747/(ABV*10/unit),
      ABV<=6 ~ 37.836/(1000/unit),
      ABV<=9 ~ 3.0268/(ABV*10/unit),
      ABV<=14 ~ 3.7836/(ABV*10/unit),
      ABV<=23 ~ 68.915/(1000/unit),
      TRUE ~ 68.915/(1000/unit)),
    Country="New Zealand")

#Norway
#Data from https://www.skatteetaten.no/en/rates/alcoholic-beverages/
Norway <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=0.7 ~ 0,
    ABV<=2.7 ~ 3.72/(ABV*10/unit),
    ABV<=3.7 ~ 13.98/(ABV*10/unit),
    ABV<=4.7 ~ 24.20/(ABV*10/unit),
    ABV<=15 ~ 5.41/(10/unit),
    TRUE ~ NA_real_),
    Wine=case_when(
      ABV<=0.7 ~ 0,
      ABV<=2.7 ~ 3.72/(ABV*10/unit),
      ABV<=3.7 ~ 13.98/(ABV*10/unit),
      ABV<=4.7 ~ 24.20/(ABV*10/unit),
      ABV<=22 ~ 5.41/(10/unit),
      TRUE ~ NA_real_),
    Spirits=case_when(
      ABV<=0.7 ~ 0,
      TRUE ~ 9.23/(10/unit)),
    Country="Norway")
  
#Poland
#Data from https://ec.europa.eu/taxation_customs/tedb/#/tax-details?taxId=4138&versionDate=1782856800&isEuro=true&taxType=EDU_ALCOHOL
Poland <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=0.5 ~ 0,
    ABV<=15 ~ 11.47*PlatoUnit/1000,
    TRUE ~ NA_real_),
    Wine=case_when(
      ABV<=1.2 ~ 0,
      ABV<=22 ~ 233/(ABV*1000/unit),
      TRUE ~ NA_real_),
    Spirits=case_when(
      ABV<=1.2 ~ 0,
      TRUE ~ 8391/(100*1000/unit)),
    Country="Poland")

#Portugal
#Data from https://ec.europa.eu/taxation_customs/tedb/#/tax-details?taxId=4141&versionDate=1782856800&isEuro=true&taxType=EDU_ALCOHOL
#Notes that EU data gets the basis of taxation wrong - tax is per hl, but in bands defined on degress Plato
#Not per hectolitre per degree Plato as stated - see https://portugalglobal.pt/en/investment/doing-business/taxation/other-indirect-taxes/
Portugal <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=0.5 ~ 0,
    ABV<=3.5 ~ 9.64/(ABV*1000/unit),
    ABV<=4.4 ~ 19.29/(ABV*1000/unit),
    ABV<=5.2 ~ 24.13/(ABV*1000/unit),
    ABV<=6 ~ 28.95/(ABV*1000/unit),
    ABV<=15 ~ 33.85/(ABV*1000/unit),
    TRUE ~ NA_real_),
    Wine=case_when(
      ABV<=22 ~ 0,
      TRUE ~ NA_real_),
    Spirits=case_when(
      ABV<=1.2 ~ 0,
      TRUE ~ 1602.51/(100*1000/unit)),
    Country="Portugal")

#Slovak Republic
#Data from https://ec.europa.eu/taxation_customs/tedb/#/tax-details?taxId=4147&versionDate=1782856800&isEuro=true&taxType=EDU_ALCOHOL
SlovakRepublic <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=0.5 ~ 0,
    ABV<=15 ~ 3.587/(1000/unit),
    TRUE ~ NA_real_),
    Wine=case_when(
      ABV<=22 ~ 0,
      TRUE ~ NA_real_),
    Spirits=case_when(
      ABV<=1.2 ~ 0,
      TRUE ~ 1490.4/(100*1000/unit)),
    Country="Slovak Republic")

#Slovenia
#Data from https://ec.europa.eu/taxation_customs/tedb/#/tax-details?taxId=4150&versionDate=1782856800&isEuro=true&taxType=EDU_ALCOHOL
Slovenia <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=0.5 ~ 0,
    ABV<=15 ~ 12.95/(1000/unit),
    TRUE ~ NA_real_),
    Wine=case_when(
      ABV<=22 ~ 0,
      TRUE ~ NA_real_),
    Spirits=case_when(
      ABV<=1.2 ~ 0,
      TRUE ~ 1412.4/(100*1000/unit)),
    Country="Slovenia")

#Spain
#Data from https://ec.europa.eu/taxation_customs/tedb/#/tax-details?taxId=4082&versionDate=1782856800&isEuro=true&taxType=EDU_ALCOHOL
Spain <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=1.2 ~ 0,
    ABV<=2.8 ~ 2.75/(ABV*1000/unit),
    ABV<4.4 ~ 7.48/(ABV*1000/unit),
    ABV<=6 ~ 9.96/(ABV*1000/unit),
    ABV<=7.6 ~ 13.56/(ABV*1000/unit),
    ABV<=15 ~ 0.91*PlatoUnit/1000,
    TRUE ~ NA_real_),
    Wine=case_when(
      ABV<=22 ~ 0,
      TRUE ~ NA_real_),
    Spirits=case_when(
      ABV<=1.2 ~ 0,
      TRUE ~ 958.94/(100*1000/unit)),
    Country="Spain")

#Sweden
#Data from https://ec.europa.eu/taxation_customs/tedb/#/tax-details?taxId=4107&versionDate=1782856800&isEuro=false&taxType=EDU_ALCOHOL
Sweden <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=2.8 ~ 0,
    ABV<=15 ~ 228/(1000/unit),
    TRUE ~ NA_real_),
    Wine=case_when(
      ABV<=2.25 ~ 0,
      ABV<=4.5 ~ 1038/(ABV*1000/unit),
      ABV<=7 ~ 1534/(ABV*1000/unit),
      ABV<=8.5 ~ 2112/(ABV*1000/unit),
      ABV<=15 ~ 2958/(ABV*1000/unit),
      ABV<=18 ~ 6190/(ABV*1000/unit),
      TRUE ~ NA_real_),
    Spirits=case_when(
      ABV<=1.2 ~ 0,
      TRUE ~ 52697/(100*1000/unit)),
    Country="Sweden")

#Switzerland
#Data from https://www.bazg.admin.ch/en/taxation-alcoholic-beverages-switzerland
Switzerland <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=0.5 ~ 0,
    ABV<=4 ~ 16.88/(ABV*1000/unit),
    ABV<=5.6 ~ 25.32/(ABV*1000/unit),
    ABV<=15 ~ 33.76/(ABV*1000/unit),
    TRUE ~ NA_real_),
    Wine=case_when(
      ABV<=18 ~ 0,
      TRUE ~ NA_real_),
    Spirits=case_when(
      ABV<=1.2 ~ 0,
      ABV<=6 ~ 116/(1000/unit),
      TRUE ~ 29/(1000/unit)),
    Country="Switzerland")

#Turkiye
#Data from (with high uncertainty over actual bases of taxation) https://birlesikgumrukleme.com.tr/mevzuat/gumruk-ve-dis-ticaret-mevzuatlari/sigara-ve-alkollu-iceceklerde-otv-artisi/
Turkiye <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=0.5 ~ 0,
    ABV<=15 ~ 14.4937/(10/unit),
    TRUE ~ NA_real_),
    Wine=case_when(
      ABV<=1.2 ~ 0,
      ABV<=22 ~ 71.2692/(ABV*10/unit),
      TRUE ~ NA_real_),
    Spirits=case_when(
      ABV<=1.2 ~ 0,
      ABV<=18 ~ 577.1023/(1000/unit),
      TRUE ~ 2227.9277/(1000/unit)),
    Country="Turkiye")

#UK
#Data from https://www.gov.uk/guidance/alcohol-duty-rates
UK <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    ABV<=1.2 ~ 0,
    ABV<3.5 ~ 9.96/(1000/unit),
    ABV<8.5 ~ 22.58/(1000/unit),
    ABV<=22 ~ 30.62/(1000/unit),
    TRUE ~ 33.99/(1000/unit)),
    Wine=case_when(
      ABV<=1.2 ~ 0,
      ABV<3.5 ~ 9.96/(1000/unit),
      ABV<8.5 ~ 26.61/(1000/unit),
      ABV<=22 ~ 30.62/(1000/unit),
      TRUE ~ 33.99/(1000/unit)),
    Spirits=case_when(
      ABV<=1.2 ~ 0,
      ABV<3.5 ~ 9.96/(1000/unit),
      ABV<8.5 ~ 26.61/(1000/unit),
      ABV<=22 ~ 30.62/(1000/unit),
      TRUE ~ 33.99/(1000/unit)),
    Country="United Kingdom")
  
#USA (federal taxes only)
#Data from https://www.ttb.gov/taxes/tax-audit/tax-and-fee-rates
#Minimum ABV for taxation is 0.5% as per https://www.ecfr.gov/current/title-23/chapter-II/subchapter-B/part-1208/section-1208.3
USA <- data.frame(ABV=seq(from=0, to=50, by=0.1)) %>% 
  mutate(Beer=case_when(
    #US beer taxes are levied per barrel, which is 31 US gallons (1 gallon ~ 3.785l)
    ABV<0.5 ~0,
    ABV<=15 ~ 18/(31*3.785*ABV*10/unit),
    TRUE ~ NA_real_),
    Wine=case_when(
      #US wine taxes are levied per wine gallon, which is 231 cubic inches (1 litre ~ 61.024 cu. inches)
      ABV<0.5 ~0,
      ABV<=16 ~ 1.07/((231/61.024)*ABV*10/unit),
      ABV<=21 ~ 1.57/((231/61.024)*ABV*10/unit),
      ABV<=24 ~ 3.15/((231/61.024)*ABV*10/unit),
      TRUE ~ NA_real_),
    Spirits=case_when(
      #US spirits taxes are levied per proof gallon, which is a gallon at 50% ABV (~3.785/2 litres of ethanol)
      ABV<0.5 ~0,
      TRUE ~ 13.50*2/((3.785)*1000/unit)),
    Country="United States")

#Exchange rates based on https://www.bankofengland.co.uk/boeapps/database/Rates.asp?TD=26&TM=Aug&TY=2026&into=GBP&rateview=D
EuroRate <- 1.1668
Exchange <- data.frame(Country=c("Australia", "Austria", "Belgium", "Canada", "Czechia", "Denmark", "Estonia", 
                                 "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy",
                                 "Japan", "Korea", "Latvia", "Lithuania", "Luxembourg", "Netherlands", "New Zealand", "Norway",
                                 "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden",
                                 "Switzerland", "United Kingdom", "United States", "Turkiye"),
                       Conversion=c(1.8964, EuroRate, EuroRate, 1.8876, 28.0966, 8.7225, EuroRate, 
                                    EuroRate, EuroRate, EuroRate, EuroRate, 421.2891, 164.11, EuroRate, 4.0490, EuroRate,
                                    216.5859, 1869.4567, EuroRate, EuroRate, EuroRate, EuroRate, 2.2902, 12.7151,
                                    5.0221, EuroRate, EuroRate, EuroRate, EuroRate, 12.9590,
                                    1.0955, 1, 1.3588, 65.3820))

#Combine and convert
Finaldata <- bind_rows(Australia, Austria, Belgium, Canada, Czechia, Denmark, Estonia, Finland, France, Germany,
                       Greece, Hungary, Iceland, Ireland, Israel, Italy, Japan, Korea, Latvia, Lithuania, Luxembourg, Netherlands, NewZealand,
                       Norway, Poland, Portugal, SlovakRepublic, Slovenia, Spain, Sweden, Switzerland, Turkiye, UK,
                       USA) %>% 
  gather(BevType, DutyRate, c("Beer","Wine", "Spirits")) %>% 
  merge(Exchange) %>% 
  mutate(DutyRate_GBP=DutyRate/Conversion)

ggplot(Finaldata, aes(x=ABV/100, y=DutyRate_GBP, colour=BevType))+
  geom_hline(yintercept=0, colour="grey20")+
  geom_vline(xintercept=0, colour="grey20")+
  geom_line()+
  scale_x_continuous(name="ABV", labels=label_percent(accuracy=1))+
  scale_y_continuous(name="Duty rate (GBP per UK unit)", labels=label_dollar(prefix="£"))+
  scale_colour_manual(values=c("#FF9900", "#00B0F0", "#7030A0"), name="")+
  facet_wrap(~Country)+
  theme_custom()+
  theme(axis.line=element_blank())

#Pull out effective duty rates for 5% beer, 12.5% wine and 40% spirits
Shortdata <- Finaldata %>% 
  filter((BevType=="Beer" & ABV==5) |
           (BevType=="Wine" & ABV==12.5) |
           (BevType=="Spirits" & ABV==40))

#Final plot. Exclude Turkiye due to high uncertainty over actual tax basis
agg_png("Outputs/OECDDutyRateComparisons.png", units="in", width=6.5, height=8, res=800)
ggplot(Shortdata %>% mutate(BevType=factor(BevType, levels=c("Beer", "Wine", "Spirits"))) %>% 
         filter(Country!="Turkiye"), 
       aes(x=DutyRate_GBP, y=fct_rev(Country), fill=BevType))+
  geom_vline(xintercept=0, colour="grey20")+
  geom_col(position="dodge")+
  scale_x_continuous(name="Duty rate (£ per UK unit)", labels=label_dollar(prefix="£"))+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("#FF9900", "#7030A0", "#00B0F0"), name="",
                    guide = guide_legend(reverse = TRUE))+
  theme_custom()+
  theme(axis.line.y=element_blank(), panel.grid.major.x=element_line(colour="grey95"),
        panel.grid.major.y=element_blank())+
  labs(title="Almost every OECD country taxes spirits at a\nhigher rate than other alcohol",
       subtitle="Excise duty rates per 10ml of pure alcohol by beverage type\nVAT and sub-national duties not included.\nRates are shown for a typical 5% beer, 12.5% wine and 40% spirit\n",
       caption="\nNo data available for Colombia or Turkiye\nChile, Costa Rica, and Mexico tax all alcohol based on price only, but have higher rates for spirits\nAustralia taxes wine based on price only\nData compiled by @VictimOfMaths")

dev.off()


