##Done with MAC OSX

library(gapminder)
library(ggplot2)
library(dplyr)
library(maps)
library(WDI)
library(ggthemes)
library(readxl)
install.packages("hrbrthemes")
library(hrbrthemes)
library(dplyr)
library(ggplot2)
library(forcats)


##Human Capital Index
library(tidyr)
library(tidyverse)
library(readxl)

HDI<- read_excel("~/Downloads/[Metode Baru] Indeks Pembangunan Manusia menurut Provinsi.xlsx", 
                                                                        col_types = c("text", "numeric"))
View(HDI)
HDIbar <- HDI %>% filter(HumanCapitalIndex>=72.28) %>% mutate(Province = fct_reorder(Province, HumanCapitalIndex))
HDIbar
ggplot(HDIbar, aes(x=Province, y=HumanCapitalIndex,fill=Province)) + 
  geom_bar(stat = "identity", width = 0.6) + 
  scale_fill_manual(values = c("green","green","green","green","green","green", "green","green","green","green","red")) +
  coord_flip()+theme_bw() + theme(legend.position="none")+labs(x="", y="Human Development Index Score",subtitle = "That scores higher than National Average on 2021", title="Comparison within Indonesia's Provinces", caption="Source: Badan Pusat Statistik")


##Occupation
PopOcc <- read_excel("~/Downloads/data-jumlah-penduduk-berdasarkan-pekerjaan-per-kelurahan-tahun-2020.xlsx")
PopOcc$Total<- as.numeric(PopOcc$Total)
options(scipen = 500)
PopAccOcc<- PopOcc %>%
  arrange(Total) %>%
  mutate(Occupation=factor(Occupation, levels=Occupation)) %>%   # This trick update the factor levels
  ggplot( aes(x=Occupation, y=Total)) +
  geom_segment( aes(xend=Occupation, yend=0)) +
  geom_point( size=4, color="green") +
  coord_flip() +
  theme_bw() +
  xlab("")+labs(x="", y="Total", title="DKI Jakarta Population", subtitle="by Occupation on 2020", caption="Source: Dinas Kependudukan dan Pencatatan Sipil")
PopAccOcc


##Poverty
Poordata <- read_excel("~/Downloads/Persentase Penduduk Miskin Di Daerah Tertinggal.xlsx")
Poordata$PercentagePoor <- as.numeric(Poordata$PercentagePoor)
poorgraph <- ggplot(Poordata, mapping=aes(x=Year, y=PercentagePoor))
poorgraph
poorgraph + geom_line(aes(group=Place), color="green") + theme_bw() +labs(x="Year", y="in percentage (%)", title="Poor People in Disadvantaged Areas", subtitle="in Indonesia", caption="Source: Susenas Badan Pusat Statistik")


##Family Planning
library(tidyr)
library(readxl)

RCU <- read_excel("~/Downloads/Angka Pemakaian Kontrasepsi (CPR) Semua Cara Pada Pasangan Usia Subur Usia 15-49 Tahun Yang Pernah Kawin (40% Bawah), Menurut Provinsi.xlsx", 
                  col_types = c("text", "numeric"))
View(RCU)
RCUpp <-RCU%>% filter(Province=="DKI Jakarta" | Province=="Special Region of Yogyakarta" |Province=="East Java" | Province=="North Sumatra" |Province== "South Sulawesi") %>% mutate(Province = fct_reorder(Province, Contraceptive))
RCUpp

ggplot(RCUpp, aes(x=Province, y=Contraceptive,fill=RCUpp$Province)) + 
  geom_bar(stat = "identity", width = 0.6) + scale_fill_manual(values = c("green", "green", "red","green","green")) +
  coord_flip()+theme_bw()+ theme(legend.position="none")+labs(x="", y="Rate of Contraceptive Usage",subtitle = "On 2019", title="Comparison within Indonesia's Biggest Provinces", caption="Source: Badan Pusat Statistik")