library('tidyverse')
library('haven')
library('dplyr')
install.packages("plm")
install.packages("glm2")
library('plm')
library('glm2')

##Load all data
##SUSENAS KOR 2015##
data2015 = read_dta('SSR - KOR 2015.dta')
data2015clean <- data2015 %>% select(r101,r102,r105,r401,r405,r407,r507,fwt_tahun) %>% rename(urut=r401,province=r101,regency=r102,female=r405,age=r407,schoolenrollment=r507,urban=r105,fwt=fwt_tahun) %>% filter(age==15|age==16|age==17|age==18)

data2015clean$year<-2015
colnames(data2015clean)

data2015clean$female<-ifelse(data2015clean$female=="2",1,0)
data2015clean$schoolenrollment<-ifelse(data2015clean$schoolenrollment=="2",1,0)
data2015clean$urban<-ifelse(data2015clean$schoolenrollment=="1",1,0)
data2015clean$regencycode<-data2015clean$province*100+data2015clean$regency


data2015clean <-transform(data2015clean,
                          percse=ave(schoolenrollment,
                                     regencycode))

unique(data2015clean$regency)

##SUSENAS KOR 2016##
data2016 = read_dta('SSR - KOR 2016.dta')
data2016clean <- data2016 %>% select(r101,r102,r105,r401,r405,r407,r507,fwt_tahun) %>% rename(urut=r401,province=r101,regency=r102,female=r405,age=r407,schoolenrollment=r507,urban=r105,fwt=fwt_tahun) %>% filter(age==15|age==16|age==17|age==18)
data2016clean$year<-2016
colnames(data2016clean)

data2016clean$female<-ifelse(data2016clean$female=="2",1,0)
data2016clean$schoolenrollment<-ifelse(data2016clean$schoolenrollment=="2",1,0)
data2016clean$urban<-ifelse(data2016clean$schoolenrollment=="1",1,0)
data2016clean$regencycode<-data2016clean$province*100+data2016clean$regency

data2016clean <-transform(data2016clean,
                          percse=ave(schoolenrollment,
                                     regencycode))
##SUSENAS KOR 2017##
data2017 = read_dta('SSR - KOR 2017.dta')
data2017clean <- data2017 %>% select(r101,r102,r401,r405,r105,r407,r514,fwt) %>% rename(urut=r401,province=r101,regency=r102,female=r405,age=r407,schoolenrollment=r514,urban=r105) %>% filter(age==15|age==16|age==17|age==18)
data2017clean$year<-2017
colnames(data2017clean)

data2017clean$female<-ifelse(data2017clean$female=="2",1,0)
data2017clean$schoolenrollment<-ifelse(data2017clean$schoolenrollment=="2",1,0)
data2017clean$urban<-ifelse(data2017clean$schoolenrollment=="1",1,0)
data2017clean$regencycode<-data2017clean$province*100+data2017clean$regency

data2017clean <-transform(data2017clean,
                          percse=ave(schoolenrollment,
                                     regencycode))

##SUSENAS KOR 2018##
data2018 = read_dta('SSR - KOR 2018.dta')
data2018clean <- data2018 %>% select(r101,r102,r401,r405,r407,r105,r612,fwt) %>% rename(urut=r401,province=r101,regency=r102,female=r405,age=r407,schoolenrollment=r612,urban=r105) %>% filter(age==15|age==16|age==17|age==18)
data2018clean$year<-2018
colnames(data2018clean)

data2018clean$female<-ifelse(data2018clean$female=="2",1,0)
data2018clean$schoolenrollment<-ifelse(data2018clean$schoolenrollment=="2",1,0)
data2018clean$urban<-ifelse(data2018clean$schoolenrollment=="1",1,0)
data2018clean$regencycode<-data2018clean$province*100+data2018clean$regency

data2018clean <-transform(data2018clean,
                          percse=ave(schoolenrollment,
                                     regencycode))

##SUSENAS KOR 2019##
data2019 = read_dta('SSR - KOR 2019.dta')
data2019clean <- data2019 %>% select(r101,r102,r105,r401,r405,r407,r612,fwt) %>% rename(urut=r401,province=r101,regency=r102,female=r405,age=r407,schoolenrollment=r612,urban=r105) %>% filter(age==15|age==16|age==17|age==18)
data2019clean$year<-2019
colnames(data2019clean)

data2019clean$female<-ifelse(data2019clean$female=="2",1,0)
data2019clean$schoolenrollment<-ifelse(data2019clean$schoolenrollment=="2",1,0)
data2019clean$urban<-ifelse(data2019clean$schoolenrollment=="1",1,0)
data2019clean$regencycode<-data2019clean$province*100+data2019clean$regency

data2019clean <-transform(data2019clean,
                          percse=ave(schoolenrollment,
                                     regencycode))

##SUSENAS KOR 2020##
data2020 = read_dta('SSR - KOR 2020.dta')
data2020clean <- data2020 %>% select(r101,r102,r105,r401,r405,r407,r612,fwt) %>% rename(urut=r401,province=r101,regency=r102,female=r405,age=r407,schoolenrollment=r612,urban=r105) %>% filter(age==15|age==16|age==17|age==18)
data2020clean$year<-2020
colnames(data2020clean)

data2020clean$female<-ifelse(data2020clean$female=="2",1,0)
data2020clean$schoolenrollment<-ifelse(data2020clean$schoolenrollment=="2",1,0)
data2020clean$urban<-ifelse(data2020clean$schoolenrollment=="1",1,0)
data2020clean$regencycode<-data2020clean$province*100+data2020clean$regency

data2020clean <-transform(data2020clean,
                          percse=ave(schoolenrollment,
                                     regencycode))

##SUSENAS KOR 2021##
data2021 = read_dta('SSR - KOR 2021.dta')
data2021clean <- data2021 %>% select(r101,r102,r105,r401,r405,r407,r612,fwt) %>% rename(urut=r401,province=r101,regency=r102,female=r405,age=r407,schoolenrollment=r612,urban=r105) %>% filter(age==15|age==16|age==17|age==18)
data2021clean$year<-2021
colnames(data2021clean)

data2021clean$female<-ifelse(data2021clean$female=="2",1,0)
data2021clean$schoolenrollment<-ifelse(data2021clean$schoolenrollment=="2",1,0)
data2021clean$urban<-ifelse(data2021clean$schoolenrollment=="1",1,0)
data2021clean$regencycode<-data2021clean$province*100+data2021clean$regency

data2021clean <-transform(data2021clean,
                          percse=ave(schoolenrollment,
                                     regencycode))

##COMBINED DATA SET##
alltimedata <- rbind(data2015clean,data2016clean,data2017clean,data2018clean,data2019clean,data2020clean,data2021clean)

##controls
library(readxl)
controlvar <- read_excel("controlvar.xlsx", 
                         col_types = c("numeric", "text", "text", 
                                       "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
View(controlvar)


completedata <- rbind(data2015clean,data2016clean,data2017clean,data2018clean,data2019clean,data2020clean,data2021clean)
regencydata<- aggregate(completedata$fwt, 
                        by = list(completedata$year,completedata$province,completedata$regencycode,completedata$schoolenrollment,completedata$female), 
                        FUN = sum)
names(regencydata) <- c("year", "province","regencycode","schoolenrollment","female","SEweighted")

regencydata<-regencydata%>%group_by(year,province,regencycode)%>%mutate(nreg=sum(SEweighted))%>%mutate(HSE_rt=((sum(SEweighted*schoolenrollment)/nreg)*100))
regencydata<-regencydata%>%group_by(year,province,regencycode,female)%>%mutate(nregbysex=sum(SEweighted))%>%mutate(HSE_rts=((sum(SEweighted*schoolenrollment)/nregbysex)*100))
regencydata1<-regencydata%>%group_by(year,province)%>%mutate(nprov=sum(SEweighted))%>%mutate(HSE_pt=((sum(SEweighted*schoolenrollment)/nprov)*100))
regencydata1<-regencydata1%>%group_by(year,province,female)%>%mutate(nprovbysex=sum(SEweighted))%>%mutate(HSE_pts=((sum(SEweighted*schoolenrollment)/nprovbysex)*100))

regencydata1$treatment<-ifelse(regencydata1$province==36,1,0)
regencydata1$time<-ifelse(regencydata1$year>=2019,1,0)
regencydata1$did <- regencydata1$treatment*regencydata1$time
regencydata1$regencycode <- as.character(regencydata1$regencycode)
regencydata1$year <- as.character(regencydata1$year)
regencydata1$yearnum<-as.numeric(regencydata1$year)

##year dummies
regencydata1$y2015<-ifelse(regencydata1$year==2015,1,0)
regencydata1$y2016<-ifelse(regencydata1$year==2016,1,0)
regencydata1$y2017<-ifelse(regencydata1$year==2017,1,0)
regencydata1$y2018<-ifelse(regencydata1$year==2018,1,0)
regencydata1$y2019<-ifelse(regencydata1$year==2019,1,0)
regencydata1$y2020<-ifelse(regencydata1$year==2020,1,0)
regencydata1$y2021<-ifelse(regencydata1$year==2021,1,0)
regencydata1$sexname<-ifelse(regencydata1$female==1,"Female","Male")


regencydata1<-regencydata1%>%mutate(yearfe=yearnum-2018)

regencydatamup<-regencydata1%>%filter(year>=2016)%>%filter(province!=36|province!=31|province!=18) %>%filter(province==35|province==82)
regencydatamup$provincename<-ifelse(regencydatamup$province==35,"East Java","North Maluku")

regencydatamup$EastJava<-ifelse(regencydatamup$province==35,1,0)
regencydatamup$post<-ifelse(regencydatamup$year>=2020,1,0)
regencydatamup1 <- merge(regencydatamup,province_fe, by = c("year", "province"))
regencydatamup1c <- merge(regencydatamup1,controlvar, by = c("year", "regencycode"))

provincedata_pt<- regencydatamup1c %>%select(year, EastJava,provincename.x,province,HSE_pt)
provincedata_pt<-distinct(provincedata_pt)

provincedata_pts<- regencydatamup1c %>%select(year, EastJava,provincename.x,province,sexname, female,HSE_pt,HSE_pts)
provincedata_pts<-distinct(provincedata_pts)

regencydatamup1c$expreg <- regencydatamup1c$expreg/1000000000
regencydatamup1c$revreg <- regencydatamup1c$revreg/1000000000
regencydata_rts<-regencydatamup1c %>%subset(select = -c(schoolenrollment,SEweighted))
regencydata_rts<-distinct(regencydata_rts)

regencydata_rt<-regencydatamup1c %>%subset(select = -c(nregbysex,nprovbysex,schoolenrollment,SEweighted,sexname,female,HSE_rts,HSE_pts))
regencydata_rt<-distinct(regencydata_rt)


#PRETRENDS
library(hrbrthemes)
install.packages("ggthemes") # Install 
library(ggthemes)

pretrendprovindo<-regencydata1%>%filter(year<=2018)%>%ggplot(aes(x = year, y = HSE_pt,group=province)) +
  geom_line() +theme_tufte(base_size = 11,base_family = "Palatino")+ labs(x="Year", y="High School Enrollment", title="Indoensia's Provinces Overview") +facet_wrap(~province)
pretrendprovindo

pretrendprovspecified<-provdata%>%filter(province==35|province==82)%>%filter(year<=2018)%>%ggplot(aes(x = year, y = HSE_pt,group=provincename.x)) +
  geom_line(aes(linetype = provincename.x)) +theme_tufte(base_size = 15,base_family = "Palatino")+ labs(x="Year", y="High School Enrollment", title="East Java and North Maluku High School Enrollment Rate", linetype="Province")
pretrendprovspecified

pretrendregsex<-regencydatamup%>%filter(province==35|province==82)%>%filter(year<=2018)%>%ggplot(aes(x = year, y = HSE_pts, group=interaction(province,female),color = sexname)) +
  geom_line(aes(linetype = provincename))  +theme_tufte(base_size = 15,base_family = "Palatino") + labs(x="Year", y="High School Enrollment", title="East Java and North Maluku High School Enrollment Rate", color="Sex",linetype="Province")
pretrendregsex

pretrendprovsex<-regencydata1%>%filter(year<=2018)%>%filter(year!=2015)%>%ggplot(aes(x = year, y = HSE_pts, group=interaction(regencycode,female),color = sexname)) +
  geom_line() +facet_wrap(~province) +theme_tufte(base_size = 11,base_family = "Palatino") + labs(x="Year", y="High School Enrollment", title="Indonesia's Provinces Overview", color="Sex")
pretrendprovsex

install.packages("lmtest")
library(lmtest)
install.packages("sandwich")
library(sandwich)
install.packages("miceadds")
library(miceadds)
install.packages("RCurl")
library(RCurl)

# import the function
url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)
require("stargazer")

##DD
##original
DIDregfullm<-lm(HSE_rt~EastJava*post,data=regencydata_rt)
DDregnocontrols<-summary(DIDregfullm, cluster=c("regencycode"))
cluster_se1 <- as.vector(summary(DIDregfullm, cluster=c("regencycode"))$coefficients[,"Std. Error"])
DDregnocontrols
bptest(DIDregfullm)

#regional controls
DIDregfullmc<-lm(HSE_rt~EastJava*post+gini	+povertygap+	NER_MSr+HDIr+nhighschool,data=regencydata_rt)
DDregwithcontrols<-summary(DIDregfullmc, cluster=c("regencycode"))
cluster_se2 <- as.vector(summary(DIDregfullmc, cluster=c("regencycode"))$coefficients[,"Std. Error"])
DDregwithcontrols
bptest(DIDregfullmc)
#CHECK MULTICOL
library(car)
multicolcheckDD<-vif(DIDregfullmc,type="predictor")
multicolcheckDD
#regional controls + regency fe

##DDD
#original
DDDregfullm<-lm(HSE_rts~EastJava*post+EastJava*female+post*female+EastJava*post*female,data=regencydata_rts)
DDDregnocontrols<-summary(DDDregfullm, cluster=c("regencycode"))
cluster_se3 <- as.vector(summary(DDDregfullm, cluster=c("regencycode"))$coefficients[,"Std. Error"])
DDDregnocontrols
#regional controls
DDDregsex<-lm(HSE_rts~EastJava*post+EastJava*female+post*female+EastJava*post*female+gini	+povertygap	+NER_MSr+HDIr	+nhighschool,data=regencydata_rts)
DDDregwithcontrols<-summary(DDDregsex, cluster=c("regencycode"))
cluster_se4 <- as.vector(summary(DDDregsex, cluster=c("regencycode"))$coefficients[,"Std. Error"])
DDDregwithcontrols
#CHECK MULTICOL
multicolcheckDDD<-vif(DDDregsex,type="predictor")
multicolcheckDDD


library(dplyr)
###EVENT STUDY
#ES_rt fe
ESnocontrol_rt<-lm(HSE_rt ~EastJava*y2016+EastJava*y2017+EastJava*y2018+EastJava*y2020+EastJava*y2021+regencycode, data=regencydata_rt)
eventstudynocontrol_rt <-summary(ESnocontrol_rt)
eventstudynocontrol_rt
#ES_rt fe+controls
ESwcontrol_rt<-lm(HSE_rt ~EastJava*y2016+EastJava*y2017+EastJava*y2018+EastJava*y2020+EastJava*y2021+regencycode+gini	+povertygap	+NER_MSr+HDIr	+nhighschool, data=regencydata_rt)
eventstudywithcontrol_rt <-summary(ESwcontrol_rt)
eventstudywithcontrol_rt
#ES_rts fe
ESnocontrol_rts<-lm(HSE_rts ~EastJava*y2016+EastJava*y2017+EastJava*y2018+EastJava*y2020+EastJava*y2021+EastJava*female+EastJava*y2016*female+EastJava*y2017*female+EastJava*y2018*female+EastJava*y2020*female+EastJava*y2021*female+regencycode, data=regencydata_rts)
eventstudynocontrol_rts <-summary(ESnocontrol_rts)
eventstudynocontrol_rts
#ES_rts fe+controls
ESwcontrol_rts<-lm(HSE_rts ~EastJava*y2016+EastJava*y2017+EastJava*y2018+EastJava*y2020+EastJava*y2021+EastJava*female+EastJava*y2016*female+EastJava*y2017*female+EastJava*y2018*female+EastJava*y2020*female+EastJava*y2021*female+regencycode+gini	+povertygap	+NER_MSr+HDIr	+nhighschool, data=regencydata_rts)
eventstudywithcontrol_rts <-summary(ESwcontrol_rts)
eventstudywithcontrol_rts


#Event Study Graph
eventstudygraph<-eventstudydata%>%ggplot(aes(x = yearnum, y = HSE_pts, group=interaction(province,female),color = sexname)) +
  geom_line(aes(linetype = provincename.x))  +
  theme_tufte(base_size = 15,base_family = "Palatino") + 
  labs(x="Year", y=NULL, 
       title="East Java and North Maluku High School Enrollment Rate", color="Sex",linetype="Province") +
  geom_segment(aes(x = 2019.5, y = 75, xend = 2019.5, yend = 84))
eventstudygraph



###TABLES
library("stargazer")
install.packages("cobalt")
library("cobalt")


regencydata_rtf <- regencydata_rts%>%filter(female==1)
regencydata_rtm <- regencydata_rts%>%filter(female==0)
##DESC STATS##
descstat<-stargazer(provincedata_pt,provincedata_pts,regencydata_rt,regencydata_rts,
                    omit=c("female","province"), 
                    type = "html",
                    keep=c("HSE_pt","HSE_rt","HSE_pts","HSE_rts","gini","litratereg","revreg","expreg","NER_MSr","povertygap","HDIr","nhighschool"),
                    summary.stat=c("n","mean","sd","min","max") , 
                    title="Summary Statistics")
writeLines(descstat, "descstat.html")
descriptivestatistics<-browseURL("descstat.html")
descriptivestatistics

descstat<-stargazer(provincedata_pt,provincedata_pts,regencydata_rt,regencydata_rts,
                    omit=c("female","province"), 
                    type = "html",
                    summary.stat=c("n","mean","sd","min","max") , 
                    title="Summary Statistics",
                    covariate.labels = c("HSE<sub>pt</sub>","HSE<sub>pts</sub>","HSE<sub>rt</sub>","HSE<sub>rts</sub>","Gini<sub>rt</sub>","Literacy<sub>rt</sub>","Revenue<sub>rt</sub> (in billions IDR)","Expenditure<sub>rt</sub> (in billions IDR)","MSE<sub>rt</sub>","Poverty Gap<sub>rt</sub>","HDI<sub>rt</sub>","Number of High School<sub>rt</sub>"))
writeLines(descstat, "descstat.html")
descriptivestatistics<-browseURL("descstat.html")
descriptivestatistics

##REGTable##
DIDregfullmtable<-stargazer(DIDregfullm, DIDregfullmc, DDDregfullm,DDDregsex,
                            se=list(cluster_se1,cluster_se2,cluster_se3,cluster_se4),
                            omit = c("gini","povertygap","NER_MSr","HDIr","nhighschool"),
                            type = "html",
                            title="Free High School Policy Effects",
                            column.sep.width = "1pt",
                            dep.var.labels.include = FALSE,
                            column.labels = c("HSE<sub>rt</sub>","HSE<sub>rts</sub>"),
                            column.separate = c(2,2),
                            add.lines=list(c("Control Variables","No", "Yes","No", "Yes")),
                            covariate.labels = c("East Java","After","Female","East Java:After","East Java:Female","After:Female","East Java:After:Female", "Constant"))
writeLines(DIDregfullmtable, "DIDregfullmtable.html")
regressiontable<-browseURL("DIDregfullmtable.html")

?stargazer
##ESTable##
ESfulltable<-stargazer(ESnocontrol_rt,ESwcontrol_rt,ESnocontrol_rts,ESwcontrol_rts,
                       omit=c("regencycode3502","regencycode3503","regencycode3504","regencycode3505",
                              "regencycode3506","regencycode3507","regencycode3508","regencycode3509","regencycode3510",
                              "regencycode3511","regencycode3512","regencycode3513","regencycode3514","regencycode3515",
                              "regencycode3516","regencycode3517","regencycode3518","regencycode3519","regencycode3520",
                              "regencycode3521","regencycode3522","regencycode3523","regencycode3524","regencycode3525",
                              "regencycode3526","regencycode3527","regencycode3528","regencycode3529","regencycode3571",
                              "regencycode3572","regencycode3573","regencycode3574","regencycode3575","regencycode3576",
                              "regencycode3577","regencycode3578","regencycode3579","regencycode8201","regencycode8202",
                              "regencycode8203","regencycode8204","regencycode8205","regencycode8206","regencycode8207",
                              "regencycode8208","regencycode8271","regencycode8272"),
                       keep = c("EastJava:y2016","EastJava:y2017","EastJava:y2018",
                                "EastJava:y2020","EastJava:y2021","EastJava:y2016:female","EastJava:y2017:female","EastJava:y2018:female",
                                "EastJava:y2020:female","EastJava:y2021:female"),
                       type = "html", 
                       title="Event Study: Free High School Policy Effects",
                       column.sep.width = "1pt",
                       dep.var.labels.include = FALSE,
                       column.labels = c("HSE<sub>rt</sub>","HSE<sub>rts</sub>"),
                       column.separate = c(2,2),
                       add.lines=list(c("Regency and Time Fixed Effects","Yes", "Yes","Yes", "Yes"),c("Control Variables","No", "Yes","No", "Yes")))
writeLines(ESfulltable, "ESfulltable.html")
regressiontable<-browseURL("ESfulltable.html")
