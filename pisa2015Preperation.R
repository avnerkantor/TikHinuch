sudo wget http://vs-web-fs-1.oecd.org/pisa/PUF_SPSS_COMBINED_CMB_STU_QQQ.zip
sudo unzip PUF_SPSS_COMBINED_CMB_STU_QQQ.zip
sudo wget http://vs-web-fs-1.oecd.org/pisa/PUF_SPSS_COMBINED_CMB_SCH_QQQ.zip
unzip PUF_SPSS_COMBINED_CMB_SCH_QQQ.zip
sudo wget http://vs-web-fs-1.oecd.org/pisa/trend_escs_SPSS.zip 
sudo unzip trend_escs_SPSS.zip

library(foreign)
stu2015 <- read.spss("CY6_MS_CMB_STU_QQQ.sav", to.data.frame=TRUE)
save(stu2015, file="stu2015.rda")
sch2015 <- read.spss("CY6_MS_CMB_SCH_QQQ.sav", to.data.frame=TRUE)
save(sch2015, file="sch2015.rda")

a<-colnames(stu2015)
b<-colnames(sch2015)
intersect(a, b)
[1] "CNTRYID"   "CNT"       "CNTSCHID"  "CYC"       "NatCen"    "Region"   
[7] "STRATUM"   "SUBNATIO"  "OECD"      "ADMINMODE" "SENWT"     "VER_DAT"  

library(dplyr)
pisa2015<-left_join(stu2015, sch2015, by=c("CNTRYID"="CNTRYID", "CNTSCHID"="CNTSCHID"))

low<-pisa2015 %>%filter(ESCS < -0.2)
high<-pisa2015%>%filter(ESCS > 0.7)
medium<-pisa2015%>%filter(ESCS <= 0.7, ESCS >= -0.2)
myna<-pisa2015%>%filter(is.na(ESCS))

low$ESCS<-"Low"
medium$ESCS<-"Medium"
high$ESCS<-"High"

pisa2015b<-bind_rows(low, high, medium, myna)