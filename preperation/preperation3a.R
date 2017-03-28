
#### General ####
#1 General mean
GeneralMean<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x) %>% select(PVREAD, W_FSTUWT)
  weighted.mean(x=temp[,"PVREAD"], w=(temp[,"W_FSTUWT"]))
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender=0, Performers=0, ESCS=0, Average=GeneralMean, GenderESCS="General")
#df<-dm
df<-rbind(df, dm)
#2 Male mean
MaleMean<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ST04Q01 == "Male") %>% select(PVREAD, W_FSTUWT)
  weighted.mean(x=temp[,"PVREAD"], w=(temp[,"W_FSTUWT"]))
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender="Male", Performers=0, ESCS=0, Average=MaleMean, GenderESCS="Male")
df<-rbind(df, dm)

#3 Female mean
FemaleMean<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ST04Q01 == "Female") %>% select(PVREAD, W_FSTUWT)
  weighted.mean(x=temp[,"PVREAD"], w=(temp[,"W_FSTUWT"]))
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender="Female", Performers=0, ESCS=0, Average=FemaleMean, GenderESCS="Female")
df<-rbind(df, dm)
#### Performers ####
#4 High Performers
HighPerformers<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, PVREAD>=expertisedf$PVREAD[1])
  nrow(temp)/nrow(data %>% filter(COUNTRY==x))*100
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender=0, Performers="High", ESCS=0, Average=HighPerformers, GenderESCS="General")
df<-rbind(df, dm)
#5 Low Performers
LowPerformers<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, PVREAD<=expertisedf$PVREAD[2])
  nrow(temp)/nrow(data %>% filter(COUNTRY==x))*100
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender=0, Performers="Low", ESCS=0, Average=LowPerformers, GenderESCS="General")
df<-rbind(df, dm)
#6 High Performers Male Mean
HighPerformersMale<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ST04Q01 == "Male", PVREAD>=expertisedf$PVREAD[1])
  nrow(temp)/nrow(data %>% filter(COUNTRY==x))*100
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender="Male", Performers="High", ESCS=0, Average=HighPerformersMale, GenderESCS="Male")
df<-rbind(df, dm)
#13 Low Performers Female Mean
LowPerformersFemale<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ST04Q01 == "Female", PVREAD<=expertisedf$PVREAD[2])
  nrow(temp)/nrow(data %>% filter(COUNTRY==x))*100
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender="Female", Performers="Low", ESCS=0, Average=LowPerformersFemale, GenderESCS="Female")
df<-rbind(df, dm)
#7 High Performers Female Mean
HighPerformersFemale<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ST04Q01 == "Female", PVREAD>=expertisedf$PVREAD[1])
  nrow(temp)/nrow(data %>% filter(COUNTRY==x))*100
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender="Female", Performers="High", ESCS=0, Average=HighPerformersFemale, GenderESCS="Female")
df<-rbind(df, dm)
#8 Low Performers Male Mean
LowPerformersMale<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ST04Q01 == "Male", PVREAD<=expertisedf$PVREAD[2])
  nrow(temp)/nrow(data %>% filter(COUNTRY==x))*100
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender="Male", Performers="Low", ESCS=0, Average=LowPerformersMale, GenderESCS="Male")
df<-rbind(df, dm)
#### ESCS General #####

#14 Low Escs Mean
LowEscsMean<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ESCS =="Low") %>% select(PVREAD, W_FSTUWT)
  weighted.mean(x=temp[,"PVREAD"], w=(temp[,"W_FSTUWT"]))
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender=0, Performers=0, ESCS="Low", Average=LowEscsMean, GenderESCS="GeneralLow")
df<-rbind(df, dm)
#15 Medium Escs Mean
MediumEscsMean<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ESCS=="Medium") %>% select(PVREAD, W_FSTUWT)
  weighted.mean(x=temp[,"PVREAD"], w=(temp[,"W_FSTUWT"]))
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender=0, Performers=0, ESCS="Medium", Average=MediumEscsMean, GenderESCS="GeneralMedium")
df<-rbind(df, dm)
#16 High Escs Mean
HighEscsMean<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ESCS=="High") %>% select(PVREAD, W_FSTUWT)
  weighted.mean(x=temp[,"PVREAD"], w=(temp[,"W_FSTUWT"]))
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender=0, Performers=0, ESCS="High", Average=HighEscsMean, GenderESCS="GeneralHigh")
df<-rbind(df, dm)
#### ESCS Proficiency #####

##### General ESCS Proficiency ##### 
#1 High Performers Escs High
HighPerformersEscsHigh<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ESCS=="High", PVREAD>=expertisedf$PVREAD[1])
  nrow(temp)/nrow(data %>% filter(COUNTRY==x))*100
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender=0, Performers="High", ESCS="High", Average=HighPerformersEscsHigh, GenderESCS="GeneralHigh")
df<-rbind(df, dm)
#2 High Performers Escs Low
HighPerformersEscsLow<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ESCS =="Low", PVREAD>=expertisedf$PVREAD[1])
  nrow(temp)/nrow(data %>% filter(COUNTRY==x))*100
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender=0, Performers="High", ESCS="Low", Average=HighPerformersEscsLow, GenderESCS="GeneralLow")
df<-rbind(df, dm)
#3 Low Performers Escs High
LowPerformersEscsHigh<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ESCS =="High", PVREAD<=expertisedf$PVREAD[2])
  nrow(temp)/nrow(data %>% filter(COUNTRY==x))*100
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender=0, Performers="Low", ESCS="High", Average=LowPerformersEscsHigh, GenderESCS="GeneralHigh")
df<-rbind(df, dm)
#4 Low Performers Escs Low
LowPerformersEscsLow<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ESCS =="Low", PVREAD<=expertisedf$PVREAD[2])
  nrow(temp)/nrow(data %>% filter(COUNTRY==x))*100
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender=0, Performers="Low", ESCS="Low", Average=LowPerformersEscsLow, GenderESCS="GeneralLow")
df<-rbind(df, dm)
#5 Low Performers Escs Medium
LowPerformersEscMedium<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ESCS =="Medium", PVREAD<=expertisedf$PVREAD[2])
  nrow(temp)/nrow(data %>% filter(COUNTRY==x))*100
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender=0, Performers="Low", ESCS="Medium", Average=LowPerformersEscMedium, GenderESCS="GeneralMedium")
df<-rbind(df, dm)
#6 High Performers Escs Medium
HighPerformersEscsMedium<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x,ESCS =="Medium", PVREAD<=expertisedf$PVREAD[2])
  nrow(temp)/nrow(data %>% filter(COUNTRY==x))*100
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender=0, Performers="High", ESCS="Medium", Average=HighPerformersEscsMedium, GenderESCS="GeneralMedium")
df<-rbind(df, dm)
##### Male ESCS Proficiency ##### 
HighPerformersEscsHighMale<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ST04Q01 == "Male", ESCS =="High", PVREAD>=expertisedf$PVREAD[1])
  nrow(temp)/nrow(data %>% filter(COUNTRY==x))*100
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender="Male", Performers="High", ESCS="High", Average=HighPerformersEscsHighMale, GenderESCS="Male")
df<-rbind(df, dm)
#2 High Performers Escs Low
HighPerformersEscsLowMale<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ST04Q01 == "Male", ESCS =="Low", PVREAD>=expertisedf$PVREAD[1])
  nrow(temp)/nrow(data %>% filter(COUNTRY==x))*100
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender="Male", Performers="High", ESCS="Low", Average=HighPerformersEscsLowMale, GenderESCS="MaleLow")
df<-rbind(df, dm)
#3 Low Performers Escs High
LowPerformersEscsHighMale<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ST04Q01 == "Male", ESCS =="High", PVREAD<=expertisedf$PVREAD[2])
  nrow(temp)/nrow(data %>% filter(COUNTRY==x))*100
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender="Male", Performers="Low", ESCS="High", Average=LowPerformersEscsHighMale, GenderESCS="MaleHigh")
df<-rbind(df, dm)
#4 Low Performers Escs Low
LowPerformersEscsLowMale<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ST04Q01 == "Male", ESCS =="Low", PVREAD<=expertisedf$PVREAD[2])
  nrow(temp)/nrow(data %>% filter(COUNTRY==x))*100
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender="Male", Performers="Low", ESCS="Low", Average=LowPerformersEscsLowMale, GenderESCS="MaleLow")
df<-rbind(df, dm)
#5 Low Performers Escs Medium
LowPerformersEscMediumMale<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ST04Q01 == "Male", ESCS =="Medium", PVREAD<=expertisedf$PVREAD[2])
  nrow(temp)/nrow(data %>% filter(COUNTRY==x))*100
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender="Male", Performers="Low", ESCS="Medium", Average=LowPerformersEscMediumMale, GenderESCS="MaleMedium")
df<-rbind(df, dm)
#6 High Performers Escs Medium
HighPerformersEscsMediumMale<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x,ST04Q01 == "Male", ESCS =="Medium", PVREAD<=expertisedf$PVREAD[2])
  nrow(temp)/nrow(data %>% filter(COUNTRY==x))*100
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender="Male", Performers="High", ESCS="Medium", Average=HighPerformersEscsMediumMale, GenderESCS="MaleMedium")
df<-rbind(df, dm)
##### Female ESCS Proficiency ##### 
HighPerformersEscsHighFemale<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ST04Q01 == "Female", ESCS =="High", PVREAD>=expertisedf$PVREAD[1])
  nrow(temp)/nrow(data %>% filter(COUNTRY==x))*100
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender="Female", Performers="High", ESCS="High", Average=HighPerformersEscsHighFemale, GenderESCS="FemaleHigh")
df<-rbind(df, dm)
#2 High Performers Escs Low
HighPerformersEscsLowFemale<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ST04Q01 == "Female", ESCS =="Low", PVREAD>=expertisedf$PVREAD[1])
  nrow(temp)/nrow(data %>% filter(COUNTRY==x))*100
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender="Female", Performers="High", ESCS="Low", Average=HighPerformersEscsLowFemale, GenderESCS="FemaleLow")
df<-rbind(df, dm)
#3 Low Performers Escs High
LowPerformersEscsHighFemale<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ST04Q01 == "Female", ESCS =="High", PVREAD<=expertisedf$PVREAD[2])
  nrow(temp)/nrow(data %>% filter(COUNTRY==x))*100
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender="Female", Performers="Low", ESCS="High", Average=LowPerformersEscsHighFemale, GenderESCS="FemaleHigh")
df<-rbind(df, dm)
#4 Low Performers Escs Low
LowPerformersEscsLowFemale<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ST04Q01 == "Female", ESCS =="Low", PVREAD<=expertisedf$PVREAD[2])
  nrow(temp)/nrow(data %>% filter(COUNTRY==x))*100
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender="Female", Performers="Low", ESCS="Low", Average=LowPerformersEscsLowFemale, GenderESCS="FemaleLow")
df<-rbind(df, dm)
#5 Low Performers Escs Medium
LowPerformersEscMediumFemale<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ST04Q01 == "Female", ESCS =="Medium", PVREAD<=expertisedf$PVREAD[2])
  nrow(temp)/nrow(data %>% filter(COUNTRY==x))*100
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender="Female", Performers="Low", ESCS="Medium", Average=LowPerformersEscMediumFemale, GenderESCS="FemaleMedium")
df<-rbind(df, dm)
#6 High Performers Escs Medium
HighPerformersEscsMediumFemale<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x,ST04Q01 == "Female", ESCS =="Medium", PVREAD<=expertisedf$PVREAD[2])
  nrow(temp)/nrow(data %>% filter(COUNTRY==x))*100
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender="Female", Performers="High", ESCS="Medium", Average=HighPerformersEscsMediumFemale, GenderESCS="FemaleMedium")
df<-rbind(df, dm)
#### ESCS Gender #####

#17 Low Escs Mean Male
LowEscsMeanMale<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ST04Q01 == "Male", ESCS =="Low") %>% select(PVREAD, W_FSTUWT)
  weighted.mean(x=temp[,"PVREAD"], w=(temp[,"W_FSTUWT"]))
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender="Male", Performers=0, ESCS="Low", Average=LowEscsMeanMale, GenderESCS="MaleLow")
df<-rbind(df, dm)
#18 Low Escs Mean Female
LowEscsMeanFemale<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ST04Q01 == "Female", ESCS =="Low") %>% select(PVREAD, W_FSTUWT)
  weighted.mean(x=temp[,"PVREAD"], w=(temp[,"W_FSTUWT"]))
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender="Female", Performers=0, ESCS="Low", Average=LowEscsMeanFemale, GenderESCS="FemaleLow")
df<-rbind(df, dm)
#19 Medium Escs Mean Male
MediumEscsMeanMale<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ST04Q01 == "Male", ESCS =="Medium") %>% select(PVREAD, W_FSTUWT)
  weighted.mean(x=temp[,"PVREAD"], w=(temp[,"W_FSTUWT"]))
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender="Male", Performers=0, ESCS="Medium", Average=MediumEscsMeanMale, GenderESCS="MaleMedium")
df<-rbind(df, dm)
#20 Medium Escs Mean Female
MediumEscsMeanFemale<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ST04Q01 == "Female", ESCS =="Medium") %>% select(PVREAD, W_FSTUWT)
  weighted.mean(x=temp[,"PVREAD"], w=(temp[,"W_FSTUWT"]))
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender="Female", Performers=0, ESCS="Medium", Average=MediumEscsMeanFemale, GenderESCS="FemaleMedium")
df<-rbind(df, dm)
#21 High Escs Mean Male
HighEscsMeanMale<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ST04Q01 == "Male", ESCS =="High") %>% select(PVREAD, W_FSTUWT)
  weighted.mean(x=temp[,"PVREAD"], w=(temp[,"W_FSTUWT"]))
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender="Male", Performers=0, ESCS="High", Average=HighEscsMeanMale, GenderESCS="MaleHigh")
df<-rbind(df, dm)
#22 High Escs Mean Female
HighEscsMeanFemale<-sapply(countries, function(x){
  temp<-data %>% filter(COUNTRY==x, ST04Q01 == "Female", ESCS  =="High") %>% select(PVREAD, W_FSTUWT)
  weighted.mean(x=temp[,"PVREAD"], w=(temp[,"W_FSTUWT"]))
})
dm<-data.frame(Year=2015, Country=countries, Subject="Reading", Gender="Female", Performers=0, ESCS="High", Average=HighEscsMeanFemale, GenderESCS="FemaleHigh")
df<-rbind(df, dm)

