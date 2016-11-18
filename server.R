#PisaSelectIndex<-read.csv("data/PisaSelectIndex.csv", header = TRUE, sep=",")
load("data/pisaData2.rda")

#download.file(url = "https://docs.google.com/spreadsheets/d/1aNfTVO9PGfNicmrdLHPdCtkOEyhCekLFdBpFPbgagg0/pub?gid=991054399&single=true&output=csv", destfile="data/pisaDictionary.csv", 'curl')
pisaDictionary<-read.csv("data/pisaDictionary.csv", header = TRUE, sep=",")
#load("data/pisaDictionary.rda")

oecdCountries<-read.csv("data/oecdCountries.csv", header = TRUE, sep=",")
oecdList<-oecdCountries$CNT
names(oecdList)<-oecdCountries$Hebrew
israelCountries<-read.csv("data/israelCountries.csv", header = TRUE, sep=",")
israelList<-israelCountries$CNT
names(israelList)<-israelCountries$Hebrew
Countries<-read.csv("data/countries.csv", header = TRUE, sep=",")
countriesList<-Countries$CNT
names(countriesList)<-Countries$Hebrew
ExpertiseLevels<-read.csv("data/ExpertiseLevels.csv", header = TRUE, sep=",")
ExpertiseLevelsLimits<-read.csv("data/expertiseLevelsLimits.csv", header = TRUE, sep=",")
#download.file(url = "https://docs.google.com/spreadsheets/d/1RlZdX9bp4d-CxGd-bpR85Ye0tN5_CAdCCqnjqTC1i0E/pub?gid=439183945&single=true&output=csv", destfile="data/LevelExplenation.csv", 'curl')
LevelExplenation<-read.csv("data/LevelExplenation.csv", header = TRUE, sep=",")

#load(url("https://storage.googleapis.com/opisa/student2012b.rda"))
#pisa2012<-read.csv(url("https://storage.googleapis.com/opisa/pisa2012.csv"))
# load("../pisa2012.rda")

pisadb<-src_bigquery("r-shiny-1141", "pisa")
pisa2012<- tbl(pisadb, "pisa2012")
pisa2009<- tbl(pisadb, "pisa2009")
pisa2006<- tbl(pisadb, "pisa2006")


groupColours<- c(
  General="#b276b2", 
  Male="#5da5da", 
  Female="#f17cb0", 
  GeneralLow="#bc99c7", 
  GeneralMedium="#b276b2", 
  GeneralHigh="#7b3a96", 
  MaleHigh="#265dab", 
  MaleLow="#88bde6", 
  MaleMedium="#5da5da", 
  FemaleHigh="#e5126f", 
  FemaleLow="#f6aac9", 
  FemaleMedium="#f17cb0"
) 

shinyServer(function(input, output, session) {
  
  source('pisa.scores.R', local=TRUE)
  source('pisa.expertise.R', local=TRUE)
  source('pisa.survey.R', local=TRUE)
  source('pisa.analyze.R', local=TRUE)
  source('urlSearch.R', local=TRUE)


  
})


