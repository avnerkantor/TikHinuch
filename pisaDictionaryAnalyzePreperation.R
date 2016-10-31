

d<-pisaDictionary%>%group_by(ID)%>%filter(n()>1)%>%select(Year, ID, Measure)%>%mutate(n=n())

#There are different measures with the same Id. for example
pisaDictionary%>%filter(ID=="ST26Q01")

