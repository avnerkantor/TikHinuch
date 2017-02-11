alonr@vision.bi

bq help load

bq load --autodetect --skip_leading_rows=1 --max_bad_records=100000000 pisa.pisa2015 gs://opisa/pisa2015.csv

bq show pisa.student2012

bq --format=json show pisa.student2012 > student2012.txt

view student2012.txt 


############ Old tries
df<-read.csv(file="s2012.csv")
field.names<-names(df)

field.type<-sapply(df, typeof)
rs<-gsub("character", "STRING", rs)
rs<-gsub("logical", "BOOLEAN", rs)
rs<-gsub("double", "FLOAT", rs)
rs<-gsub("integer", "INTEGER", rs)
#Or
field.type<-sapply(df, class)
rs<-gsub("factor", "STRING", rs)
rs<-gsub("numeric", "FLOAT", rs)
#And
rs<-paste(field.names,field.type,sep=":",collapse=",")

#Other approach
schema_fields <- function(data) {
  types <- vapply(data, data_type, character(1))
  unname(Map(function(type, name) list(name = name, type = type), types, names(data)))
}

data_type <- function(x) {
  if (is.factor(x)) return("STRING")
  if (inherits(x, "POSIXt")) return("TIMESTAMP")
  if (inherits(x, "Date")) return("TIMESTAMP")
  
  switch(typeof(x),
         character = "STRING",
         logical = "BOOLEAN",
         double = "FLOAT",
         integer = "INTEGER",
         stop("Unsupported type: ", typeof(x), call. = FALSE)
  )
}

#Other approach - in GoogleStorage