# import dataframe
nextstrain <- read.csv("nextstrain.csv")

# organize dataframe
nextstrain<-nextstrain[-c(1,3,4,6)]
nextstrain<-na.omit(nextstrain)
nextstrain$Collection_Date<-as.Date(nextstrain$Collection_Date)
names(nextstrain)<-c("coldate", "variant")

# name pango lineage to WHO nomenclature 
nextstrain$variant[nextstrain$variant=="B.1.1.7"] <- "Alpha"
nextstrain$variant[nextstrain$variant=="B.1.617.2"] <- "Delta"
nextstrain$variant[nextstrain$variant=="B.1.1.529"] <- "Omicron"
nextstrain$variant[startsWith(nextstrain$variant, "Q.")] <- "Alpha"
nextstrain$variant[startsWith(nextstrain$variant, "B.1.351")]<-"Beta"
nextstrain$variant[startsWith(nextstrain$variant, "P.1")]<-"Gamma"
nextstrain$variant[startsWith(nextstrain$variant, "AY.")]<-"Delta"
nextstrain$variant[startsWith(nextstrain$variant, "BA.")]<-"Omicron"
nextstrain$variant[nextstrain$variant!="Alpha"&nextstrain$variant!="Beta"&nextstrain$variant!="Gamma"&nextstrain$variant!="Delta"&nextstrain$variant!="Omicron"]<-"other"

# sort by count
nextstrain$coldate<-as.Date(nextstrain$coldate)
nextstrain$variant<-as.character(nextstrain$variant)

library(dplyr)
nextstrain<-nextstrain%>%group_by(coldate, variant)%>%mutate(count=n())

# remove duplicated rows
nextstrain<-nextstrain[!duplicated(nextstrain),]

## alternatively: nextstrain<-aggregate(nextstrain$variant, by=list(nextstrain$country, nextstrain$coldate, nextstrain$variant), FUN=length)

# export csv

# parse ISO8601 
gisaid<-data.frame(gisaid, str_split_fixed(gisaid$coldate, "-", 3))
names(gisaid)<-c("country", "coldate", "variant", "count", "year", "month", "day")
write.csv(gisaid, "/home/lucyhui/parsed-gisaid.csv", row.names = FALSE, col.names = TRUE)

nextstrain<-data.frame(nextstrain, str_split_fixed(nextstrain$coldate, "-", 3))
names(nextstrain)<-c("coldate", "country", "variant", "count", "year", "month", "day")
write.csv(nextstrain, "/home/lucyhui/parsed-nextstrain.csv", row.names = FALSE, col.names = TRUE)

# total by collection date
n <-aggregate(nextstrain$count, by=list(nextstrain$coldate), FUN=sum)
names(n) <- c("coldate", "total")
g <- aggregate(gisaid$count, by=list(gisaid$coldate), FUN=sum)
names(g) <- c("coldate", "total")

nextstrain <- merge(nextstrain, n, by="coldate")
gisaid <- merge(gisaid, g, by="coldate")

nextstrain$percent <- nextstrain$count / nextstrain$total
gisaid$percent <- gisaid$count / gisaid$total





# parse GISAID
gisaid <- read.csv("GISAID.csv")
gisaid<-gisaid[-c(1,3,4,6)]
gisaid<-na.omit(gisaid)
gisaid$Collection_Date<-as.Date(gisaid$Collection_Date)
names(gisaid)<-c("coldate", "variant")

gisaid$variant[gisaid$variant=="B.1.1.7"] <- "Alpha"
gisaid$variant[gisaid$variant=="B.1.617.2"] <- "Delta"
gisaid$variant[gisaid$variant=="B.1.1.529"] <- "Omicron"
gisaid$variant[startsWith(gisaid$variant, "Q.")] <- "Alpha"
gisaid$variant[startsWith(gisaid$variant, "B.1.351")]<-"Beta"
gisaid$variant[startsWith(gisaid$variant, "P.1")]<-"Gamma"
gisaid$variant[startsWith(gisaid$variant, "AY.")]<-"Delta"
gisaid$variant[startsWith(gisaid$variant, "BA.")]<-"Omicron"
gisaid$variant[gisaid$variant!="Alpha"&gisaid$variant!="Beta"&gisaid$variant!="Gamma"&gisaid$variant!="Delta"&gisaid$variant!="Omicron"]<-"other"

gisaid$coldate<-as.Date(gisaid$coldate)
gisaid$variant<-as.character(gisaid$variant)

library(dplyr)
gisaid<-gisaid%>%group_by(coldate, variant)%>%mutate(count=n())

gisaid<-gisaid[!duplicated(gisaid),]

g <- aggregate(gisaid$count, by=list(gisaid$coldate), FUN=sum)
names(g) <- c("coldate", "total")
gisaid <- merge(gisaid, g, by="coldate")
gisaid$percent <- gisaid$count / gisaid$total











# subdate cutoffs
## gisaid
gisaid <- read.csv("GISAID.csv")
gisaid<-gisaid[-c(1,4,6)]
gisaid<-na.omit(gisaid)
gisaid$Collection_Date<-as.Date(gisaid$Collection_Date)
gisaid$Submission_Date<-as.Date(gisaid$Submission_Date)
names(gisaid)<-c("coldate", "subdate", "variant")

gisaid$variant[gisaid$variant=="B.1.1.7"] <- "Alpha"
gisaid$variant[gisaid$variant=="B.1.617.2"] <- "Delta"
gisaid$variant[gisaid$variant=="B.1.1.529"] <- "Omicron"
gisaid$variant[startsWith(gisaid$variant, "Q.")] <- "Alpha"
gisaid$variant[startsWith(gisaid$variant, "B.1.351")]<-"Beta"
gisaid$variant[startsWith(gisaid$variant, "P.1")]<-"Gamma"
gisaid$variant[startsWith(gisaid$variant, "AY.")]<-"Delta"
gisaid$variant[startsWith(gisaid$variant, "BA.")]<-"Omicron"
gisaid$variant[gisaid$variant!="Alpha"&gisaid$variant!="Beta"&gisaid$variant!="Gamma"&gisaid$variant!="Delta"&gisaid$variant!="Omicron"]<-"other"

gisaid$coldate<-as.Date(gisaid$coldate)
gisaid$subdate<-as.Date(gisaid$subdate)
gisaid$variant<-as.character(gisaid$variant)

library(dplyr)
gisaid<-gisaid%>%group_by(coldate, subdate, variant)%>%mutate(count=n())
gisaid<-gisaid[!duplicated(gisaid),]

g <- aggregate(gisaid$count, by=list(gisaid$coldate), FUN=sum)
names(g) <- c("coldate", "total")
gisaid <- merge(gisaid, g, by="coldate")
gisaid$percent <- gisaid$count / gisaid$total

## nextstrain
nextstrain <- read.csv("nextstrain.csv")
nextstrain<-nextstrain[-c(1,4,6)]
nextstrain<-na.omit(nextstrain)
nextstrain$Collection_Date<-as.Date(nextstrain$Collection_Date)
nextstrain$Submission_Date<-as.Date(nextstrain$Submission_Date)
names(nextstrain)<-c("coldate", "subdate", "variant")

nextstrain$variant[nextstrain$variant=="B.1.1.7"] <- "Alpha"
nextstrain$variant[nextstrain$variant=="B.1.617.2"] <- "Delta"
nextstrain$variant[nextstrain$variant=="B.1.1.529"] <- "Omicron"
nextstrain$variant[startsWith(nextstrain$variant, "Q.")] <- "Alpha"
nextstrain$variant[startsWith(nextstrain$variant, "B.1.351")]<-"Beta"
nextstrain$variant[startsWith(nextstrain$variant, "P.1")]<-"Gamma"
nextstrain$variant[startsWith(nextstrain$variant, "AY.")]<-"Delta"
nextstrain$variant[startsWith(nextstrain$variant, "BA.")]<-"Omicron"
nextstrain$variant[nextstrain$variant!="Alpha"&nextstrain$variant!="Beta"&nextstrain$variant!="Gamma"&nextstrain$variant!="Delta"&nextstrain$variant!="Omicron"]<-"other"

nextstrain$coldate<-as.Date(nextstrain$coldate)
nextstrain$subdate<-as.Date(nextstrain$subdate)
nextstrain$variant<-as.character(nextstrain$variant)

library(dplyr)
nextstrain<-nextstrain%>%group_by(coldate, subdate, variant)%>%mutate(count=n())
nextstrain<-nextstrain[!duplicated(nextstrain),]

g <- aggregate(nextstrain$count, by=list(nextstrain$coldate), FUN=sum)
names(g) <- c("coldate", "total")
nextstrain <- merge(nextstrain, g, by="coldate")
nextstrain$percent <- nextstrain$count / nextstrain$total
