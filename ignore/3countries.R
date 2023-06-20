# Variation in submission delays among countries

## Dataframe
library(ggplot2)
library(stringr)
library(scales) 
nextstrain<-read.csv("nextstrain.csv")
GISAID<-read.csv("GISAID.csv")

# Making dataframes
a<-as.data.frame(str_split_fixed(GISAID$Location, " / ", 3))
names(a)<-c("Continent", "Country", "Region")
GISAID<-data.frame(cbind(GISAID,a))

N<-data.frame(nextstrain$date, nextstrain$date_submitted, nextstrain$country, nextstrain$Nextclade_pango)
names(N)<-c("Collection_Date", "Submission_Date", "Country", "VOC")
G<-data.frame(GISAID$Collection.date, GISAID$Submission.date, GISAID$Country, GISAID$Pango.lineage)
names(G)<-c("Collection_Date", "Submission_Date", "Country", "VOC")

N$Collection_Date<-as.Date(N$Collection_Date, format="%Y-%m-%d")
N$Submission_Date<-as.Date(N$Submission_Date)
G$Collection_Date<-as.Date(G$Collection_Date)
G$Submission_Date<-as.Date(G$Submission_Date)

"Delay"<-N$Submission_Date-N$Collection_Date
N<-data.frame(cbind(N, Delay))
"Delay"<-G$Submission_Date-G$Collection_Date
G<-data.frame(cbind(G, Delay))

write.csv(N, "/home/lucyhui/nextstrain.csv")
write.csv(G, "/home/lucyhui/GISAID.csv")

# Nextstrain
n2 <- aggregate(x=nextstrain$Delay, by=list(nextstrain$Country, nextstrain$VOC), FUN=mean, na.rm=TRUE)
names(n2)<-c("Country", "COV", "Mean Delay")
n3<-subset(n2, COV=="B.1.1.7" | COV=="B.1.351" | COV=="P.1" | COV=="B.1.617.2" | COV=="BA.1")
n4<-reshape(n3,
            idvar="Country",
            v.names="Mean Delay",
            timevar="COV",
            direction="wide")
names(n4)<-c("Country", "Alpha", "Beta", "Delta", "Omicron", "Gamma")

n4<-na.omit(n4)
n4<-as.matrix(n4) 
rownames(n4)<-c("Canada", "Denmark", "France", "Germany", "Italy", "Switzerland", "United Kingdom", "United States")
n4<-n4[,colnames(n4)!="Country"]

Ndmat<-dist(n4, method= "euclidean")
plot(Ndmat)

library(factoextra)
Ndmat2<-get_dist(n4, method="euclidean")
fviz_dist(Ndmat2, show_labels=TRUE, gradient = list(low="red", mid="white", high="blue"))

# GISAID
g2 <- aggregate(x=GISAID$Delay, by=list(GISAID$Country, GISAID$VOC), FUN=mean, na.rm=TRUE)
names(g2)<-c("Country", "COV", "Mean Delay")
g3<-subset(g2, COV=="B.1.1.7" | COV=="B.1.351" | COV=="P.1" | COV=="B.1.617.2" | COV=="BA.1")
g4<-reshape(g3,
            idvar="Country",
            v.names="Mean Delay",
            timevar="COV",
            direction="wide")
names(g4)<-c("Country", "Alpha", "Beta", "Delta", "Omicron", "Gamma")

g4<-na.omit(g4)
g4<-as.matrix(g4) 
rownames(g4)<-c("Angola", "Aruba", "Austrailia", "Austria", "Belgium", "Brazil", "Canada", "Canary Islands", "Chile", "China", "Colombia", "Costa Rica", "Croatia", "Czech Republic", "Denmark", "Finland", "France", "Germany", "Guatemala", "India", "Ireland", "Israel", "Italy", "Japan", "Jordan", "Lithuania", "Luxembourg", "Malta", "Mexico", "Netherlands", "New Zealand", "Norway", "Philippines", "Poland", "Portugal", "Puerto Rico", "Romania", "Singapore", "Solvenia", "South Korea", "Spain", "Sweden", "Switzerland", "Taiwan", "Turkey", "United Kingdom", "United States")
g4<-g4[,colnames(g4)!="Country"]

Gdmat<-dist(g4, method= "euclidean")
plot(Gdmat)

Gdmat2<-get_dist(g4, method="euclidean")
fviz_dist(Gdmat2, show_labels=TRUE, gradient = list(low="red", mid="white", high="blue"))
