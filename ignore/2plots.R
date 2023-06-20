## Scatterplot
# Alpha
Nalpha<-data.frame(x=nextstrain$date[nextstrain$Nextclade_pango=="B.1.1.7"], nextstrain$date_submitted[nextstrain$Nextclade_pango=="B.1.1.7"])
names(Nalpha)<-c("Collection_Date", "Submission_Date")
Galpha<-data.frame(GISAID$Collection.date[GISAID$Pango.lineage=="B.1.1.7"], GISAID$Submission.date[GISAID$Pango.lineage=="B.1.1.7"])
names(Galpha)<-c("Collection_Date", "Submission_Date")

Nalpha$Collection_Date<-as.Date(Nalpha$Collection_Date)
Nalpha$Submission_Date<-as.Date(Nalpha$Submission_Date)
Galpha$Collection_Date<-as.Date(Galpha$Collection_Date)
Galpha$Submission_Date<-as.Date(Galpha$Submission_Date)

"Delay"<-Nalpha$Submission_Date-Nalpha$Collection_Date
Nalpha<-data.frame(cbind(Nalpha, Delay))
"Delay"<-Galpha$Submission_Date-Galpha$Collection_Date
Galpha<-data.frame(cbind(Galpha, Delay))

ggp<-ggplot(NULL,aes(Collection_Date, Delay))+geom_point(data=Nalpha[sample(nrow(Nalpha), 500),], col="pink")+geom_point(data=Galpha[sample(nrow(Nalpha), 500),], col="skyblue")
ggp

# Beta
Nbeta<-data.frame(x=nextstrain$date[nextstrain$Nextclade_pango=="B.1.351"], nextstrain$date_submitted[nextstrain$Nextclade_pango=="B.1.351"])
names(Nbeta)<-c("Collection_Date", "Submission_Date")
Gbeta<-data.frame(GISAID$Collection.date[GISAID$Pango.lineage=="B.1.351"], GISAID$Submission.date[GISAID$Pango.lineage=="B.1.351"])
names(Gbeta)<-c("Collection_Date", "Submission_Date")

Nbeta$Collection_Date<-as.Date(Nbeta$Collection_Date, format="%Y-%m-%d")
Nbeta$Submission_Date<-as.Date(Nbeta$Submission_Date)
Gbeta$Collection_Date<-as.Date(Gbeta$Collection_Date)
Gbeta$Submission_Date<-as.Date(Gbeta$Submission_Date)

"Delay"<-Nbeta$Submission_Date-Nbeta$Collection_Date
Nbeta<-data.frame(cbind(Nbeta, Delay))
"Delay"<-Gbeta$Submission_Date-Gbeta$Collection_Date
Gbeta<-data.frame(cbind(Gbeta, Delay))

ggp<-ggplot(NULL,aes(Collection_Date, Delay))+geom_point(data=Nbeta[sample(nrow(Nbeta), 500),], col="pink")+geom_point(data=Gbeta[sample(nrow(Gbeta), 500),], col="skyblue")
ggp

# Gamma
Ngamma<-data.frame(x=nextstrain$date[nextstrain$Nextclade_pango=="P.1"], nextstrain$date_submitted[nextstrain$Nextclade_pango=="P.1"])
names(Ngamma)<-c("Collection_Date", "Submission_Date")
Ggamma<-data.frame(GISAID$Collection.date[GISAID$Pango.lineage=="P.1"], GISAID$Submission.date[GISAID$Pango.lineage=="P.1"])
names(Ggamma)<-c("Collection_Date", "Submission_Date")

Ngamma$Collection_Date<-as.Date(Ngamma$Collection_Date)
Ngamma$Submission_Date<-as.Date(Ngamma$Submission_Date)
Ggamma$Collection_Date<-as.Date(Ggamma$Collection_Date)
Ggamma$Submission_Date<-as.Date(Ggamma$Submission_Date)

"Delay"<-Ngamma$Submission_Date-Ngamma$Collection_Date
Ngamma<-data.frame(cbind(Ngamma, Delay))
"Delay"<-Ggamma$Submission_Date-Ggamma$Collection_Date
Ggamma<-data.frame(cbind(Ggamma, Delay))

ggp<-ggplot(NULL,aes(Collection_Date, Delay))+geom_point(data=Ngamma[sample(nrow(Ngamma), 500),], col="pink")+geom_point(data=Ggamma[sample(nrow(Ggamma), 500),], col="skyblue")
ggp

# Delta
Ndelta<-data.frame(x=nextstrain$date[nextstrain$Nextclade_pango=="B.1.617.2"], nextstrain$date_submitted[nextstrain$Nextclade_pango=="B.1.617.2"])
names(Ndelta)<-c("Collection_Date", "Submission_Date")
Gdelta<-data.frame(GISAID$Collection.date[GISAID$Pango.lineage=="B.1.617.2"], GISAID$Submission.date[GISAID$Pango.lineage=="B.1.617.2"])
names(Gdelta)<-c("Collection_Date", "Submission_Date")

Ndelta$Collection_Date<-as.Date(Ndelta$Collection_Date)
Ndelta$Submission_Date<-as.Date(Ndelta$Submission_Date)
Gdelta$Collection_Date<-as.Date(Gdelta$Collection_Date)
Gdelta$Submission_Date<-as.Date(Gdelta$Submission_Date)

"Delay"<-Ndelta$Submission_Date-Ndelta$Collection_Date
Ndelta<-data.frame(cbind(Ndelta, Delay))
"Delay"<-Gdelta$Submission_Date-Gdelta$Collection_Date
Gdelta<-data.frame(cbind(Gdelta, Delay))

ggp<-ggplot(NULL,aes(Collection_Date, Delay))+geom_point(data=Ndelta[sample(nrow(Ndelta), 500),], col="pink")+geom_point(data=Gdelta[sample(nrow(Gdelta), 500),], col="skyblue")
ggp

# Omicron BA.1
Nomicron<-data.frame(x=nextstrain$date[nextstrain$Nextclade_pango=="BA.1"], nextstrain$date_submitted[nextstrain$Nextclade_pango=="BA.1"])
names(Nomicron)<-c("Collection_Date", "Submission_Date")
Gomicron<-data.frame(GISAID$Collection.date[GISAID$Pango.lineage=="BA.1"], GISAID$Submission.date[GISAID$Pango.lineage=="BA.1"])
names(Gomicron)<-c("Collection_Date", "Submission_Date")

Nomicron$Collection_Date<-as.Date(Nomicron$Collection_Date, format="%Y-%m-%d")
Nomicron$Submission_Date<-as.Date(Nomicron$Submission_Date)
Gomicron$Collection_Date<-as.Date(Gomicron$Collection_Date)
Gomicron$Submission_Date<-as.Date(Gomicron$Submission_Date)

"Delay"<-Nomicron$Submission_Date-Nomicron$Collection_Date
Nomicron<-data.frame(cbind(Nomicron, Delay))
"Delay"<-Gomicron$Submission_Date-Gomicron$Collection_Date
Gomicron<-data.frame(cbind(Gomicron, Delay))

ggp<-ggplot(NULL,aes(Collection_Date, Delay))+geom_point(data=Nomicron[sample(nrow(Nomicron), 500),], col="pink")+geom_point(data=Gomicron[sample(nrow(Gomicron), 500),], col="skyblue")
ggp







## Boxplot
# Alpha
Nalpha<-data.frame(x=nextstrain$date[nextstrain$Nextclade_pango=="B.1.1.7"], nextstrain$date_submitted[nextstrain$Nextclade_pango=="B.1.1.7"])
names(Nalpha)<-c("Collection_Date", "Submission_Date")
Galpha<-data.frame(GISAID$Collection.date[GISAID$Pango.lineage=="B.1.1.7"], GISAID$Submission.date[GISAID$Pango.lineage=="B.1.1.7"])
names(Galpha)<-c("Collection_Date", "Submission_Date")

Nalpha$Collection_Date<-as.Date(Nalpha$Collection_Date)
Nalpha$Submission_Date<-as.Date(Nalpha$Submission_Date)
Galpha$Collection_Date<-as.Date(Galpha$Collection_Date)
Galpha$Submission_Date<-as.Date(Galpha$Submission_Date)

"Delay"<-Nalpha$Submission_Date-Nalpha$Collection_Date
Nalpha<-data.frame(cbind(Nalpha, Delay))
"Delay"<-Galpha$Submission_Date-Galpha$Collection_Date
Galpha<-data.frame(cbind(Galpha, Delay))

a<-data.frame(Nalpha[sample(nrow(Nalpha), 500),], rep("Nextstrain",500))
names(a)<-c("Collection_Date", "Submission_Date", "Delay", "Database")
b<-data.frame(Galpha[sample(nrow(Galpha), 500),], rep("GISAID",500))
names(b)<-c("Collection_Date", "Submission_Date", "Delay", "Database")

c<-rbind(a,b)

ggplot(c,aes(y=Delay, fill=Database))+geom_boxplot()+scale_x_discrete()

# Beta
Nbeta<-data.frame(x=nextstrain$date[nextstrain$Nextclade_pango=="B.1.351"], nextstrain$date_submitted[nextstrain$Nextclade_pango=="B.1.351"])
names(Nbeta)<-c("Collection_Date", "Submission_Date")
Gbeta<-data.frame(GISAID$Collection.date[GISAID$Pango.lineage=="B.1.351"], GISAID$Submission.date[GISAID$Pango.lineage=="B.1.351"])
names(Gbeta)<-c("Collection_Date", "Submission_Date")

Nbeta$Collection_Date<-as.Date(Nbeta$Collection_Date, format="%Y-%m-%d")
Nbeta$Submission_Date<-as.Date(Nbeta$Submission_Date)
Gbeta$Collection_Date<-as.Date(Gbeta$Collection_Date)
Gbeta$Submission_Date<-as.Date(Gbeta$Submission_Date)

"Delay"<-Nbeta$Submission_Date-Nbeta$Collection_Date
Nbeta<-data.frame(cbind(Nbeta, Delay))
"Delay"<-Gbeta$Submission_Date-Gbeta$Collection_Date
Gbeta<-data.frame(cbind(Gbeta, Delay))

a<-data.frame(Nbeta[sample(nrow(Nbeta), 500),], rep("Nextstrain",500))
names(a)<-c("Collection_Date", "Submission_Date", "Delay", "Database")
b<-data.frame(Gbeta[sample(nrow(Gbeta), 500),], rep("GISAID",500))
names(b)<-c("Collection_Date", "Submission_Date", "Delay", "Database")

c<-rbind(a,b)

ggplot(c, aes(y=Delay, fill=Database))+geom_boxplot()+scale_x_discrete()

# Gamma
Ngamma<-data.frame(x=nextstrain$date[nextstrain$Nextclade_pango=="P.1"], nextstrain$date_submitted[nextstrain$Nextclade_pango=="P.1"])
names(Ngamma)<-c("Collection_Date", "Submission_Date")
Ggamma<-data.frame(GISAID$Collection.date[GISAID$Pango.lineage=="P.1"], GISAID$Submission.date[GISAID$Pango.lineage=="P.1"])
names(Ggamma)<-c("Collection_Date", "Submission_Date")

Ngamma$Collection_Date<-as.Date(Ngamma$Collection_Date)
Ngamma$Submission_Date<-as.Date(Ngamma$Submission_Date)
Ggamma$Collection_Date<-as.Date(Ggamma$Collection_Date)
Ggamma$Submission_Date<-as.Date(Ggamma$Submission_Date)

"Delay"<-Ngamma$Submission_Date-Ngamma$Collection_Date
Ngamma<-data.frame(cbind(Ngamma, Delay))
"Delay"<-Ggamma$Submission_Date-Ggamma$Collection_Date
Ggamma<-data.frame(cbind(Ggamma, Delay))

a<-data.frame(Ngamma[sample(nrow(Ngamma), 500),], rep("Nextstrain",500))
names(a)<-c("Collection_Date", "Submission_Date", "Delay", "Database")
b<-data.frame(Ggamma[sample(nrow(Ggamma), 500),], rep("GISAID",500))
names(b)<-c("Collection_Date", "Submission_Date", "Delay", "Database")

c<-rbind(a,b)

ggplot(c, aes(y=Delay, fill=Database))+geom_boxplot()+scale_x_discrete()

# Delta
Ndelta<-data.frame(x=nextstrain$date[nextstrain$Nextclade_pango=="B.1.617.2"], nextstrain$date_submitted[nextstrain$Nextclade_pango=="B.1.617.2"])
names(Ndelta)<-c("Collection_Date", "Submission_Date")
Gdelta<-data.frame(GISAID$Collection.date[GISAID$Pango.lineage=="B.1.617.2"], GISAID$Submission.date[GISAID$Pango.lineage=="B.1.617.2"])
names(Gdelta)<-c("Collection_Date", "Submission_Date")

Ndelta$Collection_Date<-as.Date(Ndelta$Collection_Date)
Ndelta$Submission_Date<-as.Date(Ndelta$Submission_Date)
Gdelta$Collection_Date<-as.Date(Gdelta$Collection_Date)
Gdelta$Submission_Date<-as.Date(Gdelta$Submission_Date)

"Delay"<-Ndelta$Submission_Date-Ndelta$Collection_Date
Ndelta<-data.frame(cbind(Ndelta, Delay))
"Delay"<-Gdelta$Submission_Date-Gdelta$Collection_Date
Gdelta<-data.frame(cbind(Gdelta, Delay))

a<-data.frame(Ndelta[sample(nrow(Ndelta), 500),], rep("Nextstrain",500))
names(a)<-c("Collection_Date", "Submission_Date", "Delay", "Database")
b<-data.frame(Gdelta[sample(nrow(Gdelta), 500),], rep("GISAID",500))
names(b)<-c("Collection_Date", "Submission_Date", "Delay", "Database")

a$Submission_Date<-as.Date(a$Submission_Date)
b$Submission_Date<-as.Date(b$Submission_Date)

c<-rbind(a,b)

ggplot(c, aes(y=Delay, fill=Database))+geom_boxplot()+scale_x_discrete()

# Omicron
Nomicron<-data.frame(x=nextstrain$date[nextstrain$Nextclade_pango=="BA.1"], nextstrain$date_submitted[nextstrain$Nextclade_pango=="BA.1"])
names(Nomicron)<-c("Collection_Date", "Submission_Date")
Gomicron<-data.frame(GISAID$Collection.date[GISAID$Pango.lineage=="BA.1"], GISAID$Submission.date[GISAID$Pango.lineage=="BA.1"])
names(Gomicron)<-c("Collection_Date", "Submission_Date")

Nomicron$Collection_Date<-as.Date(Nomicron$Collection_Date)
Nomicron$Submission_Date<-as.Date(Nomicron$Submission_Date)
Gomicron$Collection_Date<-as.Date(Gomicron$Collection_Date)
Gomicron$Submission_Date<-as.Date(Gomicron$Submission_Date)

"Delay"<-Nomicron$Submission_Date-Nomicron$Collection_Date
Nomicron<-data.frame(cbind(Nomicron, Delay))
"Delay"<-Gomicron$Submission_Date-Gomicron$Collection_Date
Gomicron<-data.frame(cbind(Gomicron, Delay))

a<-data.frame(Nomicron[sample(nrow(Nomicron), 500),], rep("Nextstrain",500))
names(a)<-c("Collection_Date", "Submission_Date", "Delay", "Database")
b<-data.frame(Gomicron[sample(nrow(Gomicron), 500),], rep("GISAID",500))
names(b)<-c("Collection_Date", "Submission_Date", "Delay", "Database")

a$Submission_Date<-as.Date(a$Submission_Date)
b$Submission_Date<-as.Date(b$Submission_Date)

c<-rbind(a,b)

ggplot(c, aes(y=Delay, fill=Database))+geom_boxplot()+scale_x_discrete()







## Global
# Scatterplot
nextstrain<-data.frame(x=nextstrain$date, nextstrain$date_submitted)
names(nextstrain)<-c("Collection_Date", "Submission_Date")
gisaid<-data.frame(GISAID$Collection.date, GISAID$Submission.date)
names(gisaid)<-c("Collection_Date", "Submission_Date")

nextstrain$Collection_Date<-as.Date(nextstrain$Collection_Date, format="%Y-%m-%d")
nextstrain$Submission_Date<-as.Date(nextstrain$Submission_Date)
gisaid$Collection_Date<-as.Date(gisaid$Collection_Date)
gisaid$Submission_Date<-as.Date(gisaid$Submission_Date)

"Delay"<-nextstrain$Submission_Date-nextstrain$Collection_Date
nextstrain<-data.frame(cbind(nextstrain, Delay))
"Delay"<-gisaid$Submission_Date-gisaid$Collection_Date
gisaid<-data.frame(cbind(gisaid, Delay))

ggp<-ggplot(NULL,aes(Collection_Date, Delay))+geom_point(data=nextstrain[sample(nrow(nextstrain), 500),], col="pink")+geom_point(data=gisaid[sample(nrow(gisaid), 500),], col="skyblue")
ggp

# Boxplot
N<-data.frame(x=nextstrain$date, nextstrain$date_submitted)
names(N)<-c("Collection_Date", "Submission_Date")
G<-data.frame(GISAID$Collection.date, GISAID$Submission.date)
names(G)<-c("Collection_Date", "Submission_Date")

N$Collection_Date<-as.Date(N$Collection_Date, format="%Y-%m-%d")
N$Submission_Date<-as.Date(N$Submission_Date)
G$Collection_Date<-as.Date(G$Collection_Date)
G$Submission_Date<-as.Date(G$Submission_Date)

"Delay"<-N$Submission_Date-N$Collection_Date
N<-data.frame(cbind(N, Delay))
"Delay"<-G$Submission_Date-G$Collection_Date
G<-data.frame(cbind(G, Delay))

a<-data.frame(N[sample(nrow(N), 500),], rep("Nextstrain",500))
names(a)<-c("Collection_Date", "Submission_Date", "Delay", "Database")
b<-data.frame(G[sample(nrow(G), 500),], rep("GISAID",500))
names(b)<-c("Collection_Date", "Submission_Date", "Delay", "Database")

c<-rbind(a,b)

ggplot(c,aes(y=Delay, fill=Database))+geom_boxplot()+scale_x_discrete()









### Country
## Scatterplot
# GISAID
library(stringr)
a<-as.data.frame(str_split_fixed(GISAID$Location, " / ", 3))
names(a)<-c("Continent", "Country", "Region")
GISAID<-data.frame(cbind(GISAID,a))

Nalpha<-data.frame(nextstrain$date, nextstrain$date_submitted, nextstrain$country)
names(Nalpha)<-c("Collection_Date", "Submission_Date", "Country")
Galpha<-data.frame(GISAID$Collection.date, GISAID$Submission.date, GISAID$Country)
names(Galpha)<-c("Collection_Date", "Submission_Date", "Country")

Nalpha$Collection_Date<-as.Date(Nalpha$Collection_Date, format="%Y-%m-%d")
Nalpha$Submission_Date<-as.Date(Nalpha$Submission_Date)
Galpha$Collection_Date<-as.Date(Galpha$Collection_Date)
Galpha$Submission_Date<-as.Date(Galpha$Submission_Date)

"Delay"<-Nalpha$Submission_Date-Nalpha$Collection_Date
Nalpha<-data.frame(cbind(Nalpha, Delay))
"Delay"<-Galpha$Submission_Date-Galpha$Collection_Date
Galpha<-data.frame(cbind(Galpha, Delay))

ggp<-ggplot(Galpha,aes(x=Collection_Date, y=Delay, colour=Country))+geom_point()
ggp






# 2D Histogram
library(hexbin)
par(cex=1, cex.axis=1, cex.lab=1)

nextstrain<-data.frame(x=nextstrain$date, nextstrain$date_submitted)
names(nextstrain)<-c("Collection_Date", "Submission_Date")
gisaid<-data.frame(GISAID$Collection.date, GISAID$Submission.date)
names(gisaid)<-c("Collection_Date", "Submission_Date")

nextstrain$Collection_Date<-as.Date(nextstrain$Collection_Date, format="%Y-%m-%d")
nextstrain$Submission_Date<-as.Date(nextstrain$Submission_Date)
gisaid$Collection_Date<-as.Date(gisaid$Collection_Date)
gisaid$Submission_Date<-as.Date(gisaid$Submission_Date)

"Delay"<-nextstrain$Submission_Date-nextstrain$Collection_Date
nextstrain<-data.frame(cbind(nextstrain, Delay))
"Delay"<-gisaid$Submission_Date-gisaid$Collection_Date
gisaid<-data.frame(cbind(gisaid, Delay))

b<-hexbin(nextstrain$Collection_Date, nextstrain$Delay)
plot(b)

c<-hexbin(gisaid$Collection_Date, gisaid$Delay)
plot(c)








