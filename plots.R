library(ggplot2)
library(scales) 
#n extstrain<-read.delim("metadata.tsv.gz")
# GISAID<-read.delim("gisaid.metadata.tsv.gz)

## Histogram  
## Nextstrain
# Alpha 
alpha<-data.frame(nextstrain$date[nextstrain$Nextclade_pango=="B.1.1.7"], nextstrain$date_submitted[nextstrain$Nextclade_pango=="B.1.1.7"])
names(alpha)<-c("Collection_Date", "Submission_Date")
library(ggplot2)
library(scales) 
alpha$Collection_Date<-as.Date(alpha$Collection_Date)
alpha$Submission_Date<-as.Date(alpha$Submission_Date)
ggplot(alpha, aes(x=Collection_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="3 months")
# OR qplot(x=Collection_Date, data=alpha)
ggplot(alpha, aes(x=Submission_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="3 months")

# Beta 
beta<-data.frame(nextstrain$date[nextstrain$Nextclade_pango=="B.1.351"], nextstrain$date_submitted[nextstrain$Nextclade_pango=="B.1.351"])
names(beta)<-c("Collection_Date", "Submission_Date")
beta$Collection_Date<-as.Date(beta$Collection_Date, format="%Y -%m-%d")
beta$Submission_Date<-as.Date(beta$Submission_Date)
ggplot(beta, aes(x=Collection_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="3 months")
ggplot(beta, aes(x=Submission_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="3 months")

# Gamma
gamma<-data.frame(nextstrain$date[nextstrain$Nextclade_pango=="P.1"], nextstrain$date_submitted[nextstrain$Nextclade_pango=="P.1"])
names(gamma)<-c("Collection_Date", "Submission_Date")
gamma$Collection_Date<-as.Date(gamma$Collection_Date, format="%Y -%m-%d")
gamma$Submission_Date<-as.Date(gamma$Submission_Date)
ggplot(gamma, aes(x=Collection_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="3 months")
ggplot(gamma, aes(x=Submission_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="3 months")

# Delta
delta<-data.frame(nextstrain$date[nextstrain$Nextclade_pango=="B.1.617.2"], nextstrain$date_submitted[nextstrain$Nextclade_pango=="B.1.617.2"])
names(delta)<-c("Collection_Date", "Submission_Date")
delta$Collection_Date<-as.Date(delta$Collection_Date, format="%Y -%m-%d")
delta$Submission_Date<-as.Date(delta$Submission_Date)
ggplot(delta, aes(x=Collection_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="3 months")
ggplot(delta, aes(x=Submission_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="3 months")

# Omicron
omicron<-data.frame(nextstrain$date[nextstrain$Nextclade_pango=="B.1.1.529"], nextstrain$date_submitted[nextstrain$Nextclade_pango=="B.1.1.529"])
names(omicron)<-c("Collection_Date", "Submission_Date")
omicron$Collection_Date<-as.Date(omicron$Collection_Date)
omicron$Submission_Date<-as.Date(omicron$Submission_Date)
ggplot(omicron, aes(x=Collection_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="1 month")
ggplot(omicron, aes(x=Submission_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="1 month")

# Omicron BA.1
omicron1<-data.frame(nextstrain$date[nextstrain$Nextclade_pango=="BA.1"], nextstrain$date_submitted[nextstrain$Nextclade_pango=="BA.1"])
names(omicron1)<-c("Collection_Date", "Submission_Date")
omicron1$Collection_Date<-as.Date(omicron1$Collection_Date)
omicron1$Submission_Date<-as.Date(omicron1$Submission_Date)
ggplot(omicron1, aes(x=Collection_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="1 month")
ggplot(omicron1, aes(x=Submission_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="1 month")

# Omicron BA.2
omicron2<-data.frame(nextstrain$date[nextstrain$Nextclade_pango=="BA.2"], nextstrain$date_submitted[nextstrain$Nextclade_pango=="BA.2"])
names(omicron2)<-c("Collection_Date", "Submission_Date")
omicron2$Collection_Date<-as.Date(omicron2$Collection_Date)
omicron2$Submission_Date<-as.Date(omicron2$Submission_Date)
ggplot(omicron2, aes(x=Collection_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="1 month")
ggplot(omicron2, aes(x=Submission_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="1 month")

# Omicron BA.3
omicron3<-data.frame(nextstrain$date[nextstrain$Nextclade_pango=="BA.3"], nextstrain$date_submitted[nextstrain$Nextclade_pango=="BA.3"])
names(omicron3)<-c("Collection_Date", "Submission_Date")
omicron3$Collection_Date<-as.Date(omicron3$Collection_Date)
omicron3$Submission_Date<-as.Date(omicron3$Submission_Date)
ggplot(omicron3, aes(x=Collection_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="1 month")
ggplot(omicron3, aes(x=Submission_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="1 month")

# Omicron BA.4
omicron4<-data.frame(nextstrain$date[nextstrain$Nextclade_pango=="BA.4"], nextstrain$date_submitted[nextstrain$Nextclade_pango=="BA.4"])
names(omicron4)<-c("Collection_Date", "Submission_Date")
omicron4$Collection_Date<-as.Date(omicron4$Collection_Date)
omicron4$Submission_Date<-as.Date(omicron4$Submission_Date)
ggplot(omicron4, aes(x=Collection_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="1 month")
ggplot(omicron4, aes(x=Submission_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="1 month")

# Omicron BA.5
omicron5<-data.frame(nextstrain$date[nextstrain$Nextclade_pango=="BA.5"], nextstrain$date_submitted[nextstrain$Nextclade_pango=="BA.5"])
names(omicron5)<-c("Collection_Date", "Submission_Date")
omicron5$Collection_Date<-as.Date(omicron5$Collection_Date)
omicron5$Submission_Date<-as.Date(omicron5$Submission_Date)
ggplot(omicron5, aes(x=Collection_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="1 month")
ggplot(omicron5, aes(x=Submission_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="1 month")

## GISAID 
# AlphaG
alphaG<-data.frame(GISAID$Collection.date[GISAID$Pango.lineage=="B.1.1.7"], GISAID$Submission.date[GISAID$Pango.lineage=="B.1.1.7"])
names(alphaG)<-c("Collection_Date", "Submission_Date")
alphaG$Collection_Date<-as.Date(alphaG$Collection_Date)
alphaG$Submission_Date<-as.Date(alphaG$Submission_Date)
ggplot(alphaG, aes(x=Collection_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="3 months")
ggplot(alphaG, aes(x=Submission_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="3 months")

# BetaG
BetaG<-data.frame(GISAID$Collection.date[GISAID$Pango.lineage=="B.1.351"], GISAID$Submission.date[GISAID$Pango.lineage=="B.1.351"])
names(BetaG)<-c("Collection_Date", "Submission_Date")
BetaG$Collection_Date<-as.Date(BetaG$Collection_Date)
BetaG$Submission_Date<-as.Date(BetaG$Submission_Date)
ggplot(BetaG, aes(x=Collection_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="3 months")
ggplot(BetaG, aes(x=Submission_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="3 months")

# GammaG
GammaG<-data.frame(GISAID$Collection.date[GISAID$Pango.lineage=="P.1"], GISAID$Submission.date[GISAID$Pango.lineage=="P.1"])
names(GammaG)<-c("Collection_Date", "Submission_Date")
GammaG$Collection_Date<-as.Date(GammaG$Collection_Date)
GammaG$Submission_Date<-as.Date(GammaG$Submission_Date)
ggplot(GammaG, aes(x=Collection_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="5 months")
ggplot(GammaG, aes(x=Submission_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="3 months")

# DeltaG
DeltaG<-data.frame(GISAID$Collection.date[GISAID$Pango.lineage=="B.1.617.2"], GISAID$Submission.date[GISAID$Pango.lineage=="B.1.617.2"])
names(DeltaG)<-c("Collection_Date", "Submission_Date")
DeltaG$Collection_Date<-as.Date(DeltaG$Collection_Date)
DeltaG$Submission_Date<-as.Date(DeltaG$Submission_Date)
ggplot(DeltaG, aes(x=Collection_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="3 months")
ggplot(DeltaG, aes(x=Submission_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="3 months")

# Omicron BA.1
OmicronG<-data.frame(GISAID$Collection.date[GISAID$Pango.lineage=="BA.1"], GISAID$Submission.date[GISAID$Pango.lineage=="BA.1"])
names(OmicronG)<-c("Collection_Date", "Submission_Date")
OmicronG$Collection_Date<-as.Date(OmicronG$Collection_Date)
OmicronG$Submission_Date<-as.Date(OmicronG$Submission_Date)
ggplot(OmicronG, aes(x=Collection_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="3 months")
ggplot(OmicronG, aes(x=Submission_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="3 months")

# Omicron BA.2
OmicronG2<-data.frame(GISAID$Collection.date[GISAID$Pango.lineage=="BA.2"], GISAID$Submission.date[GISAID$Pango.lineage=="BA.2"])
names(OmicronG2)<-c("Collection_Date", "Submission_Date")
OmicronG2$Collection_Date<-as.Date(OmicronG2$Collection_Date)
OmicronG2$Submission_Date<-as.Date(OmicronG2$Submission_Date)
ggplot(OmicronG2, aes(x=Collection_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="3 months")
ggplot(OmicronG2, aes(x=Submission_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="3 months")

# Omicron BA.3
OmicronG3<-data.frame(GISAID$Collection.date[GISAID$Pango.lineage=="BA.3"], GISAID$Submission.date[GISAID$Pango.lineage=="BA.3"])
names(OmicronG3)<-c("Collection_Date", "Submission_Date")
OmicronG3$Collection_Date<-as.Date(OmicronG3$Collection_Date)
OmicronG3$Submission_Date<-as.Date(OmicronG3$Submission_Date)
ggplot(OmicronG3, aes(x=Collection_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="3 months")
ggplot(OmicronG3, aes(x=Submission_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="3 months")

# Omicron BA.4
OmicronG4<-data.frame(GISAID$Collection.date[GISAID$Pango.lineage=="BA.4"], GISAID$Submission.date[GISAID$Pango.lineage=="BA.4"])
names(OmicronG4)<-c("Collection_Date", "Submission_Date")
OmicronG4$Collection_Date<-as.Date(OmicronG4$Collection_Date)
OmicronG4$Submission_Date<-as.Date(OmicronG4$Submission_Date)
ggplot(OmicronG4, aes(x=Collection_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="1 month")
ggplot(OmicronG4, aes(x=Submission_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="1 month")

# Omicron BA.5
OmicronG5<-data.frame(GISAID$Collection.date[GISAID$Pango.lineage=="BA.5"], GISAID$Submission.date[GISAID$Pango.lineage=="BA.5"])
names(OmicronG5)<-c("Collection_Date", "Submission_Date")
OmicronG5$Collection_Date<-as.Date(OmicronG5$Collection_Date)
OmicronG5$Submission_Date<-as.Date(OmicronG5$Submission_Date)
ggplot(OmicronG5, aes(x=Collection_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="1 month")
ggplot(OmicronG5, aes(x=Submission_Date))+geom_histogram()+scale_x_date(labels=date_format("%Y-%m-%d"), breaks="1 month")

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
ggp<-ggplot(NULL,aes(Collection_Date, Submission_Date))+geom_point(data=Nalpha[sample(nrow(Nalpha), 500),], col="pink")+geom_point(data=Galpha[sample(nrow(Nalpha), 500),], col="skyblue")
ggp

# Beta
Nbeta<-data.frame(x=nextstrain$date[nextstrain$Nextclade_pango=="B.1.351"], nextstrain$date_submitted[nextstrain$Nextclade_pango=="B.1.351"])
names(Nbeta)<-c("Collection_Date", "Submission_Date")
Gbeta<-data.frame(GISAID$Collection.date[GISAID$Pango.lineage=="B.1.351"], GISAID$Submission.date[GISAID$Pango.lineage=="B.1.351"])
names(Gbeta)<-c("Collection_Date", "Submission_Date")
Nbeta$Collection_Date<-as.Date(Nbeta$Collection_Date)
Nbeta$Submission_Date<-as.Date(Nbeta$Submission_Date)
Gbeta$Collection_Date<-as.Date(Gbeta$Collection_Date)
Gbeta$Submission_Date<-as.Date(Gbeta$Submission_Date)
ggp<-ggplot(NULL,aes(Collection_Date, Submission_Date))+geom_point(data=Nbeta[sample(nrow(Nbeta), 500),], col="pink")+geom_point(data=Gbeta[sample(nrow(Gbeta), 500),], col="skyblue")
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
ggp<-ggplot(NULL,aes(Collection_Date, Submission_Date))+geom_point(data=Ngamma[sample(nrow(Ngamma), 500),], col="pink")+geom_point(data=Ggamma[sample(nrow(Ggamma), 500),], col="skyblue")
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
ggp<-ggplot(NULL,aes(Collection_Date, Submission_Date))+geom_point(data=Ndelta[sample(nrow(Ndelta), 500),], col="pink")+geom_point(data=Gdelta[sample(nrow(Gdelta), 500),], col="skyblue")
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
ggp<-ggplot(NULL,aes(Collection_Date, Submission_Date))+geom_point(data=Nomicron[sample(nrow(Nomicron), 500),], col="pink")+geom_point(data=Gomicron[sample(nrow(Gomicron), 500),], col="skyblue")
ggp

## Boxplot
# Alpha
Nalpha<-data.frame(x=nextstrain$date[nextstrain$Nextclade_pango=="B.1.1.7"], nextstrain$date_submitted[nextstrain$Nextclade_pango=="B.1.1.7"])
names(Nalpha)<-c("Collection_Date", "Submission_Date")
Galpha<-data.frame(GISAID$Collection.date[GISAID$Pango.lineage=="B.1.1.7"], GISAID$Submission.date[GISAID$Pango.lineage=="B.1.1.7"])
names(Galpha)<-c("Collection_Date", "Submission_Date")

a<-data.frame(Nalpha$Submission_Date[sample(nrow(Nalpha), 500)], rep("Nextstrain",500))
names(a)<-c("Submission_Date", "Database")
b<-data.frame(Galpha$Submission_Date[sample(nrow(Galpha), 500)], rep("GISAID",500))
names(b)<-c("Submission_Date", "Database")

a$Submission_Date<-as.Date(a$Submission_Date)
b$Submission_Date<-as.Date(b$Submission_Date)

c<-rbind(a,b)

ggplot(c,aes(y=Submission_Date, fill=Database))+geom_boxplot()+scale_x_discrete()

# Beta
Nbeta<-data.frame(x=nextstrain$date[nextstrain$Nextclade_pango=="B.1.351"], nextstrain$date_submitted[nextstrain$Nextclade_pango=="B.1.351"])
names(Nbeta)<-c("Collection_Date", "Submission_Date")
Gbeta<-data.frame(GISAID$Collection.date[GISAID$Pango.lineage=="B.1.351"], GISAID$Submission.date[GISAID$Pango.lineage=="B.1.351"])
names(Gbeta)<-c("Collection_Date", "Submission_Date")

a<-data.frame(Nbeta$Submission_Date[sample(nrow(Nbeta), 500)], rep("Nextstrain",500))
names(a)<-c("Submission_Date", "Database")
b<-data.frame(Gbeta$Submission_Date[sample(nrow(Gbeta), 500)], rep("GISAID",500))
names(b)<-c("Submission_Date", "Database")

a$Submission_Date<-as.Date(a$Submission_Date)
b$Submission_Date<-as.Date(b$Submission_Date)

c<-rbind(a,b)

ggplot(c, aes(y=Submission_Date, fill=Database))+geom_boxplot()+scale_x_discrete()

# Gamma
Ngamma<-data.frame(x=nextstrain$date[nextstrain$Nextclade_pango=="P.1"], nextstrain$date_submitted[nextstrain$Nextclade_pango=="P.1"])
names(Ngamma)<-c("Collection_Date", "Submission_Date")
Ggamma<-data.frame(GISAID$Collection.date[GISAID$Pango.lineage=="P.1"], GISAID$Submission.date[GISAID$Pango.lineage=="P.1"])
names(Ggamma)<-c("Collection_Date", "Submission_Date")

a<-data.frame(Ngamma$Submission_Date[sample(nrow(Ngamma), 500)], rep("Nextstrain",500))
names(a)<-c("Submission_Date", "Database")
b<-data.frame(Ggamma$Submission_Date[sample(nrow(Ggamma), 500)], rep("GISAID",500))
names(b)<-c("Submission_Date", "Database")

a$Submission_Date<-as.Date(a$Submission_Date)
b$Submission_Date<-as.Date(b$Submission_Date)

c<-rbind(a,b)

ggplot(c, aes(y=Submission_Date, fill=Database))+geom_boxplot()+scale_x_discrete()

# Delta
Ndelta<-data.frame(x=nextstrain$date[nextstrain$Nextclade_pango=="B.1.617.2"], nextstrain$date_submitted[nextstrain$Nextclade_pango=="B.1.617.2"])
names(Ndelta)<-c("Collection_Date", "Submission_Date")
Gdelta<-data.frame(GISAID$Collection.date[GISAID$Pango.lineage=="B.1.617.2"], GISAID$Submission.date[GISAID$Pango.lineage=="B.1.617.2"])
names(Gdelta)<-c("Collection_Date", "Submission_Date")

a<-data.frame(Ndelta$Submission_Date[sample(nrow(Ndelta), 500)], rep("Nextstrain",500))
names(a)<-c("Submission_Date", "Database")
b<-data.frame(Gdelta$Submission_Date[sample(nrow(Gdelta), 500)], rep("GISAID",500))
names(b)<-c("Submission_Date", "Database")

a$Submission_Date<-as.Date(a$Submission_Date)
b$Submission_Date<-as.Date(b$Submission_Date)

c<-rbind(a,b)

ggplot(c, aes(y=Submission_Date, fill=Database))+geom_boxplot()+scale_x_discrete()

# Omicron
Nomicron<-data.frame(x=nextstrain$date[nextstrain$Nextclade_pango=="BA.1"], nextstrain$date_submitted[nextstrain$Nextclade_pango=="BA.1"])
names(Nomicron)<-c("Collection_Date", "Submission_Date")
Gomicron<-data.frame(GISAID$Collection.date[GISAID$Pango.lineage=="BA.1"], GISAID$Submission.date[GISAID$Pango.lineage=="BA.1"])
names(Gomicron)<-c("Collection_Date", "Submission_Date")

a<-data.frame(Nomicron$Submission_Date[sample(nrow(Nomicron), 500)], rep("Nextstrain",500))
names(a)<-c("Submission_Date", "Database")
b<-data.frame(Gomicron$Submission_Date[sample(nrow(Gomicron), 500)], rep("GISAID",500))
names(b)<-c("Submission_Date", "Database")

a$Submission_Date<-as.Date(a$Submission_Date)
b$Submission_Date<-as.Date(b$Submission_Date)

c<-rbind(a,b)

ggplot(c, aes(y=Submission_Date, fill=Database))+geom_boxplot()+scale_x_discrete()


