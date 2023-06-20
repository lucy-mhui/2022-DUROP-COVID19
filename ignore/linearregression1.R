ssh -L 9001:localhost:8787 lucyhui@129.100.26.211
http://localhost:9001/

# total is by collection date (not country specific)
nextstrain <- read.csv("parsed-nextstrain.csv")
gisaid <- read.csv("parsed-gisaid.csv")

# 0-2019
## GISAID
g<-subset(gisaid, year > "0" & year < "2020")
### alpha
### beta
### gamma
gamma<-g[g$variant=="Gamma",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=gamma)
summary(fit)
### delta
delta<-g[g$variant=="Delta",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=delta)
summary(fit)
### omicron
## nextstrain
n<-subset(nextstrain, year > "0" & year < "2020")
### alpha
### beta
### gamma
### delta
### omicron

# 0-2020
## GISAID
g<-subset(gisaid, year > "0" & year < "2021")
### alpha
alpha<-g[g$variant=="Alpha",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=alpha)
summary(fit)
### beta
beta<-g[g$variant=="Beta",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=beta)
summary(fit)
### gamma
gamma<-g[g$variant=="Gamma",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=gamma)
summary(fit)
### delta
delta<-g[g$variant=="Delta",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=delta)
summary(fit)
### omicron
omicron<-g[g$variant=="Omicron",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=omicron)
summary(fit)
## nextstrain
n<-subset(nextstrain, year > "0" & year < "2021")
### alpha
alpha<-n[n$variant=="Alpha",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=alpha)
summary(fit)
### beta
beta<-n[n$variant=="Beta",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=beta)
summary(fit)
### gamma
gamma<-n[n$variant=="Gamma",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=gamma)
summary(fit)
### delta
delta<-n[n$variant=="Delta",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=delta)
summary(fit)
### omicron
omicron<-n[n$variant=="Omicron",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=omicron)
summary(fit)

# 0-2021
## GISAID
g<-subset(gisaid, year > "0" & year < "2022")
### alpha
alpha<-g[g$variant=="Alpha",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=alpha)
summary(fit)
### beta
beta<-g[g$variant=="Beta",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=beta)
summary(fit)
### gamma
gamma<-g[g$variant=="Gamma",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=gamma)
summary(fit)
### delta
delta<-g[g$variant=="Delta",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=delta)
summary(fit)
### omicron
omicron<-g[g$variant=="Omicron",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=omicron)
summary(fit)
## nextstrain
n<-subset(nextstrain, year > "0" & year < "2022")
### alpha
alpha<-n[n$variant=="Alpha",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=alpha)
summary(fit)
### beta
beta<-n[n$variant=="Beta",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=beta)
summary(fit)
### gamma
gamma<-n[n$variant=="Gamma",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=gamma)
summary(fit)
### delta
delta<-n[n$variant=="Delta",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=delta)
summary(fit)
### omicron
omicron<-n[n$variant=="Omicron",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=omicron)
summary(fit)



# Boxplot
# Alpha
library("ggplot2")
g<-data.frame(as.Date(gisaid$coldate[gisaid$variant=="Alpha"]))
names(g)<-"coldate"
n<-data.frame(as.Date(nextstrain$coldate[nextstrain$variant=="Alpha"]))
names(n)<-"coldate"
a<-data.frame(g$coldate[sample(nrow(n),500)], rep("GISAID", 500))
names(a)<-c("coldate", "database")
b<-data.frame(n$coldate[sample(nrow(n),500)], rep("Nextstrain", 500))
names(b)<-c("coldate","database")
c<-rbind(a,b)
ggplot(c, aes(y=coldate, fill=database))+geom_boxplot()+scale_x_discrete()
# Beta
library("ggplot2")
g<-data.frame(as.Date(gisaid$coldate[gisaid$variant=="Beta"]))
names(g)<-"coldate"
n<-data.frame(as.Date(nextstrain$coldate[nextstrain$variant=="Beta"]))
names(n)<-"coldate"
a<-data.frame(g$coldate[sample(nrow(n),500)], rep("GISAID", 500))
names(a)<-c("coldate", "database")
b<-data.frame(n$coldate[sample(nrow(n),500)], rep("Nextstrain", 500))
names(b)<-c("coldate","database")
c<-rbind(a,b)
ggplot(c, aes(y=coldate, fill=database))+geom_boxplot()+scale_x_discrete()
# Gamma
library("ggplot2")
g<-data.frame(as.Date(gisaid$coldate[gisaid$variant=="Gamma"]))
names(g)<-"coldate"
n<-data.frame(as.Date(nextstrain$coldate[nextstrain$variant=="Gamma"]))
names(n)<-"coldate"
a<-data.frame(g$coldate[sample(nrow(n),500)], rep("GISAID", 500))
names(a)<-c("coldate", "database")
b<-data.frame(n$coldate[sample(nrow(n),500)], rep("Nextstrain", 500))
names(b)<-c("coldate","database")
c<-rbind(a,b)
ggplot(c, aes(y=coldate, fill=database))+geom_boxplot()+scale_x_discrete()
# Delta
library("ggplot2")
g<-data.frame(as.Date(gisaid$coldate[gisaid$variant=="Delta"]))
names(g)<-"coldate"
n<-data.frame(as.Date(nextstrain$coldate[nextstrain$variant=="Delta"]))
names(n)<-"coldate"
a<-data.frame(g$coldate[sample(nrow(n),500)], rep("GISAID", 500))
names(a)<-c("coldate", "database")
b<-data.frame(n$coldate[sample(nrow(n),500)], rep("Nextstrain", 500))
names(b)<-c("coldate","database")
c<-rbind(a,b)
ggplot(c, aes(y=coldate, fill=database))+geom_boxplot()+scale_x_discrete()
# Omicron
library("ggplot2")
g<-data.frame(as.Date(gisaid$coldate[gisaid$variant=="Omicron"]))
names(g)<-"coldate"
n<-data.frame(as.Date(nextstrain$coldate[nextstrain$variant=="Omicron"]))
names(n)<-"coldate"
a<-data.frame(g$coldate[sample(nrow(n),500)], rep("GISAID", 500))
names(a)<-c("coldate", "database")
b<-data.frame(n$coldate[sample(nrow(n),500)], rep("Nextstrain", 500))
names(b)<-c("coldate","database")
c<-rbind(a,b)
ggplot(c, aes(y=coldate, fill=database))+geom_boxplot()+scale_x_discrete()



# continuing censor
## Alpha
### GISAID
g<-subset(gisaid, coldate > "0-0-0" & coldate < "2020-03-10")
alpha<-g[g$variant=="Alpha",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=alpha)
summary(fit)
### Nextstrain
n<-subset(nextstrain, coldate > "0-0-0" & coldate < "2020-11-06")
alpha<-n[n$variant=="Alpha",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=alpha)
summary(fit)
## Beta
### GISAID
g<-subset(gisaid, coldate > "0-0-0" & coldate < "2020-09-08")
beta<-g[g$variant=="Beta",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=beta)
summary(fit)
### Nextstrain
n<-subset(nextstrain, coldate > "0-0-0" & coldate < "2021-01-03")
beta<-n[n$variant=="Beta",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=beta)
summary(fit)
## Gamma
### GISAID
g<-subset(gisaid, coldate > "0-0-0" & coldate < "2021-02-20")
gamma<-g[g$variant=="Gamma",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=gamma)
summary(fit)
### Nextstrain
n<-subset(nextstrain, coldate > "0-0-0" & coldate < "2021-03-03")
gamma<-n[n$variant=="Gamma",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=gamma)
summary(fit)
## Deta
### GISAID
g<-subset(gisaid, coldate > "0-0-0" & coldate < "2020-03-13")
delta<-g[g$variant=="Delta",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=delta)
summary(fit)
### Nextstrain
n<-subset(nextstrain, coldate > "0-0-0" & coldate < "2021-03-04")
gamma<-n[n$variant=="Gamma",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=gamma)
summary(fit)
## Omicron
### GISAID
g<-subset(gisaid, coldate > "0-0-0" & coldate < "2021-01-28")
omicron<-g[g$variant=="Omicron",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=omicron)
summary(fit)
### Nextstrain
n<-subset(nextstrain, coldate > "0-0-0" & coldate < "2021-01-30")
omicron<-n[n$variant=="Omicron",]
fit<-glm(formula=cbind(count, total)~as.Date(coldate), family="binomial", data=omicron)
summary(fit)





# trial 1 plot
gisaid$alpha<-gisaid$variant=="Alpha"
gisaid$alpha<-as.numeric(gisaid$alpha)
gisaid$coldate<-as.Date(gisaid$coldate)
g<-subset(gisaid, coldate > "2019-01-01")
model<-glm(alpha~coldate, data=g, family=binomial(link="logit"))
plot(alpha~coldate, data=g, pch=19)
curve(predict(model, data.frame(coldate=x), type="response"), lty=1, lwd=2, col="blue", add=TRUE) # doesn't work because coldate isn't numeric

# trial 2 plot
require(ggplot2)
require(scales)
ggplot(g, aes(x=coldate, y=alpha))+
  geom_point(color="indianred3", size=1)+
  geom_smooth()+
  scale_x_date(date_breaks="3 months", labels=date_format("%b-%y"))+
  theme_minimal()

# trial 3 plot
require(ggplot2)
ggplot(g, aes(x=coldate, y=alpha))+geom_point()+stat_smooth(method="glm",method.args=list(familiy="binomial"), se=FALSE)
