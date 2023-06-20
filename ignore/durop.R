ssh -L 9001:localhost:8787 lucyhui@129.100.26.211
http://localhost:9001/
  
# import dataframe 
nextstrain <- read.csv("parsed-nextstrain2.csv")
gisaid <- read.csv("parsed-gisaid2.csv")

# render coldate as Date values
nextstrain$coldate<-as.Date(nextstrain$coldate)
gisaid$coldate<-as.Date(gisaid$coldate)

# week 1 is 2020-09-01 
nextstrain<-subset(nextstrain, coldate > "2020-09-01")
gisaid<-subset(gisaid, coldate > "2020-09-01")

# subset by week
require(lubridate)
require(dplyr)
nextstrain <- nextstrain %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
nextstrain<-nextstrain%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(nextstrain$count, by=list(nextstrain$week), FUN=sum)
names(n) <- c("week", "total")
nextstrain <- merge(nextstrain, n, by="week")
nextstrain$percent <- nextstrain$count / nextstrain$total
write.csv(nextstrain, "\\Users\\lucyh\\Desktop\\parsed-nextstrain3a.csv", row.names = FALSE)

gisaid <- gisaid %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
gisaid<-gisaid%>%group_by(week, variant)%>%mutate(count=n())
g <-aggregate(gisaid$count, by=list(gisaid$week), FUN=sum)
names(g) <- c("week", "total")
gisaid <- merge(gisaid, g, by="week")
gisaid$percent <- gisaid$count / gisaid$total
write.csv(gisaid, "\\Users\\lucyh\\Desktop\\parsed-gisaid3a.csv", row.names = FALSE)

# logistic regression and censoring by coldate
## Alpha
### GISAID
g<-subset(gisaid, week > 1 & week < 9)
alpha<-g[g$variant=="Alpha",]
fit<-glm(formula=cbind(count, total)~week, family="binomial", data=alpha)
summary(fit)
### Nextstrain
n<-subset(nextstrain, week > 1 & week < 12)
alpha<-n[n$variant=="Alpha",]
fit<-glm(formula=cbind(count, total)~week, family="binomial", data=alpha)
summary(fit)
## Beta
### GISAID
g<-subset(gisaid, week > 1 & week < 6)
beta<-g[g$variant=="Beta",]
fit<-glm(formula=cbind(count, total)~week, family="binomial", data=beta)
summary(fit)
### Nextstrain
n<-subset(nextstrain, week > 1 & week < 18)
beta<-n[n$variant=="Beta",]
fit<-glm(formula=cbind(count, total)~week, family="binomial", data=beta)
summary(fit)
## Gamma
### GISAID
g<-subset(gisaid, week > 5 & week < 17)
gamma<-g[g$variant=="Gamma",]
fit<-glm(formula=cbind(count, total)~week, family="binomial", data=gamma)
summary(fit)
### Nextstrain
n<-subset(nextstrain, week > 5 & week < 23)
gamma<-n[n$variant=="Gamma",]
fit<-glm(formula=cbind(count, total)~week, family="binomial", data=gamma)
summary(fit)
## Deta
### GISAID
g<-subset(gisaid, week > 27 & week < 67)
delta<-g[g$variant=="Delta",]
fit<-glm(formula=cbind(count, total)~week, family="binomial", data=delta)
summary(fit)
### Nextstrain
n<-subset(nextstrain, week > 27 & week < 50)
delta<-n[n$variant=="Delta",]
fit<-glm(formula=cbind(count, total)~week, family="binomial", data=delta)
summary(fit)
## Omicron
### GISAID
g<-subset(gisaid, week > 53 & week < 64)
omicron<-g[g$variant=="Omicron",]
fit<-glm(formula=cbind(count, total)~week, family="binomial", data=omicron)
summary(fit)
### Nextstrain
n<-subset(nextstrain, week > 53 & week < 66)
omicron<-n[n$variant=="Omicron",]
fit<-glm(formula=cbind(count, total)~week, family="binomial", data=omicron)
summary(fit)

# histogram
require(ggplot2)
a<-gisaid[gisaid$variant=="Alpha",]
ggplot(a)+geom_bar(aes(x=week, y=count), stat="identity")
a<-nextstrain[nextstrain$variant=="Alpha",]
ggplot(a)+geom_bar(aes(x=week, y=count), stat="identity")

a<-gisaid[gisaid$variant=="Beta",]
ggplot(a)+geom_bar(aes(x=week, y=count), stat="identity")
a<-nextstrain[nextstrain$variant=="Beta",]
ggplot(a)+geom_bar(aes(x=week, y=count), stat="identity")

a<-gisaid[gisaid$variant=="Gamma",]
ggplot(a)+geom_bar(aes(x=week, y=count), stat="identity")
a<-nextstrain[nextstrain$variant=="Gamma",]
ggplot(a)+geom_bar(aes(x=week, y=count), stat="identity")

a<-gisaid[gisaid$variant=="Delta",]
ggplot(a)+geom_bar(aes(x=week, y=count), stat="identity")
a<-nextstrain[nextstrain$variant=="Delta",]
ggplot(a)+geom_bar(aes(x=week, y=count), stat="identity")

a<-gisaid[gisaid$variant=="Omicron",]
ggplot(a)+geom_bar(aes(x=week, y=count), stat="identity")
a<-nextstrain[nextstrain$variant=="Omicron",]
ggplot(a)+geom_bar(aes(x=week, y=count), stat="identity")

# logistic regression visualization
require(ggplot2)
g<-subset(gisaid, week > 1 & week < 9)
alpha<-g[g$variant=="Alpha",]
ggplot(alpha,aes(x=week, y=percent))+
  geom_point(alpha=0.5)+
  stat_smooth(method="glm", se=FALSE, method.args=list(family=binomial))

n<-subset(nextstrain, week > 1 & week < 12)
alpha<-nextstrain[nextstrain$variant=="Alpha",]
ggplot(alpha,aes(x=week, y=percent))+
  geom_point(alpha=0.5)+
  stat_smooth(method="glm", se=FALSE, method.args=list(family=binomial))

g<-subset(gisaid, week > 1 & week < 6)
beta<-gisaid[gisaid$variant=="Beta",]
ggplot(beta,aes(x=week, y=percent))+
  geom_point(alpha=0.5)+
  stat_smooth(method="glm", se=FALSE, method.args=list(family=binomial))

n<-subset(nextstrain, week > 1 & week < 18)
beta<-nextstrain[nextstrain$variant=="Beta",]
ggplot(beta,aes(x=week, y=percent))+
  geom_point(alpha=0.5)+
  stat_smooth(method="glm", se=FALSE, method.args=list(family=binomial))

g<-subset(gisaid, week > 5 & week < 17)
gamma<-g[g$variant=="Gamma",]
ggplot(gamma,aes(x=week, y=percent))+
  geom_point(alpha=0.5)+
  stat_smooth(method="glm", se=FALSE, method.args=list(family=binomial))

n<-subset(nextstrain, week > 5 & week < 23)
gamma<-n[n$variant=="Gamma",]
ggplot(gamma,aes(x=week, y=percent))+
  geom_point(alpha=0.5)+
  stat_smooth(method="glm", se=FALSE, method.args=list(family=binomial))

g<-subset(gisaid, week > 27 & week < 67)
delta<-g[g$variant=="Delta",]
ggplot(delta,aes(x=week, y=percent))+
  geom_point(alpha=0.5)+
  stat_smooth(method="glm", se=FALSE, method.args=list(family=binomial))

n<-subset(nextstrain, week > 27 & week < 50)
delta<-n[n$variant=="Delta",]
ggplot(delta,aes(x=week, y=percent))+
  geom_point(alpha=0.5)+
  stat_smooth(method="glm", se=FALSE, method.args=list(family=binomial))

g<-subset(gisaid, week > 53 & week < 64)
omicron<-g[g$variant=="Omicron",]
ggplot(omicron,aes(x=week, y=percent))+
  geom_point(alpha=0.5)+
  stat_smooth(method="glm", se=FALSE, method.args=list(family=binomial))

n<-subset(nextstrain, week > 53 & week < 66)
omicron<-nextstrain[nextstrain$variant=="Omicron",]
ggplot(omicron,aes(x=week, y=percent))+
  geom_point(alpha=0.5)+
  stat_smooth(method="glm", se=FALSE, method.args=list(family=binomial))

# scatterplot
g<-gisaid[gisaid$variant=="Alpha",]
plot(g$percent~g$week)

g<-gisaid[gisaid$variant=="Beta",]
plot(g$percent~g$week)

g<-gisaid[gisaid$variant=="Gamma",]
plot(g$percent~g$week)

g<-gisaid[gisaid$variant=="Delta",]
plot(g$percent~g$week)

g<-gisaid[gisaid$variant=="Omicron",]
plot(g$percent~g$week)

n<-nextstrain[nextstrain$variant=="Alpha",]
plot(n$percent~n$week)

n<-nextstrain[nextstrain$variant=="Beta",]
plot(n$percent~n$week)

n<-nextstrain[nextstrain$variant=="Gamma",]
plot(n$percent~n$week)

n<-nextstrain[nextstrain$variant=="Delta",]
plot(n$percent~n$week)

n<-nextstrain[nextstrain$variant=="Omicron",]
plot(n$percent~n$week)