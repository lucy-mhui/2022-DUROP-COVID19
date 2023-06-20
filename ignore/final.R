require(lubridate)
require(dplyr)

n <- read.csv("finaln.csv")
g <- read.csv("finalg.csv")

### Delay Boxplot (subdate - coldate)
## alpha
a<-data.frame(n$coldate[n$variant=="Alpha"], n$subdate[n$variant=="Alpha"])
names(a)<-c("coldate", "subdate")
a$coldate<-as.Date(a$coldate)
a$subdate<-as.Date(a$subdate)
"delay"<-a$subdate-a$coldate
a<-data.frame(cbind(a,delay))
a<-na.omit(a)
a<-data.frame(a[sample(nrow(a), 500),], rep("Nextstrain",500))
names(a)<-c("coldate", "subdate", "delay", "database")

b<-data.frame(g$coldate[g$variant=="Alpha"], g$subdate[g$variant=="Alpha"])
names(b)<-c("coldate", "subdate")
b$coldate<-as.Date(b$coldate)
b$subdate<-as.Date(b$subdate)
"delay"<-b$subdate-b$coldate
b<-data.frame(cbind(b,delay))
b<-na.omit(b)
b<-data.frame(b[sample(nrow(b), 500),], rep("GISAID",500))
names(b)<-c("coldate", "subdate", "delay", "database")

c<-rbind(a,b)
ggplot(c, aes(y=delay, fill=database))+geom_boxplot()+scale_x_discrete()

# beta
a<-data.frame(n$coldate[n$variant=="Beta"], n$subdate[n$variant=="Beta"])
names(a)<-c("coldate", "subdate")
a$coldate<-as.Date(a$coldate)
a$subdate<-as.Date(a$subdate)
"delay"<-a$subdate-a$coldate
a<-data.frame(cbind(a,delay))
a<-na.omit(a)
a<-data.frame(a[sample(nrow(a), 500),], rep("Nextstrain",500))
names(a)<-c("coldate", "subdate", "delay", "database")

b<-data.frame(g$coldate[g$variant=="Beta"], g$subdate[g$variant=="Beta"])
names(b)<-c("coldate", "subdate")
b$coldate<-as.Date(b$coldate)
b$subdate<-as.Date(b$subdate)
"delay"<-b$subdate-b$coldate
b<-data.frame(cbind(b,delay))
b<-na.omit(b)
b<-data.frame(b[sample(nrow(b), 500),], rep("GISAID",500))
names(b)<-c("coldate", "subdate", "delay", "database")

c<-rbind(a,b)
ggplot(c, aes(y=delay, fill=database))+geom_boxplot()+scale_x_discrete()

# gamma
a<-data.frame(n$coldate[n$variant=="Gamma"], n$subdate[n$variant=="Gamma"])
names(a)<-c("coldate", "subdate")
a$coldate<-as.Date(a$coldate)
a$subdate<-as.Date(a$subdate)
"delay"<-a$subdate-a$coldate
a<-data.frame(cbind(a,delay))
a<-na.omit(a)
a<-data.frame(a[sample(nrow(a), 500),], rep("Nextstrain",500))
names(a)<-c("coldate", "subdate", "delay", "database")

b<-data.frame(g$coldate[g$variant=="Gamma"], g$subdate[g$variant=="Gamma"])
names(b)<-c("coldate", "subdate")
b$coldate<-as.Date(b$coldate)
b$subdate<-as.Date(b$subdate)
"delay"<-b$subdate-b$coldate
b<-data.frame(cbind(b,delay))
b<-na.omit(b)
b<-data.frame(b[sample(nrow(b), 500),], rep("GISAID",500))
names(b)<-c("coldate", "subdate", "delay", "database")

c<-rbind(a,b)
ggplot(c, aes(y=delay, fill=database))+geom_boxplot()+scale_x_discrete()

# delta
a<-data.frame(n$coldate[n$variant=="Delta"], n$subdate[n$variant=="Delta"])
names(a)<-c("coldate", "subdate")
a$coldate<-as.Date(a$coldate)
a$subdate<-as.Date(a$subdate)
"delay"<-a$subdate-a$coldate
a<-data.frame(cbind(a,delay))
a<-na.omit(a)
a<-data.frame(a[sample(nrow(a), 500),], rep("Nextstrain",500))
names(a)<-c("coldate", "subdate", "delay", "database")

b<-data.frame(g$coldate[g$variant=="Delta"], g$subdate[g$variant=="Delta"])
names(b)<-c("coldate", "subdate")
b$coldate<-as.Date(b$coldate)
b$subdate<-as.Date(b$subdate)
"delay"<-b$subdate-b$coldate
b<-data.frame(cbind(b,delay))
b<-na.omit(b)
b<-data.frame(b[sample(nrow(b), 500),], rep("GISAID",500))
names(b)<-c("coldate", "subdate", "delay", "database")

c<-rbind(a,b)
ggplot(c, aes(y=delay, fill=database))+geom_boxplot()+scale_x_discrete()

# omicron
a<-data.frame(n$coldate[n$variant=="Omicron"], n$subdate[n$variant=="Omicron"])
names(a)<-c("coldate", "subdate")
a$coldate<-as.Date(a$coldate)
a$subdate<-as.Date(a$subdate)
"delay"<-a$subdate-a$coldate
a<-data.frame(cbind(a,delay))
a<-na.omit(a)
a<-data.frame(a[sample(nrow(a), 500),], rep("Nextstrain",500))
names(a)<-c("coldate", "subdate", "delay", "database")

b<-data.frame(g$coldate[g$variant=="Omicron"], g$subdate[g$variant=="Omicron"])
names(b)<-c("coldate", "subdate")
b$coldate<-as.Date(b$coldate)
b$subdate<-as.Date(b$subdate)
"delay"<-b$subdate-b$coldate
b<-data.frame(cbind(b,delay))
b<-na.omit(b)
b<-data.frame(b[sample(nrow(b), 500),], rep("GISAID",500))
names(b)<-c("coldate", "subdate", "delay", "database")

c<-rbind(a,b)
ggplot(c, aes(y=delay, fill=database))+geom_boxplot()+scale_x_discrete()

# subdate cutoff
n102020<-subset(n, subdate < "2020-10-01")
n112020<-subset(n, subdate < "2020-11-01")
n122020<-subset(n, subdate < "2020-12-01")
n012021<-subset(n, subdate < "2021-01-01")
n022021<-subset(n, subdate < "2021-02-01")
n032021<-subset(n, subdate < "2021-03-01")
n042021<-subset(n, subdate < "2021-04-01")
n052021<-subset(n, subdate < "2021-05-01")
n062021<-subset(n, subdate < "2021-06-01")
n072021<-subset(n, subdate < "2021-07-01")
n082021<-subset(n, subdate < "2021-08-01")
n092021<-subset(n, subdate < "2021-09-01")
n102021<-subset(n, subdate < "2021-10-01")
n112021<-subset(n, subdate < "2021-11-01")
n122021<-subset(n, subdate < "2021-12-01")
n012022<-subset(n, subdate < "2022-01-01")
n022022<-subset(n, subdate < "2022-02-01")
n032022<-subset(n, subdate < "2022-03-01")
n042022<-subset(n, subdate < "2022-04-01")
n052022<-subset(n, subdate < "2022-05-01")

g102020<-subset(g, subdate < "2020-10-01")
g112020<-subset(g, subdate < "2020-11-01")
g122020<-subset(g, subdate < "2020-12-01")
g012021<-subset(g, subdate < "2021-01-01")
g022021<-subset(g, subdate < "2021-02-01")
g032021<-subset(g, subdate < "2021-03-01")
g042021<-subset(g, subdate < "2021-04-01")
g052021<-subset(g, subdate < "2021-05-01")
g062021<-subset(g, subdate < "2021-06-01")
g072021<-subset(g, subdate < "2021-07-01")
g082021<-subset(g, subdate < "2021-08-01")
g092021<-subset(g, subdate < "2021-09-01")
g102021<-subset(g, subdate < "2021-10-01")
g112021<-subset(g, subdate < "2021-11-01")
g122021<-subset(g, subdate < "2021-12-01")
g012022<-subset(g, subdate < "2022-01-01")
g022022<-subset(g, subdate < "2022-02-01")
g032022<-subset(g, subdate < "2022-03-01")
g042022<-subset(g, subdate < "2022-04-01")
g052022<-subset(g, subdate < "2022-05-01")

# subset by week 
n102020 <- n102020 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
n102020<-n102020%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(n102020$count, by=list(n102020$week), FUN=sum)
names(n) <- c("week", "total")
n102020 <- merge(n102020, n, by="week")
n102020$percent <- n102020$count / n102020$total

n112020 <- n112020 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
n112020<-n112020%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(n112020$count, by=list(n112020$week), FUN=sum)
names(n) <- c("week", "total")
n112020 <- merge(n112020, n, by="week")
n112020$percent <- n112020$count / n112020$total

n122020 <- n122020 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
n122020<-n122020%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(n122020$count, by=list(n122020$week), FUN=sum)
names(n) <- c("week", "total")
n122020 <- merge(n122020, n, by="week")
n122020$percent <- n122020$count / n122020$total

n012021 <- n012021 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
n012021<-n012021%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(n012021$count, by=list(n012021$week), FUN=sum)
names(n) <- c("week", "total")
n012021 <- merge(n012021, n, by="week")
n012021$percent <- n012021$count / n012021$total

n022021 <- n022021 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
n022021<-n022021%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(n022021$count, by=list(n022021$week), FUN=sum)
names(n) <- c("week", "total")
n022021 <- merge(n022021, n, by="week")
n022021$percent <- n022021$count / n022021$total

n032021 <- n032021 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
n032021<-n032021%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(n032021$count, by=list(v$week), FUN=sum)
names(n) <- c("week", "total")
n032021 <- merge(n032021, n, by="week")
n032021$percent <- n032021$count / n032021$total

n042021 <- n042021 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
n042021<-n042021%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(n042021$count, by=list(v$week), FUN=sum)
names(n) <- c("week", "total")
n042021 <- merge(n042021, n, by="week")
n042021$percent <- n042021$count / n042021$total

n052021 <- n052021 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
n052021<-n052021%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(n052021$count, by=list(v$week), FUN=sum)
names(n) <- c("week", "total")
n052021 <- merge(n052021, n, by="week")
n052021$percent <- n052021$count / n052021$total

n062021 <- n062021 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
n062021<-n062021%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(n062021$count, by=list(v$week), FUN=sum)
names(n) <- c("week", "total")
n062021 <- merge(n062021, n, by="week")
n062021$percent <- n062021$count / n062021$total

n072021 <- n072021 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
n072021<-n072021%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(n072021$count, by=list(v$week), FUN=sum)
names(n) <- c("week", "total")
n072021 <- merge(n072021, n, by="week")
n072021$percent <- n072021$count / n072021$total

n082021 <- n082021 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
n082021<-n082021%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(n082021$count, by=list(v$week), FUN=sum)
names(n) <- c("week", "total")
n082021 <- merge(n082021, n, by="week")
n082021$percent <- n082021$count / n082021$total

n092021 <- n092021 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
n092021<-n092021%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(n092021$count, by=list(v$week), FUN=sum)
names(n) <- c("week", "total")
n092021 <- merge(n092021, n, by="week")
n092021$percent <- n092021$count / n092021$total

n102021 <- n102021 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
n102021<-n102021%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(n102021$count, by=list(v$week), FUN=sum)
names(n) <- c("week", "total")
n102021 <- merge(n102021, n, by="week")
n102021$percent <- n102021$count / n102021$total

n112021 <- n112021 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
n112021<-n112021%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(n112021$count, by=list(v$week), FUN=sum)
names(n) <- c("week", "total")
n112021 <- merge(n112021, n, by="week")
n112021$percent <- n112021$count / n112021$total

n122021 <- n122021 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
n122021<-n122021%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(n122021$count, by=list(v$week), FUN=sum)
names(n) <- c("week", "total")
n122021 <- merge(n122021, n, by="week")
n122021$percent <- n122021$count / n122021$total

n012022 <- n012022 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
n012022<-n012022%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(n012022$count, by=list(v$week), FUN=sum)
names(n) <- c("week", "total")
n012022 <- merge(n012022, n, by="week")
n012022$percent <- n012022$count / n012022$total

n022022 <- n022022 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
n022022<-n022022%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(n022022$count, by=list(v$week), FUN=sum)
names(n) <- c("week", "total")
n022022 <- merge(n022022, n, by="week")
n022022$percent <- n022022$count / n022022$total

n032022 <- n032022 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
n032022<-n032022%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(n032022$count, by=list(v$week), FUN=sum)
names(n) <- c("week", "total")
n032022 <- merge(n032022, n, by="week")
n032022$percent <- n032022$count / n032022$total

n042022 <- n042022 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
n042022<-n042022%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(n042022$count, by=list(v$week), FUN=sum)
names(n) <- c("week", "total")
n042022 <- merge(n042022, n, by="week")
n042022$percent <- n042022$count / n042022$total

n052022 <- n052022 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
n052022<-n052022%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(n052022$count, by=list(v$week), FUN=sum)
names(n) <- c("week", "total")
n052022 <- merge(n052022, n, by="week")
n052022$percent <- n052022$count / n052022$total


g102020 <- g102020 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
g102020<-g102020%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(g102020$count, by=list(g102020$week), FUN=sum)
names(n) <- c("week", "total")
g102020 <- merge(g102020, n, by="week")
g102020$percent <- g102020$count / g102020$total

g112020 <- g112020 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
g112020<-g112020%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(g112020$count, by=list(g112020$week), FUN=sum)
names(n) <- c("week", "total")
g112020 <- merge(g112020, n, by="week")
g112020$percent <- g112020$count / g112020$total

g122020 <- g122020 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
g122020<-g122020%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(g122020$count, by=list(g122020$week), FUN=sum)
names(n) <- c("week", "total")
g122020 <- merge(g122020, n, by="week")
g122020$percent <- g122020$count / g122020$total

g012021 <- g012021 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
g012021<-g012021%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(g012021$count, by=list(g012021$week), FUN=sum)
names(n) <- c("week", "total")
g012021 <- merge(g012021, n, by="week")
g012021$percent <- g012021$count / g012021$total

g022021 <- g022021 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
g022021<-g022021%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(g022021$count, by=list(g022021$week), FUN=sum)
names(n) <- c("week", "total")
g022021 <- merge(g022021, n, by="week")
g022021$percent <- g022021$count / g022021$total

g032021 <- g032021 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
g032021<-g032021%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(g032021$count, by=list(g032021$week), FUN=sum)
names(n) <- c("week", "total")
g032021 <- merge(g032021, n, by="week")
g032021$percent <- g032021$count / g032021$total

g042021 <- g042021 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
g042021<-g042021%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(g042021$count, by=list(g042021$week), FUN=sum)
names(n) <- c("week", "total")
g042021 <- merge(g042021, n, by="week")
g042021$percent <- g042021$count / g042021$total

g052021 <- g052021 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
g052021<-g052021%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(g052021$count, by=list(g052021$week), FUN=sum)
names(n) <- c("week", "total")
g052021 <- merge(g052021, n, by="week")
g052021$percent <- g052021$count / g052021$total

g062021 <- g062021 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
g062021<-g062021%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(g062021$count, by=list(g062021$week), FUN=sum)
names(n) <- c("week", "total")
g062021 <- merge(g062021, n, by="week")
g062021$percent <- g062021$count / g062021$total

g072021 <- g072021 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
g072021<-g072021%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(g072021$count, by=list(g072021$week), FUN=sum)
names(n) <- c("week", "total")
g072021 <- merge(g072021, n, by="week")
g072021$percent <- g072021$count / g072021$total

g082021 <- g082021 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
g082021<-g082021%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(g082021$count, by=list(g082021$week), FUN=sum)
names(n) <- c("week", "total")
g082021 <- merge(g082021, n, by="week")
g082021$percent <- g082021$count / g082021$total

g092021 <- g092021 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
g092021<-g092021%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(g092021$count, by=list(g092021$week), FUN=sum)
names(n) <- c("week", "total")
g092021 <- merge(g092021, n, by="week")
g092021$percent <- g092021$count / g092021$total

g102021 <- g102021 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
g102021<-g102021%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(g102021$count, by=list(g102021$week), FUN=sum)
names(n) <- c("week", "total")
g102021 <- merge(g102021, n, by="week")
g102021$percent <- g102021$count / g102021$total

g112021 <- g112021 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
g112021<-g112021%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(g112021$count, by=list(g112021$week), FUN=sum)
names(n) <- c("week", "total")
g112021 <- merge(g112021, n, by="week")
g112021$percent <- g112021$count / g112021$total

g122021 <- g122021 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
g122021<-g122021%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(g122021$count, by=list(g122021$week), FUN=sum)
g122021(n) <- c("week", "total")
n122021 <- merge(g122021, n, by="week")
g122021$percent <- g122021$count / g122021$total

g012022 <- g012022 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
g012022<-g012022%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(g012022$count, by=list(g012022$week), FUN=sum)
names(n) <- c("week", "total")
g012022 <- merge(g012022, n, by="week")
g012022$percent <- g012022$count / g012022$total

g022022 <- g022022 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
g022022<-g022022%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(g022022$count, by=list(g022022$week), FUN=sum)
names(n) <- c("week", "total")
g022022 <- merge(g022022, n, by="week")
g022022$percent <- g022022$count / g022022$total

g032022 <- g032022 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
g032022<-g032022%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(g032022$count, by=list(g032022$week), FUN=sum)
names(n) <- c("week", "total")
g032022 <- merge(g032022, n, by="week")
g032022$percent <- g032022$count / g032022$total

g042022 <- g042022 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
g042022<-g042022%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(g042022$count, by=list(g042022$week), FUN=sum)
names(n) <- c("week", "total")
g042022 <- merge(g042022, n, by="week")
g042022$percent <- g042022$count / g042022$total

g052022 <- g052022 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
g052022<-g052022%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(g052022$count, by=list(g052022$week), FUN=sum)
names(n) <- c("week", "total")
g052022 <- merge(g052022, n, by="week")
g052022$percent <- g052022$count / g052022$total

g122021 <- g122021 %>%
  mutate(week=cut.Date(coldate, breaks="1 week", labels=FALSE)) %>%
  arrange(coldate)
g122021<-g122021%>%group_by(week, variant)%>%mutate(count=n())
n <-aggregate(g122021$count, by=list(g122021$week), FUN=sum)
names(n) <- c("week", "total")
g122021 <- merge(g122021, n, by="week")
g122021$percent <- g122021$count / g122021$total

# logistic regression
alpha<-n022021[n022021$variant=="Alpha",]
fit<-glm(formula=cbind(count, total)~week, family="binomial", data=alpha)
summary(fit)

beta<-n032021[n032021$variant=="Beta",]
fit<-glm(formula=cbind(count, total)~week, family="binomial", data=beta)
summary(fit)

gamma<-n032021[n032021$variant=="Gamma",]
fit<-glm(formula=cbind(count, total)~week, family="binomial", data=gamma)
summary(fit)

delta<-n102021[n102021$variant=="Delta",]
fit<-glm(formula=cbind(count, total)~week, family="binomial", data=delta)
summary(fit)

omicron<-n032022[n032022$variant=="Omicron",]
fit<-glm(formula=cbind(count, total)~week, family="binomial", data=omicron)
summary(fit)


alpha<-g122020[g122020$variant=="Alpha",]
fit<-glm(formula=cbind(count, total)~week, family="binomial", data=alpha)
summary(fit)

beta<-g122020[g122020$variant=="Beta",]
fit<-glm(formula=cbind(count, total)~week, family="binomial", data=beta)
summary(fit)

gamma<-g022021[g022021$variant=="Gamma",]
fit<-glm(formula=cbind(count, total)~week, family="binomial", data=gamma)
summary(fit)

delta<-g042021[g042021$variant=="Delta",]
fit<-glm(formula=cbind(count, total)~week, family="binomial", data=delta)
summary(fit)

omicron<-g122021[g122021$variant=="Omicron",]
fit<-glm(formula=cbind(count, total)~week, family="binomial", data=omicron)
summary(fit)

# visualize logistic regression
g<-g122020[g122020$variant=="Alpha",]
ggplot(g,aes(x=week, y=cbind(count / total)))+
  geom_point(g=0.5)+
  stat_smooth(method="glm", se=FALSE, method.args=list(family=binomial))

g<-g122020[g122020$variant=="Beta",]
ggplot(g,aes(x=week, y=cbind(count / total)))+
  geom_point(g=0.5)+
  stat_smooth(method="glm", se=FALSE, method.args=list(family=binomial))

g<-g022021[g022021$variant=="Gamma",]
ggplot(g,aes(x=week, y=cbind(count / total)))+
  geom_point(g=0.5)+
  stat_smooth(method="glm", se=FALSE, method.args=list(family=binomial))

g<-g042021[g042021$variant=="Delta",]
ggplot(g,aes(x=week, y=cbind(count / total)))+
  geom_point(g=0.5)+
  stat_smooth(method="glm", se=FALSE, method.args=list(family=binomial))

g<-g122021[g122021$variant=="Omicron",]
ggplot(g,aes(x=week, y=cbind(count / total)))+
  geom_point(g=0.5)+
  stat_smooth(method="glm", se=FALSE, method.args=list(family=binomial))


n<-n022021[n022021$variant=="Alpha",]
ggplot(n,aes(x=week, y=cbind(count / total)))+
  geom_point(n=0.5)+
  stat_smooth(method="glm", se=FALSE, method.args=list(family=binomial))

n<-n032021[n032021$variant=="Beta",]
ggplot(n,aes(x=week, y=cbind(count / total)))+
  geom_point(n=0.5)+
  stat_smooth(method="glm", se=FALSE, method.args=list(family=binomial))

n<-n032021[n032021$variant=="Gamma",]
ggplot(n,aes(x=week, y=cbind(count / total)))+
  geom_point(n=0.5)+
  stat_smooth(method="glm", se=FALSE, method.args=list(family=binomial))

n<-n102021[n102021$variant=="Delta",]
ggplot(n,aes(x=week, y=cbind(count / total)))+
  geom_point(n=0.5)+
  stat_smooth(method="glm", se=FALSE, method.args=list(family=binomial))

n<-n032022[n032022$variant=="Omicron",]
ggplot(n,aes(x=week, y=cbind(count / total)))+
  geom_point(n=0.5)+
  stat_smooth(method="glm", se=FALSE, method.args=list(family=binomial))