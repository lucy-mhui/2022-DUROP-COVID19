
parsed.gisaid4$coldate<-as.Date(parsed.gisaid4$coldate)
parsed.gisaid4$subdate<-as.Date(parsed.gisaid4$subdate)

"delay"<-parsed.gisaid4$subdate-parsed.gisaid4$coldate
parsed.gisaid4<-data.frame(cbind(parsed.gisaid4,delay))

/

parsed.nextstrain4$coldate<-as.Date(parsed.nextstrain4$coldate)
parsed.nextstrain4$subdate<-as.Date(parsed.nextstrain4$subdate)

"delay"<-parsed.nextstrain4$subdate-parsed.nextstrain4$coldate
parsed.nextstrain4<-data.frame(cbind(parsed.nextstrain4,delay))

/

parsed.gisaid4<-data.frame(cbind(parsed.gisaid4,rep("GISAID",437683)))
parsed.nextstrain4<-data.frame(cbind(parsed.nextstrain4,rep("Nextstrain",98490)))

/  
  
colnames(parsed.gisaid4)[7]<-"database"
colnames(parsed.nextstrain4)[7]<-"database"

/
  
parsed.gisaid4<-parsed.gisaid4[!(parsed.gisaid4$variant=="other"),]
parsed.nextstrain4<-parsed.nextstrain4[!(parsed.nextstrain4$variant=="other"),]

/
  
combined<-rbind(parsed.gisaid4,parsed.nextstrain4)  

/

library(ggplot2)
a<-ggplot(combined, aes(x=variant, y=delay, fill=database))+geom_boxplot()+scale_x_discrete()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_discrete(name=NULL)+theme(legend.key.size = unit(2, 'cm'))
a<-a+labs(x="SARS-CoV-2 Variant of Concern", y="Publication delay (Days)")
a+theme(axis.text=element_text(size=40),
        axis.title=element_text(size=40,face="bold"))
a

/
  
ggsave("delay.png",width=48,height=11,units="in",dpi=1800)  

/
  
t.test(combined$delay[combined$variant=="Alpha"&combined$database=="GISAID"],combined$delay[combined$variant=="Alpha"&combined$database=="Nextstrain"])

t.test(combined$delay[combined$variant=="Beta"&combined$database=="GISAID"],combined$delay[combined$variant=="Beta"&combined$database=="Nextstrain"])

t.test(combined$delay[combined$variant=="Delta"&combined$database=="GISAID"],combined$delay[combined$variant=="Delta"&combined$database=="Nextstrain"])

t.test(combined$delay[combined$variant=="Gamma"&combined$database=="GISAID"],combined$delay[combined$variant=="Gamma"&combined$database=="Nextstrain"])

t.test(combined$delay[combined$variant=="Omicron"&combined$database=="GISAID"],combined$delay[combined$variant=="Omicron"&combined$database=="Nextstrain"])

/
  
library(dplyr)
library(tidyverse)

parsed.gisaid4<- parsed.gisaid4 %>% 
  add_count(country) %>% 
  filter(dense_rank(-n) < 10)

ggplot(parsed.gisaid4, aes(x=country, y=count))+
  geom_bar(stat="identity",fill = "#FF6666")+labs(x="Country", y="Number of submissions")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+theme(axis.text=element_text(size=15),axis.title=element_text(size=20,face="bold"))

gisad<-data.frame(parsed.gisaid4$country,parsed.gisaid4$count)
gisad<-gisad[!duplicated(gisad),]
gisad$parsed.gisaid4.count<-as.numeric(gisad$parsed.gisaid4.count)
gisad<-aggregate(gisad$parsed.gisaid4.count,by = list(gisad$parsed.gisaid4.country),FUN = sum)
pie(gisad$x,labels=gisad$Group.1)

bp<-ggplot(gisad, aes(x="", y=x, fill=Group.1))+
  theme_void()+
  scale_fill_discrete(name=NULL)+
  geom_col(color = "black",aes(size=10)) + 
  coord_polar(theta="y")+
  scale_fill_brewer(palette="Spectral")
bp

/

parsed.nextstrain4<- parsed.nextstrain4 %>% 
  add_count(country) %>% 
  filter(dense_rank(-n) < 11)
ggplot(parsed.nextstrain4, aes(x=country, y=count))+
  geom_bar(stat="identity",fill = "#00BFC4")+labs(x="Country", y="Number of submissions")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+theme(axis.text=element_text(size=10),axis.title=element_text(size=20,face="bold"))

nexts<-data.frame(parsed.nextstrain4$country,parsed.nextstrain4$count)
nexts<-nexts[!duplicated(nexts),]
nexts$parsed.nextstrain4<-as.numeric(nexts$parsed.nextstrain4.count)
nexts<-aggregate(nexts$parsed.nextstrain4.count,by = list(nexts$parsed.nextstrain4.country),FUN = sum)
pie(nexts$x,labels=nexts$Group.1)

bp<-ggplot(nexts, aes(x="", y=x, fill=Group.1))+
  theme_void()+
  scale_fill_discrete(name=NULL)+
  geom_col(color = "black") + 
  coord_polar(theta="y")+
  scale_fill_brewer(palette="RdYlBu")
bp