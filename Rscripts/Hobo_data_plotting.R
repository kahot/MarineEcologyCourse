library(ggplot2)
library(ggthemes)
nsite<-read.csv("Data/Nearshore_site.csv",stringsAsFactors = F)

#remove the unnecessary colums
nsite<-nsite[-1,2:4]
colnames(nsite)<-c("Date","Temp","Lux")
nsite$Temp<-as.numeric(nsite$Temp)
nsite$Lux<-as.numeric(nsite$Lux)

osite<-read.csv("Data/Offshore_site.csv", stringsAsFactors = F)
osite<-osite[-1,2:4]
colnames(osite)<-c("Date","Temp","Lux")
osite$Temp<-as.numeric(osite$Temp)
osite$Lux<- as.numeric(osite$Lux)

#merge the two to get rid of unpaired dates
mbay<-merge(osite,nsite,by="Date")

n<-mbay[,c(1,4,5)]
o<-mbay[,c(1,2,3)]
#add the site info before conbining the two file

n$Site<-"Nearshore"
o$Site<-"Offshore"

colnames(n)[2:3]<-c("Temp","Lux")
colnames(o)[2:3]<-c("Temp","Lux")
env<-rbind(n,o)


#only disply every 30 dates (create a vector)
dates<-env$Date
for (i in 2:length(dates)){
    if (i%%30!=0) dates[i]<-'' 
    
}

ggplot(env,aes(x=Date, y=Temp, color=Site))+
    geom_line(aes(x=Date, y=Temp, group=Site), linetype = 1, size=0.2)+
    scale_color_manual(values=c("orange","blue")) +
    ylab(expression("Temperature ("*~degree*C*")"))+theme_few()+
    scale_x_discrete(labels=dates)+xlab("")+
    theme(axis.text.x = element_text(size =6, angle=90, hjust=1, color="gray20"), axis.ticks.x=element_blank())+
    theme(panel.background = element_rect(color="white"))+
    theme(panel.grid.major.y = element_line(color="gray60", linetype=2,size=0.2))
ggsave("Output/Temperature_comparison.pdf", width = 8, height = 3.5)


ggplot(env,aes(x=Date, y=Lux, color=Site))+
    geom_line(aes(x=Date, y=Lux, group=Site), linetype = 1, size=0.2)+
    scale_color_manual(values=c("orange","blue")) +
    ylab("Light intensity (Lux)")+theme_few()+
    scale_x_discrete(labels=dates)+xlab("")+
    theme(axis.text.x = element_text(size =6, angle=90, hjust=1, color="gray20"), axis.ticks.x=element_blank())+
    theme(panel.background = element_rect(color="white"))+
    theme(panel.grid.major.y = element_line(color="gray60", linetype=2,size=0.2)) 
ggsave("Output/LUX_comparison.pdf", width = 8, height = 3.5)

