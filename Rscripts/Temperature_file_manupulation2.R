library(lubridate)
library(reshape2)
library(ggplot2)
library(colorspace)
### 

#Hawaii data #every 30 minutes
HIFeb<-read.csv("Data/PacIOOS_WQB-04_202001312338_202003010006.csv", stringsAsFactors = F)
HIAug<-read.csv("Data/PacIOOS_WQB-04_202007281538_202008271621.csv", stringsAsFactors = F)

#temp only
hiFeb<-HIFeb[,1:2]
hiAug<-HIAug[,1:2]

colnames(hiAug)<-c("HST","Hawaii")
colnames(hiFeb)<-c("HST","Hawaii")

#change to date format 
#find out the different time zones use OlsonNames()
OlsonNames()
hiAug$HST<-as.POSIXct(hiAug$HST,tz="US/Hawaii")
hiFeb$HST<-as.POSIXct(hiFeb$HST,tz="US/Hawaii")

#Convert to PST
hiAug$PST<-with_tz(hiAug$HST, "US/Pacific")
hiFeb$PST<-with_tz(hiFeb$HST, "US/Pacific")


# Read the Morro Bay data
CA<-read.csv("Data/mlml_mlml_sea_5536_d855_a740.csv",stringsAsFactors = F, skip=1)
CA<-CA[,-3]
mean(CA$degree_Celsius, na.rm = T) #13.3
min(CA$degree_Celsius, na.rm=T) #-8.9159
#change to date format (need to remove T and Z)
CA$UTC<-gsub("T"," ", CA$UTC)
CA$UTC<-gsub("Z","",  CA$UTC)

#Change the time to 'time object'
CA$UTC<-as.POSIXct(CA$UTC,tz="GMT")

#Select this year's data only
CA<-CA[CA$UTC>='2020-01-01',]
min(CA$degree_Celsius, na.rm = T) -3.514165
CA[which.min(CA$degree_Celsius),]
#Change the column name
colnames(CA)[2]<-"ML"

#change the time to PST
CA$PST<-with_tz(CA$UTC, "US/Pacific")


#Adjust Morro Bay date format to get rid of the seconds
CA$PST<-format(CA$PST,format='%Y-%m-%d %H:%M')
CA$PST<-as.POSIXct(CA$PST)

#subtract a minute
#CA$PST<-CA$PST-60


#select August 2020
caAug<-CA[CA$PST>="2020-08-01",]
#select Feb 2020
caFeb<-CA[CA$PST>="2020-02-01"& CA$PST<"2020-03-01",]

#August is 2 mintues off
caAug$PST<-caAug$PST-120
#Feb is 1 mintue off
caFeb$PST<-caFeb$PST-60


write.csv(caFeb,"Output/ML_Feb.csv", row.names = F)
write.csv(caAug,"Output/ML_Aug.csv", row.names = F)

#CA has every 5 minutes, but HI has every 15 minutes. 
#Select the minutes 0 and 30 from Morro Bay data
keep<-(minute(caAug$PST)==0|minute(caAug$PST)==15|minute(caAug$PST)==30|minute(caAug$PST)==45)
caAug<-caAug[keep,]

keep<-(minute(caFeb$PST)==0|minute(caFeb$PST)==15|minute(caFeb$PST)==30|minute(caFeb$PST)==45)
caFeb<-caFeb[keep,]

write.csv(caFeb,"Output/MLML_Feb_15min.csv", row.names = F)
write.csv(caAug,"Output/MLML_Aug_15min.csv", row.names = F)

write.csv(kaneFeb,"Output/HiloBay_Feb.csv", row.names = F)
write.csv(kaneAug,"Output/HiloBay_Aug.csv", row.names = F)

#Combine CA and HI data


summer<-merge(caAug[,2:3],hiAug[,2:3], by="PST", all=T)
winter<-merge(caFeb[,2:3],hiFeb[,2:3], by="PST", all=T)
    
#plot(summer$PST,summer$ML, pch=16, col="purple", cex=0.5, ylim=c(10,30),
#     ylab="Temperature", xlab="")
#points(summer$PST,summer$Hawaii, pch=16, cex=.5, col="blue")

mean(summer$ML, na.rm = T)
mean(summer$Hawaii, na.rm = T)
mean(winter$ML, na.rm = T)#12.41498
mean(winter$Hawaii, na.rm = T)#24.21587

sum.m<-melt(summer, id.vars = "PST")
colnames(sum.m)[2:3]<-c("Site", "Temperature")

win.m<-melt(winter, id.vars = "PST")
colnames(win.m)[2:3]<-c("Site", "Temperature")


cols<-qualitative_hcl(5, palette="Dark3")
hcl_palettes(plot = TRUE)

ggplot(sum.m, aes(x=PST, y=Temperature, color=Site))+
    geom_point(size =1)+
    scale_color_manual(values=cols[c(1,4)])+
    ylab(expression('Temperature ('*~degree*C*')'))+
    theme_bw()+
    labs(x="")+
    theme(legend.title = element_blank())


ggplot(sum.m, aes(x=PST, y=Temperature, color=Site))+
    geom_line()+
    scale_color_manual(values=cols[c(4,1)], labels=c("Moss Landing","Hilo"))+
    ylab(expression('Temperature ('*~degree*C*')'))+
    theme_bw()+
    labs(x="")+
    theme(legend.title = element_blank())
ggsave("Output/ML_Hilo_Temp_Aug.pdf", width = 5, height = 3)

#For Feb
ggplot(win.m, aes(x=PST, y=Temperature, color=Site))+
    geom_line()+
    scale_color_manual(values=cols[c(4,1)], labels=c("Moss Landing","Hilo"))+
    ylab(expression('Temperature ('*~degree*C*')'))+
    theme_bw()+
    labs(x="")+
    theme(legend.title = element_blank())

min(CA$ML)
ggsave("Output/ML_Hilo_Temp_Feb.pdf", width = 5, height = 3)


## Averages
summary<-data.frame(Site=c("HI","HI","CA","CA"))
summary$month<-c("Aug","Feb","Aug","Feb")

summary$mean<-c(mean(hiAug$Hawaii, na.rm=T),mean(hiFeb$Hawaii, na.rm=T), 
                mean(caAug$ML, na.rm=T),mean(caFeb$ML, na.rm=T))
summary$SD<-c(sd(hiAug$Hawaii, na.rm=T),sd(hiFeb$Hawaii, na.rm=T), 
              sd(caAug$ML, na.rm=T),sd(caFeb$ML, na.rm=T))

ggplot(summary, aes(x=Site, y=mean, color=Site, shape=month))+
    geom_point(position=position_dodge(width=0.5),size=2)+
    scale_color_manual(values=cols[c(4,1)])+
    scale_shape_manual(values=c(19,17))+
    geom_errorbar(aes(ymin=mean-SD, ymax=mean+SD), width=.2, size=.2,
                  position=position_dodge(width=0.5))+
    theme_bw()+
    labs(x="")+ylab(expression('Temperature ('*~degree*C*')'))+
    theme(legend.title = element_blank())
ggsave("Output/ML_Hilo_AverageTemp.pdf",width=3.5, height = 3)
