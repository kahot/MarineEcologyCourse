library(lubridate)
library(reshape2)
library(ggplot2)
library(colorspace)
### 

#Hawaii data #every 15 minutes
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
#remove the 3rd column
CA<-CA[,-3]

#Average temperature so far.
mean(CA$degree_Celsius, na.rm = T) #13.3
min(CA$degree_Celsius, na.rm=T) #-8.9159
#change to date format (need to remove T and Z)
CA$UTC<-gsub("T"," ", CA$UTC)
CA$UTC<-gsub("Z","",  CA$UTC)

#Change the time to 'time object'
CA$UTC<-as.POSIXct(CA$UTC,tz="GMT")

#Select this year's data only
CA<-CA[CA$UTC>='2020-01-01',]
write.csv(CA, "Output/MLML_2020_temp.csv")

#minimum temperature
min(CA$degree_Celsius, na.rm = T) -3.514165
CA[which.min(CA$degree_Celsius),]
#                       UTC degree_Celsius
#924036 2020-06-23 22:02:56      -3.514165

#Change the column name
colnames(CA)[2]<-"ML"

#change the time to PST
CA$PST<-with_tz(CA$UTC, "US/Pacific")


#Adjust Morro Bay date format to get rid of the seconds
CA$PST<-format(CA$PST,format='%Y-%m-%d %H:%M')
CA$PST<-as.POSIXct(CA$PST)

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

hiAug<-hiAug[hiAug$PST>="2020-08-01"]

write.csv(hiFeb,"Output/HiloBay_Feb.csv", row.names = F)
write.csv(hiAug,"Output/HiloBay_Aug.csv", row.names = F)




#Combine CA and HI data
#don't change the time
colnames(hiAug)[1]<-"Date"
colnames(hiFeb)[1]<-"Date"
colnames(caAug)[3]<-"Date"
colnames(caFeb)[3]<-"Date"

summer<-merge(caAug[,2:3],hiAug[,1:2], by="Date")
winter<-merge(caFeb[,2:3],hiFeb[,1:2], by="Date")
    
#plot(summer$PST,summer$ML, pch=16, col="purple", cex=0.5, ylim=c(10,30),
#     ylab="Temperature", xlab="")
#points(summer$PST,summer$Hawaii, pch=16, cex=.5, col="blue")

temp.sum<-data.frame(Site=c("CA-summer","HI-summer","CA-winter","HI-winter"))
temp.sum$Ave.temp<-c(mean(summer$ML, na.rm = T), mean(summer$Hawaii, na.rm = T),
                    mean(winter$ML, na.rm = T), mean(winter$Hawaii, na.rm = T))


temp.sum$SD<-c(sd(summer$ML, na.rm = T), sd(summer$Hawaii, na.rm = T),
               sd(winter$ML, na.rm = T), sd(winter$Hawaii, na.rm = T))
write.csv(temp.sum,"Output/ML-HI_temperature_summary.csv")

sum.m<-melt(summer, id.vars = "Date")
colnames(sum.m)[2:3]<-c("Site", "Temperature")

win.m<-melt(winter, id.vars = "Date")
colnames(win.m)[2:3]<-c("Site", "Temperature")


cols<-qualitative_hcl(5, palette="Dark3")
hcl_palettes(plot = TRUE)

#ggplot(sum.m, aes(x=Date, y=Temperature, color=Site))+
#    geom_point(size =1)+
#    scale_color_manual(values=cols[c(1,4)])+
#    ylab(expression('Temperature ('*~degree*C*')'))+
#    theme_bw()+
#    labs(x="")+
#    theme(legend.title = element_blank())
#

ggplot(sum.m, aes(x=Date, y=Temperature, color=Site))+
    geom_line()+
    scale_color_manual(values=cols[c(4,1)], labels=c("Moss Landing","Hilo"))+
    ylab(expression('Temperature ('*~degree*C*')'))+
    theme_bw()+
    labs(x="")+
    geom_hline(yintercept=temp.sum$Ave.temp[1:2], color="gray", linetype = "dashed")+
    theme(legend.title = element_blank())
ggsave("Output/ML_Hilo_Temp_Aug.pdf", width = 5, height = 3)

#For Feb
ggplot(win.m, aes(x=Date, y=Temperature, color=Site))+
    geom_line()+
    scale_color_manual(values=cols[c(4,1)], labels=c("Moss Landing","Hilo"))+
    ylab(expression('Temperature ('*~degree*C*')'))+
    theme_bw()+
    labs(x="")+
    geom_hline(yintercept=temp.sum$Ave.temp[3:4], color="gray", linetype = "dashed")+
    theme(legend.title = element_blank())


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
