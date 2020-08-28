library(lubridate)
library(reshape2)
library(ggplot2)
library(colorspace)
### 

#Hawaii data #every 30 minutes
kaneFeb<-read.csv("Data/PacIOOS_CDIP198_202001312338_202003010021.csv", stringsAsFactors = F)
kaneAug<-read.csv("Data/PacIOOS_CDIP198_202007312340_202008271419.csv", stringsAsFactors = F)
kaneFeb<-kaneFeb[-1,]
kaneAug<-kaneAug[-1,]

colnames(kaneAug)<-c("HST","Hawaii")
colnames(kaneFeb)<-c("HST","Hawaii")

#change to date format 
#find out the different time zones use OlsonNames()
OlsonNames()
kaneAug$HST<-as.POSIXct(kaneAug$HST,tz="US/Hawaii")
kaneFeb$HST<-as.POSIXct(kaneFeb$HST,tz="US/Hawaii")
#remove seconds
#kaneAug$HST<-format(kaneAug$HST,format='%Y-%m-%d %H:%M')
#kaneFeb$HST<-format(kaneFeb$HST,format='%Y-%m-%d %H:%M')

#Convert to PST
kaneAug$PST<-with_tz(kaneAug$HST, "US/Pacific")
kaneFeb$PST<-with_tz(kaneFeb$HST, "US/Pacific")


# Read the Morro Bay data
morro<-read.csv("Data/edu_calpoly_marine_morro_5536_d855_a740.csv",stringsAsFactors = F, skip=1)
morro<-morro[,-3]

#change to date format (need to remove T and Z)
morro$UTC<-gsub("T"," ", morro$UTC)
morro$UTC<-gsub("Z","", morro$UTC)

#Change the time to time object
morro$UTC<-as.POSIXct(morro$UTC,tz="GMT")

#Change the column name
colnames(morro)[2]<-"MorroBay"

#change the time to PST
morro$PST<-with_tz(morro$UTC, "US/Pacific")

#select this year's data only
morro<-morro[morro$UTC>="2020-01-01",]

#Adjust Morro Bay date format
morro$PST<-format(morro$PST,format='%Y-%m-%d %H:%M')
morro$PST<-as.POSIXct(morro$PST)
#subtracy a minute
morro$PST<-morro$PST-60


#select August 2020
morroAug<-morro[morro$UTC>="2020-08-01",]
#select Feb 2020
morroFeb<-morro[morro$UTC>="2020-02-01"&morro$UTC<"2020-03-01",]

write.csv(morroFeb,"Output/MorroBay_Feb.csv", row.names = F)
write.csv(morroAug,"Output/MorroBay_Aug.csv", row.names = F)

#Morro has every 15 minutes, but Kaneohe has every 30 minutes. 
#Select the minutes 0 and 30 from Morro Bay data
keep<-(minute(morroAug$PST)==0|minute(morroAug$PST)==30)
morroAug<-morroAug[keep,]

keep<-(minute(morroFeb$PST)==0|minute(morroFeb$PST)==30)
morroFeb<-morroFeb[keep,]

#alternatively, we can just keep the dates that appear in Hawaii data file
#morroAug<-morroAug[morroAug$PST %in%kaneAug$PST, ]


write.csv(morroFeb,"Output/MorroBay_Feb_30min.csv", row.names = F)
write.csv(morroAug,"Output/MorroBay_Aug_30min.csv", row.names = F)

write.csv(kaneFeb,"Output/KaneoheBay_Feb.csv", row.names = F)
write.csv(kaneAug,"Output/KaneoheBay_Aug.csv", row.names = F)

#Combine morro and kaneohe data


summer<-merge(morroAug[,2:3],kaneAug[,2:3], by="PST", all=T)
winter<-merge(morroFeb[,2:3],kaneFeb[,2:3], by="PST", all=T)
    
#plot(summer$PST,summer$MorroBay, pch=16, col="purple", cex=0.5, ylim=c(10,30),
#     ylab="Temperature", xlab="")
#points(summer$PST,summer$Hawaii, pch=16, cex=.5, col="blue")

mean(summer$MorroBay, na.rm = T)
mean(summer$Hawaii, na.rm = T)
mean(winter$MorroBay, na.rm = T)#13.10609
mean(winter$Hawaii, na.rm = T)#24.37853

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
    scale_color_manual(values=cols[c(1,4)])+
    ylab(expression('Temperature ('*~degree*C*')'))+
    theme_bw()+
    labs(x="")+
    theme(legend.title = element_blank())
ggsave("Output/MorroBay_KaneoheBay_Temp_Aug.pdf", width = 5, height = 3)

#For Feb

ggplot(win.m, aes(x=PST, y=Temperature, color=Site))+
    geom_line()+
    scale_color_manual(values=cols[c(1,4)])+
    ylab(expression('Temperature ('*~degree*C*')'))+
    theme_bw()+
    labs(x="")+
    theme(legend.title = element_blank())
ggsave("Output/MorroBay_KaneoheBay_Temp_Feb.pdf", width = 5, height = 3)
