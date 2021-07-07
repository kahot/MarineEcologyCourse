library(ggplot2)
library(colorspace)
cols<-qualitative_hcl(6, palette="Dark3")

whale<-read.csv("Data/whale.csv")
whale$Month<-factor(whale$Month, levels=c("Janurary", "February", "March","April"))

ggplot(data=whale, aes(x=Month, y=Count, color=factor(Year), shape=Sex))+
    geom_point(size=2.5)+
    geom_path(aes(x=Month, y=Count,group=interaction(Year,Sex)),linetype = 1, size=0.2)+
    theme_bw()+
    theme(legend.title = element_blank(),panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+
    ylab('Whale count')
ggsave("Output/Whale_colorByYear.pdf", width = 4.5, height = 3.5)    


ggplot(data=whale, aes(x=Month, y=Count, color=Sex, shape=factor(Year)))+
    geom_point(size=2.5)+
    geom_path(aes(x=Month, y=Count,group=interaction(Year,Sex)),linetype = 1, size=0.2)+
    theme_bw()+
    theme(legend.title = element_blank(),panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+
    ylab('Whale count')
ggsave("Output/Whale_colorBySex.pdf", width = 4.5, height = 3.5)    

#create a summary
sum<-data.frame(aggregate(whale$Count, by=list(whale$Month, whale$Sex), FUN=mean, na.action=na.omit))
sum$x[c(4,8)]<-c(41,19)
sum$SD<-c(17.50238079, 12.52996409,	8.185352772, 0, 17.89785834, 14.52583905, 6.806859286, 0)
colnames(sum)[1:3]<-c("Month","Sex","Mean")

ggplot(data=sum, aes(x=Month, y=Mean, color=Sex))+
    geom_point(size=2.5, position=position_dodge(width=0.3))+
    geom_path(aes(x=Month, y=Mean,group=Sex),linetype = 1, size=0.2, position=position_dodge(width=0.3))+
    geom_errorbar(data=sum, aes(x=Month,ymin=Mean-SD, ymax=Mean+SD, group=Sex), width=.2, size=.2, position=position_dodge(width=0.3))+
    theme_bw()+
    theme(legend.title = element_blank(),panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+
    ylab('Whale count')
ggsave("Output/Whale_colorBymonthSD.pdf", width = 4.5, height = 3.5)    
