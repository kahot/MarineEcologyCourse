library(reshape2)
urchin=data.frame(low=c(0,0,3,5,7,0,1,1,3,6), mid=c(1,0,1,3,2,0,0,1,3,2), high=c(0,0,1,0,0,0,0,2,0,0))

us<-melt(urchin)

library(Rcmdr) #(testing homegeneity of variance)
leveneTest(value ~ variable, data=us)

shapiro.test((us$value)) 

### Corallite Weight

library(colorspace)
cols2<-qualitative_hcl(6, palette="Dark3")


weight<-data.frame(treatment=c('Control', "Mid CO2","High CO2"),
                   Weight=c(212.93,	211.72,	132.70),
                   SD=c(86.39,	80.54,	48.53))
library(ggplot2)
weight$treatment<-factor(weight$treatment,levels=c("Control","Mid CO2","High CO2"))
ggplot(weight,aes(x=treatment,y=Weight, ymin=Weight-SD, ymax=Weight+SD))+
    geom_bar( stat="identity",width=0.8, color=cols2[4], fill=paste0(cols2[4], '99'))+
    geom_errorbar(position=position_dodge(.9), width=.2, color="gray30")+
    theme(axis.title.x=element_blank())+
    #ggtitle("Effects of elevated CO2 on corallite weight")+
    theme_linedraw()+ylab('Corallite weight (μg)')+
    labs(x="")+
    theme(axis.text.x = element_text(size=12, angle=45, hjust=1),axis.title.y = element_text(size=12))+
    theme(panel.grid.major.x = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size=13))+
    theme(plot.title = element_text(size=14,hjust=0.5))
ggsave("Output/Corallite_weight.pdf", width = 4.5, height = 4)
          


length<-data.frame(treatment=c('Control', "Mid CO2","High CO2"),
                   Weight=c(2.50,	2.44,	2.59),
                   SD=c(0.27,	0.32,	0.32))

length$treatment<-factor(length$treatment,levels=c("Control","Mid CO2","High CO2"))
ggplot(length,aes(x=treatment,y=Weight, ymin=Weight-SD, ymax=Weight+SD))+
    geom_bar( stat="identity",width=0.8, color=cols2[4], fill=paste0(cols2[4], '99'))+
    geom_errorbar(position=position_dodge(.9), width=.2, color="gray30")+
    theme(axis.title.x=element_blank())+
    #ggtitle("Effects of elevated CO2 on corallite weight")+
    theme_linedraw()+ylab('Septum length (mm)')+
    labs(x="")+
    theme(axis.text.x = element_text(size=12, angle=45, hjust=1),axis.title.y = element_text(size=12))+
    theme(panel.grid.major.x = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size=13))+
    theme(plot.title = element_text(size=14,hjust=0.5))
ggsave("Output/Corallite_weight.pdf", width = 4.5, height = 4)


#Test symbiont number data normality:
sy<-read.csv("Data/no.symbionts.per.host.csv",stringsAsFactors = F)


library(Rcmdr) #(testing homegeneity of variance)
leveneTest(Count ~ Transmission, data=sy)

shapiro.test(logit(sy$Count))

wilcox.test(sy$Count[sy$Transmission=="Vertical"], sy$Count[sy$Transmission=="Horizontal"], "greater")
wilcox.test(sy$Count[sy$Transmission=="Vertical"], sy$Count[sy$Transmission=="Horizontal"], "less")
wilcox.test(sy$Count[sy$Transmission=="Vertical"], sy$Count[sy$Transmission=="Horizontal"], "two.sided")


