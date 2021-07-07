library(ggplot2)
library(colorspace)
cols<-qualitative_hcl(6, palette="Dark3")

corals<-data.frame(Origin=c("HV","HV","MV","MV"), site=c("HV","MV","HV","MV"), 
                   fraction=c(0.8,0.47,0.675,0.45))
corals$site<-factor(corals$site, levels = c("MV","HV"))
ggplot(data=corals, aes(x=site, y=fraction, color=Origin, fill=Origin ))+
    scale_color_manual(values=cols[c(1,4)])+
    scale_fill_manual(values=cols[c(1,4)])+
    geom_point(size=7, shape=21)+ylab('Fraction of chlorophyll retained')+xlab('Transplant site')+
    geom_segment(data=corals, mapping=aes(x=1, y=corals[2,3], xend=2, yend=corals[1,3]), size=.8, color=cols[1])+
    geom_segment(data=corals, mapping=aes(x=2, y=corals[3,3], xend=1, yend=corals[4,3]), size=.8,  color=cols[4])
ggsave("Output/Coral_acclimatization_exp.pdf", width = 4,height = 3)


ex1<-data.frame(Coral=c("A","B"), Chl=c(2.5,2.5))
ex2<- data.frame(Coral=c("A","B"), Chl=c(0.25,0.5))
ex2<- data.frame(Coral=c("A","B"), Chl=c(0.25,0.5))

ggplot(data=ex2, aes(x=Coral, y=Chl))+
    geom_bar(stat="identity",width=0.4, color=cols[3], fill=cols[3])+
    theme_classic()+scale_y_continuous(limits=c(0,1))+ylab("Chlorophyll")+xlab('')
ggsave("Output/Coral_fraction.pdf", width = 2.5, height = 2.3)

ggplot(data=ex1, aes(x=Coral, y=Chl))+
    geom_bar(stat="identity",width=0.4, color=cols[5], fill=cols[5])+
    theme_classic()+scale_y_continuous(limits=c(0,10))+ylab("Chlorophyll")+xlab('')
ggsave("Output/Coral_count.pdf", width = 2.5, height = 2.3)



### Cadmium plot

cols<-c("orange","red")
cad<-data.frame(Sediment=c("Control area","Control area","Foundry Cove","Foundry Cove"), Animal=c("Control area","Foundry Cove","Control area","Foundry Cove"), 
                   Survivors=c(9.7,9.9,0.2, 8.8))
#cad$Animal<-factor(corals$site, levels = c("MV","HV"))
ggplot(data=cad, aes(x=Sediment, y=Survivors, color=Animal, fill=Animal ))+
    scale_color_manual(values=cols, guide="none")+
    scale_fill_manual(values=cols)+
    geom_point(size=7, shape=21)+ylab('Number of suvivors')+xlab('Sediment source')+
    geom_segment(data=cad, mapping=aes(x=1, y=cad[1,3]-0.3, xend=2, yend=cad[3,3]), size=.8,  color=cols[1])+
    geom_segment(data=cad, mapping=aes(x=2, y=cad[4,3], xend=1, yend=cad[2,3]), size=.8,  color=cols[2])+
    theme_linedraw()+
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    labs(fill="Animal source")

ggsave("Output/Cadmium_adaptation.pdf", width = 4.8,height = 3)


