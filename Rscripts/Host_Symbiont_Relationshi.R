#read the data
sym<-read.csv("Data/journal.pone.0044970.s003.CSV", stringsAsFactors = F)
host<-read.csv("Data/journal.pone.0044970.s004.CSV", stringsAsFactors = F)
#host$Genus<-gsub(' [A-z ]*', '' ,host$Coral.species)

host<-host[host$Transmission!='',]
coralnames<-host$Coral.species

#only the one that has horizontal or vertical information
#4833 rows
sym<-sym[sym$Host_Sci_Name %in% coralnames,] #2322

#make the Type consistent (no redundant names)
sym1<-sym[sym$Redundancy=="N_A",]
sym2<-sym[sym$Redundancy!="N_A",]

sym2$type1<-sym2$Type
sym2$type2<-sym2$Redundancy
sym2$type2<-gsub("same_as_",'',sym2$type2)

sym2$type3<-apply(sym2[c("type1","type2")],1, function(x) if (nchar(x["type1"])<=nchar(x["type2"])) x<-x["type1"] else x["type2"] )
unique(sym2$type3)    #42 types

sym2$Type<-sym2$type3
symb<-rbind(sym1,sym2[,1:34])

#remove "N_A"
symb<-symb[symb$Type!="N_A",]

unique(symb$Type)  #196


#species level
comb<-table(symb$Host_Sci_Name, symb$Type)
comb.df<-as.data.frame.matrix(comb)
comb.df$Coral.species<-rownames(comb.df)
DF<-merge(host,comb.df, by="Coral.species")
write.csv(DF, "Output/Host.sp-symb_matrix_transmission.csv")

## Analyze the data 

DF$No.symb<-apply(DF[4:198], 1,function(x) length(x[x>0]))
hor<-DF[DF$Transmission=="Horizontal",]
ver<-DF[DF$Transmission=="Vertical",]

hist(hor$No.symb)
hist(ver$No.symb)

byTrans<-aggregate(DF[,"No.symb"], list(DF$Transmission), mean)

#     Group.1        x
#1 Horizontal 4.478992
#2      Mixed 5.000000
#3   Vertical 5.386364

#how many host per symbiont?
sDF<-data.frame(t(DF), stringsAsFactors = F)
colnames(sDF)<-as.character(sDF[1,])
sDF<-sDF[-c(1,2),]
sDF$No.host<-apply(sDF, 1,function(x) length(x[x>0]))

###########
###########
#include all?
symA<-read.csv("Data/journal.pone.0044970.s003.CSV", stringsAsFactors = F)
#remove "N_A"
symA<-symA[symA$Type!="N_A",]

#make the Type consistent (no redundant names)
sym1<-symA[symA$Redundancy=="N_A",]
sym2<-symA[symA$Redundancy!="N_A",]

sym2$type1<-sym2$Type
sym2$type2<-sym2$Redundancy
sym2$type2<-gsub("same_as_",'',sym2$type2)

sym2$type3<-apply(sym2[c("type1","type2")],1, function(x) if (nchar(x["type1"])<=nchar(x["type2"])) x<-x["type1"] else x["type2"] )

sym2$Type<-sym2$type3
symbA<-rbind(sym1,sym2[,1:34])

unique(symbA$Type)  
symbA$Type<-gsub("in as ",'',symbA$Type)
unique(symbA$Type) #446

#species level
combA<-table(symbA$Host_Sci_Name, symbA$Type)
combA.df<-as.data.frame.matrix(combA)
#count the non-zero column numbers
combA.df$No.symb<-apply(combA.df, 1,function(x) length(x[x>0]))

combA.df$Coral.species<-rownames(combA.df)
#remove "N_A"
combA.df<-combA.df[combA.df$Coral.species!="N_A",]

mean(combA.df$No.symb) # 3.602541

#merge with transmission mode data
all<-merge(host,combA.df, by="Coral.species", all.y=T)



#calculate # of hosts per symbiont
all["No.host",3:448]<-apply(all[3:448], 2,function(x) length(x[x>0]))
all["No.host",]

No.host<-as.vector(t(all["No.host",3:448]))
hist(No.host, breaks=100, xlim=c(0,40))
mean(No.host) #4.450673

write.csv(all, "Output/Host-symb_matrix_transmission.All.csv")

#Answers for assignment
max(all$No.symb, na.rm=T)
all$Coral.species[which.max(all$No.symb)]

#Q1 3.602541
#Q2 60, "Sorites sp."

#Q3
mean(all$No.symb[all$Transmission=="Vertical"], na.rm=T)
#[1] 5.454545
mean(all$No.symb[all$Transmission=="Horizontal"], na.rm=T)
#[1] 4.487395

########################
#Work on Symbionts; How many host per symbionts?

all2<-data.frame(t(all), stringsAsFactors = F)
#write.csv(all2, "Output/Symb_no.host.all.csv")

#all2<-read.csv("Output/Symb_no.host.all.csv", stringsAsFactors = F, row.names = 1)
names<-paste0(all2[1,1:ncol(all2)])
#colnames(all2)<-paste0(names)
all2<-all2[-1,]
all2<-all2[-1,]
colnames(all2)<-names
all2<-apply(all2, 1, as.numeric)
all2<-data.frame(t(all2))
all2$Num.host<-apply(all2[1:], 1,function(x) length(x[x>0]))



#Only the species with transmission info
all1<-all[!is.na(all$Transmission),]
all1["No.host",3:448]<-apply(all1[3:448], 2,function(x) length(x[x>0]))

nohost<-t(all1[166,3:448])
hist(nohost, breaks=100,xlim=c(0,40))

gen<-which(all1[166,]>=2 )
generalist<-all1[,c(1:2,449,gen)]

gen.sum<-data.frame(symbiont=colnames(generalist)[4:ncol(generalist)])
k=1
for (i in 4:ncol(generalist)){
    type<-colnames(generalist)[i]
    df<-generalist[1:165,c(1:3, i)]
    re<-df$Transmission[df[,type]!=0]
    gen.sum$hor[k]<-length(re[re=="Horizontal"])
    gen.sum$ver[k]<-length(re[re=="Vertical"])
    k=k+1
}    

spe<-which(all1[166,]==1)
specialist<-all1[,c(1:2,449,spe)]

spe.sum<-data.frame(symbiont=colnames(specialist)[4:ncol(specialist)])
k=1
for (i in 4:ncol(specialist)){
    type<-colnames(specialist)[i]
    df<-specialist[1:165,c(1:3, i)]
    re<-df$Transmission[df[,type]!=0]
    spe.sum$hor[k]<-length(re[re=="Horizontal"])
    spe.sum$ver[k]<-length(re[re=="Vertical"])
    k=k+1
}    


sum(spe.sum[,2])
sum(spe.sum[,3])
aggregate(spe.sum[,2:3],by=c("Horizontal","Vertical") sum)

special<-data.frame(t(specialist))

del<-which(colSums(specialist[4:ncol(specialist)])==0)
del2<-del+3
specialist<-specialist[,-del2]
sum<-data.frame(symbiont=colnames(specialist)[4:ncol(specialist)])

k=1
for (i in 4:ncol(specialist)){
    re<-specialist$Transmission[specialist[,i]]
    sum$hor[k]<-length(re[re=="Horizontal"])
    sum$ver[k]<-length(re[re=="Vertical"])
    k=k+1
}    
    




specialist$Transmission[specialist[,i]]
apply(specialist[1:165,4:429], 2, function(x) )

table(generalist$Transmission)
table(specialist$Transmission)
