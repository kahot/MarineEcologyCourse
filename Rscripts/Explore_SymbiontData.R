#read the data
sym<-read.csv("Data/journal.pone.0044970.s003.CSV", stringsAsFactors = F)
host<-read.csv("Data/journal.pone.0044970.s004.CSV", stringsAsFactors = F)
host$Genus<-gsub(' [A-z ]*', '' ,host$Coral.species)

host<-host[host$Transmission!='',]
coralnames<-host$Coral.species

#only the one that has horizontal or vertical information
#4833 rows
sym<-sym[sym$Host_Sci_Name %in% coralnames,] #2322

unique(sym$Type)
table(sym$Host_Genus, sym$Clade)
#table(sym$Host_Genus, sym$Type)

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

#simplify
symb$Type<-gsub("in as ",'',symb$Type)
symb$Type<-gsub(' [A-z ]*', '' ,symb$Type)
symb$Type<-gsub('(type[0-9])', '' ,symb$Type)
symb$Type<-gsub("\\(\\)", '', symb$Type)
symb$Type<-gsub("_new", '', symb$Type)
symb$Type<-gsub("porites", '', symb$Type)

symb$Type2<-symb$Type
symb$Type2<-gsub('_.*', '' ,symb$Type2)
symb$Type2<-gsub('\\..*', '' ,symb$Type2)
symb$Type2<-gsub('\\:.*', '' ,symb$Type2)
symb$Type2<-gsub("([0-9])[A-z]", '\\1' ,symb$Type2)
symb$Type2<-gsub('\\-.*', '' ,symb$Type2)
symb<-symb[symb$Type2!="C#",]
symb$Type2<-gsub('\\#', '' ,symb$Type2)


unique(symb$Type2) #97

comb<-table(symb$Host_Genus, symb$Type2)
comb.df<-as.data.frame.matrix(comb)

write.csv(comb.df, "Output/Host-symb_matrix_transmission.csv")

#species level
comb<-table(symb$Host_Sci_Name, symb$Type2)
comb.df<-as.data.frame.matrix(comb)
comb.df$Coral.species<-rownames(comb.df)
DF<-merge(host,comb.df, by="Coral.species")
write.csv(DF, "Output/Host.sp-symb_matrix_transmission.csv")



host$Genus<-gsub(' [A-z ]*', '' ,host$Coral.species)
host<-host[host$Transmission!='',]



###  # No.2
sym<-read.csv("Data/journal.pone.0044970.s003.CSV", stringsAsFactors = F)
host<-read.csv("Data/journal.pone.0044970.s004.CSV", stringsAsFactors = F)
host$Genus<-gsub(' [A-z ]*', '' ,host$Coral.species)

host<-host[host$Transmission!='',]
coralnames<-host$Coral.species

#only the one that has horizontal or vertical information
#4833 rows
sym<-sym[sym$Host_Sci_Name %in% coralnames,] #2322

unique(sym$Type)
table(sym$Host_Genus, sym$Clade)
#table(sym$Host_Genus, sym$Type)

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

symb$Type<-gsub("in as ",'',symb$Type)
symb$Type<-gsub(' [A-z ]*', '' ,symb$Type)

comb<-table(symb$Host_Sci_Name, symb$Type)
comb.df<-as.data.frame.matrix(comb)
comb.df$Coral.species<-rownames(comb.df)
DF<-merge(host,comb.df, by="Coral.species")
write.csv(DF, "Output/Host.sp-symb_matrix_transmission_nosimplify.csv")

DF$No.symb.type<-apply(DF[4:199], 1,function(x) length(x[x>0]))
hor<-DF[DF$Transmission=="Horizontal",]
ver<-DF[DF$Transmission=="Vertical",]

hist(hor$No.symb.type)
hist(ver$No.symb.type)
mean(hor$No.symb.type)

byGenus<-aggregate(DF[,"No.symb.type"], list(DF$Genus), mean)
plot(byGenus$x)
