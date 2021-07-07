
salmon<-read.csv("~/Marine Ecology Class/Resrouces Spring2021/Week14/RangeChange_BLUE_QUEBES/Atlantic Salmon/AtlanticSalmon.csv", stringsAsFactors = F)

salm<-salmon[,c("year","decimalLatitude")]

salm<-salm[salm$year>=1900,]
salm<-salm[!is.na(salm$decimalLatitude),]

plot(salm)
range(salm$year)

salmon<-salmon[salmon$year>=1900,]
salmon<-salmon[!is.na(salmon$decimalLatitude),]
salmon2<-salmon[seq(1,nrow(salmon), 10),]
plot(salmon2$year, salmon2$decimalLatitude, pch=".")

salmon3<-salmon2[salmon2$decimalLatitude>30,]
plot(salmon3$year, salmon3$decimalLatitude, pch=16, cex=0.5, col="blue")

pike<-read.csv("~/Marine Ecology Class/Resrouces Spring2021/Week14/RangeChange_BLUE_QUEBES/Pike/Pike.csv", stringsAsFactors = F)
pike<-pike[pike$year>1900,]
pike2<-pike[!is.na(pike$decimalLatitude),]

pike3<-pike2[seq(1,nrow(pike2), 10),]

plot(pike3$year,pike3$decimalLatitude, pch='.')
write.csv(pike3, "Output/Pike_trimmed.csv", row.names =F )



df<-read.csv("~/Marine Ecology Class/Resrouces Spring2021/Week14/RangeChange_BLUE_QUEBES/Lobster/AmericanLobster.csv", stringsAsFactors = F)
df<-df[df$year>=1900,]
df<-df[!is.na(df$decimalLatitude),]
plot(df$year,df$decimalLatitude, pch='.')
write.csv(df, "~/Marine Ecology Class/Resrouces Spring2021/Week14/RangeChange_BLUE_QUEBES/DataFiles/Lobster.csv", row.names = F)



files<-list.files("Data/BLUE/")

for (i in 1:length(files)){
    sp<-gsub(".csv","",files[i])
    df<-read.csv(paste0("Data/BLUE/",files[i]))
    df<-df[df$year>=1900,]
    df<-df[!is.na(df$decimalLatitude),]
    plot(df$year,df$decimalLatitude, pch='.', main=sp)
    #write.csv(df, paste0("~/Marine Ecology Class/Resrouces Spring2021/Week14/RangeChange_BLUE_QUEBES/DataFiles/", sp,".csv"), row.names = F)
    
}


# number of observation
files<-list.files("~/Marine Ecology Class/Resrouces Spring2021/Week14/RangeChange_BLUE_QUEBES/DataFiles/", pattern=".csv")
for (i in 1:length(files)){
    sp<-gsub(".csv","",files[i])
    df<-read.csv(paste0("~/Marine Ecology Class/Resrouces Spring2021/Week14/RangeChange_BLUE_QUEBES/DataFiles/",files[i]))
    n<-nrow(df)
    cat(sp, n, "\n")
}    
