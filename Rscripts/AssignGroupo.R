require(dplyr)


# Split into 7 groups
stu<-read.csv("Data/students2021.csv", stringsAsFactors = F)
#random order
s<-sample(stu$Student)

df<-data.frame(students=s)
df$Group<-rep_len(1:7, length.out=nrow(df))

assignedGroup<-data.frame(Group=paste0("Group ",1:7))

for (i in 1:7){
    assignedGroup$Students[i]<-paste(df$students[df$Group==i],collapse = ", ")
}



# determine the order of selection
v<-1:7
sample(v)
