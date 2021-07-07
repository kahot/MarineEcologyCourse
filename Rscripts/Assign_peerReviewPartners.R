require(dplyr)
students<-data.frame(Name=c("Breana","Cecilia","Charlotte","Danielle","Erick",
            "Jessenia","Safiya","Tariq","Taylor"))

#students<-data.frame(Name=c("Breana","Cecilia","Charlotte","Danielle","Erick",
                            "Safiya","Tariq","Taylor"))


students$Reviewee<-sample(students$Name)
dup1<-apply(students[1:2], 1, function(x) length(unique(x))) == 2
while (length(unique(dup1))!=1){
    print("run again")
    students$Reviewee<-sample(students$Name)
    dup1<-apply(students[1:2], 1, function(x) length(unique(x))) != 2
}

View(students)

students$Review2<-sample(students$Name)
dup2<-apply(students[1:3], 1, function(x) length(unique(x))) != 3

while (length(unique(dup2))!=1){
    print("run again")
    students$Review2<-sample(students$Name)
    dup2<-apply(students[1:3], 1, function(x) length(unique(x))) != 3
}

write.csv(students,"Output/Peer-review_assignment2.csv")

sample(stu$Student)

#Split into 3 groups:
students<-data.frame(Name=c("Breana","Cecilia","Charlotte","Danielle","Erick",
                            "Jessenia","Safiya","Tariq","Taylor"), stringsAsFactors = F)
set.seed(47)
rows<-sample(nrow(students))
students2<-data.frame(Name=students[rows,], stringsAsFactors = F)
#students2$Group<-c(rep(1:3, times=3),1)

#Split into 4 groups
students2$Group<-c(rep(1:4, times=2),1)
sections<-c("Introduction", "Methods","Results","Discussion")
assignedGroup<-data.frame(Group=paste0("Group ",1:4))

for (i in 1:nrow(assignedGroup)){
    assignedGroup$Students[i]<-paste(students2$Name[students2$Group==i], collapse = ", ")
    assignedGroup$Section[i]<-sections[i]
}


print(students2$Name[students2$Group==1])
print(students2$Name[students2$Group==2])
print(students2$Name[students2$Group==3])

#groups<-setNames(split(students2$Name, 1:3), c("Group1","Group2","Group3"))
groups

