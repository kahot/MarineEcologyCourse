
p1<-c(3/9,3/9,3/9)
p2<-c(1/14,1/14,12/14)
p3<-c(2/9,7/9)

shannon<-function(p){
    h<-p*log(p)
    H<-sum(h)
    return(-H)
}


shannon(p1)
shannon(p2)
shannon(p3)

simp<-function(p){
    h<-p^2
    H<-1/sum(h)
    return(H)
}

simp(p1)
simp(p2)
simp(p3)
p1^2
1/3*1/3
