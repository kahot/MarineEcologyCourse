library(ggplot2)

mid<-read.csv("Data/Midterm_2021.csv")


ggplot(mid,aes(x=Percentage))+ 
    geom_histogram(binwidth=5, color="black", fill="lightblue")+
    geom_vline(aes(xintercept=mean(Percentage)),
              color="blue", linetype="dashed", size=1)+
    theme_bw()+ylab('')

ggplot(mid,aes(x=Percentage))+ 
    geom_histogram(binwidth=10, color="black", fill="lightblue")+
    geom_vline(aes(xintercept=mean(Percentage)),
               color="blue", linetype="dashed", size=1)+
    theme_bw()+ylab('')+ylim(0,8)
ggsave("Output/Midterm2021_histogram.pdf", height = 3.5,width = 4)

