library(ggplot2)
library(ggh4x)
#setwd("~/R")   # Set your working directory

result1<-readRDS(file="Sensitive_n300p500_A10_partial.rds")
n<-dim(result1)[1]
value<-c(result1[,1:5])
type<-rep(c("L2-estimation Error","Test AUC","Classification Error","TPR","FPR"),each=n)
name1<-c("Local","Targeted-IFS","Targeted-IFS-all")
name2<-rep(name1, c(5,5,1))
method<-rep(name2, 5*n/11)
method<-factor(method, order=TRUE, levels = name1)


Ah<-c(rep(c(0.25,0.5,1,2,4),2),0)
Ah<-rep(Ah,5*n/11)

a<-data.frame(value,type,method,Ah)
res<-aggregate(a$value,by=list(a$method,a$type,a$Ah),mean )
colnames(res)<-c("method","type","Ah","value")
index<-which(res$type=="TPR" | res$type=="FPR")
res1<-res[index,]

index<-which(res1$method=="Local")
res1<-res1[index,]
res1$Ah_factor<-factor(res1$Ah,levels = c(0.25, 0.5, 1, 2, 4))
print(res1)






