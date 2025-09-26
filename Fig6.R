library(ggplot2)
library(ggh4x)
setwd("~/simulation_result")  # Read the simulation results from the simulation_result file (or from the directory you specified)

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
res2<-res[-index,]

res3<-res2[1:3,]
res3<-rbind(res3,res3,res3,res3,res3)
res3$Ah<-rep(c(0.25,0.5,1,2,4),each=3)
res2<-rbind(res3,res2[-c(1:3),])


res2$Ah_factor<-factor(res2$Ah,levels = c(0.25, 0.5, 1, 2, 4))

a<-bquote("Different scales of " ~ lambda[beta]^local)
print(a)

cols <- c("#FFB000","#8C3333","#D2691E")
plot <- ggplot2::ggplot(res2, ggplot2::aes(x = Ah_factor, y = value, color = method,group =method))
plot <- plot + 
  geom_point(aes(shape = method,size=method))+
  scale_shape_manual(values = c(10,16,17))+
  scale_size_manual(values =rep(3.5,6))+
  scale_color_manual(values = cols)+
  geom_line(aes(linetype = method),size = 0.8) +
  #scale_linetype_manual(values = c("solid","solid","solid","twodash","solid","solid","twodash"))+
  scale_linetype_manual(values = c("solid","solid","twodash"))+
  #facet_grid(type~., switch = "both",scales = "free_y")+
  facet_manual(vars(type),design =matrix(c(1:3),1,3),
               strip.position = "left",
               scales = "free_y")+
  labs(x =a, y="")+
  #ggtitle(c("n=500, p=500"))+
  #scale_y_continuous(breaks=seq(0,0.1,0.02),limits = c(0,0.1))
  #scale_colour_manual(values=cols)+
  ggplot2::theme(legend.position = "right",
                 plot.title = element_text(hjust = 0.5),
                 legend.title = ggplot2::element_blank(),
                 legend.key = ggplot2::element_blank(),
                 panel.grid.major.y = ggplot2::element_line(color = "#DDDDDD"), 
                 panel.grid.major.x = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 #axis.title.y = ggplot2::element_blank(),
                 panel.border = ggplot2::element_rect(fill = NA))+
  theme(text = element_text(size =12),
        strip.text = element_text(size = 12),
        legend.position = "top")+
  guides(color=guide_legend(nrow=1, byrow=TRUE))

plot









