library(ggplot2)
library(ggh4x)
#setwd("~/R")   # Set your working directory
#####
result1<-readRDS(file="adap_n300p500_M10_heter.rds")

n<-dim(result1)[1]
value<-c(result1[,1:3])
type<-rep(c("L2-estimation Error","Test AUC","Classification Error"),each=n)
name1<-c("Local","Oracle Targeted-IFS","Targeted-IFS","Targeted-IFS-all","Trans","Ah-Trans")
method<-rep(name1, 3*n/6)
method<-factor(method, order=TRUE, levels = name1)
Ah<-rep(result1[,4],3)
a<-data.frame(value,type,method,Ah)
res<-aggregate(a$value,by=list(a$method,a$type,a$Ah),mean )
colnames(res)<-c("method","type","Ah","value")
index<-which(res$method=="Trans")


cols <- c("#FFB000","#FF7256","#8C3333","#D2691E","#7A9D54","#016A70")
plot <- ggplot2::ggplot(res[-index,], ggplot2::aes(x = Ah, y = value, color = method))
plot <- plot + 
  geom_point(aes(shape = method,size=method))+
  scale_shape_manual(values = c(10,13,16,17,7,3))+
  scale_size_manual(values =rep(3.5,6))+
  scale_color_manual(values = cols)+
  geom_line(aes(linetype = method),size = 0.8) + 
  #scale_linetype_manual(values = c("solid","solid","solid","twodash","solid","solid","twodash"))+
  scale_linetype_manual(values = c("solid","solid","solid","twodash","twodash","twodash"))+
  #facet_grid(type~., switch = "both",scales = "free_y")+
  facet_manual(vars(type),design =matrix(c(1:3),1,3),
               strip.position = "left",
               scales = "free_y")+
  labs(x = "Number of partial transferable source", y="")+
  #ggtitle(c("n=500, p=500"))+
  #scale_y_continuous(breaks=seq(0,0.1,0.02),limits = c(0,0.1))
  #scale_colour_manual(values=cols)+
  scale_x_continuous(breaks =seq(0,12,2))+
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

#####
result1<-readRDS(file="adap_n300p500_M20_heter.rds")

n<-dim(result1)[1]
value<-c(result1[,1:3])
type<-rep(c("L2-estimation Error","Test AUC","Classification Error"),each=n)
name1<-c("Local","Oracle Targeted-IFS","Targeted-IFS","Targeted-IFS-all","Trans","Ah-Trans")
method<-rep(name1, 3*n/6)
method<-factor(method, order=TRUE, levels = name1)
Ah<-rep(result1[,4],3)
a<-data.frame(value,type,method,Ah)
res<-aggregate(a$value,by=list(a$method,a$type,a$Ah),mean )
colnames(res)<-c("method","type","Ah","value")
index<-which(res$method=="Trans")


cols <- c("#FFB000","#FF7256","#8C3333","#D2691E","#7A9D54","#016A70")
plot <- ggplot2::ggplot(res[-index,], ggplot2::aes(x = Ah, y = value, color = method))
plot <- plot + 
  geom_point(aes(shape = method,size=method))+
  scale_shape_manual(values = c(10,13,16,17,7,3))+
  scale_size_manual(values =rep(3.5,6))+
  scale_color_manual(values = cols)+
  geom_line(aes(linetype = method),size = 0.8) + 
  #scale_linetype_manual(values = c("solid","solid","solid","twodash","solid","solid","twodash"))+
  scale_linetype_manual(values = c("solid","solid","solid","twodash","twodash","twodash"))+
  #facet_grid(type~., switch = "both",scales = "free_y")+
  facet_manual(vars(type),design =matrix(c(1:3),1,3),
               strip.position = "left",
               scales = "free_y")+
  labs(x = "Number of partial transferable source", y="")+
  #ggtitle(c("n=500, p=500"))+
  #scale_y_continuous(breaks=seq(0,0.1,0.02),limits = c(0,0.1))
  #scale_colour_manual(values=cols)+
  scale_x_continuous(breaks =seq(0,12,2))+
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
