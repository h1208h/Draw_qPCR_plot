library(ggfortify)
library(ggrepel)
library(factoextra)
library(data.table)
library(gtable)
library(grid)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
color=brewer.pal(n = 8, name = 'Dark2')
#Method#################################################################
data<-read.csv("20200322_HH_serial_dilution_method_compare_raw_data_t.csv",check.names = F,sep=',')
data.melt<-reshape2::melt(data)
method_levels=levels(data.melt$Method)
max_value=max(data.melt$value)
k=1
for (i in 1:length(method_levels)){
  print (method_levels[i])
  data.melt.tmp<-data.melt[data.melt$Method == method_levels[i],]
  #title=paste0("20.03.22 E coli serial dilution qPCR Method ",method_levels[i])
  title=paste0("Method_",method_levels[i])
  p<-ggplot(data=data.melt.tmp,aes(x=variable,y=value,group=Sample,color=Cell))
  p<-p+geom_line(size=0.7)+theme_bw()
  p<-p+scale_color_manual(values=color)
  #p<-p+scale_color_manual(values=brewer.pal(n = 5, name = 'Dark1'))
  p<-p+labs(title=title,x='',y='',size=15)
  p<-p+theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7),
                 axis.text.y = element_text(size=10),
                 plot.margin = unit(c(0, 0, 0, 0), "cm"),
                 plot.title = element_text(size=15))+ylim(0,max_value+1)
  assign(paste0("p",k),p)
  k<-k+1
}

data.melt.total=data.melt
data.melt.total$Method<-NULL
p<-ggplot(data=data.melt.total,aes(x=variable,y=value,group=Sample,color=Cell))
p<-p+geom_line(size=0.7)+theme_bw()
p<-p+scale_color_manual(values=color)
p<-p+labs(title="Method_Whole",x='',y='',size=15)
p<-p+theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7),
           axis.text.y = element_text(size=10),
           plot.margin = unit(c(0, 0, 0, 0), "cm"),
           plot.title = element_text(size=15))+ylim(0,max_value+1)

merged_p<-ggarrange(p,p1,p2,p3,p4,p5,labels = c("1)","2)","3)","4)","5)","6)"),
                    ncol=3,nrow=2, common.legend = T,legend="right")
annotate_figure(merged_p,
                top = text_grob("20.03.22 E. coli serial dilution qPCR", size = 20))
######
#Cell#
######

data<-read.csv("20200322_HH_serial_dilution_method_compare_raw_data_t.csv",check.names = F,sep=',')
data.melt<-reshape2::melt(data)
cell_levels=levels(data.melt$Cell)
max_value=max(data.melt$value)
k=1
for (i in 1:length(cell_levels)){
  print (cell_levels[i])
  data.melt.tmp<-data.melt[data.melt$Cell == cell_levels[i],]
  #title=paste0("20.03.22 E coli serial dilution qPCR Method ",method_levels[i])
  title=paste0("Cell_",cell_levels[i])
  p<-ggplot(data=data.melt.tmp,aes(x=variable,y=value,group=Sample,color=Method))
  p<-p+geom_line(size=0.7)+theme_bw()
  p<-p+scale_color_manual(values=color)
  #p<-p+scale_color_manual(values=brewer.pal(n = 5, name = 'Dark1'))
  p<-p+labs(title=title,x='',y='',size=15)
  p<-p+theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7),
             axis.text.y = element_text(size=10),
             plot.margin = unit(c(0, 0, 0, 0), "cm"),
             plot.title = element_text(size=15))+ylim(0,max_value+1)
  assign(paste0("p",k),p)
  k<-k+1
}

data.melt.total=data.melt
data.melt.total$Cell<-NULL
p<-ggplot(data=data.melt.total,aes(x=variable,y=value,group=Sample,color=Method))
p<-p+geom_line(size=0.7)+theme_bw()
p<-p+scale_color_manual(values=color)
p<-p+labs(title="Cell_Whole",x='',y='',size=15)
p<-p+theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7),
           axis.text.y = element_text(size=10),
           plot.margin = unit(c(0, 0, 0, 0), "cm"),
           plot.title = element_text(size=15))+ylim(0,max_value+1)

merged_p<-ggarrange(p,p1,p2,p3,p4,labels = c("1)","2)","3)","4)","5)"),
                    ncol=3,nrow=2, common.legend = T,legend="right")
annotate_figure(merged_p,
                top = text_grob("20.03.22 E. coli serial dilution qPCR", size = 20))

