library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
Data <- read_csv("~/Documents/R Project/For Project/Homework-/Data.csv")
data1<-gather(Data,years,value,-1)
data1$years<-factor(data1$years)
ggplot(data1, aes(x = years, y = value, group = type,linetype = type)) +
  geom_line(key_glyph = "timeseries") + geom_point() + theme(axis.text=element_text(size=13),
  axis.title=element_text(size=25,face="bold"),
  legend.text = element_text(size=20),
  legend.title = element_text(size=23),
  legend.key.size = unit(20,"pt"))
  

##GDP与M0
M0G<-data.frame(data1[which(data1$type=="M0"|
                              data1$type=="GDP"),])
ggplot(M0G, aes(x = years, y = value, group = type,linetype = type)) +
  geom_line(key_glyph = "timeseries") + geom_point() + 
  theme(axis.text=element_text(size=13),axis.title=element_text(size=25,face="bold"),
   legend.text = element_text(size=20),
   legend.title = element_text(size=23),
   legend.key.size = unit(20,"pt"))

M0G<-spread(M0G,key=type,value = value,1)
ggplot(data = M0G, aes(x = M0, y = GDP)) + 
  geom_point()+theme(axis.text=element_text(size=13),
                     axis.title=element_text(size=25,face="bold"))
model<-lm(GDP~M0,data = M0G)
ggplot(data = M0G, aes(x = M0, y = GDP)) + 
  geom_point()+theme(axis.text=element_text(size=13),
                   axis.title=element_text(size=25,face="bold"))+
  geom_smooth(method = 'lm')

model<-lm(GDP~log(M0),data = M0G)
ggplot(data = M0G, aes(x = M0, y = log(GDP))) + 
  geom_point()+theme(axis.text=element_text(size=13),
                     axis.title=element_text(size=25,face="bold"))+
  geom_smooth(method = 'lm')


#model<-lm(M0~GDP,data = M0G)
#ggplot(data = M0G, aes(x = GDP, y = M0)) + 
#  geom_point()+theme(axis.text=element_text(size=13),
#                     axis.title=element_text(size=25,face="bold"))+
#  geom_smooth(method = 'lm')



##GDP与M1
M1G<-data.frame(data1[which(data1$type=="M1"|
                              data1$type=="GDP"),])
ggplot(M1G, aes(x = years, y = value, group = type,linetype = type)) +
  geom_line(key_glyph = "timeseries") + geom_point() + 
  theme(axis.text=element_text(size=13),axis.title=element_text(size=25,face="bold"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=23),
        legend.key.size = unit(20,"pt"))

M1G<-spread(M1G,key=type,value = value,1)
ggplot(data = M1G, aes(x = M1, y = GDP)) + 
  geom_point()+theme(axis.text=element_text(size=13),
                     axis.title=element_text(size=25,face="bold"))
model1<-lm(GDP~M1,data = M1G)
ggplot(data = M1G, aes(x = M1, y = GDP)) + 
  geom_point()+theme(axis.text=element_text(size=13),
                     axis.title=element_text(size=25,face="bold"))+
  geom_smooth(method = 'lm')

model2<-lm(M1~GDP,data = M1G)
ggplot(data = M1G, aes(x = GDP, y = M1)) + 
  geom_point()+theme(axis.text=element_text(size=13),
                     axis.title=element_text(size=25,face="bold"))+
  geom_smooth(method = 'lm')

model3<-lm(log(GDP)~M1,data = M1G)
ggplot(data = M1G, aes(x = M1, y = log(GDP))) + 
  geom_point()+theme(axis.text=element_text(size=13),
                     axis.title=element_text(size=25,face="bold"))+
  geom_smooth(method = 'lm')

model4<-lm(GDP~log(M1),data = M1G)
ggplot(data = M1G, aes(x = log(M1), y = GDP)) + 
  geom_point()+theme(axis.text=element_text(size=13),
                     axis.title=element_text(size=25,face="bold"))+
  geom_smooth(method = 'lm')



##GDP与M2
M2G<-data.frame(data1[which(data1$type=="M2"|
                              data1$type=="GDP"),])
ggplot(M2G, aes(x = years, y = value, group = type,linetype = type)) +
  geom_line(key_glyph = "timeseries") + geom_point() + 
  theme(axis.text=element_text(size=13),axis.title=element_text(size=25,face="bold"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=23),
        legend.key.size = unit(20,"pt"))

M2G<-spread(M2G,key=type,value = value,1)
ggplot(data = M2G, aes(x = M2, y = GDP)) + 
  geom_point()+theme(axis.text=element_text(size=13),
                     axis.title=element_text(size=25,face="bold"))
model1<-lm(GDP~M2,data = M2G)
ggplot(data = M2G, aes(x = M2, y = GDP)) + 
  geom_point()+theme(axis.text=element_text(size=13),
                     axis.title=element_text(size=25,face="bold"))+
  geom_smooth(method = 'lm')

model2<-lm(M2~GDP,data = M2G)
ggplot(data = M2G, aes(x = GDP, y = M2)) + 
  geom_point()+theme(axis.text=element_text(size=13),
                     axis.title=element_text(size=25,face="bold"))+
  geom_smooth(method = 'lm')

model3<-lm(log(GDP)~M2,data = M2G)
ggplot(data = M2G, aes(x = M2, y = log(GDP))) + 
  geom_point()+theme(axis.text=element_text(size=13),
                     axis.title=element_text(size=25,face="bold"))+
  geom_smooth(method = 'lm')

model4<-lm(GDP~log(M2),data = M2G)
ggplot(data = M2G, aes(x = log(M2), y = GDP)) + 
  geom_point()+theme(axis.text=element_text(size=13),
                     axis.title=element_text(size=25,face="bold"))+
  geom_smooth(method = 'lm')


### CPI,M2 and GDP 

##求各自变化率后另外储存











##附加
##data1<-spread(data1,key=Type,value = value,1) ## 列行变量转制，更改表格模式
##ggplot(data1, aes(x = years, y = M0, group = 1)) +
##  geom_line()
