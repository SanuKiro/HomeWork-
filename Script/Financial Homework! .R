library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
Data <- read_csv("~/Documents/R Project/For Project/Homework-/Data.csv")
data1<-gather(Data,years,value,-1)
data1$years<-factor(data1$years)
ggplot(data1, aes(x = years, y = value, group = type,linetype = type)) +
  geom_line(key_glyph = "timeseries",size = 1) + geom_point(size = 4) + 
  theme(axis.text=element_text(size=13),
  axis.title=element_text(size=25,face="bold"),
  legend.text = element_text(size=20),
  legend.title = element_text(size=23),
  legend.key.size = unit(20,"pt"))

###中国外汇储备变化与货币供应量（M0, M1, M2) 变动的关系
  
##外汇与M0
M0F<-data.frame(data1[which(data1$type=="M0"|
                              data1$type=="Foreign exchange reserves"),])
ggplot(M0F, aes(x = years, y = value, group = type,linetype = type)) +
  geom_line(size = 1) + geom_point(size = 4)+ theme(axis.text=element_text(size=13),
                                                    axis.title=element_text(size=25,face="bold"),
                                                    legend.text = element_text(size=20),
                                                    legend.title = element_text(size=23),
                                                    legend.key.size = unit(20,"pt"))
M0F<-spread(M0F,key=type,value = value,1)
M0F<-rename(M0F,"FER"="Foreign exchange reserves")
ggplot(data = M0F, aes(x =  FER, y = M0)) + geom_point()

#建模，回归分析
model<-lm(FER~M0,data = M0F)
ggplot(data = M0F, aes(x =  M0, y = FER)) +  
  geom_point(size = 4)+
  geom_smooth(method = 'lm')+
theme(axis.text=element_text(size=30),
      axis.title=element_text(size=30,face="bold"))
#随后自变量交换
model<-lm(M0~FER,data = M0F)
ggplot(data = M0F, aes(x =  FER, y = M0)) +  
  geom_point(size = 4)+
  geom_smooth(method = 'lm')+
theme(axis.text=element_text(size=30),
      axis.title=element_text(size=30,face="bold"))




##外汇与M1
M1F<-data.frame(data1[which(data1$type=="M1"|
                              data1$type=="Foreign exchange reserves"),])
ggplot(M1F, aes(x = years, y = value, group = type,linetype = type)) +
  geom_line(size = 1) + geom_point(size = 4)+ theme(axis.text=element_text(size=13),
                                                    axis.title=element_text(size=25,face="bold"),
                                                    legend.text = element_text(size=20),
                                                    legend.title = element_text(size=23),
                                                    legend.key.size = unit(20,"pt"))
M1F<-spread(M1F,key=type,value = value,1)
M1F<-rename(M1F,"FER"="Foreign exchange reserves")
ggplot(data = M1F, aes(x =  FER, y = M1)) + geom_point()
model<-lm(Fer~M1,data = M1F)
ggplot(data = M1F, aes(x = FER, y = M1)) + geom_point(size = 4)+geom_smooth(method = 'lm')+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))


##外汇与M2
M2F<-data.frame(data1[which(data1$type=="M2"|
                              data1$type=="Foreign exchange reserves"),])
ggplot(M2F, aes(x = years, y = value, group = type,linetype = type)) +
  geom_line(size = 1) + geom_point(size = 4)+ theme(axis.text=element_text(size=13),
                                                    axis.title=element_text(size=25,face="bold"),
                                                    legend.text = element_text(size=20),
                                                    legend.title = element_text(size=23),
                                                    legend.key.size = unit(20,"pt"))
M2F<-spread(M2F,key=type,value = value,1)
M2F<-rename(M2F,"FER"="Foreign exchange reserves")
ggplot(data = M2F, aes(x =  FER, y = M2)) + geom_point()
model<-lm(Fer~M2,data = M2F)
ggplot(data = M2F, aes(x =  FER, y = M2)) + geom_point(size = 4)+geom_smooth(method = 'lm')+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))


##FER与(M1-M0)和(M2-M1)
FM012<-data.frame(years=unique(data1$years),Y=NA,X=NA,M0=NA,FER=NA)
                                            ## Y 即M2-M1, X 即M1-M0
##往数据框内输入Y 
for(i in 0:20){
  t1 <- data1[which(data1$type == "M2" & data1$years == 2000+i),3] 
  t2 <- data1[which(data1$type == "M1" & data1$years == 2000+i),3]
  t3 <- t1-t2
  FM012[i+1,2]<-t3
}
##往数据框内输入X 
for(i in 0:20){
  t1 <- data1[which(data1$type == "M1" & data1$years == 2000+i),3] 
  t2 <- data1[which(data1$type == "M0" & data1$years == 2000+i),3]
  t3 <- t1-t2
  FM012[i+1,3]<-t3
}
##往数据框内输入M0
for(i in 0:20){
  t <- data1[which(data1$type == "M0" & data1$years == 2000+i),3] 
  FM012[i+1,4]<-t
}
##往数据框内输入FER
for(i in 0:20){
  t <- data1[which(data1$type == "Foreign exchange reserves" & data1$years == 2000+i),3] 
  FM012[i+1,5]<-t
}
modelX <- lm(FER~X+Y+M0, data = FM012 )
summary(modelX)



###中国国内生产总值与货币供应量（M0, M1, M2) 变动的关系


##GDP与M0
M0G<-data.frame(data1[which(data1$type=="M0"|
                              data1$type=="GDP"),])
ggplot(M0G, aes(x = years, y = value, group = type,linetype = type)) +
  geom_line(key_glyph = "timeseries",size = 1) + geom_point(size = 4) + 
  theme(axis.text=element_text(size=13),axis.title=element_text(size=25,face="bold"),
   legend.text = element_text(size=20),
   legend.title = element_text(size=23),
   legend.key.size = unit(20,"pt"))

M0G<-spread(M0G,key=type,value = value,1)
ggplot(data = M0G, aes(x = M0, y = GDP)) + 
  geom_point(size = 4)+theme(axis.text=element_text(size=13),
                     axis.title=element_text(size=25,face="bold"))
model<-lm(GDP~M0,data = M0G)
ggplot(data = M0G, aes(x = M0, y = GDP)) + 
  geom_point(size = 4)+theme(axis.text=element_text(size=13),
                   axis.title=element_text(size=25,face="bold"))+
  geom_smooth(method = 'lm')

model<-lm(GDP~log(M0),data = M0G)
ggplot(data = M0G, aes(x = M0, y = log(GDP))) + 
  geom_point(size = 4)+theme(axis.text=element_text(size=13),
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
  geom_line(key_glyph = "timeseries",size = 1) + geom_point(size = 4) + 
  theme(axis.text=element_text(size=13),axis.title=element_text(size=25,face="bold"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=23),
        legend.key.size = unit(20,"pt"))

M1G<-spread(M1G,key=type,value = value,1)
ggplot(data = M1G, aes(x = M1, y = GDP)) + 
  geom_point(size = 4)+theme(axis.text=element_text(size=13),
                     axis.title=element_text(size=25,face="bold"))
model1<-lm(GDP~M1,data = M1G)
ggplot(data = M1G, aes(x = M1, y = GDP)) + 
  geom_point(size = 4)+theme(axis.text=element_text(size=13),
                     axis.title=element_text(size=25,face="bold"))+
  geom_smooth(method = 'lm')

model2<-lm(M1~GDP,data = M1G)
ggplot(data = M1G, aes(x = GDP, y = M1)) + 
  geom_point(size = 4)+theme(axis.text=element_text(size=13),
                     axis.title=element_text(size=25,face="bold"))+
  geom_smooth(method = 'lm')

model3<-lm(log(GDP)~M1,data = M1G)
ggplot(data = M1G, aes(x = M1, y = log(GDP))) + 
  geom_point(size = 4)+theme(axis.text=element_text(size=13),
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
  geom_line(key_glyph = "timeseries",size = 1) + geom_point(size = 4) + 
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
  geom_point(size = 4)+theme(axis.text=element_text(size=13),
                     axis.title=element_text(size=25,face="bold"))+
  geom_smooth(method = 'lm')

model2<-lm(M2~GDP,data = M2G)
ggplot(data = M2G, aes(x = GDP, y = M2)) + 
  geom_point(size = 4)+theme(axis.text=element_text(size=13),
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
data0<-data.frame(years=unique(data1$years)[-1],M2=NA,GDP=NA,CPI=NA)
for(i in 1:20){
  t1 <- data1[which(data1$type == "M2" & data1$years == 2000+i),3] 
  t2 <- data1[which(data1$type == "M2" & data1$years == 2000+i-1),3]
  t3 <- (t1-t2)/t2*100
  data0[i,2]<-t3
}
for(i in 1:20){
  t1 <- data1[which(data1$type == "GDP" & data1$years == 2000+i),3] 
  t2 <- data1[which(data1$type == "GDP" & data1$years == 2000+i-1),3]
  t3 <- (t1-t2)/t2*100
  data0[i,3]<-t3
}
for(i in 1:20){
  t1 <- data1[which(data1$type == "CPI" & data1$years == 2000+i),3] 
  t2 <- data1[which(data1$type == "CPI" & data1$years == 2000+i-1),3]
  t3 <- (t1-t2)/t2*100
  data0[i,4]<-t3
}
data0.1<-gather(data0,type,value,2:4)
ggplot(data0.1, aes(x = years, y = value, group = type,linetype = type)) +
  geom_line(key_glyph = "timeseries",size = 1) + geom_point(size = 4) +
  theme(axis.text=element_text(size=15,face="bold"),axis.title=element_text(size=25,face="bold"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=23),
        legend.key.size = unit(20,"pt"))


model1 <- lm(CPI~GDP+M2, data = data0)
ggplot(data = data0, aes(x = M2, y = CPI)) + 
  geom_point(size = 4)+theme(axis.text=element_text(size=13),
                     axis.title=element_text(size=25,face="bold"))+
  geom_smooth(method = 'lm')
ggplot(data = data0, aes(x = GDP, y = CPI)) + 
  geom_point(size = 4)+theme(axis.text=element_text(size=13),
                             axis.title=element_text(size=25,face="bold"))+
  geom_smooth(method = 'lm')

model2 <- lm(M2~CPI+GDP, data = data0)
ggplot(data = data0, aes(x = M2, y = CPI)) + 
  geom_point(size = 4)+theme(axis.text=element_text(size=13),
                             axis.title=element_text(size=25,face="bold"))+
  geom_smooth(method = 'lm')
ggplot(data = data0, aes(x = GDP, y = CPI)) + 
  geom_point(size = 4)+theme(axis.text=element_text(size=13),
                             axis.title=element_text(size=25,face="bold"))+
  geom_smooth(method = 'lm')

model3 <- lm(GDP~M2+CPI, data = data0)
ggplot(data = data0, aes(x = M2, y = CPI)) + 
  geom_point(size = 4)+theme(axis.text=element_text(size=13),
                             axis.title=element_text(size=25,face="bold"))+
  geom_smooth(method = 'lm')
ggplot(data = data0, aes(x = GDP, y = CPI)) + 
  geom_point(size = 4)+theme(axis.text=element_text(size=13),
                             axis.title=element_text(size=25,face="bold"))+
  geom_smooth(method = 'lm')





##附加
##data1<-spread(data1,key=Type,value = value,1) ## 列行变量转制，更改表格模式
##ggplot(data1, aes(x = years, y = M0, group = 1)) +
##  geom_line()
