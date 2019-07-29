library(readr)
library(ggplot2)
library(jsonlite)
library(dplyr)
library(plyr)
library(tibble)
library(maps)
library(RCurl)
library(ggmap)
library(mapdata)
library(devtools)
library(lubridate)
library(DataExplorer)
library(ggpubr)
library(gridExtra)
library(tidyverse)
library(reshape2)
library(ggrepel)
library(viridis)
library(ggcorrplot)
library(gridExtra)
#
CS <- read_csv('Champions.csv')
View(CS)
#
dim(CS)
glimpse(CS)
#Most populated primary role by original and current Year_Released?
CS = CS[!(CS$Year_Released %in% c("N/A","2009","2019")),]
temp = subset(CS,select=c(Primary_Role,Year_Released,Name))
length(unique(temp$Primary_Role))
#
#x %>% f %>% g %>% h is equivalent to h(g(f(x)))
temp = temp %>% group_by(Primary_Role) %>% summarise(count=n()) %>% arrange(desc(count))
ggplot(head(temp,10),aes(reorder(Primary_Role,count),count,fill=Primary_Role))+
    geom_bar(stat="identity")+
    theme(axis.text.x = element_text(angle=90,vjust=0.5),legend.position="none")+
    ggtitle("Top 10 game Roles for OG and Current Year_Released Champions")+
    coord_flip()+labs(x="Role",y="Count")
##Classes?
temp=CS %>% select(Class,Name) %>% group_by(Class) %>% summarise(count=n()) %>% arrange(desc(count))
temp$percentage= round((temp$count/sum(temp$count))*100,digits=2)
ggplot(temp,aes(Class,count,fill=Class))+geom_bar(stat="identity")+geom_label(aes(label=temp$percentage),size=2)+theme(axis.text.x = element_text(angle=90,vjust=0.3),plot.title = element_text(hjust=0.5,face='italic'),legend.position="none")+ggtitle("Class with most game releases")+labs(x="Class",y="Count")

#
temp = CS %>% select(Class,Year_Released) 
require(plyr)
temp=ddply(temp,.(Class,Year_Released),transform,count=length(Class))
temp=unique(temp)
detach('package:plyr',TRUE)
ggplot(temp,aes(Year_Released,count,group=1,color=Class))+geom_line(size=1)+theme(axis.text.x = element_text(angle=90,vjust=0.3),plot.title = element_text(hjust=0.5,face='italic'),legend.position="none")+ggtitle("Trend of Class release by Year")+labs(x="Year",y="Count")+facet_wrap(~Class,scales='free_y',ncol=4)

#Most successul role?
temp = CS %>% select(Primary_Role,Win_Rate,Year_Released)
ggplot(temp,aes(Primary_Role,Win_Rate,fill=Primary_Role))+geom_boxplot(stat="boxplot",position="dodge",outlier.color="red")+theme(axis.text.x = element_text(angle=90,vjust=0.3),plot.title = element_text(hjust=0.5,face='italic'),plot.subtitle = element_text(hjust=0.5,face='bold'),legend.position="bottom")+ggtitle("Trend of Overall Winrate")+labs(x="Primary_Role",y="Mean Winrate",subtitle="Winrate")+scale_y_log10()

#Most Banworthy Champs for s9 so far?
temp = CS %>% select(Name,Ban_Rate,Primary_Role) %>% arrange(desc(Ban_Rate)) 

ggplot(head(temp,20),aes(factor(Name,levels=Name),Ban_Rate,fill=Primary_Role))+geom_bar(stat="identity")+theme(axis.text.x = element_text(angle=90,vjust=0.3),plot.title = element_text(hjust=0.5,face='italic'),legend.position="bottom")+ggtitle("Top 20 Banworthy Champs")+labs(x="Name",y="Banrate")

#
top3class = CS %>% group_by(Class) %>% summarise(count=n()) %>% arrange(desc(count)) %>% head(3)
temp = CS[CS$Class %in% top3class$Class,]
temp = temp %>% group_by(Class,Year_Released) %>% summarize(count=n()) 
ggplot(temp,aes(Year_Released,Class,fill=count))+geom_tile(color="white",size=0.4)+theme(axis.text.x = element_text(size=10,hjust=0.5),plot.title=element_text(hjust=0.5,face='italic'),legend.position="bottom",legend.key.width = unit(3, "cm"))+ggtitle("Pick_Rate Trends over the Years")+scale_fill_viridis(
  name="count", 
  option = 'C', 
  direction = -1, 
  na.value = "white",
  limits = c(0, max(temp$count)))
#Count vs. Pickrate
temp= CS[CS$Class %in% top3class$Class,]
temp = temp %>% group_by(Class) %>% summarise(meanpick=mean(Pick_Rate)) %>% arrange(desc(meanpick))
temp = merge(top3class,temp,by="Class")
ggplot(temp,aes(x=count,y=meanpick,col=factor(Class),size=meanpick))+
  geom_point(alpha=0.4)+theme(legend.position="bottom",plot.title = element_text(size=10,hjust=0.5))+labs(title="Mean Pick Rate",col="Class")