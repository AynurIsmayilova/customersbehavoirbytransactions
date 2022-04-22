install.packages("dplyr")
library(dplyr)
install.packages("data.table")
library(data.table)
library(ggplot2)
#### get data
getwd()
setwd("C:\\Users\\aisma\\Desktop\\R")
transactions<-read.csv2("transactions.txt",header = FALSE)
colnames(transactions)<-c("Customer_ID", "Credit/Debit",
                          "Amount_EUR","Counterparty_country")
View(transactions)
summary(transactions)
str(transactions)
head(transactions,5)
tail(transactions,5)
unique(transactions$Counterparty_country)
unique(transactions$Customer_ID)
##### add index column
transactions<-cbind(Index=1:nrow(transactions),transactions)
View(transactions)
#####outliers
mymean=mean(transactions$Amount_EUR)
mystd =sd(transactions$Amount_EUR)
Tmin=mymean-(3*mystd)
Tmax=mymean+(3*mystd)
outliers<-which(transactions$Amount_EUR<Tmin|transactions$Amount_EUR>Tmax)
c(outliers)
View(outliners)
outliers_data<-transactions[transactions$Amount_EUR>134000, ]
View(outliers_data)
#### group by customers behavoir
transactions1 <-data.table(transactions)
df_means<-transactions1 [ ,.(mean=mean(Amount_EUR)),by=Customer_ID]
df_max<- transactions1 [ ,.(max=max(Amount_EUR)),by=Customer_ID]
df_min <- transactions1 [ ,.(min=min(Amount_EUR)),by=Customer_ID]
data.frame(df_means)
customer<-merge(df_means,df_max,by="Customer_ID")
customers<-merge(customer,df_min,by="Customer_ID")
View(customers)
a<-qplot(data = customers,x=Customer_ID,y=mean,size=I(4),color=max,shape=I(19),
                            main="Outliers by mean and maximum")+
  theme(plot.title = element_text(colour = "Dark blue",size=20))
a+xlim(6740,6850)
a+xlim(570,2900)
### grouping by mean customers 
low_risk_by_mean<- customers [customers$mean >=0 & customers$mean<=5000, ]
risk_5001_10000_by_mean<-customers [customers$mean >=5001 & customers$mean<=1000, ]
high_risk_by_mean<- customers [customers$mean >=10001 , ]

#### grouping by customers id by amount of transactions 
without_risk<- transactions[transactions$Amount_EUR >=-1000 & transactions$Amount_EUR<=1000, ]
low<- transactions[transactions$Amount_EUR<-1001 &  transactions$Amount_EUR >=1001 & transactions$Amount_EUR<=5000, ]
risk<- transactions[transactions$Amount_EUR >=5001 & transactions$Amount_EUR<=10000, ]
high_risk<-transactions[transactions$Amount_EUR >=10001 , ]
trigger<-high_risk
View(high_risk)
ggplot(data=high_risk,aes(x=Counterparty_country,y=Amount_EUR,color=I("Blue")))+geom_boxplot()+geom_jitter()+ggtitle("High risk by country")+
    theme(plot.title = element_text(color="blue",size=15,face="bold",hjust =0.5))
####sum by country
sum_by_country <-transactions1 [ ,.(sum=sum(Amount_EUR)),by=Counterparty_country]
View(sum_by_country)
summary(sum_by_country)
ggplot(data=sum_by_country,aes(x=Counterparty_country,y=sum,size=sum,color=sum))+geom_point()+
  ggtitle("Transactions by Country")+theme(plot.title = element_text(color="blue",size=15,face="bold",hjust =0.5))
means_by_country<-transactions1 [ ,.(country_mean=mean(Amount_EUR)),by=Counterparty_country]
max_by_country<-transactions1 [ ,.(country_max=max(Amount_EUR)),by=Counterparty_country]
min_by_country<-transactions1 [ ,.(country_min=min(Amount_EUR)),by=Counterparty_country]
View(means_by_country)
romania<-transactions [transactions$Counterparty_country=="RO", ]
View(romania)
without_risk_ro<- romania[romania$Amount_EUR >=-1000 & romania$Amount_EUR<=1000, ]
low_risk_ro<- romania[romania$Amount_EUR<=1000 &  romania$Amount_EUR >=1001 & romania$Amount_EUR<=5000, ]
risk_ro<-romania[romania$Amount_EUR >=5001 & romania$Amount_EUR<=10000, ]
high_risk_ro<-romania[romania$Amount_EUR>=10001 , ]
View(without_risk_ro)
View(low_risk_ro)
View(risk_ro)
View(high_risk_ro)
ggplot(data = romania,aes(x=Amount_EUR))+geom_histogram(color=I("dark green"),binwidth = 20)+
ggtitle("Transactions in Romania")+theme(plot.title = element_text(colour = "Dark blue",size=20,hjust = 0.5))

       