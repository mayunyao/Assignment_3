
  mydata<-read.csv('C:/Users/001/OneDrive - 인하대학교/桌面/price_index_Feb20201.csv')
  
  ##### check my data
  head(mydata)
  dim(mydata)
  str(mydata)
  
  summary(mydata)
  
  ##### Check for missing values
  colSums(is.na(mydata))
  
  ##### remove the rows which contain NA value
  good<-complete.cases(mydata)
  
  mydata_clean<-mydata[good,]
  
  ##### Check the quantiles of quantitative variables
  quantile(mydata_clean$Data_value)
  
  ##### convert dataframe to table
  library(data.table)
  
  tmydata_clean=data.table(mydata_clean)
  
  
  
  ##### check how many evaluation
  tmydata_clean[,table(SER_REF)]
  
  tmydata_clean[,table(TIME_REF)]
  tmydata_clean[,table(DATA_VAL)]
  tmydata_clean[,table(STATUS)]
  tmydata_clean[,table(UNITS)]
  tmydata_clean[,table(Subject)]
  tmydata_clean[,table(Group)]
  tmydata_clean[,table(Series_title_1)]
  tmydata_clean[,table(Series_title_2)]
  tmydata_clean[,table(Series_title_3)]
  
  
  
  ##### remove the status,units,subjects and groups which remain the same property.
  data1<-mydata_clean[,c(8,3)]
  
  library(plyr)
  arrange(data1,DATA_VAL) #Sort data by DATA_VAL
  
  
  ##### Cuttingin produced factor varibles
  library(Hmisc)
  data1$Groups=cut2(data1$DATA_VAL,g=4)
  
  table(data1$Groups)
  
  ##### Cast data frames
  data2<-data1[,c(1:2)]
  
  ##### Format the data
  library(reshape2)
  newdata<-dcast(data2,Series_title_1~DATA_VAL)
  
  
  #######################################################
  spvalue=split(data1$DATA_VAL,data1$Series_title_1)
  ##### lapply
  lapply(spvalue,mean)
  ##### tapply
  tapply(data1$DATA_VAL,data1$Series_title_1,mean)
  ##### sapply
  data1$factor<-factor(data1$Series_title_1)
  dat<-sapply(split(data1$DATA_VAL,data1$factor),mean)
  dat
  
  
  ##### Draw bar graphs on commodities and average prices
  newdata1<-data.frame(dat)
  ##### Convert row names to column
  name <- rownames(newdata1)
  ##### bind the 'name' and 'dat'
  cbind(name,newdata1$dat)
  rownames(newdata1) <- NULL
  newdata2 <- cbind(name,newdata1)
  library(ggplot2)
  p<-ggplot(data=newdata2, aes(x=name, y=dat))+geom_bar(stat="identity",fill = 'pink',
                                                        
                                                        