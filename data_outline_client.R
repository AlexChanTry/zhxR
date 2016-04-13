#
source("data_outline.R")
data<-read.table("iris.csv",header = T,sep = ',')
data_outline(data,ncol(data))