#统计量分析
data_outline<-function(x,.col){#x为数据集，.col为列数
  outline <- data.frame()
  for (i in 1:.col) {
    if(is.numeric(as.matrix(x[i]))==TRUE){
      .x<- as.matrix(x[i])
      n<-nrow(.x)
      .mean<-colMeans(.x)
      #.var<-var(.x)
      .mode <- as.numeric(names(table(.x))[which.max(table(.x))])#众数,只能求得其中一个众数
      .median<-median(.x)#中位数
      .range<-.x[which.max(.x)]-.x[which.min(.x)]#极差
      .quantile <- as.matrix(quantile(.x))#四分位数
      .sd<-sd(.x)
      .change<-100*.sd/.mean#变异系数
      .skewness<-n/((n-1)*(n-2))*sum((.x-.mean)^3)/.sd^3#偏度
      .kurtosis<-((n*(n+1))/((n-1)*(n-2)*(n-3))*sum((.x-.mean)^4)/.sd^4-(3*(n-1)^2)/((n-2)*(n-3)))#峰度
      outline1<-data.frame(
        Num=n,
        Mean=.mean,
        Mode=.mode,
        Median=.median,
        Range = .range,
        quantile_0 = .quantile[1],
        quantile_25=.quantile[2],
        quantile_50=.quantile[3],
        quantile_75=.quantile[4],
        quantile_100=.quantile[5],
        std.dev=.sd,
        CV=.change,
        Skewness=.skewness,
        Kurtosis=.kurtosis,
        row.names=colnames(.x)
        )
      outline <- rbind(outline,outline1)
    }
    else
      outlinefactor <- summary(x[i])#因子，此处未做深入处理
  }
  print(outline)
  print(outlinefactor)
      #outline2 <- levels(as.factor(as.matrix(x[i])))
}



