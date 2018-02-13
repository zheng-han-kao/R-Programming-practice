data <- read.csv(file = "RFM.csv", header = TRUE)



#----------把消費者ID、時間、購買金額取出來
rfm <- data.frame(id = data$SRCNO,                 
                  date = data$date,
                  sales = data$SALE_AMT)



#----------把缺失消費金額用平均消費金額補齊
sales_mean <- mean( rfm$sales[!is.na(rfm$sales)] )    
rfm$sales[is.na(rfm$sales)] <- sales_mean



#----------把原始資料的時間跟日期合起來
rfm$date = strptime(paste(data$date,data$time),format = "%Y-%m-%d %H:%M:%S") 
now_date = strptime(Sys.time(),format = "%Y-%m-%d %H:%M:%S")
interval <- now_date - rfm$date     #間隔時間等於現在時間減去最近一次消費時間



#----------把ID、間隔時間、消費金額整理在一起
tmp_rfm <- data.frame(id = rfm$id,
                      interval = interval,
                      sales = rfm$sales)

tmp_rfm <- tmp_rfm[order(tmp_rfm$id,decreasing=FALSE),]          #排序ID
id_class <- unique(tmp_rfm$id)                                   #把ID的種類(扣掉重複購買)取出來就是會員人數



#----------計算RFM---------------
r <- numeric(0)
f <- numeric(0)
m <- numeric(0)
for(i in 1:length(id_class)){
  r <- c( r, min( tmp_rfm$interval[tmp_rfm$id == id_class[i]] ) )       #把同一個會員取出來取第一個間隔就是R(因為要取最近的消費時間)
  f <- c( f, length( tmp_rfm$interval[tmp_rfm$id == id_class[i]] ) )  #購買幾次
  m <- c( m, max( tmp_rfm$sales[tmp_rfm$id == id_class[i]] ) )        #最大銷售金額
}

final_rfm <- data.frame(id = id_class,
                        r = r,
                        f = f,
                        m = m)





#-------------------create RFM levels------------------------
mean_r <- mean(final_rfm$r)
mean_f <- mean(final_rfm$f)
mean_m <- mean(final_rfm$m)
for(i in 1:length(final_rfm$id)){
  if(final_rfm$r[i] >= mean_r & final_rfm$f[i] >= mean_f & final_rfm$m[i] >= mean_m){
    final_rfm$group[i] <- 1 
  }else if(final_rfm$r[i] >= mean_r & final_rfm$f[i] < mean_f & final_rfm$m[i] >= mean_m){
    final_rfm$group[i] <- 2
  }else if(final_rfm$r[i] < mean_r & final_rfm$f[i] >= mean_f & final_rfm$m[i] >= mean_m){
    final_rfm$group[i] <- 3
  }else if(final_rfm$r[i] < mean_r & final_rfm$f[i] < mean_f & final_rfm$m[i] >= mean_m){
    final_rfm$group[i] <- 4
  }else if(final_rfm$r[i] >= mean_r & final_rfm$f[i] >= mean_f & final_rfm$m[i] < mean_m){
    final_rfm$group[i] <- 5
  }else if(final_rfm$r[i] >= mean_r & final_rfm$f[i] < mean_f & final_rfm$m[i] < mean_m){
    final_rfm$group[i] <- 6
  }else if(final_rfm$r[i] < mean_r & final_rfm$f[i] >= mean_f & final_rfm$m[i] < mean_m){
    final_rfm$group[i] <- 7
  }else if(final_rfm$r[i] < mean_r & final_rfm$f[i] < mean_f & final_rfm$m[i] < mean_m){
    final_rfm$group[i] <- 8
  }
}

lbls <- c("Group1","Group2","Group3","Group4","Group5","Group6","Group7","Group8")
pct <- prop.table(table(final_rfm$group))
lbls <- paste(lbls, "-",round(pct,2)*100) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(table(final_rfm$group),labels = lbls, col=rainbow(length(lbls)),
    main="Rank Distribution"
)