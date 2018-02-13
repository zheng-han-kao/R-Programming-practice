data <- read.csv(file = "Pagerank_0525.CSV", header = TRUE)
N <- nrow(data)       #有幾個元素
pr <- c(0)
d <- 0.15

#原始分數
for(i in 1:N){
  pr[i] <- 1/N
  pr[i] <- round(pr[i], digits = 7)
}

#機率矩陣
for(i in 1:ncol(data)){
  data[,i] <- data[,i]/sum(data[,i])
}

pagerank <- function(N1, data1, pr1, iter){
  pr_tmp <- c(0)

  for(x in 1:iter){              #幾次迭代
    for(i in 1:N1){
      tmp <- 0
      for(j in 1:N1){
        tmp <- tmp + d*pr1[j]*data1[i,j]
      }
      pr_tmp[i] <- (1-d)/N1 + tmp           #計算分數
      pr_tmp[i] <- round(pr_tmp[i], digits = 7)
    }

    count <- 0
    for(y in 1:N1){
        if(pr1[y] == pr_tmp[y]){
          count <- count + 1
        }
    }
    
    if(count == N1){
        print("converged!!")
    }
    pr1 <- pr_tmp                             #取代原本的分數
    print(paste("第",x , "次 iteration : ", toString(pr1)))
  }
}

pagerank(N, data, pr, 100)