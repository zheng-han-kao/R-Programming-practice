data <- read.csv(file = "hw4_DT.csv", header = TRUE)

data1 <- data.frame(A1=data[,1],O=data[,1])
data2 <- data.frame(A2=data[,2],O=data[,1])
data3 <- data.frame(A3=data[,3],O=data[,1])
data4 <- data.frame(A4=data[,4],O=data[,1])

subset1 <- summary(as.factor(data[,1]))
subset2 <- summary(as.factor(data[,2]))
subset3 <- summary(as.factor(data[,3]))
subset4 <- summary(as.factor(data[,4]))

m <- function(data,subset){
  info <- 0
  num_row <- nrow(data)
  class <- factor(data[,2])[1]

  
  for(i in 1:length(subset)){
    class1 <- 0
    class2 <- 0
    sub_len <- as.numeric(subset[i])
    set <- data[data[,1] == names(subset)[i],]

    for(j in 1:sub_len){
      if(set[j,2] == class){
        class1 <- class1 + 1
      }else{
        class2 <- class2 + 1
      }
    }
    if(class1 == 0){
      class1 <- sub_len
    }
    log_tmp1 <- log((class1/sub_len),2)
    if(class2 == 0){
      class2 <- sub_len
    }
    log_tmp2 <- log((class2/sub_len),2)
    
    info <- info + (sub_len/num_row) * ( ((-class1/sub_len)*log_tmp1) + ((-class2/sub_len)*log_tmp2) )
  }
  print(info)
}

m(data1,subset1)
m(data2,subset2)
m(data3,subset3)
m(data4,subset4)