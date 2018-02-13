relief1 <- read.csv(file = "hw1_RF.csv", header=TRUE)

data1 <- c(relief1[,1], relief1[,4])
data2 <- c(relief1[,2], relief1[,4])
data3 <- c(relief1[,3], relief1[,4])

I1 <- matrix(data1,nrow = 7, ncol = 2)
I2 <- matrix(data2,nrow = 7, ncol = 2)
I3 <- matrix(data3,nrow = 7, ncol = 2)


classify <- function(I){
  class0 <- matrix(data = NA , nrow = 3 , ncol = 1)
  class1 <- matrix(data = NA , nrow = 4 , ncol = 1)
  count0 = 1
  count1 = 1
  w = 0
  #classify(I1)
  for( i in 1 : 7){
    if(I[i,2] == 0){
      class0[count0] = I[i,1]
      count0 = count0 + 1
    }else if(I[i,2] == 1){
      class1[count1] = I[i,1]
      count1 = count1 + 1
    }
  }
  w_temp = 0
  for(i in 1:7){
    min_numberA = 999
    min_numberB = 999
    for(j in 1:3){
      w_temp = (class0[j] - I[i,1])^2
      if(w_temp < min_numberA & w_temp != 0)
        min_numberA = w_temp
    }
    for(j in 1:4){
      w_temp = (class1[j] - I[i,1])^2
      if(w_temp < min_numberB & w_temp != 0)
        min_numberB = w_temp
    }
    if(I[i,2] == 0){
      w = w - min_numberA + min_numberB
    }else{ 
      w = w - min_numberB + min_numberA
    }
  }
  w = w / 7
  print(w)
  
}

classify(I1)
classify(I2)
classify(I3)

