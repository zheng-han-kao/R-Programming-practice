bayesian <- read.csv(file = "hw3_NB.csv", header = TRUE)
col_num <- length(bayesian[1,])
class <- unique(bayesian$C)
input <- c(1,2,1)

classify <- function(input){
  ans <- c(0,0)
  for(j in 1:length(class)){
    sum <- 0
    for(i in 1:(col_num-1)){
      A <- bayesian[bayesian[,col_num] == class[j],i]
      if(i == 1){
        sum <- length(A[A == input[i]])/length(A) 
      }else
        sum <- sum * length(A[A == input[i]])/length(A) 
    }
    ans[j] <- sum * length(bayesian[bayesian$C == class[j],col_num])/length(bayesian$C)
  }
  if(ans[1] > ans[2]){
    cat(input,"is class1")
  }else if(ans[1] < ans[2]){
    cat(input,"is class2")
  }else
    cat(input,"can't be classified.")
}

classify(input)