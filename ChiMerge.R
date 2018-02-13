chimerge <- read.csv(file = "hw2_CM.csv", header=TRUE)
data <- data.frame(I1=chimerge[,1],O=chimerge[,4])
data <- data[order(data[,1],decreasing=FALSE),]
print(data)

combdata <- data.frame(I1=data[1,1],O=data[1,2]) #把第一筆資料放入combined data
m1 <- matrix(data = 0 , nrow = 3 , ncol = 3)
if(combdata[1,2] == 0){
  m1[1,1] <- 1
}else{
  m1[1,2] <- 1
}
m1[1,3] <- m1[1,1] + m1[1,2]
count <- 1


for(i in 2:length(chimerge[,1])){
  if(data[i,2] == 0){
    m1[2,1] <- 1
    m1[2,2] <- 0
  }
  else{
    m1[2,1] <- 0
    m1[2,2] <- 1
  }
  
  m1[2,3] <- m1[2,1] + m1[2,2]   #矩陣
  m1[3,1] <- m1[1,1] + m1[2,1]
  m1[3,2] <- m1[1,2] + m1[2,2]
  m1[3,3] <- m1[3,1] + m1[3,2]
  print(m1)
  
  E11 <- m1[3,1] * m1[1,3] / m1[3,3]
  if(E11 == 0){
    E11 <- 0.1
  }
  E12 <- m1[3,2] * m1[1,3] / m1[3,3]
  if(E12 == 0){
    E12 <- 0.1
  }
  E21 <- m1[3,1] * m1[2,3] / m1[3,3]
  if(E21 == 0){
    E21 <- 0.1
  }
  E22 <- m1[3,2] * m1[2,3] / m1[3,3]
  if(E22 == 0){
    E22 <- 0.1
  }
  #cat("期望值 = ", E11,E12,E21,E22,"\n")
  chi <- 0
  chi <- ((m1[1,1] - E11)^2/E11) + ((m1[1,2] - E12)^2/E12) +    #計算卡方值
         ((m1[2,1] - E21)^2/E21) + ((m1[2,2] - E22)^2/E22)
  
  #cat("chi = ", chi,"\n")
  if(chi < 2.706){
    combdata[count+1,1] <- data[i,1]
    combdata[count+1,2] <- data[i,2]
    m1[1,1] <- m1[1,1] + m1[2,1]
    m1[1,2] <- m1[1,2] + m1[2,2]
    m1[1,3] <- m1[1,1] + m1[1,2]
    count <- count + 1
  }
}

cat("Final discretization : [", floor(combdata[1,1]),",",ceiling(combdata[count,1]),"]") 
cat("Interval representatives: ", ( floor(combdata[1,1]) + ceiling(combdata[count,1]) )/2 )
