data <- read.csv(file = "hw5_data.csv", header = TRUE)

row_num <- nrow(data)
col_num <- ncol(data)

dist_mat <- matrix(nrow = row_num, ncol = row_num)  #距離矩陣

#計算rao's coef = a/(a+b+c+d)
for(i in 1:row_num){
    for(j in i:row_num){
        dist_mat[i,j] <- length( data[j,][data[j,][data[i,] == 1]] ) / col_num  #倆倆比較，將都等於一的個數取出來
    }  
}
#對角線變一
for(i in 1:row_num){
  dist_mat[i,i] <- 1
}
#NA變一
dist_mat[is.na(dist_mat)] <- 1
min(dist_mat)
