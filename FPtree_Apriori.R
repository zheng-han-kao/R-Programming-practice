install.packages("arules")
install.packages("arulesViz")
install.packages("arulesSequences")

library(arules)
library(arulesSequences)
library(arulesViz)

tran_list <- 
  list( 
    c( "A", "B" ,"C","D"),
    c( "A", "C", "D","F" ),
    c( "C", "D", "E", "G","A"),
    c("A","D","F","B"),
    c("B","C","G"),
    c("D","F","G"),
    c("A","B","G"),
    c("C","D","F","G")
  )

trans <- as( tran_list, "transactions" )
inspect(trans)


# Apriori

##threshold=5
transrule1 <-
  apriori( trans,
           parameter = list( support = 5/8, target = "frequent itemsets")
  )
inspect(transrule1)

##threshold=3
transrule2 <-
  apriori( trans,
           parameter = list( support = 3/8, target = "frequent itemsets")
  )
inspect(transrule2)




