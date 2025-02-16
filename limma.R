

counts_test <- counts_test[,-c(1,2,4)]

counts_test[1,1] <- ''

row.names(counts_test) <- counts_test$V3

counts_test <- counts_test[,-1]



