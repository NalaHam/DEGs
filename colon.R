
#GTEx_genes <- read.delim(file="GTEx_Analysis_2017-06-05_v8_RNASeQCv1.1.9_gene_tpm.gct", skip=2)
GTEx_colon <- read.delim(file="gene_reads_colon_sigmoid.gct", skip=2)



df <- t(GTEx_colon_og)

df_test <- df[1:10,]

library(tibble)

df_test <- tibble::rownames_to_column(df_test, "sample")

r_names <- colnames(GTEx_colon_og)

df[,1] <- r_names

df <- data.frame(df)

row.names(df) <- c(1:376) #rename rows to be numbers from 1 to k

c_names <- df_test[3,]

df_test$subject <- substr(df_test$sample, 1, 10) #keeps coln1 first 10 characters for each row

subject_ids <- df_test$subject

colnames(df_test) <- c_names[1,]

GTEx_reads_n_phenotype <- merge()
