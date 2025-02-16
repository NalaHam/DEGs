
#merge gene info df with count data with demographics

counts <- colon_counts_w_demographics

names(counts) <- counts[1,]

counts <- counts[-1,]

genes_test <- gene_count_chr[,3:4]

counts_with_chr <- merge(counts, genes_test, by = 'name')

library(dplyr)
counts_with_chr <- counts_with_chr %>% relocate('chromosome_name') 


no_Y_counts <- filter(counts_with_chr, is.na(counts_with_chr$chromosome_name) | 
                        counts_with_chr$chromosome_name != "Y")


write.csv(no_Y_counts, "counts_chr_demo_no_Y.csv")
