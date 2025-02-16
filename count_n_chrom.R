#take the relevant information from the ref_genome
gene_chr <- ref_genome[, c("Chromosome", "Name", "Symbol", "Gene.Type")]

names(gene_chr)[3] <- "gene"

#take the relevant information from the colon_counts_w_demographics
gene_counts <- colon_counts_w_demographics[,c(1:2)]

names(gene_counts) <- c("name", "gene")

gene_counts <- gene_counts[-1,]

#merge
gene_count_chr <- merge(gene_counts, gene_chr, by = "gene", all.x = TRUE)

-----------------------------biomart--------------------------------------------
library(biomaRt)
library(dplyr)
ensembl = useEnsembl(biomart="ensembl", dataset="hsapiens_gene_ensembl")

gene_counts <- gene_counts %>%
  mutate(ensembl_gene_id = substr(name, 1, 15))

ensembl_genes <- getBM(attributes=c('ensembl_gene_id',
                                    'hgnc_symbol','chromosome_name',
                                    'start_position','end_position', 
                                    'description'), 
                       filters = 'ensembl_gene_id', values = gene_counts$ensembl_gene_id, mart = ensembl)
#55474 genes

names(ensembl_genes)[2] <- "gene"

gene_count_chr <- merge(gene_counts, ensembl_genes, by = c("ensembl_gene_id", "gene"), all.x = TRUE)

gene_count_chr <- left_join(gene_counts, ensembl_genes, by = "ensembl_gene_id", keep = FALSE)

gene_count_chr <- gene_count_chr[,-4]

write.csv(gene_count_chr, "colon_genes.csv")



#test
test <- gene_counts[1:10,]

testa <- merge(test, ensembl_genes, by = c("ensembl_gene_id", "gene"))

testa <- left_join(test, ensembl_genes, by = c("ensembl_gene_id"))

test_ensembl <- filter(ensembl_genes, ensembl_gene_id %in% test$ensembl_gene_id)


# Identify the specific ensembl_gene_id
ensembl_gene_id_to_replace <- "ENSG00000238009"

# Replace the empty "gene" value in test_ensembl with the corresponding value from test
test_ensembl <- test_ensembl %>%
  left_join(test %>% filter(ensembl_gene_id == ensembl_gene_id_to_replace), 
            by = "ensembl_gene_id", suffix = c("_test_ensembl", "_test")) %>%
  transmute(
    ensembl_gene_id,
    gene = coalesce(gene_test, gene_test_ensembl),
    chromosome_name
  )











unique_ref_genome <- ref_genome %>%
  distinct(Symbol, .keep_all = TRUE)
ref_genes <- unique_ref_genome[,c(6,7,9)]

ref_count_genes <- merge(gene_count_chr, ref_genes, by = "gene", all.x = TRUE)

