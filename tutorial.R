if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("edgeR")

library(edgeR)

counts <- read.delim("all_counts.txt", row.names = 1)
head(counts)

#storing data into a list for processing
d0 <- DGEList(counts)

#calculation normalizing factors from full data
d0 <- calcNormFactors(d0) #calcNormFactors doesn’t normalize the data, it just calculates normalization factors for use downstream.
d0

#filter out low-expression genes
cutoff <- 1
drop <- which(apply(cpm(d0), 1, max) < cutoff)
d <- d0[-drop,] 
dim(d) # number of genes left = 21080

#sample factors
snames <- colnames(counts) # Sample names
snames

cultivar <- substr(snames, 1, nchar(snames) - 2) #deletes the last two characters in the sample names to get the cultivar ("C", "I5", or "I8")
time <- substr(snames, nchar(snames) - 1, nchar(snames) - 1) #deletes the cultivar and the last character to get time
cultivar
time

#make group that combines the cultivar and time factors
group <- interaction(cultivar, time)
group

plotMDS(d, col = as.numeric(group))

#------------Voom transformation and calculation of variance weights------------

#Because voom uses variances of the model residuals, we specify the model to be fitted before doing analysis.
#This specifies a model where each coefficient corresponds to a group mean
mm <- model.matrix(~0 + group)

#voom analysis
y <- voom(d, mm, plot = T)

#------------Fitting linear models in limma-------------------------------------

#normalizes data:

#lmFit fits a linear model using weighted least squares for each gene:
fit <- lmFit(y, mm)
head(coef(fit))

#Comparisons between groups (log fold-changes) are obtained as contrasts of these fitted linear models:
#Comparison between times 6 and 9 for cultivar I5
contr <- makeContrasts(groupI5.9 - groupI5.6, levels = colnames(coef(fit)))
contr

#estimate contrast for each gene for times 6 and 9 for cultivar I5
tmp <- contrasts.fit(fit, contr)

#smooth out standard errors 
tmp <- eBayes(tmp)

#top genes that are differentially expressed
top.table <- topTable(tmp, sort.by = "P", n = Inf)
head(top.table, 20)

#number of DEGs
length(which(top.table$adj.P.Val < 0.05)) #4680

#write the DEGs to a file
top.table$Gene <- rownames(top.table)
top.table <- top.table[,c("Gene", names(top.table)[1:6])]
write.table(top.table, file = "time9_v_time6_I5.txt", row.names = F, sep = "\t", quote = F)

#--------------do a two-factor model instead of using group---------------------

mm <- model.matrix(~cultivar*time) #We are specifying that model includes 
                                  #effects for cultivar, time, and the cultivar-time 
                                  #interaction (which allows the differences between cultivars to differ across time)
colnames(mm)

y <- voom(d, mm, plot = F)
fit <- lmFit(y, mm)
head(coef(fit))

tmp <- contrasts.fit(fit, coef = 2) # Directly test second coefficient
tmp <- eBayes(tmp)
top.table <- topTable(tmp, sort.by = "P", n = Inf)
head(top.table, 20)























