
GTEx_colon <- GTEx_colon_og

#delete id column and fancy gene name (not informative)
GTEx_colon <- GTEx_colon[,-c(1,2)]

#make columns the rows and the rows the columns
GTEx_colon <- t(GTEx_colon)

#makes the matrix a data frame
GTEx_colon <- data.frame(GTEx_colon)


#make new column of the df's row names and rename the rows 1:k
library(tibble)
GTEx_colon <- tibble::tibble(sample = row.names(GTEx_colon), GTEx_colon, .name_repair = "minimal")

#change . to - for in the samples column
GTEx_colon$sample <- gsub('\\.', '-', GTEx_colon$sample)

#make new column with first 10 characters of sample column to get the subject's id
GTEx_colon$subject <- substr(GTEx_colon$sample, 1, 10)

#remove all special charcters in both SubjectPhenotypes and df
#GTEx_SubjectPhenotypesDS$subject <- gsub('-', '', GTEx_SubjectPhenotypesDS$subject)

GTEx_colon$subject <- gsub('-', '', GTEx_colon$subject)


#reorder columns
library(dplyr)
GTEx_colon <- GTEx_colon %>% relocate(subject) #moves subject column to be the first column

#combine the data frame and the subject's info
#GTEx_SubjectPhenotypesDS <- GTEx_SubjectPhenotypesDS %>% 
 # rename("subject" = "SUBJID") #rename to get the same column name to merge by


#merge dfs
library(plyr)
GTEx_colon <- join(GTEx_SubjectPhenotypesDS, GTEx_colon, by = "subject", type = "right", match = "all")

#this data frame allows us to look at the samples and subject demographics 

#now to get it into a format to run DEG analysis, we need to get it back into 
#the original format, but with the samples categorized by demographic variables. 

#Add a column, if sex = 1, then add an m
GTEx_colon <- GTEx_colon %>%
  mutate(factors = if_else(SEX %in% 1, 'm', 'f'))

GTEx_colon <- GTEx_colon %>% relocate(factors) #moves subject column to be the first column

#Add a column, if age = 20-29, 30-39, 40-49, 50-59, 60-69, 70-79 then add an a,b,c,d,e,f
GTEx_colon <- GTEx_colon %>%
  mutate(factors_1 = 
           if_else(AGE %in% '20-29', 'a',
                   if_else(AGE %in% '30-39', 'b', 
                           if_else(AGE %in% '40-49', 'c', 
                                   if_else(AGE %in% '50-59', 'd', 
                                           if_else(AGE %in% '60-69', 'e', 'f'))))))


GTEx_colon <- GTEx_colon %>% relocate(factors_1) #moves subject column to be the first column

#combine factors and factors_1
GTEx_colon$factors <- paste(GTEx_colon$factors,GTEx_colon$factors_1)

#delete empty space in factors
GTEx_colon$factors <- gsub(' ', '', GTEx_colon$factors)


#delete factors_1
GTEx_colon <- GTEx_colon[,-1]

#add numbers to the factors column for each of the unique samples
unique(GTEx_colon$factors) #"md" "me" "fe" "fa" "mb" "fd" "fb" "ma" "fc" "mc" "mf" "ff"
table(GTEx_colon$factors) #fa fb fc fd fe ff ma mb mc md me mf 
                          #11 14 26 37 40  6 25 25 28 72 83  7 

subject_counts <- data.frame("subject type" = c("fa", "fb", "fc", "fd", "fe", "ff", "ma", "mb", "mc", "md", "me", "mf" ),
                             "freq" = c(11, 14, 26, 37, 40,  6, 25, 25, 28, 72, 83,  7 ))
#first row
first_row <- GTEx_colon_test[1,]
first_row$factors <- ""
first_row <- first_row %>% relocate(factors) #moves subject column to be the first column

#md
md_df <- filter(GTEx_colon, GTEx_colon$factors == "md")
md_df$sample_nums <- c(1:72)
md_df <- md_df %>% relocate(sample_nums) #moves subject column to be the first column
md_df$factors <- paste(md_df$factors,md_df$sample_nums)
md_df$factors <- gsub(' ', '', md_df$factors)
md_df <- md_df[,-1]

#me
me_df <- filter(GTEx_colon, GTEx_colon$factors == "me")
me_df$sample_nums <- c(1:83)
me_df <- me_df %>% relocate(sample_nums) #moves subject column to be the first column
me_df$factors <- paste(me_df$factors,me_df$sample_nums)
me_df$factors <- gsub(' ', '', me_df$factors)
me_df <- me_df[,-1]

#fe
fe_df <- filter(GTEx_colon, GTEx_colon$factors == "fe")
fe_df$sample_nums <- c(1:40)
fe_df <- fe_df %>% relocate(sample_nums) #moves subject column to be the first column
fe_df$factors <- paste(fe_df$factors,fe_df$sample_nums)
fe_df$factors <- gsub(' ', '', fe_df$factors)
fe_df <- fe_df[,-1]

#fa
fa_df <- filter(GTEx_colon, GTEx_colon$factors == "fa")
fa_df$sample_nums <- c(1:11)
fa_df <- fa_df %>% relocate(sample_nums) #moves subject column to be the first column
fa_df$factors <- paste(fa_df$factors,fa_df$sample_nums)
fa_df$factors <- gsub(' ', '', fa_df$factors)
fa_df <- fa_df[,-1]

#mb
mb_df <- filter(GTEx_colon, GTEx_colon$factors == "mb")
mb_df$sample_nums <- c(1:25)
mb_df <- mb_df %>% relocate(sample_nums) #moves subject column to be the first column
mb_df$factors <- paste(mb_df$factors,mb_df$sample_nums)
mb_df$factors <- gsub(' ', '', mb_df$factors)
mb_df <- mb_df[,-1]

#fd
fd_df <- filter(GTEx_colon, GTEx_colon$factors == "fd")
fd_df$sample_nums <- c(1:37)
fd_df <- fd_df %>% relocate(sample_nums) #moves subject column to be the first column
fd_df$factors <- paste(fd_df$factors,fd_df$sample_nums)
fd_df$factors <- gsub(' ', '', fd_df$factors)
fd_df <- fd_df[,-1]

#fb
fb_df <- filter(GTEx_colon, GTEx_colon$factors == "fb")
fb_df$sample_nums <- c(1:14)
fb_df <- fb_df %>% relocate(sample_nums) #moves subject column to be the first column
fb_df$factors <- paste(fb_df$factors,fb_df$sample_nums)
fb_df$factors <- gsub(' ', '', fb_df$factors)
fb_df <- fb_df[,-1]

#ma
ma_df <- filter(GTEx_colon, GTEx_colon$factors == "ma")
ma_df$sample_nums <- c(1:25)
ma_df <- ma_df %>% relocate(sample_nums) #moves subject column to be the first column
ma_df$factors <- paste(ma_df$factors,ma_df$sample_nums)
ma_df$factors <- gsub(' ', '', ma_df$factors)
ma_df <- ma_df[,-1]

#fc
fc_df <- filter(GTEx_colon, GTEx_colon$factors == "fc")
fc_df$sample_nums <- c(1:26)
fc_df <- fc_df %>% relocate(sample_nums) #moves subject column to be the first column
fc_df$factors <- paste(fc_df$factors,fc_df$sample_nums)
fc_df$factors <- gsub(' ', '', fc_df$factors)
fc_df <- fc_df[,-1]

#mc
mc_df <- filter(GTEx_colon, GTEx_colon$factors == "mc")
mc_df$sample_nums <- c(1:28)
mc_df <- mc_df %>% relocate(sample_nums) #moves subject column to be the first column
mc_df$factors <- paste(mc_df$factors,mc_df$sample_nums)
mc_df$factors <- gsub(' ', '', mc_df$factors)
mc_df <- mc_df[,-1]

#mf
mf_df <- filter(GTEx_colon, GTEx_colon$factors == "mf")
mf_df$sample_nums <- c(1:7)
mf_df <- mf_df %>% relocate(sample_nums) #moves subject column to be the first column
mf_df$factors <- paste(mf_df$factors,mf_df$sample_nums)
mf_df$factors <- gsub(' ', '', mf_df$factors)
mf_df <- mf_df[,-1]

#ff
ff_df <- filter(GTEx_colon, GTEx_colon$factors == "ff")
#this df contains an NA value because I choose to do if else statements above. The quick fix is to just take this out now.
ff_df <- ff_df[2:6,]
ff_df$sample_nums <- c(1:5)
ff_df <- ff_df %>% relocate(sample_nums) #moves subject column to be the first column
ff_df$factors <- paste(ff_df$factors,ff_df$sample_nums)
ff_df$factors <- gsub(' ', '', ff_df$factors)
ff_df <- ff_df[,-1]


GTEx_colon <- rbind(first_row, md_df, me_df, fe_df, fa_df, mb_df, fd_df, fb_df, ma_df, fc_df, mc_df, mf_df, ff_df)

GTEx_colon_before_trans <- GTEx_colon
write.csv(GTEx_colon_before_trans, file = "transposed_read_counts_w_multi_demo.csv")

GTEx_colon <- GTEx_colon[,-c(2:6)]

GTEx_colon <- t(GTEx_colon)

#makes the matrix a data frame
GTEx_colon <- data.frame(GTEx_colon)

#rename rows 1:k
row.names(GTEx_colon) <- c(1:56201)

#files
write.csv(GTEx_colon, file = "colon_read_counts_w_demographics.csv")

write.table(GTEx_colon, file = "colon_read_counts_w_demographics.txt", 
            append = FALSE, sep = "\t",
            row.names = TRUE, col.names = TRUE)








