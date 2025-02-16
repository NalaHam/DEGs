
df <- t(colon_counts)

df <- data.frame(df)

library(dplyr)

#make new column with first 10 characters of sample column to get the subject's id
df$SUBJID <- substr(df$X2, 1, 10)

#to make it easier to see, move the SUBJID column to the front of the data frame
df <- df %>% relocate(SUBJID) #moves SUBJID column to be the first column

#remove all special characters in both Subject_Phenotypes and df
df$SUBJID <- gsub('-', '', df$SUBJID)

Subject_Phenotypes$SUBJID <- gsub('-', '', Subject_Phenotypes$SUBJID)

#merge dfs
df <- merge(Subject_Phenotypes, df, by = "SUBJID", all.y = TRUE )

#move name row to the top
last_row <- df[nrow(df), ]

# Remove the last row from the data frame
df <- df[-nrow(df), ]

# Insert the last row at the beginning
df <- rbind(last_row, df)

#rename rows
row.names(df) <- c(1:nrow(df))

#add in a column "factors" with different demographic variables in it
#Add a column, if sex = 1, then add an m
df <- df %>%
  mutate(sex_factor = case_when(is.na(SEX) ~ NA_character_,
                                SEX == 1 ~ 'm', 
                                SEX == 2 ~ 'f'))

df <- df %>% relocate(sex_factor) #moves subject column to be the first column

#Add a column, if age = 20-29, 30-39, 40-49, 50-59, 60-69, 70-79 then add an a,b,c,d,e,f
df <- df %>% 
  mutate(age_factor = case_when(is.na(AGE) ~ NA_character_,
                                AGE == "20-29" ~ "a",
                                AGE == "30-39" ~ "b",
                                AGE == "40-49" ~ "c",
                                AGE == "50-59" ~ "d",
                                AGE == "60-69" ~ "e",
                                AGE == "70-79" ~ "f"))

df <- df %>% relocate(age_factor) #moves subject column to be the first column

#combine the two factor columns
df$factors <- paste(df$sex_factor,df$age_factor)

df$factors <- ifelse(is.na(df$sex_factor) | is.na(df$age_factor), NA_character_, 
                     paste(df$sex_factor, df$age_factor, sep = ''))

df <- df %>% relocate(factors) 

#add numbers to represent different individuals of the same sex and age group
#e.g. the first "md" gets converted to "md1" and the second "md" gets converted
#to "md2" and so on
df <- df %>%
  group_by(factors) %>%
  mutate(factors = ifelse(!is.na(factors), paste(factors, row_number(), sep = ""), 
                          factors)) %>%
  ungroup()

#remove unnecessary columns
df <- df[,-c(2:8)] #removes columns:"age_factor", "sex_factor", "SEX","AGE", "SUBJID", 
                  #"DTHHRDY", and original sample ids

#transpose df 
df <- t(df)

df <- data.frame(df)

row.names(df) <- c(1:nrow(df))

#add in new descriptors 
df[1,1] <- "name"
df[1,2] <- "gene"

write.table(df, "colon_counts_w_demographics.txt")














