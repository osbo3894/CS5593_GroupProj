# Libraries used
library(tm) # For list of stopwords

# ----------------------------------------------------
# ----------------------------------------------------

### Loading data
## Identify the newest file
all.files <- list.files("./Data Outputs/", pattern = "include*")
file.in <- all.files[which.max(as.Date(substr(all.files, 48, 57)))]
tweet_data <- read.csv(paste("./Data Outputs/", file.in, sep=''))

# Last row is full of NA's
na.row <- which(rowSums(!is.na(tweet_data))==0)
if(!is.null(na.row)){
  ## rm the na row
    tweet_data <- tweet_data[-na.row,]
}

# ----------------------------------------------------
# ----------------------------------------------------
#
# Pre-processing of Twitter data
#
# ----------------------------------------------------
# ----------------------------------------------------

## Step 1: handling missing values ----
# How many missing values are there?
sum(is.na(tweet_data))

# How many attributes have NO missing values?
sum(colSums(is.na(tweet_data)) == 0)

# Removing attributes with missing values
index_no_na <- colSums(is.na(tweet_data)) == 0
tweet_data_no_na <- tweet_data[, index_no_na]

## Step 2: Removing stopwords ----
# Creating list of words to possibly remove
remove_list <- list()
count = 1

for(i in 2:ncol(tweet_data_no_na)){
  
  if((colnames(tweet_data_no_na)[i] %in% stopwords("en")) == TRUE){
    
    remove_list[[count]] <- i
    count = count + 1
    
  } else if((colnames(tweet_data_no_na)[i] %in% stopwords("german")) == TRUE){
    
    remove_list[[count]] <- i
    count = count + 1
    
  } else if((colnames(tweet_data_no_na)[i] %in% stopwords("french")) == TRUE){
    
    remove_list[[count]] <- i
    count = count + 1
    
  } else if((colnames(tweet_data_no_na)[i] %in% stopwords("italian")) == TRUE){
    
    remove_list[[count]] <- i
    count = count + 1
    
  } else if((colnames(tweet_data_no_na)[i] %in% stopwords("spanish")) == TRUE){
    
    remove_list[[count]] <- i
    count = count + 1
  }
}

# The words to be removed are:
word_index <- unlist(remove_list, use.names = FALSE)
colnames(tweet_data_no_na[, word_index])

# Now removing them:
tweet_data_no_na_stopwords <- tweet_data_no_na[, -word_index]

## Step 3: Removing non-ASCII letters ----
non_ascii <- colnames(tweet_data_no_na_stopwords)[which(grepl("[^\x01-\x7F]+", 
                                                              colnames(tweet_data_no_na_stopwords)))]
remove_list <- list()
count = 1

for(i in 2:ncol(tweet_data_no_na_stopwords)){
  
  if((colnames(tweet_data_no_na_stopwords)[i] %in% non_ascii) == TRUE){
    
    remove_list[[count]] <- i
    count = count + 1
    
  }
}

# The words to be removed are:
word_index <- unlist(remove_list, use.names = FALSE)
colnames(tweet_data_no_na_stopwords[, word_index])

# Now removing them:
tweet_data_no_na_stopwords_nascii <- tweet_data_no_na_stopwords[, -word_index]


## Step 4: Remove words with length less than or equal to 2 ----
remove_list <- list()
count = 1

for(i in 2:ncol(tweet_data_no_na_stopwords_nascii)){
  
  if((nchar(colnames(tweet_data_no_na_stopwords_nascii)[i])) <= 2){
    
    remove_list[[count]] <- i
    count = count + 1
    
  }
}

# The words to be removed are:
word_index <- unlist(remove_list, use.names = FALSE)
colnames(tweet_data_no_na_stopwords_nascii[, word_index])

# Now removing them
tweet_data_no_na_stopwords_nascii_less2 <- tweet_data_no_na_stopwords_nascii[, -word_index]

# ------------------------------------------------------------------------------------------------------

# Final tweet data set
tweet_data_final <- tweet_data_no_na_stopwords_nascii_less2

# ----------------------------------------------------
# ----------------------------------------------------
#
# Exporting the data
#
# ----------------------------------------------------
# ----------------------------------------------------

# Exporting the data
write.csv(tweet_data_final, file = "processed_tweet_data.csv")
