#! /bin/env Rscript
library(tidyverse);
set.seed(711861)



# min_count <- 5
# select_method <- 1
# top_percentage <- 0.2
# neighbouring=F

args <- commandArgs(trailingOnly = TRUE)

min_count <- as.integer(args[1])
select_method <- as.integer(args[2])
top_percentage <- as.numeric(args[3])
#neighbouring <- as.logical(args[4])

print(min_count)
print(select_method)
print(top_percentage)
#print(neighbouring)

flush.console()

orig_data <- read.csv('fake-news/train.csv', header=TRUE)


test_perc <- 0.8

orig_data <- orig_data[sample(1:length(orig_data$label)), ] #random shuffle
orig_data <- data.frame(orig_data)
#length(orig_data[,"label"])


# Convert everything to lowercase, remove punctuation, and split into separate words
orig_data <- orig_data |> mutate(Text = tolower(text)) |>
  mutate(Text = str_replace_all(Text, "[[:punct:]]", " ")) |>
  mutate(Text = str_replace_all(Text, fixed(" the "), " ")) |>
  mutate(Text = str_replace_all(Text, fixed(" a "), " ")) |>
  mutate(Text = str_replace_all(Text, fixed(" s "), " ")) |>
  mutate(Text = str_replace_all(Text, fixed("   "), " ")) |>
  mutate(Text = str_replace_all(Text, fixed("  "), " "))

val_data <- orig_data[as.integer(test_perc * length(orig_data$label)):length(orig_data$label),]
data <- orig_data[1:as.integer(test_perc * length(orig_data$label)),]

# length(data[,"label"])
# length(val_data[,"label"])


# Count the occurrences of each word for each label
word_counts <- data |>
  separate_rows(Text, sep = "\\s+") |>
  group_by(label, Text) |>
  summarise(count = n()) |>
  arrange(-count)

# Reformat the word_counts data frame
reformatted_word_counts <- word_counts %>%
  pivot_wider(names_from = label, values_from = count, values_fill = 0)%>%
  mutate(Sum = rowSums(across(-Text))) |> arrange(-Sum)

colnames(reformatted_word_counts) <- c("Text","One","Zero", "Total")


filtered_word_counts <- reformatted_word_counts %>%
  filter(Total > min_count) 


mutual_info <- function(N_11,N_01,N_10,N_00){
  N <- N_00+N_01+N_10+N_11
  N_1. <- N_01+N_11
  N_.1 <- N_10+N_11
  N_0. <- N_00+N_10
  N_.0 <- N_01+N_00
  
  info <- (N_11/N)*log2(N*N_11/(N_1.*N_.1)) + 
    (N_10/N)*log2(N*N_10/(N_0.*N_.1)) + 
    (N_01/N)*log2(N*N_01/(N_1.*N_.0)) +
    (N_00/N)*log2(N*N_00/(N_0.*N_.0))
  
  return(info)
}


chi2 <- function(N_11,N_01,N_10,N_00){
  N <- sum(N_00,N_10,N_11,N_01)
  E_11 <- (N_11+N_10)*(N_11+N_01)/N
  E_01 <- (N_01+N_11)*(N_01+N_00)/N
  E_10 <- (N_10+N_11)*(N_10+N_00)/N
  E_00 <- (N_00+N_10)*(N_00+N_01)/N
  
  chi <- sum((N_11-E_11)**2/E_11,(N_10-E_10)**2/E_10,(N_01-E_01)**2/E_01,(N_00-E_00)**2/E_00)
  
  return(chi) 
}


# Create a Vector with Columns
columns = c("Text","One","Zero")
col_order = c("Text","One","Zero")

#Create a Empty DataFrame with 0 rows and n columns
results = data.frame(matrix(nrow = nrow(filtered_word_counts), ncol = length(columns))) 

# Assign column names
colnames(results) = columns

# Add word column
results$Text <- filtered_word_counts$Text


col_ind <- c("One","Zero")
row_ind <- c(1:nrow(filtered_word_counts))

for (y in col_ind) {
  dummydf <- filtered_word_counts[,c("Text",y,"Total")]
  for (x in row_ind) {
    N_11 <- as.numeric(dummydf[x,2])
    N_01 <- as.numeric(sum(dummydf[-x,2])) 
    N_10 <- as.numeric(dummydf[x,3] - dummydf[x,2]) 
    N_00 <- as.numeric(sum(dummydf[-x,3]-dummydf[-x,2])) 
    
    if (select_method == 1) {
      results[x,y] <- chi2(N_11,N_01,N_10,N_00)
    }
    else {
      results[x,y] <- mutual_info(N_11,N_01,N_10,N_00)
    }
  }
}


wrds <- vector()

for (y in col_ind) {
  dummydf <- results[,c("Text",y)]
  
  # Select rows with results in the top A%
  
  selected_rows <- dummydf %>%
    slice_max(dummydf[[2]] ,prop = top_percentage)
  rws <- as.vector(selected_rows$Text)
  wrds <- union(wrds,rws)
}
(length(wrds))


filter_words <- function(string, word_list) {
  # Split the string into individual words
  words <- strsplit(string, " ")[[1]]
  
  # Filter the words based on the match with the word list
  matched_words <- words[words %in% word_list]
  
  # Combine the matched words into a single string
  result <- paste(matched_words, collapse = " ")
  
  return(result)
}


dfout <- orig_data

for (i in c(1:nrow(dfout))) {
  dfout[i,2] <- filter_words(dfout[i,2],wrds)
}

# print(head(dfout[,2]))
# print(head(orig_data[,2]))

data <- dfout[1:as.integer(test_perc * length(dfout$label)),]
val_data <- dfout[as.integer(test_perc * length(dfout$label)):length(dfout$label),]

# print(length(data[,"label"]))
# print(length(val_data[,"label"]))

pc <- c(0,0)
for(i in 0:1) {
  pc[i+1] <- length(data[data[, "label"]==i, "label"])
}

full_text <- paste(data$Text, sep=" ", collapse=" ")
tokens <- unique(strsplit(full_text, " ")[[1]])
tokens <- tokens[!(tokens=="")]

## multinomial model
ptc <- data.frame(
  token = tokens, 
  ct0 = 0,
  ct1 = 0,
  pt0 = 0,
  pt1 = 0
)



for(i in 0:1) {
  class_text <- strsplit(paste(data[data[, "label"]==i, "Text"], sep=" ", collapse=" "), " ")[[1]]
  
  for(j in 1:length(tokens)) {
    ptc[j,paste("ct",i, sep="")] <- sum(class_text==ptc[j, "token"]) + 1
  }
  ptc[,paste("pt",i, sep="")] <- ptc[,paste("ct",i, sep="")] / (length(class_text) + length(tokens))
}

ptc <- ptc[,c("token","pt0", "pt1")]



right_tr <- 0
right_test <- 0

for(k in 1:length(data[,"label"])) {
  vec <- strsplit(data[k, "Text"], " ")[[1]]
  vec <- vec[!(vec=="")]
  probs <- log(pc)
  
  
  for(h in vec) {
    for(i in 0:1) {
      inc <- log(ptc[ptc[,"token"]==h ,paste("pt",i, sep="")])
      if ( length(inc) == 1 && ! any(is.na(inc)) ) {
        probs[i+1] <- probs[i+1] + inc
      }
    }
  }
  
  if( (0:1)[probs == max(probs)] == data[k, "label"]) {
    right_tr <- right_tr + 1
  }
}

acc_train <- 100*right_tr/length(data[,"label"])

for(k in 1:length(val_data[,"label"])) {
  vec <- strsplit(val_data[k, "Text"], " ")[[1]]
  vec <- vec[!(vec=="")]
  probs <- log(pc)
  
  
  for(h in vec) {
    for(i in 0:1) {
      inc <- log(ptc[ptc[,"token"]==h ,paste("pt",i, sep="")])
      if ( length(inc) == 1 && ! any(is.na(inc)) ) {
        probs[i+1] <- probs[i+1] + inc
      }
    }
  }
  
  if( (0:1)[probs == max(probs)] == val_data[k, "label"]) {
      right_test <- right_test + 1
  }
}

acc_test <- 100*right_test/length(val_data[,"label"])

print(acc_train)

print(acc_test)

