library(readr)
library(stringi)

#read stopword list in from csv file. CSV file contains names, honorifics and common stopwords.

custom_english_words <- read.csv("data/custom_english_words.csv", stringsAsFactors = FALSE)
custom_english_words <- as.character(custom_english_words$Town)

english_words = readLines("data/words.txt")

all_english_words <- c(english_words, custom_english_words)
all_english_words <- tolower(all_english_words)

all_stop_words <- read.csv("data/stopwordslist.csv", stringsAsFactors = FALSE)
all_stop_words <- as.character(all_stop_words$x)
write.table(all_stop_words, "data/all_stop_words.txt", sep = " ")





