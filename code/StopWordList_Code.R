library(readr)
library(stringi)

#read stopword list in from csv file. CSV file contains names, honorifics and common stopwords.

stop_wordslist <- read.csv("data/stopwordslist.csv", stringsAsFactors = FALSE)
stop_wordslist <- as.character(stop_wordslist$x)

