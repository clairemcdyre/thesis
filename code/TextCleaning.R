library(tm)
library(SnowballC)
#source https://medium.com/@shubhanshugupta/data-preprocessing-in-r-2f0e25487bb

speech_corpus <- VCorpus(VectorSource(speeches_df$speech))
##Removing Punctuation
speech_corpus <- tm_map(speech_corpus, content_transformer(removePunctuation))
##Removing numbers
speech_corpus <- tm_map(speech_corpus, removeNumbers)
##Converting to lowercase
speech_corpus <- tm_map(speech_corpus, content_transformer(tolower))
##Removing stop words
speech_corpus <- tm_map(speech_corpus, content_transformer(removeWords), stopwords("english"))
##Stemming
speech_corpus <- tm_map(speech_corpus, stemDocument)
##Whitespace
speech_corpus <- tm_map(speech_corpus, stripWhitespace)

# Create Document Term Matrix
dtm <- DocumentTermMatrix(speech_corpus)
# Removing all terms whose sparsity is greater than 95% 
speech_corpus <- removeSparseTerms(dtm, 0.95)

library(data.table)
library(ggplot2)
library(ggthemes)
colS <- colSums(as.matrix(speech_corpus))
doc_features <- data.table(name = attributes(colS)$names, count = colS)
doc_features 
