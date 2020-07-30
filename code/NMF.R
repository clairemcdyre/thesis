#source: https://methods.sagepub.com/dataset/howtoguide/non-negative-matrix-factorization-in-news-2016

install.packages('tm')
install.packages('NMF')
library(NMF)
library(tm)


corpus = Corpus(VectorSource(enc2utf8(Wilson1$speech)))
#pre-processing
corpus = tm_map(corpus, content_transformer(tolower))
replacePunctuation = function(x) {return (gsub("[[:punct:]]"," ", x))}
corpus = tm_map(corpus, content_transformer(replacePunctuation))
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, content_transformer(stripWhitespace))

#the document-term matrix from the processed documents with TFIDF weighting of each word,
dtm = DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))

#Finally, we compute the NMF of the document-term matrix, and we pick the number of topics to be 10:
k = 10
topic_model=nmf(as.matrix(dtm), k)

#With the estimated model, we first extract the word–topic matrix H as the following:

words_in_topic = t(topic_model$H)


#We then print out the top 10 words with the largest weights in each topic:
        
for (i in 1:10){
                print(paste(“topic”,i))
                print(sort(words_in_topic[,i], decreasing=TRUE)[1:10])}