#ldavis.cpsievert.me/reviews/reviews.html tutorial
# stop_words <- stopwords("SMART")

#Wilson1 dataset
#pre-processing
Wilson1_speeches <- gsub("'","", Wilson1_speeches$speech)#remove apostrophes
Wilson1_speeches <- gsub("[[:punct:]]", " ", Wilson1_speeches)  # replace punctuation with space
Wilson1_speeches <- gsub("[[:cntrl:]]", " ", Wilson1_speeches)  # replace control characters with space
Wilson1_speeches <- gsub("[[:space:]]+$", "", Wilson1_speeches) # remove whitespace at end of documents
Wilson1_speeches <- gsub('[[:digit:]]+', '', Wilson1_speeches) #remove digits
Wilson1_speeches <- tolower(Wilson1_speeches)  # force to lowercase
Wilson1_speeches <- lemmatize_words(Wilson1_speeches)  # lemmatize words



#tokenise on space and output as a list
Wilson1_list <- strsplit(Wilson1_speeches, "[[:space:]]+")

#compute the table of terms
wilson1.term.table <- table(unlist(Wilson1_list))
wilson1.term.table <- sort(wilson1.term.table, decreasing = TRUE)

#remove terms that are stop words or occur fewer than 5 times
del <- names(wilson1.term.table) %in% stop_wordslist | wilson1.term.table < 5
wilson1.term.table <- wilson1.term.table[!del]
vocab <- names(wilson1.term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
        index <- match(x, vocab)
        index <- index[!is.na(index)]
        rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
wilson1_documents <- lapply(Wilson1_list, get.terms)                  

# Compute some statistics related to the data set:
Wilson1_D <- length(wilson1_documents)  # number of documents 
Wilson1_W <- length(vocab)  # number of terms in the vocab 
Wilson1.doc.length <- sapply(wilson1_documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
Wilson1_N <- sum(Wilson1.doc.length)  # total number of tokens in the data (546,827)
Wilson1_term.frequency <- as.integer(wilson1.term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]


# MCMC and model tuning parameters:
K <- 5
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
set.seed(357)
Wilson1_t1 <- Sys.time()
Wilson1_fit<- lda.collapsed.gibbs.sampler(documents = wilson1_documents, K = K, vocab =vocab, 
                                          num.iterations = G, alpha = alpha, 
                                          eta = eta, initial = NULL, burnin = 0,
                                          compute.log.likelihood = TRUE)
Wilson1_t2 <- Sys.time()
Wilson1_t2 - Wilson1_t1  # about 24 minutes on laptop

theta <- t(apply(Wilson1_fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(Wilson1_fit$topics) + eta, 2, function(x) x/sum(x)))

Wilson1_done <- list(phi = phi,
                     theta = theta,
                     doc.length = Wilson1.doc.length,
                     vocab = vocab,
                     term.frequency = Wilson1_term.frequency)

library(servr)

# create the JSON object to feed the visualization:
wilson1_json <- createJSON(phi = Wilson1_done$phi, 
                           theta = Wilson1_done$theta, 
                           doc.length = Wilson1_done$doc.length, 
                           vocab = Wilson1_done$vocab, 
                           term.frequency = Wilson1_done$term.frequency)

serVis(wilson1_json, out.dir = 'vis', open.browser = TRUE)

# #investigating results
# Wilson1_toptopicwords <- top.topic.words(Wilson1_fit$topics, num.words = 20, by.score = FALSE)
# wilson1_topics_out <- write.csv(Wilson1_toptopicwords, "data/Wilson1_toptopicwords.csv")#output to excel for further inspection
# Wilson1_topdocs <- top.topic.documents(Wilson1_fit$document_sums, num.documents = 20, alpha = 0.1)
# 
# wilson1_docs_out <- write.csv(Wilson1_topdocs, "data/Wilson1_docs.csv")#output to excel for further inspection

#Wilson2 dataset
#pre-processing
Wilson2_speeches <- gsub("'","", Wilson2_speeches$speech)#remove apostrophes
Wilson2_speeches <- gsub("[[:punct:]]", " ", Wilson2_speeches)  # replace punctuation with space
Wilson2_speeches <- gsub("[[:cntrl:]]", " ", Wilson2_speeches)  # replace control characters with space
Wilson2_speeches <- gsub("[[:space:]]+$", "", Wilson2_speeches) # remove whitespace at end of documents
Wilson2_speeches <- gsub('[[:digit:]]+', '', Wilson2_speeches) #remove digits
Wilson2_speeches <- tolower(Wilson2_speeches)  # force to lowercase
Wilson2_speeches <- lemmatize_words(Wilson2_speeches)  # lemmatize words



#tokenise on space and output as a list
Wilson2_list <- strsplit(Wilson2_speeches, "[[:space:]]+")

#compute the table of terms
wilson2.term.table <- table(unlist(Wilson2_list))
wilson2.term.table <- sort(wilson2.term.table, decreasing = TRUE)

#remove terms that are stop words or occur fewer than 5 times
del <- names(wilson2.term.table) %in% stop_wordslist | wilson2.term.table < 5
wilson2.term.table <- wilson2.term.table[!del]
wilson2_vocab <- names(wilson2.term.table)

# now put the documents into the format required by the lda package:

wilson2_documents <- lapply(Wilson2_list, get.terms)                  

# Compute some statistics related to the data set:
Wilson2_D <- length(wilson2_documents)  # number of documents 
Wilson2_W <- length(wilson2_vocab)  # number of terms in the vocab 
Wilson2.doc.length <- sapply(wilson2_documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
Wilson2_N <- sum(Wilson2.doc.length)  # total number of tokens in the data (546,827)
Wilson2_term.frequency <- as.integer(wilson2.term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]


# MCMC and model tuning parameters:
K <- 100
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
set.seed(357)
Wilson2_t1 <- Sys.time()
Wilson2_fit<- lda.collapsed.gibbs.sampler(documents = wilson2_documents, K = K, vocab =wilson2_vocab, 
                                          num.iterations = G, alpha = alpha, 
                                          eta = eta, initial = NULL, burnin = 0,
                                          compute.log.likelihood = TRUE)
Wilson2_t2 <- Sys.time()
Wilson2_t2 - Wilson2_t1  # about 24 minutes on laptop

theta <- t(apply(Wilson2_fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(Wilson2_fit$topics) + eta, 2, function(x) x/sum(x)))

Wilson2_done <- list(phi = phi,
                     theta = theta,
                     doc.length = Wilson2.doc.length,
                     vocab = wilson2_vocab,
                     term.frequency = Wilson2_term.frequency)

#library(servr)

# create the JSON object to feed the visualization:
wilson2_json <- createJSON(phi = Wilson2_done$phi, 
                           theta = Wilson2_done$theta, 
                           doc.length = Wilson2_done$doc.length, 
                           vocab = Wilson2_done$vocab, 
                           term.frequency = Wilson2_done$term.frequency)

serVis(wilson2_json, out.dir = 'vis', open.browser = TRUE)

#Heath1 dataset
#pre-processing
Heath1_speeches <- gsub("'","", Heath1_speeches$speech)#remove apostrophes
Heath1_speeches <- gsub("[[:punct:]]", " ", Heath1_speeches)  # replace punctuation with space
Heath1_speeches <- gsub("[[:cntrl:]]", " ", Heath1_speeches)  # replace control characters with space
Heath1_speeches <- gsub("[[:space:]]+$", "", Heath1_speeches) # remove whitespace at end of documents
Heath1_speeches <- gsub('[[:digit:]]+', '', Heath1_speeches) #remove digits
Heath1_speeches <- tolower(Heath1_speeches)  # force to lowercase
Heath1_speeches <- lemmatize_words(Heath1_speeches)  # lemmatize words



#tokenise on space and output as a list
Heath1_List <- strsplit(Heath1_speeches, "[[:space:]]+")

#compute the table of terms
heath1.term.table <- table(unlist(Heath1_List))
heath1.term.table <- sort(heath1.term.table, decreasing = TRUE)

#remove terms that are stop words or occur fewer than 5 times
del <- names(heath1.term.table) %in% stop_wordslist | heath1.term.table < 5
heath1.term.table <- heath1.term.table[!del]
heath1_vocab <- names(heath1.term.table)

# now put the documents into the format required by the lda package:

heath1_documents <- lapply(Heath1_List, get.terms)                  

# Compute some statistics related to the data set:
Heath1_D <- length(heath1_documents)  # number of documents 
Heath1_W <- length(heath1_vocab)  # number of terms in the vocab 
Heath1.doc.length <- sapply(heath1_documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
Heath1_N <- sum(Heath1.doc.length)  # total number of tokens in the data (546,827)
Heath1_term.frequency <- as.integer(heath1.term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]


# MCMC and model tuning parameters:
K <- 50
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
set.seed(357)
Heath1_t1 <- Sys.time()
Heath1_Fit<- lda.collapsed.gibbs.sampler(documents = heath1_documents, K = K, vocab =heath1_vocab, 
                                          num.iterations = G, alpha = alpha, 
                                          eta = eta, initial = NULL, burnin = 0,
                                          compute.log.likelihood = TRUE)
Heath1_t2 <- Sys.time()
Heath1_t2 - Heath1_t1  # about 24 minutes on laptop

theta <- t(apply(Heath1_Fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(Heath1_Fit$topics) + eta, 2, function(x) x/sum(x)))

Heath1_done <- list(phi = phi,
                     theta = theta,
                     doc.length = Heath1.doc.length,
                     vocab = heath1_vocab,
                     term.frequency = Heath1_term.frequency)

#library(servr)

# create the JSON object to feed the visualization:
Heath1_json <- createJSON(phi = Heath1_done$phi, 
                           theta = Heath1_done$theta, 
                           doc.length = Heath1_done$doc.length, 
                           vocab = Heath1_done$vocab, 
                           term.frequency = Heath1_done$term.frequency)

serVis(Heath1_json, out.dir = 'vis', open.browser = TRUE)