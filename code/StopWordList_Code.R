library(readr)
library(stringi)

#read stopword list in from csv file. CSV file contains names, honorifics and common stopwords.

custom_english_words <- read.csv("data/custom_english_words.csv", stringsAsFactors = FALSE)
custom_english_words <- as.character(custom_english_words$Town)

english_words = readLines("data/words.txt")

all_english_words <- c(english_words, custom_english_words)
all_english_words <- tolower(all_english_words)

custom_stop_words <- c("hon","honourable","hn", "sir", "member","friend", "friends","opposite","rt","right","minister","learned","gallant","speaker","acting","act","secretary","gentleman","committee","mrs","mr","northern", "ireland", "members", "nthe", "nthis","â","dr","esquire", "government", "house", "debate", "â","")    
stop_words <- stopwords("SMART")
all_stop_words <- c(custom_stop_words, stop_words)


#Wilson1 dataset
#pre-processing
Wilson1_speeches <- tibble(id = Wilson1$main_id, speech = Wilson1$speech)

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

#keep only english words, remove names and places

keep <- names(wilson1.term.table) %in% all_english_words
wilson1.term.table <- wilson1.term.table[keep]

#remove terms that are stop words or occur fewer than 5 times
del <- names(wilson1.term.table) %in% all_stop_words | wilson1.term.table < 5
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
K <- 30
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

library(LDAvis)

# create the JSON object to feed the visualization:
wilson1_json <- createJSON(phi = Wilson1_done$phi, 
                           theta = Wilson1_done$theta, 
                           doc.length = Wilson1_done$doc.length, 
                           vocab = Wilson1_done$vocab, 
                           term.frequency = Wilson1_done$term.frequency)

serVis(wilson1_json, out.dir = 'vis', open.browser = TRUE)



#Heath1 dataset
#pre-processing
Wilson2_speeches <- tibble(id = Wilson2$main_id, speech = Wilson2$speech)

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

#keep only english words, remove names and places

keep <- names(wilson2.term.table) %in% all_english_words
wilson2.term.table <- wilson2.term.table[keep]

#remove terms that are stop words or occur fewer than 5 times
del <- names(wilson2.term.table) %in% all_stop_words | wilson2.term.table < 5
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
K <- 50
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
set.seed(357)
Wilson2_t1 <- Sys.time()
Wilson2_fit<- lda.collapsed.gibbs.sampler(documents = wilson2_documents, K = K, vocab = wilson2_vocab, 
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

library(LDAvis)

# create the JSON object to feed the visualization:
wilson2_json <- createJSON(phi = Wilson2_done$phi, 
                           theta = Wilson2_done$theta, 
                           doc.length = Wilson2_done$doc.length, 
                           vocab = Wilson2_done$vocab, 
                           term.frequency = Wilson2_done$term.frequency)

serVis(wilson2_json, out.dir = 'vis', open.browser = TRUE)


theta <- t(apply(Wilson2_fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(Wilson2_fit$topics) + eta, 2, function(x) x/sum(x)))

Wilson2_done <- list(phi = phi,
                     theta = theta,
                     doc.length = Wilson2.doc.length,
                     vocab = wilson2_vocab,
                     term.frequency = Wilson2_term.frequency)

library(LDAvis)

# create the JSON object to feed the visualization:
wilson2_json <- createJSON(phi = Wilson2_done$phi, 
                           theta = Wilson2_done$theta, 
                           doc.length = Wilson2_done$doc.length, 
                           vocab = Wilson2_done$vocab, 
                           term.frequency = Wilson2_done$term.frequency)

serVis(wilson2_json, out.dir = 'vis', open.browser = TRUE)


