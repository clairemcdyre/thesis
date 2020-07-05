library(readr)
library(stringi)
english_names <- read_csv("data/names-surnames-NL-UK-IT-PT-ES 3/english_names.txt", col_names = FALSE)

english_surnames <- read_csv("data/names-surnames-NL-UK-IT-PT-ES 3/english_surnames.txt")

english_names <- tolower(english_names)
english_surnames <- lapply(english_surnames, tolower)
english_surnames <- lapply(english_surnames, as.character)
english_surnames <- lapply(english_surnames, unlist)


english_surnames <- lapply(english_surnames, as.character)
english_surnames <- names( unlist(english_surnames[,1]))

names_stopwordlist <- rbind(english_names,english_surnames)

all_stop_words <- c(stop_words, english_surnames)
write.csv(all_stop_words, "data/again.csv")     

# custom_stop_words <- c("hon","honourable","hn", "sir", "member","friend","opposite","rt","right","minister","learned","gallant","speaker","acting","act","secretary","gentleman","committee","mrs","mr","northern", "ireland", "members", "nthe", "nthis","â","dr","esquire", "government", "house", "debate")    

