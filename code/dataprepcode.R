#load libraries
library(devtools)
library(everypoliticianR)
library(twfyR)
library(jsonlite)
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(wordcloud)
library(knitr)
library(openxlsx)
library(tm)
library(wordcloud)
library(tokenizers)
library(SnowballC)
library(udpipe)
library(rJava)
library(mallet)
library(lattice)
library(XML)
library(methods)
library(openxlsx)
library(xlsx)
library(lubridate)
library(dplyr)
library(XLConnect)
library(rlist)
library(sqldf)
library(gmodels)
library(widyr)
library(igraph)
library(ggraph)
library(lda)
library(LDAvis)
library(textstem)


# Part 1: Members dataset - taken from UK Parliament Website API
commons_df <- read.csv("data/House Of Commons Members.csv", na.strings=c("","NA"))

#initial inspection of the data frame
head(commons_df)
str(commons_df)
dim(commons_df)
summary(commons_df)
colSums(is.na(commons_df))#Find missing values
anyDuplicated(commons_df$Member_Id)# verify if member_id can be used as the unique identifier for a MP

## First Draft Prep
### Keep columns: member_id, dods_id, pims_id, displayAs, ListAs, FullTitle, DOB, Gender, Party, ID (party_id),house, 
### MemberFrom, HouseStartDate, HouseEndDate
### Remove columns: Clerks_Id, LayingMinisterName, DateOfDeath, Id2, IsActive, Name, Reason, StartDate
### Rename columns: Id to Party_Id
### Remove rows where house == Lords
### Convert categorical columns to factors

commons_df <- subset(commons_df, select = -c(Clerks_Id, LayingMinisterName, DateOfDeath, Id2, IsActive, Name, Reason, StartDate))
commons_df<- subset(commons_df, House == "Commons")
colnames(commons_df)[10] <-  "Party_Id"

#Convert to factors: party, gender and memberfrom
commons_df$Party <- as.factor(commons_df$Party)
commons_df$Gender <- as.factor(commons_df$Gender)
commons_df$MemberFrom <- as.factor(commons_df$MemberFrom)

levels(commons_df$Party)
levels(commons_df$Gender)
levels(commons_df$MemberFrom)

table(commons_df$Party)   

## Second Prep: Clean Up Member Names -- leaving as is. 
## looks like it doesn't recognise fadas but the number of errors seems low so leaving for now.

## Part 2: Speeches dataset for Mongodb
###pre 1979 speeches dataset - read in from CSV and convert to dataframe
pre79_out <- lapply(readLines("data/NISpeeches_Pre1979.json"), fromJSON)
pre79_df <- data.frame(matrix(unlist(pre79_out), nrow=39347, byrow=T),stringsAsFactors=FALSE, header = TRUE) 

#inspect pre79_df
head(pre79_df)
str(pre79_df)
dim(pre79_df)
summary(pre79_df)

###post 1979 speeches dataset - read in from CSV and convert to dataframe
post79_out <- lapply(readLines("data/NISpeeches_Post1979_6.json"), fromJSON)
post79_df <- as.data.frame(matrix(unlist(post79_out), nrow=68354, byrow=T),stringsAsFactors=FALSE, header = TRUE) 

head(post79_df)
str(post79_df)
dim(post79_df)
summary(post79_df)

## Speeches data prep and cleaning
### Add column names 

colnames(pre79_df) <- c("_id","speech", "id","hansard_membership_id","speech_date","year","speakerid","person_id",
                        "speakername","colnum","time","url","as_speaker","afinn_sentiment","afinn_sd","jockers_sentiment",
                        "jockers_sd","nrc_sentiment","nrc_sd","sentiword_sentiment","sentiword_sd","hu_sentiment","hu_sd","word_count")

colnames(post79_df) <- c("_id", "id","speech", "afinn_sentiment","afinn_sd","jockers_sentiment","jockers_sd",
                         "nrc_sentiment","nrc_sd","sentiword_sentiment","sentiword_sd","hu_sentiment","hu_sd","word_count",
                         "speech_date","time","url","as_speaker","speakerid","person_id","hansard_membership_id","mnis_id","age",
                         "party_group","ministry","government","proper_name","house_start_date","date_of_birth","house_end_date",
                         "gender","party","dods_id","pims_id")

## List of columns varies between Pre79_df and Post79_df:
### 1. Adding additional columns to each so that they can be merged

#### Pre79_df

extraCols_pre79_df <- data.frame(matrix(ncol = 13, nrow = 39347))
pre79names <- c("mnis_id", "age","party_group", "ministry", "government","proper_name","house_start_date", 
                "date_of_birth", "house_end_date", "gender", "party", "dods_id", "pims_id")
colnames(extraCols_pre79_df) <- pre79names

head(extraCols_pre79_df)

pre79_df <- cbind(pre79_df,extraCols_pre79_df)
head(pre79_df)

#### Post79_df

extraCols_post79_df <- data.frame(matrix(ncol = 3, nrow = 68354))
post79names <- c("year", "speakername","colnum")
colnames(extraCols_post79_df) <- post79names

head(extraCols_post79_df)

post79_df <- cbind(post79_df,extraCols_post79_df)
head(post79_df)

### 2. Rearranging the columns so that the two dataframes so that the two can be merged into one dataframe
pre79_df <- pre79_df[c("_id","id","speech","hansard_membership_id","speech_date","year","speakerid","person_id",
                       "speakername","colnum","time","mnis_id","age","url","as_speaker","party_group","ministry",
                       "government","proper_name","house_start_date","date_of_birth","house_end_date","gender",
                       "party","dods_id","pims_id","afinn_sentiment","afinn_sd","jockers_sentiment","jockers_sd",
                       "nrc_sentiment","nrc_sd","sentiword_sentiment","sentiword_sd","hu_sentiment","hu_sd","word_count")]
head(pre79_df)

post79_df <- post79_df[c("_id","id","speech","hansard_membership_id","speech_date","year","speakerid","person_id",
                         "speakername","colnum","time","mnis_id","age","url","as_speaker","party_group","ministry",
                         "government","proper_name","house_start_date","date_of_birth","house_end_date","gender",
                         "party","dods_id","pims_id","afinn_sentiment","afinn_sd","jockers_sentiment","jockers_sd",
                         "nrc_sentiment","nrc_sd","sentiword_sentiment","sentiword_sd","hu_sentiment","hu_sd","word_count")]
head(post79_df)

### 3. Inspect pre79_df and post79_df for NAs, person identifiers

### Explore how to complete unique identifiers for each speaker.
#### Variables that could be used for this are hansard_membership_id, speakerid, person_id, speakername, mnis_id, dods_id, pims_id
#### mnis_id in speeches_df == Member_Id in commons_df, for pre1979 records have no mnis_id, all post1979 records have an mnis_id.
#### Blanks in predf person data will have to be resolved manually by exporting to excel and completing the blanks using a combination of pims_id, dods_id and names values. 
### Post1979 values can be resolved by joining commons_df and post79_df on mnis_id, Member_ID
### unique identifier for a person needs to be implemented or identified after this stage of cleaning. 
filter(speeches_df_2, is.na(hansard_membership_id))

# Exporting pre79_df to Excel, removing some unneed columns, these will be added in again after cleaning. 
pre79_df_export <- subset(pre79_df, select = c("_id","id","hansard_membership_id","speech_date","year","speakerid","person_id",
                                               "speakername","colnum","time","mnis_id","age","url","as_speaker","party_group","ministry",
                                               "government","proper_name","house_start_date","date_of_birth","house_end_date","gender",
                                               "party","dods_id","pims_id","afinn_sentiment","afinn_sd","jockers_sentiment","jockers_sd",
                                               "nrc_sentiment","nrc_sd","sentiword_sentiment","sentiword_sd","hu_sentiment","hu_sd","word_count"))


write.csv(pre79_df_export, 'data/pre79_df_export.csv')

#Exporting commons_df to csv to help with person identification on pre79_df in Excel
commons_df_export <- subset(commons_df, select = c("Member_Id", "Dods_Id", "Pims_Id", "DisplayAs", "ListAs", "FullTitle"))

write.csv(commons_df_export, 'data/commons_df_export.csv')

#After working on the file in Excel - re-import pre79_df__export again. 
clean_pre79_df <- read.csv('data/pre79_df__export.csv', stringsAsFactors = FALSE)
#check it
head(clean_pre79_df)
colnames((clean_pre79_df))

#change the name of col[2] to "X_id" on pre79_df so it matches clean_pre79_df and can be used as the merge key
colnames(pre79_df)[1] <- "main_id"
colnames(clean_pre79_df)
colnames(clean_pre79_df)[2] <- "main_id"


#join clean_pre79_df with pre79_df
merged_pre79_df <- merge(pre79_df, clean_pre79_df, by = "main_id" )

# verify the merge worked correctly
dim(merged_pre79_df)
head(merged_pre79_df)
colnames(merged_pre79_df)
merged_pre79_df[55] == "Joe Dean"
filter(merged_pre79_df, merged_pre79_df[48] == "B089")#more testing needed
filter(merged_pre79_df, merged_pre79_df[55] == "Gerry Fitt")#more testing needed

#drop unneeded duplicate columns so that merged_pre79_df cols == post79_df
colnames(merged_pre79_df)
merged_pre79_df <- subset(merged_pre79_df, select = -c(2,4:38,74:77))
test2 <- subset(merged_pre79_df, select = -c(4:38))
test2 <- subset(merged_pre79_df, select = -c(39:41))
colnames(merged_pre79_df)
head(merged_pre79_df)
dim(merged_pre79_df)

#rename and rearrange columns
colnames(merged_pre79_df) <- c("main_id","speech", "id","hansard_membership_id","speech_date","year","speakerid","person_id",
                               "speakername","colnum","time","mnis_id", "age","url","as_speaker", "party_group", "ministry", "government","proper_name","house_start_date", 
                               "date_of_birth", "house_end_date", "gender", "party", "dods_id", "pims_id","afinn_sentiment","afinn_sd","jockers_sentiment",
                               "jockers_sd","nrc_sentiment","nrc_sd","sentiword_sentiment","sentiword_sd","hu_sentiment","hu_sd","word_count")

merged_pre79_df <- merged_pre79_df[c("main_id","id","speech","hansard_membership_id","speech_date","year","speakerid","person_id",
                                     "speakername","colnum","time","mnis_id","age","url","as_speaker","party_group","ministry",
                                     "government","proper_name","house_start_date","date_of_birth","house_end_date","gender",
                                     "party","dods_id","pims_id","afinn_sentiment","afinn_sd","jockers_sentiment","jockers_sd",
                                     "nrc_sentiment","nrc_sd","sentiword_sentiment","sentiword_sd","hu_sentiment","hu_sd","word_count")]

#filter to remove any records pre 1964 as these will not be part of the analysis.
merged_pre79_df <- subset(merged_pre79_df, merged_pre79_df$year >= "1964")
dim(merged_pre79_df)
summary((merged_pre79_df))

#filter to remove any records from post79_df post 2011 as these will not be part of the analysis (post Labour government ended May 2010)


## need to run this on post79_df to create year column     
post79_df = post79_df %>% 
        mutate(date = ymd(speech_date)) %>% 
        mutate_at(vars(speech_date), funs(year, month, day))

post79_df <- subset(post79_df, select = -c(date, month, day))


post79_df <- subset(post79_df, post79_df$year <= "2011")#subset post79_df to exclude records after 2011
dim(post79_df)
summary((post79_df))

### 3. Merge Pre79_df and Post79_df
colnames(post79_df)[1] <- "main_id"
speeches_df <- rbind(merged_pre79_df,post79_df)
head(speeches_df,-10)
str(speeches_df)
dim(speeches_df)
summary(speeches_df)


#### More cleaning

#Rename gender values so that it can be converted to a factor
speeches_df$gender
speeches_df$gender[speeches_df$gender == "F"] <- "Female"
speeches_df$gender[speeches_df$gender == "M"] <- "Male"
speeches_df$gender[speeches_df$gender == "M "] <- "Male"
speeches_df$gender[speeches_df$gender == "0"] <- NA
speeches_df$gender[speeches_df$gender == "#N/A"] <- NA

#Convert features to factors
speeches_df$gender <- as.factor(speeches_df$gender)
speeches_df$party <- as.factor(speeches_df$party)
speeches_df$party_group <- as.factor(speeches_df$party_group)
speeches_df$as_speaker <- as.factor(speeches_df$as_speaker)
speeches_df$ministry <- as.factor(speeches_df$ministry)
speeches_df$government <- as.factor(speeches_df$government)
speeches_df$year<- as.factor(speeches_df$year)
speeches_df$age<- as.factor(speeches_df$age)
speeches_df$mnis_id<- as.factor(speeches_df$mnis_id)

#Convert word count to a number
speeches_df$word_count<- as.numeric(speeches_df$word_count)
speeches_df$afinn_sentiment <- as.numeric(speeches_df$afinn_sentiment)
speeches_df$afinn_sd <- as.numeric(speeches_df$afinn_sd)
speeches_df$jockers_sentiment <- as.numeric(speeches_df$jockers_sentiment)
speeches_df$jockers_sd <- as.numeric(speeches_df$jockers_sd)
speeches_df$nrc_sentiment <- as.numeric(speeches_df$nrc_sentiment)
speeches_df$nrc_sd <- as.numeric(speeches_df$nrc_sd)
speeches_df$sentiword_sentiment <- as.numeric(speeches_df$sentiword_sentiment)
speeches_df$sentiword_sd <- as.numeric(speeches_df$sentiword_sd)
speeches_df$hu_sentiment <- as.numeric(speeches_df$hu_sentiment)
speeches_df$hu_sd <- as.numeric(speeches_df$hu_sd)

##Remove some unwanted columns - day, month, date, url, colnum, speech_date, dods_id, pims_id, date_of_birth, house_start_date, house_end_date
colnames(speeches_df)
speeches_df <- subset(speeches_df, select = -c(time,url, colnum, speech_date, dods_id, pims_id,date_of_birth,house_start_date, house_end_date))


## 4. Data Exploration
table(speeches_df$year)
table(speeches_df$party)
table(speeches_df$party_group)
table(speeches_df$ministry)

## Some party names are duplicated and need to be merged
speeches_df$party <- as.character(speeches_df$party)
speeches_df$party <- as.factor(speeches_df$party)

speeches_df$party[speeches_df$party== "Democratic Unionist"] <- "Democratic Unionist Party"
speeches_df$party[speeches_df$party == "Ulster Unionist Party."] <- "Ulster Unionist Party"
speeches_df$party[speeches_df$party == "SNP."] <- "Scottish National Party"

speeches_df$party <- as.factor(speeches_df$party)
speeches_df$party_group <- as.factor(speeches_df$party_group)
# SDLP MP Gerry Fitt wrongly classified under party Social Democratic Party instead of SDLP - this code doesn't work? 
speeches_df <- within(speeches_df, speeches_df$party[speeches_df[15]== "Gerry Fitt"] <- 'Social Democratic & Labour Party')
# Ulster Unionist Party. incorrectly categorised - needs to be re-categorised under party_group 'Other'
speeches_df$party_group <- as.character(speeches_df$party_group)
speeches_df$party_group[speeches_df$party_group == 'Ulster Unionist Party.'] <- 'Other'
speeches_df$party_group <- as.factor(speeches_df$party_group)

#get totals for each year
table(speeches_df$party_group,speeches_df$year)
table(speeches_df$party,speeches_df$year)
table(speeches_df$ministry)
table(speeches_df$ministry, speeches_df$government)
table(speeches_df$ministry, speeches_df$party_group)
table(speeches_df$ministry, speeches_df$party)
table(speeches_df$ministry,speeches_df$year)
table(speeches_df$government,speeches_df$year)
table(speeches_df$proper_name,speeches_df$year)

#Export for tableau to create some exploratory graphs
tableau_export <- subset(speeches_df, select = c("main_id","year","mnis_id","party_group","ministry","government","proper_name","gender","party","word_count"))

write.csv(tableau_export, "data/tableau_export_hansard.csv")

#split speeches_df by ministry
Wilson1 <- subset(speeches_df, speeches_df$ministry == "Wilson1")
Wilson2 <- subset(speeches_df, speeches_df$ministry == "Wilson2")
Heath1 <- subset(speeches_df, speeches_df$ministry == "Heath1")
Wilson3 <- subset(speeches_df, speeches_df$ministry == "Wilson3")
Wilson4 <- subset(speeches_df, speeches_df$ministry == "Wilson4")
Callaghan1 <- subset(speeches_df, speeches_df$ministry == "Callaghan1")
Thatcher1 <- subset(speeches_df, speeches_df$ministry == "Thatcher1")
Thatcher2 <- subset(speeches_df, speeches_df$ministry == "Thatcher2")
Thatcher3 <- subset(speeches_df, speeches_df$ministry == "Thatcher3")
Major1 <- subset(speeches_df, speeches_df$ministry == "Major1")
Major2 <- subset(speeches_df, speeches_df$ministry == "Major2")
Blair1 <- subset(speeches_df, speeches_df$ministry == "Blair1")
Blair2 <- subset(speeches_df, speeches_df$ministry == "Blair2")
Blair3 <- subset(speeches_df, speeches_df$ministry == "Blair3")
Brown <- subset(speeches_df, speeches_df$ministry == "Brown")


# Separate the speeches column so it can be tidied up - remove stopwords, punctuation, to_lowercase
Wilson1_speeches <- tibble(id = Wilson1$main_id, speech = Wilson1$speech)
Wilson3_speeches <- tibble(id = Wilson2$main_id, speech = Wilson2$speech)
Heath1_speeches <- tibble(id = Heath1$main_id, speech = Heath1$speech)
Wilson3_speeches <- tibble(id = Wilson3$main_id, speech = Wilson3$speech)
Wilson4_speeches <- tibble(id = Wilson4$main_id, speech = Wilson4$speech)
Callaghan1_speeches <- tibble(id = Callaghan1$main_id, speech = Callaghan1$speech)
Thatcher1_speeches <- tibble(id = Thatcher1$main_id, speech = Thatcher1$speech)
Thatcher2_speeches <- tibble(id = Thatcher2$main_id, speech = Thatcher2$speech)
Thatcher3_speeches <- tibble(id = Thatcher3$main_id, speech = Thatcher3$speech)
Major1_speeches <- tibble(id = Major1$main_id, speech = Major1$speech)
Major2_speeches <- tibble(id = Major2$main_id, speech = Major2$speech)
Blair1_speeches <- tibble(id = Blair1$main_id, speech = Blair1$speech)
Blair2_speeches <- tibble(id = Blair2$main_id, speech = Blair2$speech)
Blair3_speeches <- tibble(id = Blair3$main_id, speech = Blair3$speech)
Brown1_speeches <- tibble(id = Brown$main_id, speech = Brown$speech)


#Wilson1 dataset
#pre-processing
Wilson1_speeches <- gsub("'","", Wilson1_speeches$speech)#remove apostrophes
Wilson1_speeches <- gsub("[[:punct:]]", " ", Wilson1_speeches)  # replace punctuation with space
Wilson1_speeches <- gsub("[[:cntrl:]]", " ", Wilson1_speeches)  # replace control characters with space
Wilson1_speeches <- gsub("[[:space:]]+$", "", Wilson1_speeches) # remove whitespace at end of documents
Wilson1_speeches <- gsub('[[:digit:]]+', '', Wilson1_speeches) #remove digits
Wilson1_speeches <- tolower(Wilson1_speeches)  # force to lowercase
Wilson1_speeches <- lemmatize_words(Wilson1_speeches)  # lemmatize words

write.csv(Wilson1_speeches, "data/Wilson1_speeches.csv")# export to csv for MALLET lda modelling

#Wilson2 dataset
#pre-processing
Wilson2_speeches <- gsub("'","", Wilson2_speeches$speech)#remove apostrophes
Wilson2_speeches <- gsub("[[:punct:]]", " ", Wilson2_speeches)  # replace punctuation with space
Wilson2_speeches <- gsub("[[:cntrl:]]", " ", Wilson2_speeches)  # replace control characters with space
Wilson2_speeches <- gsub("[[:space:]]+$", "", Wilson2_speeches) # remove whitespace at end of documents
Wilson2_speeches <- gsub('[[:digit:]]+', '', Wilson2_speeches) #remove digits
Wilson2_speeches <- tolower(Wilson2_speeches)  # force to lowercase
Wilson2_speeches <- lemmatize_words(Wilson2_speeches)  # lemmatize words

write.csv(Wilson2_speeches, "data/Wilson2_speeches.csv")# export to csv for MALLET lda modelling

#Heath1 dataset
#pre-processing
Heath1_speeches <- gsub("'","", Heath1_speeches$speech)#remove apostrophes
Heath1_speeches <- gsub("[[:punct:]]", " ", Heath1_speeches)  # replace punctuation with space
Heath1_speeches <- gsub("[[:cntrl:]]", " ", Heath1_speeches)  # replace control characters with space
Heath1_speeches <- gsub("[[:space:]]+$", "", Heath1_speeches) # remove whitespace at end of documents
Heath1_speeches <- gsub('[[:digit:]]+', '', Heath1_speeches) #remove digits
Heath1_speeches <- tolower(Heath1_speeches)  # force to lowercase
Heath1_speeches <- lemmatize_words(Heath1_speeches)  # lemmatize words

write.csv(Heath1_speeches, "data/Heath1_speeches.csv")# export to csv for MALLET lda modelling

#Wilson3 dataset
#pre-processing
Wilson3_speeches <- gsub("'","", Wilson3_speeches$speech)#remove apostrophes
Wilson3_speeches <- gsub("[[:punct:]]", " ", Wilson3_speeches)  # replace punctuation with space
Wilson3_speeches <- gsub("[[:cntrl:]]", " ", Wilson3_speeches)  # replace control characters with space
Wilson3_speeches <- gsub("[[:space:]]+$", "", Wilson3_speeches) # remove whitespace at end of documents
Wilson3_speeches <- gsub('[[:digit:]]+', '', Wilson3_speeches) #remove digits
Wilson3_speeches <- tolower(Wilson3_speeches)  # force to lowercase
Wilson3_speeches <- lemmatize_words(Wilson3_speeches)  # lemmatize words

write.csv(Wilson3_speeches, "data/Wilson3_speeches.csv")# export to csv for MALLET lda modelling

#Wilson4 dataset
#pre-processing
Wilson4_speeches <- gsub("'","", Wilson4_speeches$speech)#remove apostrophes
Wilson4_speeches <- gsub("[[:punct:]]", " ", Wilson4_speeches)  # replace punctuation with space
Wilson4_speeches <- gsub("[[:cntrl:]]", " ", Wilson4_speeches)  # replace control characters with space
Wilson4_speeches <- gsub("[[:space:]]+$", "", Wilson4_speeches) # remove whitespace at end of documents
Wilson4_speeches <- gsub('[[:digit:]]+', '', Wilson4_speeches) #remove digits
Wilson4_speeches <- tolower(Wilson4_speeches)  # force to lowercase
Wilson4_speeches <- lemmatize_words(Wilson4_speeches)  # lemmatize words

write.csv(Wilson4_speeches, "data/Wilson4_speeches.csv")# export to csv for MALLET lda modelling

#Callaghan1_speeches
#pre-processing
Callaghan1_speeches <- gsub("'","", Callaghan1_speeches$speech)#remove apostrophes
Callaghan1_speeches <- gsub("[[:punct:]]", " ", Callaghan1_speeches)  # replace punctuation with space
Callaghan1_speeches <- gsub("[[:cntrl:]]", " ", Callaghan1_speeches)  # replace control characters with space
Callaghan1_speeches <- gsub("[[:space:]]+$", "", Callaghan1_speeches) # remove whitespace at end of documents
Callaghan1_speeches <- gsub('[[:digit:]]+', '', Callaghan1_speeches) #remove digits
Callaghan1_speeches <- tolower(Callaghan1_speeches)  # force to lowercase
Callaghan1_speeches <- lemmatize_words(Callaghan1_speeches)  # lemmatize words

write.csv(Callaghan1_speeches, "data/Callaghan1_speeches.csv")# export to csv for MALLET lda modelling
#Thatcher1_speeches
#pre-processing
Thatcher1_speeches <- gsub("'","", Thatcher1_speeches$speech)#remove apostrophes
Thatcher1_speeches <- gsub("[[:punct:]]", " ", Thatcher1_speeches)  # replace punctuation with space
Thatcher1_speeches <- gsub("[[:cntrl:]]", " ", Thatcher1_speeches)  # replace control characters with space
Thatcher1_speeches <- gsub("[[:space:]]+$", "", Thatcher1_speeches) # remove whitespace at end of documents
Thatcher1_speeches <- gsub('[[:digit:]]+', '', Thatcher1_speeches) #remove digits
Thatcher1_speeches <- tolower(Thatcher1_speeches)  # force to lowercase
Thatcher1_speeches <- lemmatize_words(Thatcher1_speeches)  # lemmatize words

write.csv(Thatcher1_speeches, "data/Thatcher1_speeches.csv")# export to csv for MALLET lda modelling

#Thatcher2_speeches
#pre-processing
Thatcher2_speeches <- gsub("'","", Thatcher2_speeches$speech)#remove apostrophes
Thatcher2_speeches <- gsub("[[:punct:]]", " ", Thatcher2_speeches)  # replace punctuation with space
Thatcher2_speeches <- gsub("[[:cntrl:]]", " ", Thatcher2_speeches)  # replace control characters with space
Thatcher2_speeches <- gsub("[[:space:]]+$", "", Thatcher2_speeches) # remove whitespace at end of documents
Thatcher2_speeches <- gsub('[[:digit:]]+', '', Thatcher2_speeches) #remove digits
Thatcher2_speeches <- tolower(Thatcher2_speeches)  # force to lowercase
Thatcher2_speeches <- lemmatize_words(Thatcher2_speeches)  # lemmatize words

write.csv(Thatcher2_speeches, "data/Thatcher2_speeches.csv")# export to csv for MALLET lda modelling

#Thatcher3_speeches
#pre-processing
Thatcher3_speeches <- gsub("'","", Thatcher3_speeches$speech)#remove apostrophes
Thatcher3_speeches <- gsub("[[:punct:]]", " ", Thatcher3_speeches)  # replace punctuation with space
Thatcher3_speeches <- gsub("[[:cntrl:]]", " ", Thatcher3_speeches)  # replace control characters with space
Thatcher3_speeches <- gsub("[[:space:]]+$", "", Thatcher3_speeches) # remove whitespace at end of documents
Thatcher3_speeches <- gsub('[[:digit:]]+', '', Thatcher3_speeches) #remove digits
Thatcher3_speeches <- tolower(Thatcher3_speeches)  # force to lowercase
Thatcher3_speeches <- lemmatize_words(Thatcher3_speeches)  # lemmatize words

write.csv(Thatcher3_speeches, "data/Thatcher3_speeches.csv")# export to csv for MALLET lda modelling

#Major1_speeches
#pre-processing
Major1_speeches <- gsub("'","", Major1_speeches$speech)#remove apostrophes
Major1_speeches <- gsub("[[:punct:]]", " ", Major1_speeches)  # replace punctuation with space
Major1_speeches <- gsub("[[:cntrl:]]", " ", Major1_speeches)  # replace control characters with space
Major1_speeches <- gsub("[[:space:]]+$", "", Major1_speeches) # remove whitespace at end of documents
Major1_speeches <- gsub('[[:digit:]]+', '', Major1_speeches) #remove digits
Major1_speeches <- tolower(Major1_speeches)  # force to lowercase
Major1_speeches <- lemmatize_words(Major1_speeches)  # lemmatize words

write.csv(Major1_speeches, "data/Major1_speeches.csv") # export to csv for MALLET lda modelling

#Major2_speeches
#pre-processing
Major2_speeches <- gsub("'","", Major2_speeches$speech)#remove apostrophes
Major2_speeches <- gsub("[[:punct:]]", " ", Major2_speeches)  # replace punctuation with space
Major2_speeches <- gsub("[[:cntrl:]]", " ", Major2_speeches)  # replace control characters with space
Major2_speeches <- gsub("[[:space:]]+$", "", Major2_speeches) # remove whitespace at end of documents
Major2_speeches <- gsub('[[:digit:]]+', '', Major2_speeches) #remove digits
Major2_speeches <- tolower(Major2_speeches)  # force to lowercase
Major2_speeches <- lemmatize_words(Major2_speeches)  # lemmatize words

write.csv(Major2_speeches, "data/Major2_speeches.csv")# export to csv for MALLET lda modelling

#Blair1_speeches
#pre-processing
Blair1_speeches <- gsub("'","", Blair1_speeches$speech)#remove apostrophes
Blair1_speeches <- gsub("[[:punct:]]", " ", Blair1_speeches)  # replace punctuation with space
Blair1_speeches <- gsub("[[:cntrl:]]", " ", Blair1_speeches)  # replace control characters with space
Blair1_speeches <- gsub("[[:space:]]+$", "", Blair1_speeches) # remove whitespace at end of documents
Blair1_speeches <- gsub('[[:digit:]]+', '', Blair1_speeches) #remove digits
Blair1_speeches <- tolower(Blair1_speeches)  # force to lowercase
Blair1_speeches <- lemmatize_words(Blair1_speeches)  # lemmatize words

write.csv(Blair1_speeches, "data/Blair1_speeches.csv")# export to csv for MALLET lda modelling

#Blair2_speeches
#pre-processing
Blair2_speeches <- gsub("'","", Blair2_speeches$speech)#remove apostrophes
Blair2_speeches <- gsub("[[:punct:]]", " ", Blair2_speeches)  # replace punctuation with space
Blair2_speeches <- gsub("[[:cntrl:]]", " ", Blair2_speeches)  # replace control characters with space
Blair2_speeches <- gsub("[[:space:]]+$", "", Blair2_speeches) # remove whitespace at end of documents
Blair2_speeches <- gsub('[[:digit:]]+', '', Blair2_speeches) #remove digits
Blair2_speeches <- tolower(Blair2_speeches)  # force to lowercase
Blair2_speeches <- lemmatize_words(Blair2_speeches)  # lemmatize words

write.csv(Blair2_speeches, "data/Blair2_speeches.csv")# export to csv for MALLET lda modelling

#Blair3_speeches
#pre-processing
Blair3_speeches <- gsub("'","", Blair3_speeches$speech)#remove apostrophes
Blair3_speeches <- gsub("[[:punct:]]", " ", Blair3_speeches)  # replace punctuation with space
Blair3_speeches <- gsub("[[:cntrl:]]", " ", Blair3_speeches)  # replace control characters with space
Blair3_speeches <- gsub("[[:space:]]+$", "", Blair3_speeches) # remove whitespace at end of documents
Blair3_speeches <- gsub('[[:digit:]]+', '', Blair3_speeches) #remove digits
Blair3_speeches <- tolower(Blair3_speeches)  # force to lowercase
Blair3_speeches <- lemmatize_words(Blair3_speeches)  # lemmatize words

write.csv(Blair3_speeches, "data/Blair3_speeches.csv")# export to csv for MALLET lda modelling

#Brown1_speeches
#pre-processing
Brown1_speeches <- gsub("'","", Brown1_speeches$speech)#remove apostrophes
Brown1_speeches <- gsub("[[:punct:]]", " ", Brown1_speeches)  # replace punctuation with space
Brown1_speeches <- gsub("[[:cntrl:]]", " ", Brown1_speeches)  # replace control characters with space
Brown1_speeches <- gsub("[[:space:]]+$", "", Brown1_speeches) # remove whitespace at end of documents
Brown1_speeches <- gsub('[[:digit:]]+', '', Brown1_speeches) #remove digits
Brown1_speeches <- tolower(Brown1_speeches)  # force to lowercase
Brown1_speeches <- lemmatize_words(Brown1_speeches)  # lemmatize words

write.csv(Brown1_speeches, "data/Brown1_speeches.csv")# export to csv for MALLET lda modelling


