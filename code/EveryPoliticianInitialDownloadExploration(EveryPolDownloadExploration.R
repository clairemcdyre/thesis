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


# ##attempt 1 from http://everypolitician.org/uk/
# 
# #get politician info for the UK House of Commons. Data type = Large List
        # house_of_commons = everypolitician("United Kingdom")
        # 
        # #accessing elements in the list and assigning to dataframe
        # persons = as.data.frame(house_of_commons$persons[4:7])
        # organisations = as.data.frame(house_of_commons$organizations[2])
        # memberships = as.data.frame(house_of_commons$memberships[1:10])
        # areas = as.data.frame(house_of_commons$areas[1:3])
        # areas$identifiers <- NULL #get rid of identifier column
        # periods = as.data.frame(house_of_commons$periods)
        # 
        # # Observations # 
        # ##dataframes that are useful for my analysi: Persons, Memberships, Areas
        # ## persons can have > 1 memberships
        # ## the three dataframes need to be merged.
        # 
        # 
        # ## renaming some id columns for consistency
        # names(persons) <- c("family_name", "gender", "given_name", "person_id")
        # names(persons)
        # 
        # names(areas) <- c("area_id", "area_name")
        # names(areas)
        # 
        # ## merge three dataframes
        # politician_datatemp <- merge(persons, memberships, by = "person_id")
        # politician_data <- merge(politician_datatemp, areas, by = "area_id")
        # 
        # ##attempt 2 from https://github.com/mysociety/parlparse/blob/master/members/people.json
        # people <- fromJSON("data/people.json")
        # #explore structure of people json
        # names(people)
        # names(people$memberships)
        # names(people$organizations)
        # names(people$persons)
        # names(people$posts)
        # 
        # head(people$memberships, 10)
        # head(people$organizations, 10)
# head(people$persons, 10)
# head(people$posts, 10)
# 
# memberships_df <- as.data.frame(people$memberships)
# organizations_df <- as.data.frame(people$organizations)
# persons_df <- as.data.frame(people$persons)
# posts_df <- as.data.frame(people$posts)
# 
# str(memberships_df)
# head(memberships_df)
# dim(memberships_df)
# summary(memberships_df)
# sum(is.na(memberships_df))
# 
# str(organizations_df)
# head(organizations_df)
# dim(organizations_df)
# summary(organizations_df)
# 
# str(persons_df)
# head(persons_df)
# dim(persons_df)
# summary(persons_df)
# head(persons_df$other_names)
# 
# str(posts_df)
# head(posts_df)
# dim(posts_df)
# summary(posts_df)
# head(posts_df$identifiers)
# 
# colSums(is.na(memberships_df))#many NAs
# colSums(is.na(organizations_df))#1 NA
# colSums(is.na(persons_df))#many NAs current_constituency, current_party, redirect
# colSums(is.na(posts_df))#many NAs end_date, 18 NAs start_date
# 
# 
# anyDuplicated(persons_df$id)
# 
# #ATTEMPT #3 | http://data.parliament.uk/membersdataplatform/services/mnis/members/query/House=Commons%7CMembership=all/
# 
# # Give the input file name to the function.
# doc <- xmlParse(file = "data/houseofcommons_members.xml")
# print(doc,1)
# class(doc)
# 
# doc2 <- xmlTreeParse(file = "data/houseofcommons_members.xml")
# print(doc2,1)
# class(doc2)
# 
# root = xmlRoot(doc2)
# child = xmlChildren(root)
# subn=xmlChildren(child)
# print(root)
# #Attempt 4 (after saving the xml to Excel) import excel into R as dataframe....3 days later!! 

# Part 1: Members dataset
#df <- read.xlsx("data/House Of Commons Members.xlsx", detectDates = TRUE) -- fail doesn't read dates
commons_df <- read.csv("data/House Of Commons Members.csv", na.strings=c("","NA")) #works!

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
###pre 1979 speeches dataset
pre79_out <- lapply(readLines("data/NISpeeches_Pre1979.json"), fromJSON)#works
pre79_df <- data.frame(matrix(unlist(pre79_out), nrow=39347, byrow=T),stringsAsFactors=FALSE, header = TRUE) 

head(pre79_df)
str(pre79_df)
dim(pre79_df)
summary(pre79_df)

###post 1979 speeche dataset
post79_out <- lapply(readLines("data/NISpeeches_Post1979_6.json"), fromJSON)#works
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

### 3. Merge Pre79_df and Post79_df

speeches_df <- rbind(pre79_df,post79_df)
head(speeches_df,-10)
str(speeches_df)
dim(speeches_df)
summary(speeches_df)

#### More cleaning
speeches_df$gender <- as.factor(speeches_df$gender)
speeches_df$party <- as.factor(speeches_df$party)
speeches_df$party_group <- as.factor(speeches_df$party_group)
speeches_df$as_speaker <- as.factor(speeches_df$as_speaker)
speeches_df$ministry <- as.factor(speeches_df$ministry)
speeches_df$government <- as.factor(speeches_df$government)
speeches_df$year<- as.factor(speeches_df$year)


## create year columns      
speeches_df = speeches_df %>% 
        mutate(date = ymd(speech_date)) %>% 
        mutate_at(vars(speech_date), funs(year, month, day))

table(speeches_df$year) 
speeches_df$year<- as.factor(speeches_df$year)
levels(speeches_df$year)

##Remove some unwanted columns - day, month, date, url, colnum
colnames(speeches_df)
speeches_df <- subset(speeches_df, select = c("_id", "id", "speech", "hansard_membership_id", "speech_date", 
                                              "year", "speakerid", "person_id", "speakername", "time", "mnis_id", 
                                              "age", "as_speaker", "party_group", "ministry", "government", "proper_name", 
                                              "house_start_date", "date_of_birth", "house_end_date", "gender", "party", 
                                              "dods_id", "pims_id", "afinn_sentiment", "afinn_sd", "jockers_sentiment", "jockers_sd", 
                                              "nrc_sentiment", "nrc_sd", "sentiword_sentiment", "sentiword_sd", "hu_sentiment", 
                                              "hu_sd", "word_count"))

## Deal with NAs
###characters are not showing as NAs - think about shortening this code - function? lapply?
speeches_df <- speeches_df %>% mutate_all(na_if,"NA")
colSums(is.na(speeches_df))

### important column with many NAs that need to be replaced are: Person_id, Speakername, party_group, party, ministry, government, proper_name, 
### house_start_date, house_end_date, gender (these are speeches from the pre1979 dataset)

## More analysis - removing some columns for faster filtering and analsysing on how to handle missing values. 

speeches_df_2 <- subset(speeches_df, select = c("_id", "id", "hansard_membership_id", "speech_date", 
                                              "year", "speakerid", "person_id", "speakername", "time", "mnis_id", 
                                              "age", "as_speaker", "party_group", "ministry", "government", "proper_name", 
                                              "house_start_date", "house_end_date", "gender", "party", 
                                              "dods_id", "pims_id", "word_count"))
head(speeches_df_2)

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

### 3. Merge Pre79_df and Post79_df
colnames(post79_df)[1] <- "main_id"
speeches_df <- rbind(merged_pre79_df,post79_df)
head(speeches_df,-10)
str(speeches_df)
dim(speeches_df)
summary(speeches_df)


