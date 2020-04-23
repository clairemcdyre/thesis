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


##attempt 1 from http://everypolitician.org/uk/

#get politician info for the UK House of Commons. Data type = Large List
house_of_commons = everypolitician("United Kingdom")

#accessing elements in the list and assigning to dataframe
persons = as.data.frame(house_of_commons$persons[4:7])
organisations = as.data.frame(house_of_commons$organizations[2])
memberships = as.data.frame(house_of_commons$memberships[1:10])
areas = as.data.frame(house_of_commons$areas[1:3])
areas$identifiers <- NULL #get rid of identifier column
periods = as.data.frame(house_of_commons$periods)

# Observations # 
##dataframes that are useful for my analysi: Persons, Memberships, Areas
## persons can have > 1 memberships
## the three dataframes need to be merged.


## renaming some id columns for consistency
names(persons) <- c("family_name", "gender", "given_name", "person_id")
names(persons)

names(areas) <- c("area_id", "area_name")
names(areas)

## merge three dataframes
politician_datatemp <- merge(persons, memberships, by = "person_id")
politician_data <- merge(politician_datatemp, areas, by = "area_id")

##attempt 2 from https://github.com/mysociety/parlparse/blob/master/members/people.json
people <- fromJSON("data/people.json")
#explore structure of people json
names(people)
names(people$memberships)
names(people$organizations)
names(people$persons)
names(people$posts)

head(people$memberships, 10)
head(people$organizations, 10)
head(people$persons, 10)
head(people$posts, 10)

memberships_df <- as.data.frame(people$memberships)
organizations_df <- as.data.frame(people$organizations)
persons_df <- as.data.frame(people$persons)
posts_df <- as.data.frame(people$posts)

str(memberships_df)
head(memberships_df)
dim(memberships_df)
summary(memberships_df)
sum(is.na(memberships_df))

str(organizations_df)
head(organizations_df)
dim(organizations_df)
summary(organizations_df)

str(persons_df)
head(persons_df)
dim(persons_df)
summary(persons_df)
head(persons_df$other_names)

str(posts_df)
head(posts_df)
dim(posts_df)
summary(posts_df)
head(posts_df$identifiers)

colSums(is.na(memberships_df))#many NAs
colSums(is.na(organizations_df))#1 NA
colSums(is.na(persons_df))#many NAs current_constituency, current_party, redirect
colSums(is.na(posts_df))#many NAs end_date, 18 NAs start_date


anyDuplicated(persons_df$id)

#ATTEMPT #3 http://data.parliament.uk/membersdataplatform/memberquery.aspx | http://data.parliament.uk/membersdataplatform/services/mnis/members/query/House=Commons%7CMembership=all/

# Give the input file name to the function.
doc <- xmlParse(file = "data/houseofcommons_members.xml")
print(doc,1)
class(doc)

doc2 <- xmlTreeParse(file = "data/houseofcommons_members.xml")
print(doc2,1)
class(doc2)

root = xmlRoot(doc2)
child = xmlChildren(root)
subn=xmlChildren(child)
print(root)
#Attempt 4 (after saving the xml to Excel) import excel into R as dataframe....3 days later!! 

df <- read.xlsx("data/House Of Commons Members.xlsx")

head(df)
str(df)
dim(df)
summary(df)


colSums(is.na(df))
anyDuplicated(df$Member_Id)
