#load libraries
library(devtools)
library(everypoliticianR)
library(twfyR)

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
