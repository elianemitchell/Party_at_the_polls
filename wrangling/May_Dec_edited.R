library(googledrive)
library(googlesheets4)
library(tidyverse)
library(rgdal)
library(raster)
library(tools)
library(finalfit)
library(dplyr)
library(fastDummies)
library(stringi)
library(conflicted)

#setwd("/Users/sophieyang/georgetown/2022Spring/100democracy")
#getwd()
rm(list=ls())

# Read cleaned Dec data
dec = read.csv("./cleaned_Dec2020.csv")

# Select needed columns in May 2020
may_selected <- May_sample %>%
  dplyr::select(Precinct, 
                Gender, 
                CountyRegistrationDate,
                StateRegistrationDate,
                StatusCode, # turns out to be all 'A'
                BirthDate,
                FirstName,LastName,MiddleName, 
                contains("-PG"),
                contains("-PP"),
                contains("-SP"),
                contains("-SG"))

# # Check duplicated full name (first+middle+last)
may_selected$full_name = paste(may_selected$FirstName,may_selected$MiddleName,may_selected$LastName)
# sum(duplicated(may_selected$full_name))
# sum(duplicated(may_selected$full_name))/dim(may_selected)[1]

# There are 9452 people with duplicated full name, about 2.4% of the dataset.
# Remove people with duplicated names for merge
May_merge = may_selected[!duplicated(may_selected$full_name),]

# # Check people with same full name
dec$full_name = 
 paste(dec$first_name,dec$middle_name,dec$last_name,sep = " ")
# sum(duplicated(dec$full_name))
# sum(duplicated(dec$full_name))/dim(dec)[1]
# # 3978 people have duplicated name, about 1.53% of the dataset

# Remove people with duplicated name to merge
Dec_merge = dec[!duplicated(dec$full_name),]

# Merge based on full name
May_Dec = merge(May_merge,Dec_merge, by="full_name")

# # Check if there's any gender conflict
# "gender" is from Dec data and "Gender" is from May data.
# sum(is.na(May_Dec$gender)) 
# sum(is.na(May_Dec$Gender))
# gender_check = May_Dec[,c("gender","Gender")]
# gender_check = na.omit(gender_check) # remove NAs to compare
# sum(gender_check$gender != gender_check$Gender)
# # There are 58 rows with conflicting gender.

May_Dec = May_Dec %>%
  dplyr::select(-first_name, # remove name information
                -middle_name,
                -last_name,
                -full_name,
                -FirstName,
                -MiddleName,
                -LastName,
                -X,
                -StatusCode)%>% 
  
  # filter out people with weird ages 
  filter(age < 100) %>%   
  
  # Filter out people with inactive voter data status)
  #filter(voter_status!="Active") %>%
  
  #create gender indicators
  mutate(Female = ifelse(gender == "Female",1,0), 
         Male = ifelse(gender == "Male",1,0)) %>%
  
  mutate(status_active = ifelse(voter_status == "Active", 1, 0),
         status_inactive = ifelse(voter_status == "Inactive", 1, 0)) %>%
  
  #remove original gender variables 
  dplyr::select(-contains("gender"),
                -voter_status)  

# Add age group dummies
May_Dec = dummy_cols(May_Dec, select_columns = "age_model_binned")


# Convert election variables to dummies
for (i in c(5:10)) {
  # Add column names(election+year)
  tmp = paste(substr(names(May_Dec)[i], 12, 13),
              substr(names(May_Dec)[i], 7, 10),sep = "")
  
  May_Dec = May_Dec %>%
    mutate( col_name = ifelse(is.na(May_Dec[,i]),0,1))
  # Rename the column with election year and 
  names(May_Dec)[names(May_Dec)=="col_name"] = tmp
}

# Check if results are same
# sum(May_Dec$SG2020)
# sum(!is.na(May_Dec[,'04/28/2020-SG']))


# editing precincts 

# list of precincts
#full_precinctlist <- read.csv("./Precinct Identifiers - Sheet1.csv")

stri_sub(May_Dec$precinct_name, 3, 2) <- "-" # add hyphens to precinct names 
                                             # to match with Vtd 1

# note that a conflict may arise between raster package and dplyr for "select"
# to resolve this, access the "conflicted" package and choose preference for "select"

# Aggregate on precinct level

# May_Dec %>% 
#   group_by(precinct_name) %>%
#   summarise(female_sum = sum(Female),
#             male_sum = sum(Male),
#             active_sum = sum(status_active),
#             inactive_sum = sum(status_inactive))
#   



# below function shows us if there are precincts in the cleaned
# dataset that are not present in the full precinct list

# BE SURE TO TEST THIS ON THE FULL DATASET 
setdiff(May_Dec$precinct_name, full_precinctlist$Vtd.1)








