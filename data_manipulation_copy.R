# Functions for Data Clean UP
# Prasiddhi Gyawali & Meghan Balk
# prasiddhi@email.arizona.edu; balkm@email.arizona.edu

################################################################################

## install packages
library(tidyverse)
library(dplyr)
library(tibble)
library(anchors)
library(plyr)
library(reshape2)
library(janitor)

################################################################################

## LOAD DATA
cougar_template <- read.csv("https://raw.githubusercontent.com/futres/fovt-data-mapping/cougar_test/Mapping%20Files/column%20name%20template.csv")
cougar_data <- read.csv("https://de.cyverse.org/dl/d/F2088922-D273-49AE-985F-8D55966627A9/1987to2019_Cougar_Weight_Length_Public_Request.csv")
aepyceros_data <- read.csv("https://de.cyverse.org/dl/d/28031164-7903-4EC1-BA86-6441741BAB35/Extant_Aepyceros_database_updated_11_2016.csv", sep = ",", dec = " ")
aepyceros_template <- read.csv("https://raw.githubusercontent.com/futres/fovt-data-mapping/cougar_test/Mapping%20Files/ontology_codeBook.csv", header = TRUE, stringsAsFactors = TRUE)
deer_data <- read.csv("https://de.cyverse.org/dl/d/0E1B3FC0-ADCC-45E7-95ED-F4E11075CF49/EAP_Florida_Modern_Deer_Measurements_FORFUTRES_1_23_2020.csv", header = TRUE, stringsAsFactors = FALSE)

################################################################################

## function to delete empty rows and columns
delete_empty_r_and_c <- function(data){
  gsub("--", NA, data)
  data <-data %>%
    remove_empty("cols") %>% # removes all NA cols
    remove_empty("rows") # removes all NA rows
  return(data)
}

################################################################################

## function to standardize materialSampleType
materialSampleType <- function(data, column, check, replace) 
{
  # data = dataframe
  # column = selected column from dataframe
  # check = values already in data frame, vector created to check for these values
  # replace = what the values from the check vector are to be replaced with
  for(i in 1:length(check)) # i is incremented by 1, it starts at one and goes through how ever many values are in check
  {
    data[,column][data[,column] == check[i]] <- replace[i]
  }
  return(data)
}

#Example
#data = cougar_data
#column = 'Status'
#check = c("A", "B", "C")
#replace = c("Whole Organism", "Part Organism - Skinned", "Part Organism - Gutted")
new.data <- materialSampleType(data = cougar_data, column = 'Status', check = c("A", "B", "C"), replace = c("Whole Organism", "Part Organism - Skinned", "Part Organism - Gutted"))

################################################################################

##function to clean up sex types if misspelled or not complete
sex <- function(data, column)
{
  # data = dataframe
  # column = selected column from data frame
  data[,column] <- replace(data[,column], grep("^f", data[,column], ignore.case = TRUE), "female") # if values in the column starts w 'f' replace it with 'female'
  data[,column] <- replace(data[,column], grep("^m", data[,column], ignore.case = TRUE), "male") # if values in the column starts w 'r' replace it with 'male'
  return(data)
}

#Example
#data = cougar_data
#column = 'Sex'
new.data <- sex(data = cougar_data, column = 'Sex')

################################################################################

##function to clean up side of bone
measurementSide <- function(data, column)
{
  # data = dataframe
  # column = selected column from data frame
  data[,column] <- replace(data[,column], grep("^l", data[,column], ignore.case = TRUE), "left") # if values in the column starts w 'l' replace it with 'left'
  data[,column] <- replace(data[,column], grep("^r", data[,column], ignore.case = TRUE), "right") # if values in the column starts w 'r' replace it with 'female'
  return(data)
}

#Example
#data = deer_data
#column = 'Side'
new.data <- measurementSide(data = deer_data, column = 'Side')

################################################################################

##function to standardize reproductive condition
reproductiveCondition <- function(data, column, reproductive, non.reproductive)
{
  # data = dataframe
  # column = column where reproductive/non.reproductive data is being stored
  # reproductive = vector of things that are classified as reproductive
  # non.reproductive = vector of things that are classified as non-reproductive
  for(i in 1:length(reproductive)) # i is incremented by 1, it starts at one and goes through how ever many values are in reproductive
  {
    data[,column][data[,column] == reproductive[i]] <- "reproductive"
  }
  for(i in 1:length(non.reproductive)) # i is incremented by 1, it starts at one and goes through how ever many values are in non.reproductive
  {
    data[,column][data[,column] == non.reproductive[i]] <- "non-reproductive"
  }
  return(data)
}

#Example
#data = deer_data
#column = reproductiveCondition
#reproductive <- c("several weeks pregnant", "\"pregnant (2 fetus) within two weeks of birth\"", "\"pregnant, 1 female fetus 22 wks\"", "\"not preg, quiet ovaries, lactating, lots of milk, enlarged glands\"", "\"non pregnant\" lactating", "\"not pregnant\" lactating", "non pregnant, not lactating but glands present, note says 2 yearlings nearby", "pregnant, 1 fetus female 23 weeks (#85-61)", "non pregnant, not lactating, medium nipples", "non pregnant, lactating", "not pregnant, maybe lactating", "not pregnant, lactating, \"fawn at side #85-66\" (although note that 85-66 in this list is 16 months so not suckling fawn, and actually 85-68 lists Dam as 85-67 so that was an error and fawn is 85-68)", "not pregnant, lactating", "non pregnant, lactating, had ~6-7mo fawn at side", "lactating [assume non pregnant]")
#non.reproductive <- c("not pregnant, not lactating", "non pregnant, not lactating", "non pregnant, non lactating")

new.data <- reproductiveCondition(data = deer_data, column = "reproductiveCondition", 
                                  reproductive = c("several weeks pregnant", "\"pregnant (2 fetus) within two weeks of birth\"", "\"pregnant, 1 female fetus 22 wks\"", "\"not preg, quiet ovaries, lactating, lots of milk, enlarged glands\"", "\"non pregnant\" lactating", "\"not pregnant\" lactating", "non pregnant, not lactating but glands present, note says 2 yearlings nearby", "pregnant, 1 fetus female 23 weeks (#85-61)", "non pregnant, not lactating, medium nipples", "non pregnant, lactating", "not pregnant, maybe lactating", "not pregnant, lactating, \"fawn at side #85-66\" (although note that 85-66 in this list is 16 months so not suckling fawn, and actually 85-68 lists Dam as 85-67 so that was an error and fawn is 85-68)", "not pregnant, lactating", "non pregnant, lactating, had ~6-7mo fawn at side", "lactating [assume non pregnant]"),
                                  non.reproductive = c("not pregnant, not lactating", "non pregnant, not lactating", "non pregnant, non lactating"))

#Example
#data = deer_data
#column = reproductiveCondition
#reproductive <- unique(deer_data$reproductiveCondition)
#reproductive <- reproductive[-c("", "--", non.reproductive)]
#non.reproductive <- c("not pregnant, not lactating", "non pregnant, not lactating", "non pregnant, non lactating")

non.reproductive <- c("not pregnant, not lactating", "non pregnant, not lactating", "non pregnant, non lactating")
reproductive <- unique(deer_data$reproductiveCondition)
index <- c("", "--", non.reproductive)
reproductive <- reproductive[!(reproductive %in% index)]

new.data <- reproductiveCondition(data = deer_data, column = "reproductiveCondition", 
                                  reproductive = reproductive,
                                  non.reproductive = non.reproductive)

################################################################################

##function to clean up life-stage
lifeStage <- function(data, column, adult, juvenile)
{
  # data = dataframe
  # adult = vector of all possible adult values
  # juvenile = vector of all possible juvenile values
  for(i in 1:length(adult)) # i is incremented by 1, it starts at one and goes through how ever many values are in adult
  {
    data[,column][data[,column] == adult[i]] <- "adult"
  }
  for(i in 1:length(juvenile)) # i is incremented by 1, it starts at one and goes through how ever many values are in juvenile
  {
    data[,column][data[,column] == juvenile[i]] <- "juvenile"
  }
  return(data)
}

#Example
#data = aepyceros_data
#column = 'Age..juv..prime.adult..older.adult..old.'
#juvenile = c("Juvenile (2)", "juvenile (22-24 months), M3 erupting", "Juvenile (?) 2+ years")
#adult = unique(aepyceros_data$Age..juv..prime.adult..older.adult..old.)
#index = c("", juvenile)
#adult = adult[!(adult %in% index)]

juvenile = c("Juvenile (2)", "juvenile (22-24 months), M3 erupting", "Juvenile (?) 2+ years")
adult = unique(aepyceros_data$Age..juv..prime.adult..older.adult..old.)
index = c("", juvenile)
adult = adult[!(adult %in% index)]

new.data <- lifeStage(data = aepyceros_data, column = 'Age..juv..prime.adult..older.adult..old.',
                      adult = adult, juvenile = juvenile)

################################################################################


## melt data & filter empty values
melt_data <- function(data, cols)
{
  # data = dataframe
  # cols = vector of columns to melt
  data <- reshape2::melt(data, measure.vars = cols) # takes data from col1 and col2 and places it all into a column called value & labels this data from col1 and col2 with the names of those two columns
  dplyr::filter(data, !is.na(value)) # deletes all NA in value column
}

#Example
#data = cougar_data
#cols = c("Weight", "Length") or c(7,8)
new.data <- melt_data(data = cougar_data, cols = c("Weight", "Length"))
new.data <- melt_data(data = cougar_data, cols = c(7,8))

################################################################################

## autopopulate measurementUnit
measurementUnit <- function(data)
{
  # data = dataframe
  # change = column name of values being changed
  # check = column name of values being checked
  data[,"measurementUnit"] <- grepl(pattern = "\\<w", data[,"variable"], ignore.case = TRUE) # if string starts w 'w' in check column values in change column are set to true if not they are set to false
  data[,"measurementUnit"][data[,"measurementUnit"] == TRUE] <- "g" # if value in change column is true replace it with 'g'
  data[,"measurementUnit"][data[,"measurementUnit"] == FALSE] <- "mm" # if value in change column is false replace it with 'mm'
  return(data)
}

#Example
#data = deer_data
#change = 'measurementUnit'
#check = 'measurementType'

## How does this work? I think it needs one more argument (e.g., something to map "length" to "mm", etc.)

################################################################################

## rename columns to match the template
template_match <- function(data, template, old, new)
{
  # data = dataframe
  # template = terms being mapped
  # old = old names of columns
  # new = new names of columns
  for(i in 1:ncol(data)){ 
    if(isTRUE(colnames(data)[i] %in% template[,old])){ 
        colnames(data)[i] <- template[,new][template[,old] == colnames(data)[i]] 
    }
    else{
      next
    }
  }
  return(data)
}

#Example
#data = new.data
#template = cougar_template
#old = 'Column.Name'
#new = 'Template.Name'
new.data <- melt_data(data = cougar_data, cols = c(7,8))
names(new.data) <- gsub("\\.", " ", colnames(new.data))
melt.data <- template_match(data = new.data, template = cougar_template, old = 'Column.Name', new = "Template.Name")

################################################################################

##doing all the things!
# new_data <- cougar_data %>%
#   delete_empty_r_and_c() %>%
#   materialSampleType("Status", c("A", "B", "C"), c("Intact", "Field Dressed", "Skinned")) %>%
#   sex("Sex") %>%
#   melt_data(c("Length", "Weight")) %>%
#   measurementUnit("measurementUnit", "variable") %>%
#   template_match(cougar_template, "Column.Name", "Template.Name")

#aepyceros_data1 <- col_rename(data = aepyceros_data, template = aepyceros_template, old = "label", new = "term")

# aepyceros_data <- aepyceros_data %>%
#   delete_empty_r_and_c() %>%
#   #status("Status", c("A", "B", "C"), c("Intact", "Field Dressed", "Skinned")) %>%
#   sex("SEX") %>%
#   #melt_data("Length", "Weight") %>%
#   #add_col() %>%
#   #measurement_unit("measurementUnit", "variable") %>%
#   template_match(aepyceros_template, "label", "term")

deer <- read.csv("https://raw.githubusercontent.com/futres/fovt-data-mapping/master/Original%20Data/EAP%20Florida%20Modern%20Deer%20Measurements_FORFUTRES_1_23_2020.csv")
colnames(deer)
