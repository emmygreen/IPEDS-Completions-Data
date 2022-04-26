
## This script reads in all of the raw completion data in batches according to how it's formatted and then cleans and combines them
# Author: Eli Groves/Emmy Green
rm(list = ls())

library(tidyverse)
library(readxl)

setwd('/Users/bankstreet/Desktop/R/Completions Data/')

rm(list = ls())

#######################
## Read the files in ##
#######################

# Create a filepath for each batch
batch1.loc <- '/Users/bankstreet/Desktop/R/Completions Data/completions/batch1_84-94'
batch2.loc <- '/Users/bankstreet/Desktop/R/Completions Data/completions/batch2_95-00'
batch3.loc <- '/Users/bankstreet/Desktop/R/Completions Data/completions/batch3_01'
batch4.loc <-'/Users/bankstreet/Desktop/R/Completions Data/completions/batch4_02'
batch5.loc <- '/Users/bankstreet/Desktop/R/Completions Data/completions/batch5_03-05'
batch6.loc <- '/Users/bankstreet/Desktop/R/Completions Data/completions/batch6_06-10'
batch7.loc <- '/Users/bankstreet/Desktop/R/Completions Data/completions/batch7_11-20'



# Create a list of files in each batch
fl.batch1 <- list.files(path = batch1.loc, pattern = "csv")
fl.batch2 <- list.files(path = batch2.loc, pattern = "csv")
fl.batch3 <- list.files(path = batch3.loc, pattern = "csv")
fl.batch4 <- list.files(path = batch4.loc, pattern = "csv")
fl.batch5 <- list.files(path = batch5.loc, pattern = "csv")
fl.batch6 <- list.files(path = batch6.loc, pattern = "csv")
fl.batch7 <- list.files(path = batch7.loc, pattern = "csv")

# This function reads an individual csv and adds a column with the filename but with the letters, spaces, periods, and parentheses removed 
# leaving only the numbers (the year)

read_csv_filename <- function(filename){
  ret <- read.csv(filename)
  ret$year <- gsub("[A-z \\.\\(\\)]", "", filename)
  ret
}

# Here we set the working directory to the folder where the files are located

setwd('/Users/bankstreet/Desktop/R/Completions Data/completions/batch1_84-94')

# Next we take all of the files in the file list and run them through our function so that the year column is appended on
# from this we are left with a list of the data frames 

batch1.import.list <- lapply(fl.batch1, read_csv_filename)

# To convert from the list to a single dataframe, we can simple rbind the rows together since the batches (by design) have the columns in common

completion1 <- do.call("rbind", batch1.import.list)

# Finally we remove the import.list from the environment 

rm(batch1.import.list)

# Repeat for each batch

# batch 2
setwd('/Users/bankstreet/Desktop/R/Completions Data/completions/batch2_95-00')
batch2.import.list <- lapply(fl.batch2, read_csv_filename)
completion2 <- do.call("rbind", batch2.import.list)
rm(batch2.import.list)

# batch 3
setwd('/Users/bankstreet/Desktop/R/Completions Data/completions/batch3_01')
batch3.import.list <- lapply(fl.batch3, read_csv_filename)
completion3 <- do.call("rbind", batch3.import.list)
rm(batch3.import.list)

# batch 4
setwd('/Users/bankstreet/Desktop/R/Completions Data/completions/batch4_02')
batch4.import.list <- lapply(fl.batch4, read_csv_filename)
completion4 <- do.call("rbind", batch4.import.list)
rm(batch4.import.list)

# batch 5
setwd('/Users/bankstreet/Desktop/R/Completions Data/completions/batch5_03-05')
batch5.import.list <- lapply(fl.batch5, read_csv_filename)
completion5 <- do.call("rbind", batch5.import.list)
rm(batch5.import.list)

# batch 6
# note that "bind_rows" was used here instead of "rbind" this is because the number of columns weren't lining up so we want to merge
# on the columns that the dfs DO have in common
setwd('/Users/bankstreet/Desktop/R/Completions Data/completions/batch6_06-10')
batch6.import.list <- lapply(fl.batch6, read_csv_filename)
completion6 <- suppressWarnings(do.call("bind_rows", batch6.import.list))
rm(batch6.import.list)

# batch 7
# "bind_rows" used here as well
setwd('/Users/bankstreet/Desktop/R/Completions Data/completions/batch7_11-20')
batch7.import.list <- lapply(fl.batch7, read_csv_filename)
completion7 <- suppressWarnings(do.call("bind_rows", batch7.import.list))
rm(batch7.import.list)

##############
## Cleaning ##
##############

## For dataframes with the "MAJORNUM" variable, we want to simply combine their counts into a single measurement. Collapse and aggregate

# completion1 clean up 

c.completion1 <- completion1 %>% 
  filter(awlevel == 7) %>%                            # awlevel 7 corresponds to Masters degrees
  mutate(crace15 = ifelse(is.na(crace15), 0, crace15)) %>% 
  mutate(crace16 = ifelse(is.na(crace16), 0, crace16)) %>% 
  mutate(total.students = crace15 + crace16) %>%      # crace15 and crace16 refer to total men and total women respectively
  select(-crace15, -crace16, -awlevel)

# completion2 clean up

c.completion2 <- completion2 %>% 
  filter(awlevel == 7) %>% 
  mutate(crace15 = ifelse(is.na(crace15), 0, crace15)) %>% 
  mutate(crace16 = ifelse(is.na(crace16), 0, crace16)) %>%
  mutate(total.students = crace15 + crace16) %>% 
  select(-crace15, - crace16, -awlevel)

# completion3 clean up

c.completion3 <- completion3 %>%
  select(unitid, majornum, cipcode, awlevel, year, crace15, crace16) %>% 
  filter(awlevel == 7) %>% 
  mutate(crace15 = ifelse(is.na(crace15), 0, crace15)) %>% 
  mutate(crace16 = ifelse(is.na(crace16), 0, crace16)) %>%
  mutate(total.students1 = crace15 + crace16) %>% 
  select(-crace15, -crace16, -awlevel) %>% 
  group_by(unitid, cipcode, year) %>% 
  summarise(total.students = sum(total.students1))

# completion4 clean up

c.completion4 <-completion4 %>% 
  select(unitid = UNITID, majornum = MAJORNUM, cipcode = CIPCODE, awlevel = AWLEVEL,
         year, crace15 = CRACE15, crace16 = CRACE16) %>% 
  filter(awlevel == 7) %>%
  mutate(crace15 = ifelse(is.na(crace15), 0, crace15)) %>% 
  mutate(crace16 = ifelse(is.na(crace16), 0, crace16)) %>%
  mutate(total.students1 = crace15 + crace16) %>% 
  select(-crace16, -crace15, -awlevel) %>% 
  group_by(unitid, cipcode, year) %>% 
  summarise(total.students = sum(total.students1))
  
# completion5 clean up
  
c.completion5 <- completion5 %>%
  select(unitid, majornum, cipcode, awlevel, year, crace15, crace16) %>% 
  filter(awlevel == 7) %>%
  mutate(crace15 = ifelse(is.na(crace15), 0, crace15)) %>% 
  mutate(crace16 = ifelse(is.na(crace16), 0, crace16)) %>%
  mutate(total.students1 = crace15 + crace16) %>% 
  select(-crace15, -crace16, -awlevel) %>% 
  group_by(unitid, cipcode, year) %>% 
  summarise(total.students = sum(total.students1))

# completion6 clean up. Noticed that the 2006 & 2007 files are different from the 2008-2010 files so they'll be split up. The CTOTALT column
# is a grand total column added in the year 2008

c.completion6.1 <- completion6 %>%
  filter(year == 2006 | year == 2007) %>% 
  select(unitid = UNITID, majornum = MAJORNUM, cipcode = CIPCODE, awlevel = AWLEVEL,
         year, crace15 = CRACE15, crace16 = CRACE16) %>% 
  filter(awlevel == 7) %>%
  mutate(crace15 = ifelse(is.na(crace15), 0, crace15)) %>% 
  mutate(crace16 = ifelse(is.na(crace16), 0, crace16)) %>%
  mutate(total.students1 = crace15 + crace16) %>% 
  select(-crace16, -crace15, -awlevel) %>% 
  group_by(unitid, cipcode, year) %>% 
  summarise(total.students = sum(total.students1))
  

c.completion6.2 <- completion6 %>% 
  filter(year == 2008 | year == 2009 | year == 2010) %>% 
  select(unitid = UNITID, majornum = MAJORNUM, cipcode = CIPCODE, awlevel = AWLEVEL,
         year, total.students1 = CTOTALT) %>% 
  filter(awlevel == 7) %>% 
  select(-awlevel) %>% 
  group_by(unitid, cipcode, year) %>% 
  summarise(total.students = sum(total.students1)) 

# completion7 clean up
  
c.completion7 <- completion7 %>% 
  select(unitid = UNITID, majornum = MAJORNUM, cipcode = CIPCODE, awlevel = AWLEVEL,
         year, total.students1 = CTOTALT) %>% 
  filter(awlevel == 7) %>% 
  select(-awlevel) %>% 
  group_by(unitid, cipcode, year) %>% 
  summarise(total.students = sum(total.students1)) 

###################################  
## Combine into single dataframe ##
###################################

completion.data <- bind_rows(list(c.completion1, c.completion2, c.completion3, c.completion4, c.completion5, c.completion6.1,
                                  c.completion6.2, c.completion7))

# An issue exists where the cipcode is being read in as a num and thus, the leading and ending zeros are being dropped. The
# typical solution would be to include a ColClasses argument in read.csv() but since we don't use this function, it's not so easy
# to implement. A second solution would be to split the cipcode column, determine which are too short, add a zero, and then
# recombine. We will do this

# First, remove 1984-1986 because cipcodes were different then and there isn't a clear way to reliably convert

# This function negates the %in% operation

'%!in%' <- function(x,y)!('%in%'(x,y))

completion.data1 <- completion.data %>%
  filter(year %!in% (1984:1986)) %>%                        # filter out the years we don't want
  filter(cipcode != 99 & cipcode != 95) %>%                 # these two cipcodes refer to summarizing values
  mutate(cipcode = as.character(cipcode)) %>%     
  separate(cipcode, c("first.half", "second.half")) %>% 
  mutate(second.half = ifelse(is.na(second.half), "0000", second.half)) %>% # some second halves appear as NA but should be "0000"
  mutate(first.half = ifelse(nchar(first.half) < 2, paste0("0", first.half), first.half)) %>%
  mutate(second.half = ifelse(nchar(second.half) == 1, paste0(second.half, "000"), second.half)) %>% 
  mutate(second.half = ifelse(nchar(second.half) == 2, paste0(second.half, "00"), second.half)) %>% 
  mutate(second.half = ifelse(nchar(second.half) == 3, paste0(second.half, "0"), second.half)) %>% 
  mutate(cipcode = paste0(first.half, ".", second.half)) %>% 
  select(-first.half, -second.half)


#########################
## Save csv and r data ##
#########################
setwd('/Users/bankstreet/Desktop/R/Completions Data/completions')

write.csv(completion.data1,'/Users/bankstreet/Desktop/R/Completions Data/completions/completion_data.csv')
saveRDS(completion.data1, '/Users/bankstreet/Desktop/R/Completions Data/completions/completion_data.rds')





