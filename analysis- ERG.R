
## This script reads in all of the completion data, additional data sets to inform the completion data, and the proceeds with
# analysis by narrowing in on particular schools. Note that the "import" file in the same folder should be run first, as it creates some of the files
# analyzed here.

library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(writexl)
library(ggplot2)

rm(list = ls())

# colSums(is.na(address.info2))

#######################
## Load the files in ##
#######################

setwd('/Users/bankstreet/Desktop/R/Completions Data/completions/')
completion.data <- readRDS("completion_data.rds")

setwd('/Users/bankstreet/Desktop/R/Completions Data/completions/directories/')
unitid.info <- read.csv("hd2020.csv")

setwd('/Users/bankstreet/Desktop/R/Completions Data/')
cip.info <- read_xlsx("c2020_a_dictionary.xlsx", sheet = 4)

setwd('/Users/bankstreet/Desktop/R/Completions Data/')
zip.info <- read_xlsx("NY_MSA_zipcodes.xlsx", col_types = c("text", "numeric", "numeric", "text", "text", "text"))

# This file includes all of the zipcodes in the NYMSA

#####################
## Clean new files ##
#####################

# We want to remove the long zipcodes when they are present. 
# Essentially: If zipcode length > 5, remove the last 5 characters
unitid.info1 <- unitid.info %>% 
  select(unitid = UNITID, sch.name = INSTNM, state = STABBR, zip.long = ZIP) %>%
  mutate(zip.long = as.character(zip.long)) %>% 
  mutate(zipcode = ifelse(nchar(zip.long) == 5, zip.long, substr(zip.long,1,nchar(zip.long)-5))) %>% 
  mutate(unitid = as.character(unitid)) %>% 
  select(-zip.long)

cip.info1 <- cip.info %>% 
  select(cipcode = codevalue, degree.program = valuelabel)

zip.info <- zip.info %>% 
  rename(zipcode = zip) %>% 
  mutate(zipcode = as.character(zipcode)) %>%  
  mutate(zipcode = str_pad(zipcode, 5, pad = "0"))

completion.data <- completion.data %>% 
  mutate(unitid = as.character(unitid))

##############
## Analysis ##
##############

# The zip.info df has all of the zipcodes in NY MSA, so we left join to it we'll get a list of all the New York ciy schools
# NA's are dropped because they simply indicate NYC zipcodes where none of the schools fall
# where there are duplicates in a left join, we get all possible combinations (the cartesian product)

nyc.schools <- zip.info %>% 
  left_join(unitid.info1, by = c("zipcode")) %>%
  select(unitid, sch.name, zipcode, city, state, county_name) %>% 
  drop_na()

# Now we can use that df to restrict completion data to only those schools
# NA's are dropped because they indicate non-NY MSA schools. The grepl statement filters for only education programs (those who's cipcode begins "13.")

nyc.completion <- completion.data %>% 
  left_join(nyc.schools, by = "unitid") %>% 
  mutate(cipcode = as.character(cipcode)) %>% 
  left_join(cip.info1, by = "cipcode") %>%
  filter(cipcode != 99) %>% 
  filter(grepl("13[.]", cipcode)) %>% 
  drop_na()

#Save output (Emmy)
output.loc <-'/Users/bankstreet/Desktop/R/Completions Data/completions/'
write_xlsx(nyc.completion, paste0(output.loc,"nyc_completions.xlsx"))

## Identify the programs that Bank Street has offered since the 80's

bank.completion <- nyc.completion %>% 
  filter(unitid == 189015)

bank.programs <- nyc.completion %>% 
  filter(unitid == 189015) %>% 
  select(degree.program, cipcode) %>% 
  unique()

# Use that list to filter for only those programs 

bank.competes <- nyc.completion %>% 
  filter(cipcode %in% bank.programs$cipcode & cipcode != 99) %>%
  arrange(desc(year))

#Save output
output.loc <-'/Users/bankstreet/Desktop/R/Completions Data/completions/'
write_xlsx(bank.competes, paste0(output.loc,"bank_competes.xlsx"))

# To get a sense of which schools are competing in these programs, sum the totals by year

competitive.schools <- bank.competes %>% 
  group_by(year, unitid, sch.name) %>% 
  summarise(graduates.in.year = sum(total.students))

#Save output
output.loc <-'/Users/bankstreet/Desktop/R/Completions Data/completions/'
write_xlsx(competitive.schools, paste0(output.loc,"competitiveschools.xlsx"))

# For some visualizations we only want to see how Bank St. compares to other schools in 2019

bank.competes.2019 <- bank.competes %>% 
  filter(year == 2019) 

###################
## Visualization ##
###################

## Stacked Bar Chart: Bank Street Graduates by Master's Degree by Year (1987-2017) ##
#####################################################################################

bank.completion.by.year.stacked <- ggplot(bank.completion, aes(fill = degree.program, y = total.students, x = year)) +
   labs(title = "Bank Street Graduates by Master's Degree by Year") +
   geom_bar(stat = "identity") 

plot(bank.completion.by.year.stacked)


## Grouped Bar Chart: Bank Street Graduates by Master's Degree by Year (2008-2017) ##
#####################################################################################

# Of the 17 programs offered by Bank Street, this visualization restricts it to the 7 most popular programs
bank.completion.2008 <- bank.completion %>% 
  filter(year %in% (2008:2019)) %>% 
  filter(cipcode == "13.0401" | cipcode == "13.0404" | cipcode == "13.1001" | cipcode == "13.1202"
         | cipcode == "13.1210" | cipcode == "13.1315" | cipcode == "13.9999")

# Having some trouble with getting the labels to fit properly on this one
bank.completion.by.year.grouped <- ggplot(bank.completion.2008) +
  geom_bar(mapping = aes(x = year, y = total.students, fill = degree.program, group = degree.program),
           position = position_dodge(width = 1), stat = "identity") +
  geom_text(bank.completion.2008, mapping = aes(x = year, y = total.students, label = format(total.students, nsmall = 0,
                                                                                             digits = 1, scientific = F)),
            position = position_dodge(0.9), hjust = 0.5)

plot(bank.completion.by.year.grouped)

## Faceted line graphs broken out by different the 7 most popular of Bank's programs ##
#######################################################################################

# For this figure we modify nyc.completion to contain fewer schools and restrict the programs to Bank Street's top 7
nyc.completion.2008 <- nyc.completion %>%
  filter(year %in% (2006:2019)) %>% 
  filter(cipcode == "13.0401" | cipcode == "13.0404" | cipcode == "13.1001" | cipcode == "13.1202"
         | cipcode == "13.1210" | cipcode == "13.1315" | cipcode == "13.9999") %>% 
  filter(unitid == "196468" | unitid == "")
  

bank.completion.facet <-ggplot(bank.completion.2008, aes(x = year, y = total.students, color = sch.name, group = sch.name)) +
  geom_line() +
  labs(title = "Completion Counts by Program (2008-2019)",
       subtitle = "This figure shows trends in Bank Street's most popular programs.",
  x = "", y = "Students Completed" ) +
  geom_point() +
  geom_text(aes(label = total.students), nudge_y = 10) +
  facet_wrap(~ degree.program, nrow = 4) 
  

plot(bank.completion.facet)

################
## Write data ##
################

output.loc <- '/Users/bankstreet/Desktop/R/Completions Data/completions/'

write_xlsx(bank.competes, paste0(output.loc,"bank_competes.xlsx"))
write_xlsx(nyc.completion, paste0(output.loc,"ny_MSA_completion.xlsx"))














