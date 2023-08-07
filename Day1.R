# creating an object 

object1 <- c(1,2,3,4,5,6,7,8,9,10)

mean(object1)

object2 <- c(1:10)
mean(object2)

median(object2)

summary(object1)

# install packages 
#install.packages('tidyverse')

# loading the package 
library(tidyverse)
#alternative way to load data
require(tidyverse)

#reading data
dat1 <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
#getting first 6 rows
head(dat1)
#getting last 6 rows
tail(dat1)
#Getting the structure of the data
str(dat1)

# Data types 
#integer
#numeric/doubles
#date
#factors
#character


#check class type
class()

#use the $ to specify a column
class(dat1$gre)

#Dimension
dim(dat1)

# pipe (%>%)
#connect lines of code

# recode the admit column( 0-deny, 1-accept) old = new
dat2 <- dat1 %>%
  mutate(admit = recode(admit, "0" = "deny", "1" = "accept"))
head(dat2)

#rename columns (new = old)
dat3 <- dat1 %>%
  rename(position = rank)
head(dat3)


#load more data 
ideal1 <- read.csv("https://raw.githubusercontent.com/cema-uonbi/R_course/main/ideal1.csv")
str(ideal1)

#data wrangling tasks 
#change CaDOB
#clean the column names 
# deal with NA  in ReasonLoss column 
# recode CalSex column 

#recode CalfSex column 
ideal1a <- ideal1 %>%
  mutate(CalfSex = recode(CalfSex, "1" = "Male", "2" = "Female"))
head(ideal1a)


# check column names
colnames(ideal1)

# clean column names (rename CADOB - calf_date_of_birth)
ideal1b <- ideal1 %>%
  rename(calf_date_of_birth = CADOB)
head(ideal1b)

# column names in R (good cofing habits)
## calf.date.of.birth
## calf_date_of_birth


# clean column names (janitor)
install.packages("janitor")

#load  library
library("janitor")

ideal1c <- ideal1 %>%
  c


#date formats 
##d - 1, 2, ...., 31(1/3/2023) 
##D - 01, 02,...31 (01/3/2023)
##m - 1, 2,.... 12
##M - 01, 02,.....12
##b - Jan, Feb, .... Dec
##B - January, February,...., December
## y - 21, 21,...
## Y - 2021, 2022,..

library(lubridate) (as_date())
ideal1d <- ideal1%>%
  mutate(CADOB = as.Date(CADOB, format = "%d/%m/%Y"))%>%
  arrange(CADOB)# arrange the data according to the date column 
head(ideal1d)

#confirm that CADOB has changed to date format 
class(ideal1d$CADOB)

#subsetting
#check sublocations
table(ideal$sublocation)

#subsetting 
#If we wanted data from one sublocation - Kidera

## using subset 
idealkidera <- subset(ideal, ideal$sublocation == 'Kidera')

#using filter

idealkidera <- ideal1 %>%
  filter(sublocation == 'Kidera')
head(idealkidera)

#load more data
ideal2 <- read.csv("https://raw.githubusercontent.com/ThumbiMwangi/R_sources/master/ideal2.csv")
head(ideal2)


# CONVERT visit Date into date format 
ideal2a <- ideal2 %>%
  mutate(VisitDate = as.Date(VisitDate, format= "%d/%m/%Y"))

#merging data set 

##left join 
ideal3 <- ideal1 %>% 
  left_join(ideal2, by = "CalfID") # uses ideal 1 as reference and takes what is in common between ideal1 and ideal2 

## right join 
ideal4 <- ideal1 %>% 
  right_join(ideal2, by = "CalfID")
## full join
ideal5 <- ideal1 %>% 
  full_join(ideal2, by = "CalfID")
## inner join 


## subset data using columns
ideal3a <- ideal3 %>% 
  select(CalfID, VisitID, VisitDate, Theileria.spp., ELISA_mutans, ELISA_parva)

## pivot (make data longer)
ideal3b <- ideal3a %>% 
  pivot_longer(cols = c(Theileria.spp., ELISA_mutans, ELISA_parva), names_to = 'tests', values_to = 'outcome')

## grouping data (group_by)
ideal3c <- ideal3 %>% 
  group_by(CalfID) %>% 
  mutate(avrg_weight = mean(Weight, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(CalfID, Weight, avrg_weight)

#load more data 
dogdemography <- read.csv("https://raw.githubusercontent.com/cema-uonbi/R_course/main/DogcohortDemographics.csv")

#data wrangling tasks 
# rename the columns 
# format the date 
# recode the categorical variables 
# get average number of household members per village

# rename the columns 
dogdemography1 <- dogdemography %>% 
  rename( interviewDate = IntDate,
          householdID = HousehldID,
          VillageID = VillageID,
          householdMembers = HhMmbrs,
          OwnDogs = OwnDogs,
          numDogsOwned = DgsOwnd,
          adultDogsOwned = AdltDgs,
          puppiesOwned = Puppies,
          dogDiedPastMonth = DogDied,
          numDogsDiedPastMonth = NumDd,
          dogBitesPastMonth = DogBite) %>% 
  mutate(interviewDate = as.Date(interviewDate, format = "%m/%d/%y")) %>%  
  mutate(dogDiedPastMonth = as.logical(dogDiedPastMonth)) %>% 
  mutate(dogBitesPastMonth = ifelse(dogBitesPastMonth == "0", "No", ifelse(dogBitesPastMonth == "1", "Yes", NA)))%>% 
  group_by(VillageID) %>% 
  mutate(avg_household_members_per_village = round(mean(householdMembers))) %>% 
  group_by(VillageID) %>% 
  mutate(avg_dogs_per_village = round(mean(numDogsOwned)))
  


## load more data 
ideal <- read.csv("https://raw.githubusercontent.com/ThumbiMwangi/R_sources/master/ideal3a.csv")

# summarize 
table(ideal$ReasonsLoss1)




