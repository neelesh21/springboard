library(dplyr)
library(tidyr)

#read file
titanic <- read.csv("titanic3.csv", stringsAsFactors = FALSE)

#Port of embarkation

#remove blank row
titanic  <- titanic %>% filter(ticket != "")

#replace blank for embarked with S
titanic <- titanic %>% mutate(embarked = replace(embarked, embarked=="", "S"))


# impute Age with median
medianAge <- median(titanic$age)


#Lifeboat
titanic <- titanic %>% mutate (boat = replace(boat, boat=="", "NA"))


#Cabin
titanic <- titanic %>% mutate (has_cabin_number = ifelse(cabin == "", 0, 1))
