install.packages("NHANES")
library(reprex)
library(NHANES)
library(dplyr)

# make a selection
nhanes_long <- NHANES %>% select(Age,AgeDecade,Education,Poverty,Work,LittleInterest,Depressed,BMI,Pulse,BPSysAve,BPDiaAve,DaysPhysHlthBad,PhysActiveDays)

# select 500 random indices
rand_ind <- sample(1:nrow(nhanes_long),500)
nhanes <- nhanes_long[rand_ind,]

summary(nhanes_long)


# convert unwanted levels to NA
# write out all the offending strings of different NAs
#used
na_strings <- c("None",
                "Some College",
                "Several")


# before replacement
table(nhanes$Education)

# replace unwanted answers/typos with NA
nhanes <- nhanes %>%
  mutate(across(everything(), 
                ~ replace(., . %in% c(na_strings), NA_character_))) %>% 
  type.convert(as.is = TRUE)

# after unwanted replaced
table(nhanes$Education)

reprex()
