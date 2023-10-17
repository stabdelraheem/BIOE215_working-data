library(tidyverse)

#Read and inspect data
surveys <- read_csv("data/portal_data_joined.csv")
head(surveys)
summary(surveys)

#Q1: What's the data type of column species_id? Of hindfoot_length?
#A1: Character; Double
typeof(surveys$species_id)
typeof(surveys$hindfoot_length)

#Q2: How many rows and columns are in surveys? 
#A2: 34786 rows x 13 columns
nrow(surveys)
ncol(surveys)

#select() works on columns
select(surveys, plot_id, species_id, weight)
select(surveys, plot_id, weight_g = weight)
select(surveys, -record_id, -species_id)

#filter() works on columns
filter(surveys, year == 1995)
filter(surveys, year == 1995, plot_id == 7)
filter(surveys, month == 2 | day == 20)

#Q3: filter() surveys to records collected in November where hindfoot_length is greater than 36.0
#A3: 
filter(surveys, month == 11, hindfoot_length > 36.0)

#Q4: fix these errors
#A4: 
filter(surveys, year = 1995) 
filter(surveys, year == 1995)
filter(surveys, polt_id == 2) 
filter(surveys, plot_id == 2) 

#PIPES
select(filter(surveys, year == 1995), plot_id, weight)
surveys_psw <- surveys %>% 
  filter(year == 1995) %>% 
  select(plot_id, weight)


#Q5: Use pipes to subset surveys to animals collected before 1995 retaining just the columns year, sex, and weight
surveys_q5 <- surveys %>% 
  filter(year < 1995) %>% 
  select(year, sex, weight)

#Add columns
surveys %>% 
  mutate(weight_kg = weight/1000) %>% 
  View()

surveys %>% 
  mutate(weight_kg = weight/1000, 
         weight_lb = weight_kg * 2.2) %>% 
  View()

surveys %>% 
  filter(!is.na(weight)) %>% 
  mutate(weight_kg = weight/1000, 
         weight_lb = weight_kg * 2.2) %>% 
  View()


#Q6: Create a new data frame from the surveys data that meets the following criteria: 
# - contains only the species_id column and 
# - a new column called hindfoot_cm containing the hindfoot_length values (currently in mm) converted to centimeters. 
#In this hindfoot_cm column, there are no NAs and all values are less than 3.

surveys_q6 <- surveys %>% 
  filter(!is.na(hindfoot_length), hindfoot_length > 3) %>% 
  mutate(hindfoot_cm = hindfoot_length / 10) %>% 
  select(species_id, hindfoot_cm) %>% 
  View()


surveys %>% 
  group_by(sex) %>% 
  summarise(mean_weight = mean(weight, na.rm = TRUE))

surveys %>% 
  group_by(species_id, sex) %>% 
  summarise(mean_weight = mean(weight, na.rm = TRUE))

surveys %>% 
  drop_na(weight) %>% 
  group_by(species_id, sex) %>% 
  summarise(mean_weight = mean(weight), 
            min_weight = min(weight), 
            .groups = "drop") %>% 
  arrange(mean_weight)


#Q7: How many animals were caught in each plot_type surveyed?
head(surveys)
surveys_q7 <- surveys %>% 
  group_by(plot_type) %>% 
  summarise(caught = n(), 
            .groups = "drop") 

#Q8: Use group_by() and summarize() to find the mean, min, and max hindfoot length for each species (using species_id). 
#Also add the number of observations (hint: see ?n).
surveys_q8 <- surveys %>% 
  group_by(species_id) %>% 
  summarise(mean = mean(hindfoot_length), 
            min = min(hindfoot_length), 
            max = max(hindfoot_length), 
            no_obs = n(), 
            .groups = "drop")

#Q9: What was the heaviest animal measured in each year? Return the columns year, genus, species_id, and weight.
surveys_q9 <- surveys %>% 
  group_by(year) %>% 
  summarise(max_weight = max(weight, na.rm = TRUE), 
            genus = genus[weight == max_weight],
            species_id = species_id [weight == max_weight], 
            .groups = "drop") %>% 
  select(year, genus, species_id, max_weight) 


surveys_q9 <- surveys %>% 
  drop_na(weight) %>% 
  group_by(year) %>% 
  filter(weight == max(weight)) %>% 
  select(year, genus, species_id, weight) %>% 
  arrange(year)
  
