rm(list = ls())
library(dplyr)
library(tidyverse)
library(tidyr)
#install.packages("rjson")
#library(rjson)
#json_file <- "https://data.cdc.gov/resource/78p9-mpg4.json"
#deaths_df <- fromJSON(file=json_file)

deaths_df <- read_csv("Assignment1-Barlow-CDC_Data Wrangling and Visualization.csv")
summary(deaths_df)

observed_df <- select(deaths_df, c("Year", "Cause of Death", "State", "HHS Region",
                              "Age Range", "Locality", "Observed Deaths"))

expected_df <- select(deaths_df, c("Year", "Cause of Death", "State", "HHS Region",
                               "Age Range", "Benchmark", "Expected Deaths"))

colnames(observed_df) <- c("Year", "Cause_of_Death", "State", "HHS_Region",
                           "Age_Range","Locality", "Observed_Deaths")

colnames(expected_df) <- c("Year", "Cause_of_Death", "State", "HHS_Region",
                           "Age_Range","Benchmark", "Expected_Deaths")

expected_df$Benchmark <- paste(deaths_df$Benchmark, deaths_df$Locality, sep = "_")
observed_df$Locality <- paste("Observed_Deaths", deaths_df$Locality, sep = "-")

#df3 <- new_df2[ ,-7]
expected_df <- expected_df %>% spread("Benchmark", "Expected_Deaths", sep = "_")

observed_df <- observed_df %>% 
  group_by(Locality) %>% 
  mutate(grouped_id = row_number())

observed_df <- observed_df %>% 
  spread("Locality", "Observed_Deaths", sep = "-") %>% 
  select(-grouped_id)

observed_df <- observed_df %>% spread(Locality, Observed_Deaths, sep = "-")

#df2 <- df2[ , -6]
nrow(distinct(observed_df))

observed_df <- distinct(observed_df)

#new_death_df <- new_death_df[ , -6]
expected_df <- expected_df[ , -c(6, 9, 12)]

joined_df <- observed_df %>% inner_join(expected_df)
#colnames(joined_df$`Locality-Metropolitan`) <- c("Observed_Locality-Metropolitan")
#colnames(joined_df)[colnames(joined_df) == "Locality-Metropolitan"] <- "Observed_Deaths_Locality-Metropolitan"

#colnames(joined_df)[colnames(joined_df) == "Observed_Locality-Nonmetropolitan"] <- "Observed_Deaths_Locality-Nonmetropolitan"

ggplot(data = joined_df) + 
  geom_bar(mapping = aes(x = Year, fill = State), position = "dodge")

ggplot(data = joined_df) + 
  geom_bar(mapping = aes(x = Year, fill = Cause_of_Death))

ggplot(data = joined_df, mapping = aes(x = Year, y = "United States")) + 
  geom_point(mapping = aes(color = Cause_of_Death)) + 
  geom_smooth(data = filter(joined_df, class == "Cancer"), se = FALSE)

ggplot(data = joined_df) + 
  geom_point(mapping = aes(x = Cause_of_Death, y = State))
