rm(list = ls())
library(dplyr)
library(tidyverse)
library(tidyr)
options(scipen = 999)
#install.packages("rjson")
#library(rjson)
#json_file <- "https://data.cdc.gov/resource/78p9-mpg4.json"
#deaths_df <- fromJSON(file=json_file)

deaths_df <- read_csv("Assignment1-Barlow-CDC_Data Wrangling and Visualization.csv")
summary(deaths_df)

observed_df <- select(deaths_df, c("Year", "Cause of Death", "State", "HHS Region",
                              "Age Range", "Benchmark", "Locality", "Observed Deaths"))

expected_df <- select(deaths_df, c("Year", "Cause of Death", "State", "HHS Region",
                               "Age Range", "Benchmark", "Expected Deaths"))

colnames(observed_df) <- c("Year", "Cause_of_Death", "State", "HHS_Region",
                           "Age_Range","Benchmark", "Locality", "Observed_Deaths")

colnames(expected_df) <- c("Year", "Cause_of_Death", "State", "HHS_Region",
                           "Age_Range","Benchmark", "Expected_Deaths")
rownames(observed_df)
rownames(expected_df)
expected_df$Benchmark <- paste(deaths_df$Benchmark, deaths_df$Locality, sep = "_")
#observed_df$Locality <- paste("Observed_Deaths", deaths_df$Locality, sep = "-")

#df3 <- new_df2[ ,-7]
expected_df <- expected_df %>% spread("Benchmark", "Expected_Deaths", sep = "_")

#observed_df <- observed_df %>% 
#  group_by(Locality) %>% 
#  mutate(grouped_id = row_number())

#observed_df <- observed_df %>% 
#  spread("Locality", "Observed_Deaths", sep = "-") %>% 
#  select(-grouped_id)

observed_df <- observed_df %>% spread(Locality, Observed_Deaths)

observed_df <- observed_df[ , -6]
nrow(distinct(observed_df))

observed_df <- distinct(observed_df)

#new_death_df <- new_death_df[ , -6]
expected_df <- expected_df[ , -c(6, 9, 12)]

joined_df <- observed_df %>% inner_join(expected_df)
head(joined_df)
colnames(joined_df)[colnames(joined_df) == "All"] <- "Observed_All"
colnames(joined_df)[colnames(joined_df) == "Metropolitan"] <- "Observed_Deaths_Metropolitan"

colnames(joined_df)[colnames(joined_df) == "Nonmetropolitan"] <- "Observed_Deaths_Nonmetropolitan"

tbl_df(joined_df)
head(joined_df)

#Plots
us_df <- filter(joined_df, HHS_Region == 0 & Year == 2005)
states <- filter(joined_df, HHS_Region > 0)
summary(states)
alabama <- filter(joined_df, State == "Alabama", Age_Range =="0-49")

ggplot(data = alabama) + 
  geom_bar(mapping = aes(x = Year, y = Observed_All, fill = Cause_of_Death))

ggplot(data = alabama) + 
  geom_bar(mapping = aes(x = Year, fill = Cause_of_Death))

ggplot(data = alabama) +
  geom_bar(mapping = aes(x = Year, fill = Year), position="dodge")
#Observed number of 2005 non-metro death by age range
ggplot(data = us_df, mapping = aes(x = Age_Range, y = Observed_Deaths_Nonmetropolitan)) + 
  geom_point(mapping = aes(color = Cause_of_Death)) 

ggplot(data = joined_df) + 
  geom_point(mapping = aes(x = Cause_of_Death, y = State))

ggplot(data = us_df) +
  geom_point(mapping = aes(x = Age_Range, y = Observed_Deaths_Nonmetropolitan))

library(ggplot2)
theme_set(theme_classic())

# Histogram on a Categorical variable
g <- ggplot(us_df, aes(Cause_of_Death))
g + geom_bar(aes(fill=Cause_of_Death), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Leading Cause of Death") 

alabama <-filter(states, State == "Alabama", Year == 2015)

g <- ggplot(alabama, aes(Observed_All))
g + geom_bar(aes(fill=Cause_of_Death), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Leading Cause of Death") 



smaller <- joined_df %>% 
  filter("Alabama")

smaller %>% 
  ggplot(aes(state)) + 
  geom_freqpoly(binwidth = 0.01)

bar <- ggplot(data = joined_df) + 
  geom_bar(
    mapping = aes(x = Year, fill = ), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()