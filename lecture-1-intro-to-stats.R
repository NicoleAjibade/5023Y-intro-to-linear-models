library(tidyverse)
library(here)

darwin <- read_csv(here("data", "darwin.csv"))

# CLEANING ----

# check the structure of the data
glimpse(darwin)

# check data is in a tidy format
head(darwin)

# check variable names
colnames(darwin)


# clean up column names

darwin <- janitor::clean_names(darwin)

# check for duplication
darwin %>% 
  duplicated() %>% 
  sum()

# check for typos - by looking at impossible values
darwin %>% 
  summarise(min=min(height, na.rm=TRUE), 
            max=max(height, na.rm=TRUE))

# check for typos by looking at distinct characters/values

darwin %>% 
  distinct(pair)

darwin %>% 
  distinct(type)

# missing values
darwin %>% 
  is.na() %>% 
  sum()

# quick summary

summary(darwin)

# PLOTTING GRAPHS ----

darwin %>% 
  ggplot(aes(x=type,
             y=height))+
  geom_point()

darwin %>% 
  ggplot(aes(x=type,
             y=height))+
  geom_boxplot()

darwin %>% 
  ggplot(aes(x=type,
             y=height))+
 geom_violin()

# MORE PLOTTING ----

darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))
