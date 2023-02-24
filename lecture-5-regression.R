# PACKAGES ---- 

library(tidyverse)
library(rstatix)
library(performance)

#______________----

# PLOTTING THE DATA ----

janka <- read_csv(here("data", "janka.csv"))

janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()

#Shows a positive correlation between wood density and wood hardness

# PEARSONS R ----
#Pearson’s r determines how closely two variables are related
#Its value ranges from -1 to +1, with 0 denoting no linear correlation, 
#-1 denoting a perfect negative linear correlation, and +1 denoting a perfect 
#positive linear correlation


correlation <- cor(janka$dens, janka$hardness, method = 'pearson')
correlation

