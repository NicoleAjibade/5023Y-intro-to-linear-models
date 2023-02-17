# Token: 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

# PACKAGES ----

library(tidyverse)
library(GGally)
library(emmeans)
library(performance)
library(here)
library(kableExtra)

#__________----

# MAKING LINEAR MODELS ----
# FINDING THE DATA FOR ONE INDEPENDANT VARIABLE (Self)

lsmodel0 <- lm(formula = height ~ 1, data = darwin)
summary(lsmodel0)

mean(darwin$height) # finding the mean height

lsmodel1 <- lm(height ~ type, data=darwin)

glimpse(lsmodel1) # full summary of model info

broom::tidy(lsmodel1) # condensing the info into only what you need

#____________----
# PLOTTING MEANS ----

darwin %>% 
  ggplot(aes(x=type, 
             y=height,
             colour=type))+
  geom_jitter(alpha=0.5,
              width=0.1)+
  stat_summary(fun=mean,
               size=1.2)+
  theme_bw()

# _____________----

# CONFIDENCE LEVELS ----

broom::tidy(lsmodel1, conf.int=T)

GGally::ggcoef_model(lsmodel1,
                     show_p_values=FALSE, 
                     conf.level=0.95)    # a plot of confidence levels
# If it doesn't cross 0 then we reject the null hypothesis

#_____________----

# FINDING THE DATA FOR THE OTHER INDEPENDANT VARIABLE (Cross) ----
darwin %>% 
  mutate(type=factor(type)) %>% 
  mutate(type=fct_relevel(type, c("Self", "Cross"))) %>% 
  lm(height~type, data=.) %>% 
  broom::tidy()
#____________----

# USING EMMEANS PACKAGE TO DO THE SAME THING AS ABOVE ----
# Does all the above in one line of code

means <- emmeans::emmeans(lsmodel1, specs = ~ type)

means

# plotting means for both cross and self

means %>% 
  as_tibble() %>% 
  ggplot(aes(x=type, 
             y=emmean))+
  geom_pointrange(aes(
    ymin=lower.CL, 
    ymax=upper.CL))
#_______________----

# RESIDUALS ----

# residuals are plots of the differences between individual data points and 
# the mean (which on the graph plotted, the mean is represented by 0 on the x axis)

performance::check_model(lsmodel1, check=c("normality","qq"))

# residual values that fall outside of the grey border or far from the line,
# suggest a lack of fit with the expected values



#_______________----

# QUANTILE-QUANTILE (QQ) PLOT ----
# QQ Plots make normal distributions linear in a graph

plot(lsmodel1, which=c(2,2))

#______________----

# OTHERS PLOTS

performance::check_model(lsmodel1, check="homogeneity")
plot(lsmodel1, which=c(1,3))

# Outliers
performance::check_model(lsmodel1, check="outliers")
plot(lsmodel1, which=c(4,4))

# SUMMARY ----
#A linear model sets one factor level as the 'intercept' estimates its mean, 
# then draws a line from the first treatment to the second treatment, the slope 
# of the line is the difference in means between the two treatments.

#The difference in means is always accompanied by a standard error of the 
#difference (SED), and this can be used to calculate a 95% confidence interval. 
#If this confidence interval does not contain the intercept value, we can reject
#the null hypothesis that there is 'no effect'.

#Linear models make a variety of assumptions, including that the noise (residual 
#differences) are approximately normally distributed, with roughly equal 
#(homogenous) variance

