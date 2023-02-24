

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
#____________________-----


# PEARSONS R ----
#Pearson’s r determines how closely two variables are related
#Its value ranges from -1 to +1, with 0 denoting no linear correlation, 
#-1 denoting a perfect negative linear correlation, and +1 denoting a perfect 
#positive linear correlation


correlation <- cor(janka$dens, janka$hardness, method = 'pearson')
correlation

#_____________----

# REGRESSION MODEL -----

janka_ls1 <- lm(hardness ~ dens, data = janka) 

# specify linear model method for line fitting

janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  geom_smooth(method="lm")

#The blue line represents the regression line, 
#and the shaded interval is the 95% confidence interval band

summary(janka_ls1)

janka_ls1 %>% 
  broom::tidy()

#intercept describes the value of y (timber hardness) when x (wood density) = 0

#_______________----

#CENTRING ----

#centering'. By subtracting the average (mean) value of x from every data point,
#the intercept (when x is 0) can effectively be right-shifted into the centre 
#of the data.

dens_mean <- janka %>% 
  summarise(mean_dens=mean(dens))

dens_mean

janka %>% 
  mutate(centered_dens = dens-pull(dens_mean)) %>% 
  lm(hardness ~ centered_dens, data = .) %>% 
  broom::tidy()

#Density is our explanatory variable, and the slope is estimated against it. So 
#if 57.5 is the value of the regression slope (with its standard error) - then 
#the timber hardness is predicted to increase by 57.5 on the janka scale for 
#every unit change of density.

#Producing upper and lower bounds of confidence intervals
confint(janka_ls1)

#R2-the proportion of the variation explained by the linear regression analysis

#_______________----

# AUGMENTS ----

#augment() generates the predicted value for each data point according to 
#the regression, and calculates the residuals for each data point.

augmented_ls1 <- janka_ls1 %>% 
  broom::augment()

augmented_ls1 %>% 
  ggplot(aes(x=dens, 
             y=.fitted))+
  geom_line()+ 
  geom_point(aes(x=dens, 
                 y=hardness))+
  geom_segment(aes(x=dens, 
                   xend=dens, 
                   y=.fitted, 
                   yend=hardness), 
               linetype="dashed", colour="red")

#black fitted regression line and red dashed line residuals

#_______________----


# PLOTTING 3 GRAPHS TO SHOW TRENDS ----

# A line connecting all the data points in order 
p1 <- augmented_ls1 %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_line()+
  ggtitle("Full Data")

# Plotting the fitted values against the independent e.g. our regression line
p2 <- augmented_ls1 %>% 
  ggplot(aes(x=dens, y=.fitted))+
  geom_line()+
  ggtitle("Linear trend")

# Plotting the residuals against the fitted values e.g. remaining variance
p3 <- augmented_ls1 %>% 
  ggplot(aes(x=.fitted, y=.resid))+
  geom_hline(yintercept=0, colour="white", size=5)+
  geom_line()+
  ggtitle("Remaining \npattern")


library(patchwork)
p1+p2+p3

# CONDENSING THE PLOTS ----

model_plot <- function(data=augmented_ls1, 
                       x="dens", 
                       y="hardness", 
                       title="Full data"){
  ggplot(aes(x=.data[[x]], 
             y=.data[[y]]), 
         data=data)+
    geom_line()+
    theme_bw()+
    ggtitle(title)
}

p1 <- model_plot()
p2 <- model_plot(y=".fitted", title="Linear prediction")
p3 <- model_plot(y=".resid", title="Remaining pattern")

p3

#_____________----

# MORE RESIDUAL STUFF ----

#only two or three points outside of the confidence levels
plot(janka_ls1, which=c(2,2))

#'standardized residuals' are the raw residual divided by the standard deviation


plot(janka_ls1, which=c(1,3))

plot(janka_ls1, which=c(4,5))

#_________________-----

# PREDICTION ----

coef(janka_ls1)

#Imagine a new wood sample with a density of 22, 35 or 65. Apply the model to 
#make a prediction
coef(janka_ls1)[1] + coef(janka_ls1)[2] * 65

#easier way
predict(janka_ls1, newdata=list(dens=c(22,35,65)))

#Adding Standard Errors
broom::augment(janka_ls1, newdata = tibble(dens=c(22,35,65)), se=TRUE)

#Adding Confidence Levels
broom::augment(janka_ls1, newdata=tibble(dens=c(22,35,65)), interval="confidence")

#Adding Standard Error and Confidence Levels together
emmeans::emmeans(janka_ls1, 
                 specs = "dens", 
                 at = list(dens = c(22, 35, 65)))

#___________________----

#COMBINING THE THREE NEW DATA POINTS AND THE ORIGINAL PLOT ----

pred_newdata <- broom::augment(janka_ls1, 
                               newdata=tibble(dens=c(22,35,65))) 

#making a new data set with the added new data points (22, 35, 65)

janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_point(data=pred_newdata, aes(y=.fitted, x=dens), colour="red")+
  geom_label(data=pred_newdata, (aes(y=(.fitted+10), x=(dens+3), label=round(.fitted, digits=0))))+
  theme_bw()+
  labs(x="Density", y="Timber Hardness")+
  scale_x_continuous(limits=c(20,80), expand=expansion(add=c(0,5)))

#highlighted the new data points that all fall along the regression line

#_______________----


