# PACKAGES----



#__________----

# STUDENTS T-TEST ----

#The one sample t-test: takes the mean of a sample and compares it with 
#the null hypothesis of zero

lm(y ~ 1)

lm (height~1)

#The two sample t-test which compares the difference between the means of two 
#samples against a null hypothesis of no difference between the means of 
#the two populations

#when sample sizes are large the t-distribution is roughly equal to a normal (z)
#distribution. However, when sample sizes are small the t-distribution has a 
#shorter and wider distribution


df <- c(1:30)

# map_dbl forces returned values to be a single vector of numbers (rather than a list)
critical_t <- map_dbl(df, ~qt(p=0.05/2, df=.x, lower.tail=FALSE))

tibble(df,critical_t) %>% 
  ggplot(aes(x=df, y=critical_t))+
  geom_point()+
  geom_line()+
  geom_hline(aes(yintercept=1.96), linetype="dashed", colour="red")+
  labs(x= "Degrees of Freedom",
       y= expression(paste("Critical value of ", italic("t"))))

# ALL THIS IS JUST TO GENERATE THE TABLE (IN GRAPH FORM) OF THE CRITICAL VALUES
# FOR THE DIFFERENCE DEGREES OF FREEDOM. Its the same values as the critical
# values tables usually provided in text books

lsmodel1 <- lm(height ~ type, data = darwin)
summary(lsmodel1)

tidy_model1 <- broom::tidy(lsmodel1)

tidy_model1[[2,2]] / tidy_model1[[2,3]] #finding the observed value of t


# PAIRED T-TEST ----

lsmodel_darwin <- lm(height ~ type + factor(pair), data = darwin)
summary(lsmodel_darwin)

#The second row now compares the mean heights of Crossed and Selfed plants 
#when they are in the same pair

#Rows 3 to 16 compare the average difference of each pair (Crossed and Selfed 
#combined) against pair 1

#generating the confidence intervals for the paired t-test
lm(height ~ type + factor(pair), data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(1:2) # just show first two rows
#______________----

#PLOTTING THE PAIRED AND UNPAIRED CONFIDENCE LEVELS ----

m1 <- lm(height ~ type, data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(2:2) %>% 
  mutate(model="unpaired")

m2 <- lm(height ~ type + factor(pair), data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(2:2) %>% 
  mutate(model="paired")

rbind(m1,m2) %>% 
  ggplot(aes(model, estimate))+
  geom_pointrange(aes(ymin=conf.high, ymax=conf.low))+
  geom_hline(aes(yintercept=0), linetype="dashed")+
  theme_minimal()+
  coord_flip()

#________________----

# REPEATING THE EXPERIMENT TO INCREASE RELIABILITY ----
set.seed(1234)

myList <- vector("list", 20)
y <- tibble()

for (i in 1:length(myList)) { 
  
  x <-  rnorm(n=12, mean=2.6, sd=2.83)
  data <- tibble(x)
  temp <- lm(x~1, data=data) %>% 
    broom::tidy(conf.int=T) 
  y <- rbind(y,temp)  
  
}

y$`experiment number` <- rep(1:20)

# the new dataframe y contains the results of 20 new experiments

y

y %>% 
  mutate(`p value < 0.05` = if_else(p.value > 0.049, "non-significant", "significant")) %>% 
  group_by(`p value < 0.05`) %>% 
  summarise(`number of experiments`=n()) 

#Shows how many of the repeats were significant and insignificant


y %>% 
  ggplot(aes(x=`experiment number`, y=estimate))+
  geom_pointrange(aes(ymin = conf.low, ymax=conf.high))+
  labs(y = "Estimated mean effect of outcrossing")+
  geom_hline(linetype="dashed", yintercept=0.05)+
  theme_minimal()

#PLOTTING ABOVE



