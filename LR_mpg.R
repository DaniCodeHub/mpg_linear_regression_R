# Utilize linear regression to analyze the mpg data set in 'ggplot2' package from R
# data()

# load in libraries 

library(tidyverse)

library(tidymodels)
set.seed(0)

# examine the 'mpg' data set

view(mpg)

glimpse(mpg)

summary(mpg)

# Lets introduce new columns that will make city 'cty' and highway 'hwy' miles per gallon in Km/L

mpg_metric <- mpg %>% 
  mutate(mpg, "cty_metric" = 0.425144 * cty) %>% 
  mutate(mpg, "hwy_metric" = 0.425144 * hwy)

view(mpg_metric)

# split the data into test and training sets 

mpg_split <- initial_split(mpg_metric, prop = 4/5)

mpg_train <- training(mpg_split)
mpg_test <- testing(mpg_split)

view(mpg_train)

# visualize the raw data in a scatterplot

ggplot(mpg_train, aes(x = displ,
                      y = hwy_metric)) +
  geom_point() +
  geom_smooth(method = 'lm')

# lets look at some basic statistics of manufacturer average fuel economy

mpg_metric %>% 
  group_by(manufacturer) %>% 
  summarize(cty_metric = mean(cty_metric),
            hwy_metric = mean(hwy_metric)) %>% 
  arrange(desc(cty_metric))

# create a linear model 

lr_model = lm(formula = hwy_metric ~ displ, data = mpg_train)
lr_model 

# lets look at some elements of the linear model

names(lr_model)

lr_model$coefficients

lr_model$model

lr_model$fitted.values

lr_model$qr

summary(lr_model)

# [ p-value: < 2.2e-16 ] The low p-value [from 'summary(lr_model)'] suggests that there is a relationship between the "displ" and "hwy" variables. 
# 'displ' refers to engine displacement in litres and 'hwy_metric' refers t highway miles per gallon 


# Lets look at the confidence interval (95%) of our model
confint(lr_model)

# Plot the model to observe to fundamental statistics
plot(lr_model)
# The plot() indicates that there might be an approximate linear relationship between these variables


# examine some more features of the linear model
tidy(lr_model)
glance(lr_model)
augment(lr_model)

# predict the dependent variable 'hwy' from the test data set using our linear model
predictions <- predict(lr_model, mpg_test)

view(predictions)
# compare to actual test variables 'hwy_metric'
view(mpg_test$hwy_metric)


# evaluate the root mean squared error 'rmse' and mean squared error 'mse' of our linear model 

sqrt(mean((mpg_test$hwy_metric - predictions)^2))

mean((mpg_test$hwy_metric - predictions)^2)


# plot the lr_model predicted values against the experimental values
ggplot(mpg_test, aes(x = hwy_metric,
                     y = predictions)) +
  geom_point() + 
  geom_smooth(method = 'lm',
              se = FALSE)

# It appears that the predicted values have a slight linear relationship with the experimental values
