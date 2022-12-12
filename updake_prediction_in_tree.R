data("CO2")

#mean of dataset

mean(CO2$conc)
mean(CO2$uptake)

#median of dataset
median(CO2$conc)
median(CO2$uptake)

#summary of dataset
summary(CO2)

#quantile of dataset
quantile(CO2$conc,probs = 0.2)

#plot a dataset
plot(CO2$conc,CO2$uptake)




#CO2$Treatment = factor(CO2$Treatment,
                       #levels = c('nonchilled','chilled'),
                       #levels = c(1, 2))
CO2$Type = factor(CO2$Type,
                       levels = c('Quebec','Mississippi'),
                       levels = c(1, 2))
CO2$Type

library(caTools)

set.seed(123)

#spliting the dataset
split = sample.split(CO2$uptake, SplitRatio = 2/3)
training_set = subset(CO2, split == TRUE)
test_set = subset(CO2, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Multiple Linear Regression to the Training set

regressor = lm(formula = uptake ~ .,
               data = training_set)
regressor

# Predicting the Test set results

y_pred = predict(regressor, newdata = test_set)
y_pred

summary(regressor)


# Visualising the Training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$conc,
                 y = training_set$uptake),
             colour = 'red') +
  geom_line(aes(x =training_set$conc ,
                y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('conc vs uptake (Training set)') +
  xlab('conc') +
  ylab('uptake')

# Visualising the Test set results

ggplot() +
  geom_point(aes(x = test_set$conc, y = test_set$uptake),
             colour = 'red') +
  geom_line(aes(x = training_set$conc,
                y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('conc vs uptake (Test set)') +
  xlab('conc') +
  ylab('uptake')






