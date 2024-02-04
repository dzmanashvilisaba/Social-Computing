# plot sampling distribution of mean -----------------------------------------------------------
set.seed(1)

population <- rnorm(10000, 3, 3)

population_mean <- mean(population)

my_sample <- sample(population, 100, replace = FALSE)

standard_error <- sqrt(var(my_sample)/length(my_sample))

sampling_distribution_of_mean <- rnorm(10000, mean = population_mean, sd = standard_error)

library(ggplot2)
ggplot(data.frame(x = sampling_distribution_of_mean), aes(x)) + geom_histogram() + geom_vline(xintercept = population_mean, color = "red")


# calculate 20 lots of 95% confidence intervals -----------------------------------------------------------

my_confidence_intervals <- function(){
  
  my_sample <- sample(population, 100, replace = FALSE)
  
  sample_mean <- mean(my_sample)
  
  standard_error <- sqrt(var(my_sample)/length(my_sample))
  
  margin_of_error <- 1.96*standard_error
  
  mean_minus_margin_of_error <- sample_mean - margin_of_error
  mean_plus_margin_of_error <- sample_mean + margin_of_error
  
  c(mean_minus_margin_of_error, mean_plus_margin_of_error)
  
}

#install.packages('plyr')
library(plyr)

xx<-llply(1:20, function(x) my_confidence_intervals())

xx<-data.frame(y=1:20*50, x=do.call(rbind, xx))

#You would want to build a data.frame containing the intervals and then add a layer of horizontal error bars to plot them. First, i transform your ranges into a data.frame

xx<-llply(1:20, function(x) my_confidence_intervals())
xx<-data.frame(y=1:20*50, x=do.call(rbind, xx))
#Now I add them to the plot

ggplot(data.frame(x = sampling_distribution_of_mean), aes(x)) + 
  geom_histogram() + 
  geom_vline(xintercept = population_mean, color = "red") + 
  geom_errorbarh(aes(y=y, x=x.1, xmin=x.1, xmax=x.2), data=xx, col="#0094EA", size=1.2)

