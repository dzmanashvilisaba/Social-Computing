#### CSS LAB 4 ####
#### Sampling distribution and inferential statistics ####

# In this lab, we will practice once more to understand how sampling distribution works step-by-step. 
# Let's say our population size is 10k and we measure the height of KAIST students. 
# The proportion of man is very high in KAIST, which will make the height distribution bimodal. 

# Let's create a hypothetical KAIST students height distribution. 
KAIST_pop <- c(rnorm(8000, 175, 3), rnorm(2000, 160, 3)) 

# Question 1: Check its distribution using a histogram and describe its shape. 
### Answer ###
hist(KAIST_pop, 40)

# Shape of KAIST_pop looks like two normal distributions merged together. It has bimodal
# shape.

# Question 2: Check the population mean and standard deviation. 
### Answer ###
population_mean = mean(KAIST_pop)  # Mean of KAIST population
population_sd = sd(KAIST_pop)  # Standard deviation of population

print(population_mean)
print(population_sd)

# Now, sample 100 students randomly from the population.
n = 100
sample <- sample(KAIST_pop, n, replace = FALSE)

# Question 3: Again, check its distribution using a histogram, and compute mean and sd, then compare them to your population distribution.
# If you're using RMarkdown and the composition of sample keeps changing whenever you run the code due to its randomness, you can set the random seed by using "set.seed(ANY NUMBERS)"
### Answer ###
hist(sample,20)

mean(sample)  # Mean of sample population
sd(sample)  # Standard deviation of sample population


# Now, let's repeat the same procedure for 1,000 times, and save their average.
sampling_distribution_n100 <- NULL # Create a vacant vector where we will save sampled means.

for (i in 1:1000) {
  sample_mean <- mean(sample(KAIST_pop, n, replace = FALSE))
  sampling_distribution_n100 <- c(sampling_distribution_n100, sample_mean)
}

# Question 4: Check the distribution of sampling distribution by using a histogram, and compute a mean and sd. 
# (continued) Describe the shape of distribution and compare it to the distribution from population. Do they look similar or not, and why?
# (continued) Describe the mean and standard deviation. Is it similar to population mean and standard deviation? 
### Answer ###
hist(sampling_distribution_n100, 40, prob=T)
lines(density(sampling_distribution_n100,na.rm=T),col='black')    # Black line is the approximating curve for the sampling distribution
curve(dnorm(x,mean=mean(sampling_distribution_n100,na.rm=T), sd=sd(sampling_distribution_n100,na.rm=T)), add=T, col='red')  # Red line is the normal distribution curve with the same mean and standard deviation as the  sampling distribution

ggplot(data.frame(x = sampling_distribution_n100), aes(x)) +
  geom_histogram(bins=50,aes(y=..density..), color="darkblue", fill="lightblue") + 
  geom_density(alpha=.3, fill="darkblue",col='darkblue',size=0.7) + 
  stat_function(fun = dnorm, args = list(mean = sampling_mean, 
                                         sd = sampling_sd), colour ='red', size =1.2) + 
  labs(x = "Averages of Samples", y = "Frequency", title="Sampling Distribution") 

sampling_mean = mean(sampling_distribution_n100)  # Mean of sampling distribution
sampling_sd = sd(sampling_distribution_n100)  # Standard Deviation of Sampling Distribution

print(sampling_mean)
print(sampling_sd)

# The sampling distribution looks similar to the normal distribution and it differs lot from original KAIST population distribution.
# That's because KAIST population distribution was bimodal, and sampling distribution is unimodal. 
# They differ, because from CLT (Central Limit Theorem) sampling distribution will be approximated to the normal distribution as long as the sample size is large.

# Mean of sampling distribution and the KAIST population are very close to each other. The difference is:
print(population_mean) # Mean of KAIST population
print(sampling_mean)   # Mean of sampling distribution

# Standard deviation of each distribution differ and they should differ by factor of sqrt(n), where n is the sample size:
sd(KAIST_pop) / sqrt(n)  # Standard Deviation of KAIST population
sd(sampling_distribution_n100)   # Standard Deviation of Sampling Distribution

#### Confidence interval ####
# Let's again sample with size 100. 
sample_n100 <- sample(KAIST_pop, n, replace = FALSE)

# Question 5: Compute the mean and standard 'error'. Then, compute the 95% confidence interval. Explain the meaning of the confidence interval in English sentences.
### Answer ###
sample_mean = mean(sample_n100)

standard_err = sd(KAIST_pop) / sqrt(n)
margin_of_error = 1.96*standard_err

confidence_interfal_left_bound = sample_mean - margin_of_error
confidence_interval_right_bound = sample_mean + margin_of_error
data_sample = data.frame(y=0.1, x=c(confidence_interfal_left_bound, confidence_interval_right_bound))

library(ggplot2)
ggplot(data.frame(x = sampling_distribution_n100), aes(x)) +
  geom_histogram(bins=50,aes(y=..density..), color="darkblue", fill="lightblue") + 
  geom_density(alpha=.3, fill="darkblue",col="darkblue", size=0.7) + 
  geom_vline(xintercept = sample_mean, color = "red", linetype = "longdash", size=1) + 
  geom_errorbarh(aes(y=y, xmin=x[1], xmax=x[2],height = .05), data_sample, col='red', size=1.2) + 
  geom_vline(xintercept = sampling_mean, color = "blue",  size=1.2) +
  annotate(x=sampling_mean,y=+Inf,label="\"True\" Mean",vjust=2,geom="label", col='blue') + 
  labs(x = "Averages of Samples", y = "Frequency", title="95% Confidence Interval") 
  
print(sample_mean)  # Mean of sample
print(standard_err)  # Standard Error

# The 95% confidence interval of sample is the  the interval for which everytime we are going to draw a sample from the distribution and calculate its mean, 95% of the time 
# 95% of the time the calculate mean will be in that exact interval. That's why we are calling it confidence interval and give the estamitation as well.

# Question 6: Does your confidence interval cover the 'true' mean? What does that mean? 
### Answer ###
# It covers the "true" mean. As explained above, "true" mean is the original unknown mean of the distribution from which the sample was drawn.
# We can't find out was is EXACTLY that mean, but from central limit theorem we can always get the value close to original one as close as possible.
# And this approximation level depends on how many times we want do traw the samples. The more samples, the better we know the "true" value.
 

# Postech students randomly sample 30 students and find out that their average is 172.8. 
set.seed(1001)
sample_postech <- rnorm(30, 173, 3)

# Question 7: Does this sample support the argument that Postech students are statistically significantly taller than KAIST students?
# Use a t-test and examine this hypothesis and interpret the results. 


