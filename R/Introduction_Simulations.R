#######################################################################
# Introduction to Simulations with R
#######################################################################

#### SAMPLES ####

# the sample function creates a random sample of the defined objects. 
## Coin tosses are simulated below (size refers to how many times we repeat this experiment)
### Note: if we dont specify a probability parameter, the function assumes an uniform distribution

sample(c("Heads","Tails"),size = 1)


# The "replace" argument indicates weather we repeat the experiment with the starting specified occurences
sample(c("Heads","Tails"),size = 10,replace=TRUE)



sample(c("Heads","Tails"),size = 10,replace=TRUE)



sample(c("Heads","Tails"),size = 10,replace=TRUE)



# it is possible to save the results in a variable and then express them using a table, with the table() function
s1000 = sample(c("Heads","Tails"),size = 1000,replace=TRUE)
table(s1000)



#### SETTING SEED ####

# You can imagine a seed as a number that refers to the observations of a certain experiment.
## That is, whenever you want to repeat that experiment, set the seed with its respective number.
### Below the seed number refering to the sample is saved in a variable.

seed = 123434534
set.seed(seed)
sample(c("Heads","Tails"),10,replace=TRUE)



set.seed(seed)
sample(c("Heads","Tails"),10,replace=TRUE)



set.seed(seed)
sample(c("Heads","Tails"),10,replace=TRUE)



# Here a dice roll is simulated 6000000 times. Notice that if we always set the seed,
## the same experiment is repeated
set.seed(seed)
dice = sample(seq(1:6),6000000,replace=TRUE)
table(dice)



#### SIMULATION CORONA INFECTION SWITZERLAND ####

# The infection in the Swiss population is modelled with a binomial distribution (https://en.wikipedia.org/wiki/Binomial_distribution#:~:text=In%20probability%20theory%20and%20statistics,%2Fone%20(with%20probability%20p).
## The "n" refers to the Swiss population, "size" to the number of times the experiment shall be repeated by person and
### "prob" the believed parameter that represents the infection rate (5%), or the probability of one being infected.

set.seed(seed)
contact = rbinom(n = 8544527, size = 1, prob = 0.05)
table(contact)
# calculating mean of the dummy variable (https://en.wikipedia.org/wiki/Dummy_variable_(statistics))
mean(contact)

# Simulation of a sample of 100 people
set.seed(seed)
sample100 = sample(contact,size = 100,replace=FALSE)
# Running a t-test with the t.test function for this sample (https://en.wikipedia.org/wiki/Student%27s_t-test)
t.test(sample100)


# Simulation of a sample of 1000 people
sample1000 = sample(contact,1000,replace=FALSE)
t.test(sample1000)


# Simulation of a sample of 10000 people
sample10000 = sample(contact,10000,replace=FALSE)
t.test(sample10000)



#### FOR LOOPS ####

# A quick introduction to for loops in R. For Loops are used to repeat some code line until the number of iterations are
## reached

# Instead of having to write (or copy paste) the text and calculations over and over again ...
print(paste("The Square of 1 is ",1^2))
print(paste("The Square of 2 is ",2^2))
print(paste("The Square of 3 is ",3^2))
# ... one can simply run a for loop with the number of wished iterations (as the case below, 100):
for (i in 1:100) {
  print(paste("The Square of", i," is ",i^2))
}


# In our Corona Virus example:

# Creating an empty "space" to save results of the for loop
z = 5
results = rep(NA,z)
results


# running for loop to simulate a sample of 100 people out of the population 5 times (variable z) and then take the mean of the infected people
set.seed(seed)
n = 100
for (i in 1:z) {
  temp = sample(contact,n,replace=FALSE)
  results[i] = mean(temp)
  # visualizing the 5 experiments means (0.08 0.08 0.04 0.06 0.07)
  print(results)
}



# repeating the process 1000 times and taking the mean
set.seed(seed)
z = 1000
results = rep(NA,z)

n = 100
for (i in 1:z) {
  temp = sample(contact,n,replace=FALSE)
  results[i] = mean(temp)
}
# to visualize such large results, a histogram is more appropriate than printing the output.
## Therefore, use the function hist() to plot the histogram with the mean values
hist(results)
