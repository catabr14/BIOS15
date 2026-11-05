#Exercise 1

#Use non-parametric bootstrapping to derive a 95% confidence interval for the CV of a variable.
#Start by writing a function that computes the CV for a variable. Then, simulate a random variable 
#and write a loop that samples many times from this variable and computes the CV


#CV function
calculate_CV <- function(x) {sd(x) / mean(x)}

#Random variable generation
set.seed(1)
x <- rnorm(n = 50, mean = 10, sd = 2)

calculate_CV(x)

#Boostrap
out <- NULL

for (i in 1:1000) {
  sample1 <- sample(x, replace = TRUE)
  out[i] <- calculate_CV(sample1)
}

#95% confidence interval
quantile(out, c(0.025, 0.975))

#Plot
hist(out,
     main = "Bootstrap distribution of CV",
     xlab = "Coefficient of variation",
     col = "lightblue")
