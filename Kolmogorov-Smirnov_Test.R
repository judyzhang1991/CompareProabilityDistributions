# Kolmogorov-Smirnov Test
# Source: https://bookdown.org/egarpor/NP-UC3M/nptests-comp.html
# Date: Dec. 16th, 2021

set.seed(123456)


# H0 = True Case

# Generate two random samples from the same distribution Gamma(1,1) with different sample sizes. 
x0 <- rgamma(n = 50, shape = 1, scale = 1)
y0 <- rgamma(n = 100, shape = 1, scale = 1)

ks.test(x = x0, y = y0) 

## Results:
## Two-Sample Kolmogorov-Smirnov Test
##
## data: x0 and y0
## D = 0.14, p-value = 0.5185 -> fail to reject H0: the two distributions are the same. 
# alternative hypothesis: two-sided


ks.test(x = x0, y = y0, alternative = "less") # Ha: F <= G
## Results:
## Two-Sample Kolmogorov-Smirnov Test
##
## data: x0 and y0
## D^- = 0.08, p-value = 0.6527 -> fail to reject H0: the two distributions are the same. 
# alternative hypothesis: the CDF of x lies below that of y (F <= G)


ks.test(x = x0, y = y0, alternative = "greater") # Ha: F >= G
## Results:
## Two-Sample Kolmogorov-Smirnov Test
##
## data: x0 and y0
## D^+ = 0.14, p-value = 0.2707 -> fail to reject H0: the two distributions are the same. 
# alternative hypothesis: the CDF of x lies above that of y (F >= G)



# H0 = False Case

# Generate two random samples from two different distributions Normal(1,1) and Normal(0,1) respectively, with different sample sizes. 
x1 <- rnorm(n = 50, mean = 1, sd = 1)
y1 <- rnorm(n = 75, mean = 0, sd = 1)

ks.test(x = x1, y = y1) 

## Results:
## Two-Sample Kolmogorov-Smirnov Test
##
## data: x1 and y1
## D = 0.46667, p-value = 2.234e-06 -> reject H0: the two distributions are the same. 
# alternative hypothesis: two-sided



ks.test(x = x1, y = y1, alternative = "less") # Ha: F <= G
## Results:
## Two-Sample Kolmogorov-Smirnov Test
##
## data: x1 and y1
## D^- = 0.52667, p-value = 5.918e-08 -> reject H0: the two distributions not the same, the CDF of x lies below that of y. 
# alternative hypothesis: the CDF of x lies below that of y (F <= G)

## Interpretation:
# Strong rejection of H0: F = G in favor of H1: F <= G when
# alternative = "less", as in reality x1 is stochastically greater than y1.
# This outcome allows stating that "there is strong evidence supporting that
# x1 is stochastically greater than y1"


ks.test(x = x1, y = y1, alternative = "greater") # Ha: F >= G
## Results:
## Two-Sample Kolmogorov-Smirnov Test
##
## data: x1 and y1
## D^+ = -4.3368e-17, p-value = 1 -> fail to reject H0: the two distributions are the same. 
# alternative hypothesis: the CDF of x lies above that of y (F >= G)


## Interpretation:
# No rejection ("strong acceptance") of H0: F = G versus H1: F >= G when
# alternative = "greater". Even if in reality x1 is stochastically greater than
# y1 (so H0 is false), the alternative to which H1 is confronted is even less
# plausible. A p-value ~ 1 indicates one is probably conducting the test in
# the uninteresting direction alternative!
