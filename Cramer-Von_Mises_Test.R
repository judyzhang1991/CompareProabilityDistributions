# Cramer-Von Mises Test
# Source: https://bookdown.org/egarpor/NP-UC3M/nptests-comp.html
# Date: Dec. 17th, 2021


# Two-sample Cramer-Von Mises Statistic

cvm2_stat <- function(x, y){
  
  # Sample Sizes
  n <- length(x)
  m <- length(y)
  
  # Pooled Sample
  
  z <- c(x, y)
  
  # Statistic Computation via ecdf()
  
  (n * m / (n + m)^2) * sum((ecdf(x)(z) - ecdf(y)(z))^2)
  
}




set.seed(123456)


# H0 = True Case

# Generate two random samples from the same distribution Gamma(1,1) with different sample sizes. 
x0 <- rgamma(n = 50, shape = 1, scale = 1)
y0 <- rgamma(n = 100, shape = 1, scale = 1)

cvm0 <- cvm2_stat(x = x0, y = y0)

# Asymptotic p-values can be obtained using `f`goftest::pCvM.
# Permutations can be used for obtaining non-asymptotic p-values and dealing with discrete samples. 
pval0 <- 1-goftest::pCvM(q = cvm0)
c("statistic" = cvm0, "p-value"= pval0)

## Statistic: 0.1294889
## p-value: 0.4585971 -> fail to reject H0: F = G at alpha = 0.05 level. 




# H0 = False Case

# Generate two random samples from two different distributions Gamma(2,1) and Gamma(1,1) respectively, with different sample sizes. 
x1 <- rgamma(n = 50, shape = 2, scale = 1)
y1 <- rgamma(n = 75, shape = 1, scale = 1)

cvm1 <- cvm2_stat(x = x1, y = y1)
pval1 <- 1-goftest::pCvM(q = cvm1)
c("statistic" = cvm1, "p-value"= pval1)


## Statistic: 1.328373
## p-value: 0.0004264598 -> reject H0: F = G at alpha = 0.05 level in favor of Ha: F != G. 


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
