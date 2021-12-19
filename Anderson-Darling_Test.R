# Anderson-Darling Test
# Source: https://bookdown.org/egarpor/NP-UC3M/nptests-comp.html
# Date: Dec. 19th, 2021


# Two-Sample Anderson-Darling Statistic

ad2_stat <- function(x, y){
  
  # Sample Sizes
  n <- length(x)
  m <- length(y)
  
  # Pooled Sample and Pooled ecdf
  z <- c(x, y)
  z <- z[-which.max(z)] # Exclude the largest point
  
  H <- rank(z) / (n + m)

  
  # Statistic computation via ecdf()
  (n * m / (n + m)^2) * sum((ecdf(x)(z) - ecdf(y)(z))^2 / ((1 - H) * H))
  
}


# Check the test for H0 true

set.seed(123456)

x0 <- rgamma(n = 50, shape = 1, scale = 1)
y0 <- rgamma(n = 100, shape = 1, scale = 1)
ad0 <- ad2_stat(x = x0, y = y0)

# Asymptotic p-values can be obtained using `goftest::pAD`. 
pval0 <- 1 - goftest::pAD(q = ad0)
c("statistic" = ad0, "p-value" = pval0)

# Statistic: 0.8603617
# p-value: 0.4394751