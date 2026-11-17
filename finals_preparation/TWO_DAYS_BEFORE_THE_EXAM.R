# Preparation 2
eps <- 1
while (1 + eps != 1) eps <- eps/2

.Machine$double.eps


# Exercise 38 -------------------------------------------------------------

# pseudocode
#
# row <- c()
# for i in 0:k
# element <- choose(k,i)
# row <- c(row, element)

row <- c()
k <- 4
for (i in 0:k) {
  element <- choose(k, i)
  row <- c(row, element)
}
row



# Exercise 41 -------------------------------------------------------------

# How many different ways can we make change of a USD 1 dollar bill, given half-dollars,
# quarters, dimes, nickels and pennies? (I.e., coins in denominations of 50, 25, 10, 5 and 1
# cents.) Implement a function which computes the number of ways to change a given amount
# a using k kinds of coins with denominations d1,...,dk using the observation that this is
# number is the sum of the number of ways to change a without using the first kind of coin
# and the number of ways to change a−d using all kinds of coins. (Convince yourself to count
# one way if the amount is 0, but zero ways if the amount is negative or there are no kinds of
# coins left.)

change <- function(amount, coins) {
  if(amount == 0) {
    return(1)
  } else if(amount < 0 || length(coins) == 0) {
    return(0)
  }
  Recall(amount, coins[-1]) + Recall(amount - coins[1], coins)
}

# EUR
change(100, c(200, 100, 50, 20, 10, 5, 2, 1))



# Exercise 42 -------------------------------------------------------------

# The remainder of the (integer) division of x by m is also called (the remainder of) x modulo
# m. Verify that the remainder of x times y modulo m can be obtained as the remainder of the
# product of the remainders of x and y modulo m modulo m. Use this to implement a recursive
# function expmod which computes the remainder of a^x modulo m without exponentation.
# (Distinguish the cases where x is zero, even or odd.)

division <- function(a,x,m) {
  if(x == 0) {
    return(1 %% m)
  } else if(x %% 2 == 0) {
    return((Recall(a, x/2, m) * Recall(a, x/2, m)) %% m)
  } else {
    return(((a %% m) * Recall(a, x-1, m)) %% m)
  }
}

division(2,4,3)

2^4 %% 3
((2^2 %% 3) * (2^2 %% 3)) %% 3


# Exercise 25 -------------------------------------------------------------

# The function logstar gives the smallest n such that the n-th iterate of the natural logarithm
# results in a value less than one. For example, logstar(123) = 3, because the second iterate
# log(log(123)) = 1.571151 ≥ 1 and the third iterate log(log(log(123))) = 0.4518085 < 1.
# Implement logstar

logstar <- function(x) {
  n <- 0
  while(x >= 1) {
    x <- log(x)
    n <- n + 1
  }
  n
}

logstar(123)
logstar(exp(123))
logstar(log(123))


# Exercise 29 -------------------------------------------------------------

# Write a function which returns (a logical indicating) whether two given natural numbers are
# relatively prime (i.e., have GCD 1).

rel_prime <- function(x, y) {
  gcd <- 1
  for(i in 2:min(x,y)){
    i_divisor <- ifelse((x%%i == 0) && (y%%i == 0), TRUE, FALSE)
    if(i_divisor) return(FALSE)
  }
  return(TRUE)
}

rel_prime(8,15)
rel_prime(8,16)
rel_prime(14,25)
rel_prime(14,21)


# Exercise 30 -------------------------------------------------------------

# Write a function is_prime returning (a logical indicating) whether a given natural number
# is a prime number or not.

is_prime <- function(x) {
  if(x == 1) return(FALSE)
  if(x == 2) return(TRUE)
  
  for(i in 2:(x-1)) {
    i_divisor <- ifelse(x%%i == 0, TRUE, FALSE)
    if(i_divisor) return(FALSE)
  }
  return(TRUE)
}

is_prime(11)
is_prime(12)
is_prime(13)


# Exercise 46 -------------------------------------------------------------

# Rational numbers are fractions of the form p/q with integers p and q > 0 satisfying gcd(p,q) =
#   1. We can represent such numbers as sequences c(p, q) of length two. Write a function
# make_rat which for two given numbers p and q returns the corresponding rational number as
# a sequence of length 2 (e.g., make_rat(19,-95) ⇒ c(-1, 5).) Also write functions numer
# and denom for extracting the numerator and denominator of such a rational number.

make_rat <- function(p,q) {
  gcd <- 1
  for(i in 2:abs(q)) {
    # print(c(p,q,i))
    if((p %% i == 0) && (q %% i == 0)) {
      gcd <- i
    }
  }
  p <- p/gcd
  q <- q/gcd
  if(p > 0 && q < 0) return(c(-p, -q))
  else if(p < 0 && q < 0) return(c(-p, -q))
  else return(c(p,q))
}

numer <- function(rat) {
  return(rat[1])
}
denom <- function(rat) {
  return(rat[2])
}


make_rat(19, -95)
numer(make_rat(19, -95))
denom(make_rat(19, -95))

make_rat(2, 4)
numer(make_rat(2, 4))
denom(make_rat(2, 4))
