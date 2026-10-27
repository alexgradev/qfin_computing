# Home Assignment 2 ----------------------------------------------------

## Task 25 --------------------------------------------------------------

# The function logstar() gives the smallest n such that the n-th iterate of
# the natural logarithm results in a value less than one. For example,
# logstar(123) = 3 because the second iterate log(log(123)) = 1.57 > 1 and
# the third iterate log(log(log(123))) = 0.45 < 1

logstar <- function(n) {
  if(!is.numeric(n) || length(n) != 1 || !is.finite(n) || n <= 0) {
    warning("Input must be a single positive real number")
    return(NaN)
  }
  current    <- n
  iterations <- 0
  
  while(current >= 1) {
    current <- log(current)
    iterations <- iterations + 1
  }
  return(iterations)
}

logstar(123)      # given example, should return 3
logstar(exp(123)) # should return 4
logstar(log(123)) # should return 2
logstar(-123)     # edge case
logstar("WU")     # edge case


## Task 26 --------------------------------------------------------------

# The greates common divisor gcd(a, b) can be obtained using Euclid's algorithm
# which is based on the following two observations:
# a) if b is a divisor of a, then gcd(a,b) = b
# b) if r is the remainder in the integer division of a by b, then
#    gcd(a,b) = gcd(b, r)

gcd <- function(a, b) {
  
  # Robustness check based on the definition here:
  # https://en.wikipedia.org/wiki/Greatest_common_divisor
  if(length(a) != 1  || length(b) != 1  ||
     !is.numeric(a)  || !is.numeric(b)  ||
     a %% 1 != 0     || b %% 1 != 0     ||
     (a == 0 && b == 0)) {
    warning("Inputs a and b should be single integers, which are not all zero")
    return(NaN)
  }
  
  # Edge cases
  if (b == 0) return(abs(a))
  if (a == 0) return(abs(b))
  
  remainder <- a %% b
  # result <- a %/% b
  # cat(a,"/",b,"=",result,"+",remainder,"\n")
  
  if(remainder != 0) {
    # Recursively call the function until the remainder is 0
    return(gcd(b, remainder))
    
  } else {
    # We have gcd(8,4) == gcd(-8,4) == gcd(9, -4) == 4
    return(abs(b))
  }
}

gcd(12, 4)    # should return 4
gcd(64, 4)    # should return 4
gcd(4, 64)    # should return 4
gcd(997, 101) # should return 1, since both are primes
gcd(-5, 0)    # should return 5, since 0 is divided by every number


## Task 27 -------------------------------------------------------------

# Write a function which, for a given natural number n, returns logical vector
# containing for each i <= n in reverse order, a sequence of length i indicating
# whether i is even or not
#
# Example: for n = 3 the function should return
# c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE)

even_or_not <- function(n) {
  if(!is.numeric(n) || length(n) != 1 || !is.finite(n) || n %% 1 != 0 || n <= 0) {
    warning("Input n should be a single natural number")
    return(NA)
  }
  result <- c()
  
  for (i in n:1) {
    i_vec <- rep(i %% 2 == 0, times = i)
    result <- c(result, i_vec)
  }
  return(result)
}

even_or_not(3)  # should return the example output
even_or_not(4)  # should return 4 times TRUE and then ever_or_not(3)
even_or_not(-1) # NaN


## Task 28 --------------------------------------------------------------

odd_seq <- function(seq) {
  if(any(!is.numeric(seq)) || any(seq %% 1 != 0) || any(seq <= 0)) {
    warning("The input sequence should consist of natural numbers")
    return(NA)
  }
  odds_vector <- seq %% 2 != 0
  return(sum(odds_vector))
}

odd_seq(1:10)
odd_seq(2)
odd_seq(-5:5)


## Task 29 --------------------------------------------------------------

relatively_prime <- function(a, b) {

  if(length(a) != 1   || length(b) != 1  ||
     !is.numeric(a)   || !is.numeric(b)  ||
     a %% 1 != 0      || b %% 1 != 0     ||
     a <= 0 || b <= 0)  {
    warning("Inputs a and b should be single natural numbers")
    return(NA)  # NA is the logical for idk
  }
  
  # if one of the inputs is 1, gcd == 1
  if (abs(b) == 1 || abs(a) == 1) {return(TRUE)}
  
  remainder <- a %% b

  if(remainder != 0) {
    # Recursively call the function until the remainder one of the inputs is 1
    return(relatively_prime(b, remainder))
  } else {
    # if there is a iteration of the function without remainder == 0 AND 
    # (a != 0 OR b != 0), then a gcd != 1 exists 
    return(FALSE)
  }
}

relatively_prime(64, 4)    # should return F
relatively_prime(4, 64)    # should return F
relatively_prime(997, 101) # should return T, since both are primes
relatively_prime(-5, 0)    # should produce warning
relatively_prime(4, 12)    # should return F


## Task 30 --------------------------------------------------------------

is_prime <- function(n) {
  
  if(length(n) != 1 || !is.numeric(n) || n %% 1 != 0 || n <= 0 || 
     !is.finite(n)) {   # Prevents NA_real_ and Inf
    warning("Input a should be single natural number")
    return(NA)          # NA is the logical for idk
  }
  # 1 is NOT a prime
  if(n == 1) {return(FALSE)}
  
  # 2 and 3 are the first prime number
  if(n == 2 || n == 3) {return(TRUE)}
  
  # No even number is prime (other than 2)
  if(n %% 2 == 0) {return(FALSE)}
  
  # for(i in seq(3, n-1, by = 2)) {
  #   if(n %% i == 0) {return(FALSE)}
  # }
  
  # Only odd divisors up to sqrt(n) need to be checked
  i <- 3
  while(i * i <= n) {
    if(n %% i == 0) {return(FALSE)}
    i <- i + 2
  }
  
  return(TRUE)
}

is_prime(NaN)
is_prime(1)
is_prime(2)
is_prime(11)
is_prime(25)
is_prime(133)


## Task 32 -----------------------------------------------------------------

# A perfect number is a positive integer which equals the sum of its proper
# divisors. The smallest perfect number is 6 = 1 + 2 + 3. Write a function 
# which tests whether a given positive integer is a perfect number, and determine 
# the first 4 perfect numbers via simple iteration.

# A proper divisor is any positive factor of a number, excluding the number itself

is_perfect <- function(n) {
  
  if(length(n) != 1 || !is.numeric(n) || 
     !is.finite(n) || n <= 0 || n %% 1 != 0) {
    warning("Input n should be a positive integer")
    return(NA)
  }
  
  # 6 is the first perfect number
  if(n < 6) {return(FALSE)}

  # Initialize empty collector
  divisors <- c()
  
  # Iterate over all numbers below n to find the proper divisors
  for(i in 1:(n-1)) {
    remainder <- n %% i
    if (remainder == 0) {
      divisors <- c(divisors, i)
    }
  }
  # Evaluate perfectness
  if(sum(divisors) == n) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

{
n <- 1
perfect_numbers <- c()
while(length(perfect_numbers) < 4) {
  if(is_perfect(n)) {perfect_numbers <- c(perfect_numbers, n)}
  n <- n + 1
}
cat("The first 4 perfect numbers:\n", perfect_numbers)
}


## Task 33 -----------------------------------------------------------------


is_highly_composite <- function(n) {
  
  # Automatically assign T for 1
  if(n == 1) return(TRUE) 
  
  divisors_of_n <- c()
  # find the divisors of n
  for(i in 1:n) {
    if(n %% i == 0) 
      divisors_of_n <- c(divisors_of_n, i)
  }

  # iterate over the integers smaller than n
  for(j in 1:(n-1)) {
    divisors_of_j <- c()
    # for a number j smaller than n count its integer
    for(k in 1:j) {
      if(j %% k == 0) {
        divisors_of_j <- c(divisors_of_j, k)
      }}

    # if such a number with same number or more divisors is found
    # break the loop and return F directly
    if(length(divisors_of_j) >= length(divisors_of_n)) {
        return(FALSE)
    }
  }
  # if the loop finished, no number with same number or more divisors was 
  # found => n is highly composite
  return(TRUE)
}

is_highly_composite(6)

# Find the first 20 highly composite numbers
n <- 1
highly_composite_numbers <- c()
while(length(highly_composite_numbers) < 20) {
  if(is_highly_composite(n)) {
    highly_composite_numbers <- c(highly_composite_numbers, n)
  }
  n <- n + 1
  print(n)
}

## Task 34 -----------------------------------------------------------------

collatz <- function(n) {
  
  if (n < 1) {
    warning("n must be >= 1")
    return(NA)
  }
  
  steps  <- 0
  
  while(n != 1) {
    if(n %% 2 == 0) { # n is even
      n <- n / 2
    } else {
      n <- 3*n + 1
    }
    steps <- steps + 1
  }
  
  return(steps)
}

# the function should output the sequence, not the steps

collatz_test    <- 1:1e5
max_steps       <- 0
which_max_steps <- 0

for(i in collatz_test) {
  steps <- collatz(i)
  
  if(steps > max_steps) {
    max_steps       <- steps
    which_max_steps <- i
  }
}


## Task 35 -----------------------------------------------------------------

binary <- function(n) {
  
  if (n == 0) return(0)
  
  bits <- c()
  
  while (n > 0) {
  # build it backwards (from last bit to the leading bit)  
    bits <- c(n %% 2, bits)
    n    <- n %/% 2
  }
  
  return(bits)
}
binary(9)

decimal <- function(bits) {
  
  highest_power   <- length(bits) - 1
  sequence_powers <- seq(highest_power, 0)
  result          <- sum(bits * 2^sequence_powers) 
  
  return(result)
}
decimal(binary(9))

## Task 36 ----------------------------------------------------------------

lcm <- function(n, m) {
  
  lcm_candidate <- max(n, m)
  is_lcm <- FALSE
  
  while(!is_lcm) {
    
    if(lcm_candidate %% n == 0 && lcm_candidate %% m == 0) {
      lcm        <- lcm_candidate
      is_lcm     <- TRUE
      
    } else {
      lcm_candidate <- lcm_candidate + 1
    }
  }
  return(lcm)
}

lcm(4,3)


## Task 38 ----------------------------------------------------------------

pascal <- function(k) {
  if(!is.numeric(k) || length(k) != 1 || k %% 1 != 0 || k < 0) {
    warning("Input k must be a non-negative integer")
    return(NaN)
  }
  row <- c()
  for(i in 0:k) {
    row <- c(row, choose(k, i))
  }
  return(row)
}

pascal(4)
pascal(2)
pascal(-5)


## Task 39 ----------------------------------------------------------------

# For loop
binomial_model_loop <- function(S_0, U, D, N) {
  
  result <- list(S_0)
  
  for (n in 1:N) {
    S_n <- c()
    for (i in 0:n) {
      S_n_i <- S_0 * U^i * D^(n-i)
      S_n <- c(S_n, S_n_i)
    }
    result[[n + 1]] <- S_n
  }
  return(result)
}


# Apply family
binomial_model_apply <- function(S_0, U, D, N) {
  N <- N + 1 # correct for starting from i=0
  
  # at t = n we have vector multiplication
  # (S_0, ..., S_0)' * (U^0, U^1, ..., U^n) * (D^n, D^(n-1), ... D^0)'
  
  result <- lapply(1:N,
                   function(x) {
                     rep(S_0, times = x) * U ^ seq(0, x-1) * D ^ seq(x-1, 0)
                   })
  
  return(result)
}

binomial_model_loop(100, 1.1, 0.9, 4)
binomial_model_apply(100, 1.1, 0.9, 4)

# the difference between using loops and using functions from the apply family
# is the fact that with apply family we can utilize the vectorization in R by
# computing all prices in a given time step at once instead of iterating over
# every scenario; also no need to track indexes which also slows computations


## Task 40 ----------------------------------------------------------------

double_sum_loop <- function(n) {
  sum_var <- 0
  
  for(r in 1:n) {
    for(c in 1:r) {
      sum_var <- sum_var + c^2 / (5 + 10*r^3)
    }
  }
  return(sum_var)
}

double_sum_funcs <- function(n) {
  M <- matrix(0, nrow = n, ncol = n)
  r <- row(M)
  c <- col(M)
  sum((c^2 / (5 + 10*r^3))[c <= r])
}

## Task 41 ----------------------------------------------------------------

change <- function(n, coins_vec) {
  
  if(n == 0) return(1)
  if(n < 0 || length(coins_vec) == 0)  return(0)
  
  ways <- change(n - coins_vec[1], coins_vec) + change(n, coins_vec[-1])
  
  return(ways)
}

change(100, c(1, 2, 5, 10, 20, 50, 100))


## Task 42 ----------------------------------------------------------------

(5*7) %% 3 

(5 %% 3) * (7 %% 3)

(0.2 %% 3) * (0.2 %% 3)

# a^x = a * a * ... * a (x times)

# expmod <- function(a, x, m) {
#   
#   if(x == 0) return(1 %% m)
#   
#   if(x > 0) {
#     
#   }
#   
#   if(x < 0) {
#     
#   }
# }

