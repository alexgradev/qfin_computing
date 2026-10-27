# Home Assignment 1 -------------------------------------------------------
# Group 1

## Task 1 ------------------------------------------------------------------
#
# using the modulo operator %% returns the remainder
170166719 %% 31079


## Task 2 ------------------------------------------------------------------

n_list <- c(10, 20, 30, 40)

for(n in n_list) {
  
  slow  <- sum(1.08^seq(1,n))
  quick <- (1.08^(n + 1) - 1) / (1.08 - 1) - 1
  
  cat("n =", n,
      "\nEqual:", quick == slow,"\n\n")
}

quick_formula_results <- (1.08^(seq(1, 100) + 1) - 1) / (1.08 - 1) - 1

## Task 3 ------------------------------------------------------------------

n_list <- c(100, 200, 400, 800)

for(n in n_list) {
  
  slow  <- sum(seq(1,n))
  quick <- (n * (n + 1)) / 2
  
  cat("n =", n,
      "\nEqual:", quick == slow,"\n\n")
}

quick_formula_results <- (seq(1,100) * (seq(1,100) + 1)) / 2


## Task 4 ------------------------------------------------------------------

n_list <- c(200, 400, 600, 800)

for(n in n_list) {
  
  slow  <- sum(seq(1,n)^2)
  quick <- (n * (n + 1) * (2*n + 1)) / 6
  
  cat("n =", n,
      "\nEqual:", quick == slow,"\n\n")
}

quick_formula_results <- (seq(1,100) * (seq(1,100) + 1) * (2*seq(1,100) + 1)) / 6


## Task 5 ------------------------------------------------------------------

n_list <- 500 * 2^(0:4)

for(n in n_list) {
  
  s <- sum(1/seq(1,n))
  logarithm <- log(n)

  cat("n =", n,
      "\nEqual:", s == logarithm,"\n\n")
}

n_list <- 500 * 2^(0:15)
for(n in n_list) {
  
  s <- sum(1/seq(1,n))
  logarithm <- log(n)
  print(s - logarithm)
}

# the difference converges to 0.5772157 (Euler constant)

## Task 6 ------------------------------------------------------------------

rep(seq(0,4), each = 5)

rep(seq(1,5), times = 5)


## Task 7 ------------------------------------------------------------------

rep(seq(1,5), each = 5) + rep(seq(0,4), times = 5)


## Task 8 ------------------------------------------------------------------

vector_func <- function(n) {
  if(n%%1 != 0 || n < 1) stop("n should be a natural number")
  result <- rep(1:n, times = 1:n)
  return(result)
}

vector_func(4)
vector_func(pi)
vector_func(0)


## Task 9 ------------------------------------------------------------------

numbers <- c(3, 5, 8, 10, 12)
dump("numbers", file = "numbers.R")
rm("numbers")
"numbers" %in% ls()
source("numbers.R")
numbers


## Task 10 -----------------------------------------------------------------

save_letters <- function(filename) {
  # LETTERS is build in vector in R that stores the Roman alphabet
  dump("LETTERS", file = filename)
}
# save_letters("upper_case_letters.R")

## Task 11 -----------------------------------------------------------------

f <- function(x) {
  if(x <= 3) {
    result <- 3*x + 2
  } else {
    result <- 2*x - 0.5*x^2
  }
  return(result)
}

x      <- seq(0, 6, by = 0.01)
f_of_x <- sapply(x, f)
plot(x, f_of_x, type = "l")


## Task 12 -----------------------------------------------------------------

sign_func <- function(x) {
  if(!is.numeric(x)) stop("x is not a number")
  
  if(x < 0)      {result <- "Negative"}
  else if(x > 0) {result <- "Positive"}
  else           {result <- "Neither negative, nor positive"}
  
  return(result)
}
sign_func(-3)
sign_func(pi)
sign_func("A")


## Task 13 -----------------------------------------------------------------

pi_func <- function(x) {
  if(!is.numeric(x)) stop("x is not a number")
  
  if(abs(x) > pi) {result <- "Exceeds"}
  else {result <- "Does not exceed"}
  
  return(result)
}

pi_func(4)
pi_func(-4)
pi_func(1)
pi_func(pi)
pi_func("A")


## Task 14 -----------------------------------------------------------------

means <- function(num1, num2, type = c("geometric", "harmonic")) {
  if(!is.numeric(num1) || !is.numeric(num2)) {
    stop("Num1 and Num2 should be of class 'numeric'")
  }
  if(length(num1) > 1 || length(num2) > 1) {
    stop("Num1 and Num2 should be single numbers")
  }
  
  # input variable type is matched to one of the allowed options, else error
  type <- match.arg(type)
  
  if(type == "geometric" && (num1 < 0 || num2 < 0)) {
    stop("geometric mean requires non-negative numbers")
  }
  if(type == "harmonic" && (num1 <= 0 || num2 <= 0)) {
    stop("harmonic mean requires positive numbers")
  }
  
  if(type == "geometric") {result <- sqrt(num1 * num2)}
  if(type == "harmonic")  {result <- (2 / (1/num1 + 1/num2))}
  return(result)
}

means(2, 8,   type = "geometric")
means(2, 8,   type = "harmonic")
means(2, "A", type = "harmonic")
means(2, 8,   type = "arithmetic")


## Task 15 -----------------------------------------------------------------

shorter_str <- function(str1, str2) {
  if(!is.character(str1) || !is.character(str2)) {
    stop("str1 and str2 should be of class 'character'")
  }
  
  # Note: blank space is considered a character
  str1_len <- nchar(str1) 
  str2_len <- nchar(str2)
  diff     <- str1_len - str2_len
  
  if(diff > 0)      {return(str2)}
  else if(diff < 0) {return(str1)}
  else {
      warning("Strings have equal lengths")
      return(c(str1, str2))
  }
}

shorter_str("Computing", "Statistics 1")
shorter_str("WU is better", "TU is worse!")

## Task 16 -----------------------------------------------------------------

str2chars <- function(str) {
  if(!is.character(str) || length(str) != 1) {
    stop("Input must be a single character string")
  }
  str_len <- strsplit(str, NULL) |> unlist() |> length()
  for(i in seq_len(str_len)){
    print(str)
  }
}
str2chars("QFin")
str2chars("WU")
str2chars("")
str2chars(c("Quantitative", "Finance"))
str2chars(2026)


## Task 17 -----------------------------------------------------------------

countdown <- function(n) {
  if(!is.numeric(n))      stop("Input must be of type 'numeric'")
  if(n%%1 != 0 || n <= 0) stop("Input must be a natural number")
  
  for(i in n:1) {print(i)}
}
countdown(10)
countdown(-3)
countdown(pi)
countdown(":)")


## Task 18 -----------------------------------------------------------------

divisor <- function(x, y) {
  if(x%%1 != 0 || x <= 0) {stop("x must be a natural number")}
  if(y%%1 != 0 || y <= 0) {stop("y must be a natural number")}
  
  is_divisible <- (x %% y == 0)
  
  if(is_divisible) {
    cat(x, "is divisible by", y, "\n")
  } else {
    cat(x, "is NOT divisible by", y, "\n")
  }
  return(invisible(is_divisible))
}

divisor(4, 2)
divisor(5, 2)
divide <- divisor(5, 2)
divide
divisor("A", 2)

## Task 19 -----------------------------------------------------------------

sum_of_digits <- function(x) {
  if(!is.numeric(x)) stop("x must be of class 'numeric'")
  if(length(x) != 1) stop("x must be of length 1")
  if(x%%1 != 0 || x <= 0) {stop("x must be a single natural number")}
  
  # walk around using characters :)
  x_str     <- x |> format(scientific = FALSE, trim = TRUE)
  x_str_dig <- strsplit(x_str, NULL) |> unlist()
  x_digits  <- as.numeric(x_str_dig)
  result    <- sum(x_digits)
  return(result)
}
sum_of_digits(20)
sum_of_digits(123)
sum_of_digits(c(123, 456))
sum_of_digits(pi)
sum_of_digits("qfin")


## Task 20 -----------------------------------------------------------------

total_digit_number <- function(input_seq) {
  if(any(input_seq %% 1 != 0) || any(input_seq <= 0)) {
    stop("Sequence must consist of natural numbers")
  }
  
  result  <- nchar(input_seq |> 
                     format(scientific = FALSE, 
                            trim = TRUE)) |> 
             sum()
  return(result)
} 

total_digit_number(1:9)  # should return 9
total_digit_number(1:10) # should return 11
total_digit_number(-5:5) # should raise error (neg numbers)
total_digit_number(10^6) # normaly R write 1e+06 which is 5 chars

# seq(1, 3, by = 0.5) = c(1, 1.5, 2, 2.5, 3), 
# i.e. not all are naturals => should raise error
total_digit_number(seq(1, 3, by = 0.5))

## Task 21 -----------------------------------------------------------------

greatest_two <- function(num1, num2, num3) {
  if(!is.numeric(num1) || !is.numeric(num2) || !is.numeric(num3)) {
    stop("Inputs should be of class 'numeric'")
  }
  vec     <- c(num1, num2, num3)
  largest <- vec[-which.min(vec)] # remove the smallest one
  result  <- sum(largest^2)
  return(result)
}

greatest_two( 1, 2, 3)    # should return 9 + 4 = 13
greatest_two(-1, 0, 1)    # should return 1 + 0 = 1
greatest_two(-1,-2,-3)    # should return 1 + 4 = 5
greatest_two("Computing", "Stats 1", "Stats 2") # should raise error


## Task 22 -----------------------------------------------------------------

cubic_solver <- function(a, b, c) {
  if(!is.numeric(a) || !is.numeric(b) || !is.numeric(c)) {
    stop("Function expects real numbers as inputs for coefficients")
  }
  
  p <- (3 * b - a^2) / 3
  q <- (2 * a^3) / 27 - (a * b) / 3 + c
  D <- (p / 3)^3 + (q / 2)^2
  
  if(D <= 0) {stop("This function can't solve your problem")}
  
  # since R cant take odd-roots of negative numbers, use work around
  # with absolute value function (if x negative, -abs(x))
  first_term  <- -a/3
  second_term <- if(-q/2 + sqrt(D) > 0) {
           (-q/2 + sqrt(D))^(1/3)} else {
      - abs(-q/2 + sqrt(D))^(1/3)
  }
  third_term <- if(-q/2 - sqrt(D) > 0) {
           (-q/2 - sqrt(D))^(1/3)} else {
      - abs(-q/2 - sqrt(D))^(1/3)
  }
  x <- first_term + second_term + third_term
  
  return(x)
}

cubic_solver(1,  2, 3)
cubic_solver(1,  2, NA)
cubic_solver(0, -3, 0)


## Task 23 -----------------------------------------------------------------

factorial_func <- function(n) {
  if(length(n) != 1) {stop("n should be of length 1")}
  if(!is.numeric(n)) {stop("n should be of class numeric")}
  if(n < 0 || n %% 1 != 0) {stop("n should be non-negative integer")}
  
  if(n == 0) {
    result <- 1
  } else {
    result <- prod(1:n)
  }
  return(result)
}
# alternatively, use factorial(n) from base R

factorial_func(10)
factorial_func(50)
factorial_func(100)
factorial_func(1000) # inf
# Note: factorial(170) < .Machine$double.xmax < factorial(171)
factorial(170) - factorial_func(170)

binom <- function(n, k) {
  if(length(n) != 1 || length(k) != 1) {stop("n and k should be of length 1")}
  if(!is.numeric(n) || !is.numeric(k)) {stop("n and k should be of class numeric")}
  if(n < 0 || n %% 1 != 0) {stop("n should be non-negative integer")}
  if(k < 0 || k %% 1 != 0) {stop("k should be non-negative integer")}
  
  numerator   <- factorial_func(n)
  denominator <- factorial_func(k) * factorial_func(n - k)
  result      <- numerator / denominator
  return(result)
}
# alternatively, use choose(n, k) from base R

binom(4, 2)
binom(50, 20)
binom(5000, 2000) # inf / inf

# by definition of binomial coef (n over k) = n!/(k! * (n-k)!)

# taking the natural log on both sides we get
# ln (n over k) = ln n! - ln k! - ln (n - k)!

# from https://en.wikipedia.org/wiki/Factorial#Growth_and_approximation
# use that ln n! = sum from x=1 to n of ln x

# we arrive at
# ln (n over k) = sum_{x=1}^n ln x - sum_{x=1}^k ln x - sum_{x=1}^{n-k} ln x

# undo the log by applying exp to both sides

# (n over k) = e^{sum_{x=1}^n ln x - sum_{x=1}^k ln x - sum_{x=1}^{n-k} ln x}

binom_improved <- function(n, k) {
  if(length(n) != 1 || length(k) != 1) {stop("n and k should be of length 1")}
  if(!is.numeric(n) || !is.numeric(k)) {stop("n and k should be of class numeric")}
  if(n < 0 || n %% 1 != 0) {stop("n should be non-negative integer")}
  if(k < 0 || k %% 1 != 0) {stop("k should be non-negative integer")}

  result <- 
    exp(sum(log(seq_len(n))) - sum(log(seq_len(k))) - sum(log(seq_len(n-k))))
  return(result)
}

binom_improved(4, 2)
binom_improved(50, 20)
binom_improved(5000, 2000) # still too large since
                           # .Machine$double.xmax < exp(sum(log(1:171))) 

## Task 24 -----------------------------------------------------------------

rho_using_gamma <- function(n) {
  rho <- gamma((n-1)/2)/(gamma(1/2)*gamma((n-2)/2))
  return(rho)
}

rho_using_gamma(2000)

# since gamma increases really fast (at n = 172 already becomes Inf),
# at n = 345 rho = Inf because gamma((n-1)/2)
# from n = 346 onwards , the denominator is Inf too -> Inf/Inf = NaN

plot(seq(168, 171, by = 0.1), sapply(seq(168, 171, by = 0.1), gamma))

rho_using_log_gamma <- function(n) {
  log_rho <- lgamma((n-1)/2) - (lgamma(1/2)+lgamma((n-2)/2))
  result  <- exp(log_rho)
  return(result)
}

for (n in 10^(1:10)) {
  result <- rho_using_log_gamma(n) / sqrt(n)
  cat("n           =", n, "",
      "\nrho/sqrt(n) =", result, "\n\n")
}
# rho_n / sqrt(n) converges to 0.3989423
