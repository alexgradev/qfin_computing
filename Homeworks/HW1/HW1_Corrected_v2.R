########################################################################.
# Home Assignment 1 - Group 1
########################################################################.

# Task 1 ------------------------------------------------------------------

# Calculate the remainder after dividing 31079 into 170166719.
170166719 %% 31079  #%% ....Modulo


# Task 2 ------------------------------------------------------------------

#Sum: 
s10 <- sum(1.08 ^ (1:10))  #calculates the sum of (1.08, 1.08^2....1.08^10)
s20 <- sum(1.08 ^ (1:20))
s30 <- sum(1.08 ^ (1:30))
s40 <- sum(1.08 ^ (1:40))

#Formular: (rn+1 − 1)/(r − 1) − 1
n <- (1:100)

Formular <- (((1.08 ^ (n+1)) -1)) / (1.08-1) -1

Formular[10] == s10 #all.equal(x,y) tolerates small rounding-errors, while == is strict
Formular[20] == s20 # 20th element of the formular vector: n =20
Formular[30] == s30
Formular[40] == s40


# Task 3 ------------------------------------------------------------------

s100 <- sum(1:100)
s200 <- sum(1:200)
s400 <- sum(1:400)
s800 <- sum(1:800)


f100 <- (100*(100+1))/2  
f200 <- (200*(200+1))/2  
f400 <- (400*(400+1))/2  
f800 <- (800*(800+1))/2 

f100 == s100 #all.equal(x,y) tolerates small rounding-errors, while == is strict
f200 == s200
f400 == s400
f800 == s800


n <- (1:100)
sum1100 <- (n * (n + 1 )) / 2   #creates vector sum1100 with n= 1 till n=100.
sum1100


# Task 4 ------------------------------------------------------------------

s2200 <- sum((1:200)^2)
s2400 <- sum((1:400)^2)
s2600 <- sum((1:600)^2)
s2800 <- sum((1:800)^2)


f2200 <- (200*(200+1)*(2*200+1))/6
f2400 <- (400*(400+1)*(2*400+1))/6
f2600 <- (600*(600+1)*(2*600+1))/6
f2800 <- (800*(800+1)*(2*800+1))/6


f2200 == s2200
f2400 == s2400
f2600 == s2600
f2800 == s2800

n <- (1:100)
sum11002 <- (n * (n + 1) * (2 * n + 1)) / 6
sum11002


# Task 5 ------------------------------------------------------------------

s3500  <- sum(1 / (1:500))
s31000 <- sum(1 / (1:1000))
s32000 <- sum(1 / (1:2000))
s34000 <- sum(1 / (1:4000))
s38000 <- sum(1 / (1:8000))



s3500  == log(500)
s31000 == log(1000)
s32000 == log(2000)
s34000 == log(4000)
s38000 == log(8000)

# limit of s_n − log(n) for n approaching inf

# Try with bigger numbers
s380000 <- sum(1/(1:80000))
all.equal(s380000,log(80000))

s38000000 <- sum(1/(1:8000000))
s38000000 - log(8000000)

s99 <- sum(1/(1:(10^7)))
s99 - log(10^7)

#converges to Euler-Mascheroni constant (gamma), which is approximately 0.5772


# Task 6 ------------------------------------------------------------------

rep(seq(0,4),each= 5)
rep(seq(1,5),times= 5)


# Task 7 ------------------------------------------------------------------

# c(1, 2, 3, 4, 5, 2, 3, 4, 5, 6, 3, 4, 5, 6, 7, 4, 5, 6, 7, 8, 5, 6, 7, 8, 9)

rep(0:4, each = 5) + rep(1:5, times = 5)


# Task 8 ------------------------------------------------------------------

vector_func <- function(n) {
  #'||' ensures that you get a single Truth Value
  if(n%%1 != 0 || n < 1) stop('Input "n" should be a natural number') 
  
  result <- rep(1:n, times = 1:n)
  return(result)
}

vector_func(4)
vector_func(pi) |> try()
vector_func(0)  |> try()


# Task 9 ------------------------------------------------------------------

numbers <- c(3, 5, 8, 10, 12)
numbers
getwd()

dump("numbers", file = "numbers.R")
rm(numbers)
"numbers" %in% ls()

source("numbers.R")


# Task 10 -----------------------------------------------------------------

# Alternatively, we can use the constant LETTERS

alphabet <- function(file_name) {
 
  letters<- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  
  # Saves the object under the file name input
  dump("letters", file = file_name)
}

# Call of the corresponding function

alphabet("alpha.R")


# Task 11 -----------------------------------------------------------------

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


plot(x, f_of_x, type = "l", col = "black", lwd = 2,
     main = "Graph of f(x)", xlab = "x", ylab = "f(x)")


# Task 12 -----------------------------------------------------------------

sign_func <- function(x) {
  if(!is.numeric(x)) stop('Input "x" should be a number')
  
  if(x < 0)      {result <- "Negative"}
  else if(x > 0) {result <- "Positive"}
  else           {result <- "Neither negative, nor positive"}
  
  return(result)
}

sign_func(-3)
sign_func(pi)
sign_func("A") |> try()


# Task 13 -----------------------------------------------------------------

# Write a function which outputs whether a given number exceeds π in absolute value.

biggerpi<- function(x) {
  y <- abs(x)
  
  if(y < pi) {cat("the absolute value of", x, "is smaller than pi")}
  else if(y == pi) {cat("the absolute value of", x, "is equal to pi")}
  else {cat("the absolute value of", x, "is bigger than pi")}
}

biggerpi(-4)
biggerpi(3)
biggerpi(pi)


# Task 13 -----------------------------------------------------------------

# Write a function which returns the geometric (or the harmonic) mean of two given numbers

means <- function(num1, num2, type = c("geometric", "harmonic")) {
  if(!is.numeric(num1) || !is.numeric(num2)) {
    stop("Num1 and Num2 should be of class 'numeric'")     # checks for everything except numeric
  }
  if(length(num1) > 1 || length(num2) > 1) {       # checks for vectors
    stop("Num1 and Num2 should be single numbers")
  }
  
  # input variable type is matched to one of the allowed options, else error
  type <- match.arg(type)   #gives and ERROR if Typ is different from geometric or harmonic
  
  # from definition of geometric mean sqrt(num1 * num2) is undefined
  # when num1 * num2 < 0
  if(type == "geometric" && (num1 < 0 || num2 < 0)) {
    stop("geometric mean requires non-negative numbers")
  }
  # from definition of harmonic mean 2 / (1 / num1 + 1 / num2) is defined for
  # positive real numbers
  if(type == "harmonic" && (num1 <= 0 || num2 <= 0)) {
    stop("harmonic mean requires positive numbers")
  }
  
  if(type == "geometric") {result <- sqrt(num1 * num2)}
  if(type == "harmonic")  {result <- (2 / (1/num1 + 1/num2))}
  return(result)
}

means(2, 8,   type = "geometric")
means(2, 8,   type = "harmonic")
means(2, "A", type = "harmonic")   |> try()
means(2, 8,   type = "arithmetic") |> try()


# Task 15 -----------------------------------------------------------------

# Write a function which outputs which of two given character strings is shorter

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


# Task 16 -----------------------------------------------------------------

# Write a function which outputs a given character string once for each character in the string.

str2chars <- function(str) {
  if(!is.character(str) || length(str) != 1) {
    stop("Input must be a single character string")
  }
  # strsplit() produces a list by default so flatten it
  str_len <- strsplit(str, NULL) |> unlist() |> length()   
  for(i in seq_len(str_len)){
    print(str)
  }
}


str2chars("QFin")
str2chars("WU")
str2chars("")
str2chars(c("Quantitative", "Finance")) |> try()
str2chars(2026) |> try()

# Task 17 -----------------------------------------------------------------
# Write a function which for a given natural number n outputs all natural 
# numbers <= n in reverse order, each on a separate line (“countdown”)

countdown <- function(n) {
  if(!is.numeric(n))      stop("Input must be of type 'numeric'")
  if(n%%1 != 0 || n <= 0) stop("Input must be a natural number")
  
  for(i in n:1) {print(i)}
}
countdown(5)



#For a real countdown you can use:
countdowndelay<- function(x){
  if(!is.numeric(x))      stop("Input must be of type 'numeric'")
  if(x%%1 != 0 || x <= 0) stop("Input must be a natural number")
  
while(x>0){
  cat(x, "\n")
  x<-(x-1)
  Sys.sleep(0.5)
}
}

countdowndelay(10)


# Task 18 -----------------------------------------------------------------

divisor <- function(x, y) {
  if(!is.numeric(x) || !is.numeric(y)) {
    stop("Inputs x and y must be numeric")
  }
  if(x%%1 != 0 || x <= 0) {stop("x must be a natural number")}
  if(y%%1 != 0 || y <= 0) {stop("y must be a natural number")}
  
  is_divisible <- (x %% y == 0) # binary variable
  
  if(is_divisible) {
    cat(x, "is divisible by", y, "\n")
  } else {
    cat(x, "is NOT divisible by", y, "\n")
  }
  return(invisible(is_divisible))   # hide TRUE / FALSE output from console
}

divisor(4, 2)
divisor(5, 2)
divide <- divisor(5, 2)
divide
divisor("A", 2) |> try()
divisor(NA,  2) |> try()



# Task 19 -----------------------------------------------------------------

# Write a function which computes the sum of the digits of a given natural number. Hint: you
# can use %% and %/% to extract and eliminate the last digit, respectively.
# We did it with R logic

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
sum_of_digits(c(123, 456)) |> try()
sum_of_digits(pi)          |> try()
sum_of_digits("qfin")      |> try()


# Task 20 -----------------------------------------------------------------

total_digit_number <- function(input_seq) {
  if(any(input_seq %% 1 != 0) || any(input_seq <= 0)) {
    stop("Sequence must consist of natural numbers")
  }
  
  # the format func with scientific = F argument is required for examples
  # such as 10000000 (9 digits) that this approach would otherwise
  # consider as 1e+07 (5 chars)
  result  <- nchar(input_seq |> 
                     format(scientific = FALSE, 
                            trim = TRUE)) |> 
    sum()
  return(result)
} 

total_digit_number(1:9)  # should return 9
total_digit_number(1:10) # should return 11
total_digit_number(-5:5) |> try() # should raise error (neg numbers)
total_digit_number(10^6) |> try() # normaly R write 1e+06 which is 5 chars

# seq(1, 3, by = 0.5) = c(1, 1.5, 2, 2.5, 3), 
# i.e. not all are naturals => should raise error
total_digit_number(seq(1, 3, by = 0.5)) |> try()


# Task 21 -----------------------------------------------------------------

# Write a function which takes three numbers as arguments and returns the sum of the squares
# of the two larger numbers.

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
greatest_two("Computing", "Stats 1", "Stats 2") |> try() # should raise error


# Task 22 -----------------------------------------------------------------

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
cubic_solver(1,  2, NA) |> try()
cubic_solver(0, -3, 0)  |> try()


# Task 23 -----------------------------------------------------------------

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

factorial_func(0)
factorial_func(50)
factorial_func(100)
factorial_func(1000) # inf
# Note: factorial(170) < .Machine$double.xmax < factorial(171)
(factorial(170) - factorial_func(170)) / factorial(170) # precision

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

binom(0, 0)
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

plot(seq(168, 171.6, by = 0.1), sapply(seq(168, 171.6, by = 0.1), gamma),
     ylim = c(gamma(168), .Machine$double.xmax),
     main = "Gamma function approaching the largest double",
     xlab = "x", ylab = "gamma(x)")

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
# rho_n / sqrt(n) converges to 1 / sqrt(2 * pi) = 0.3989423 
