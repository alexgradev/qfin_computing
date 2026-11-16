# Computing
# Preparation for Finals


# Exercise 44 -------------------------------------------------------------

# Using the two’s complement representation for signed integers, what are the 
# numbers corresponding to the k-bit sequences 1...1 and 10...01, respectively?

# Start with 1...1 => every bit is 1
# Since the leading bit is "on", we take "two's complement"
# Then this number is
# sum from i=0 to k-2 of 2^i - 2^(k-1)
# the geometric sum is equal to 2^(k-1) - 1
# Hence, the number is 2^(k-1) - 1 - 2^(k-1) = -1

# For 10...01 we have again the leading bit "on"
# Hence, 1 - 2^(k-1)


# Exercise 43 -------------------------------------------------------------

# What are the smallest and largest 32-bit integers representable by the 
# biased scheme?

# The bias is 2^31 - 1. Then the smallest number is 0 - (2^31 - 1) = 1 - 2^31
# and the largest number is (2^32 - 1) - (2^31 - 1) = 2^31


# Exercise 48 -------------------------------------------------------------

# Consider a floating-point number system with base b, precision p
# and exponent range [emin, emax].
# (a) How many different non-zero normalized numbers are there?
# (b) What are the smallest (normalized) and largest positive numbers?

# (a)
# In this system the digits d_i is in {0, ..., b-1}
# Since we are looking for normalized, d_1 != 0
# Then for d_1 we choose from b-1 numbers
# For the rest p-1 digits we choose from b numbers
# For the exponent we choose one from emin to emax => emax - emin + 1 numbers
# Since we should count both positive and negative numbers we multiply by 2
# Hence we have 2(b-1)*b^(p-1)*(emax - emin + 1)

# (b)
# The smallest normalized positive number is
# d_0 = 1, d_2 = ... = d_(p-1) = 0
# Hence (1 + 0/b + ... + 0/b^(p-1))*b^emin = b^emin
# The largest normalized positive number is 
# d_1 = d_2 = ... = d_p = b-1
# Hence ((b-1) + (b-1)/b + ... + (b-1)/b^p)*b^emax


# Exercise 49 -------------------------------------------------------------

# Consider a floating-point number system with base b, precision p
# and exponent range [emin, emax]. If b = 10, what are the smallest values of p
# and emin and largest value of emax such that both 2365.27 and 0.0000512 
# can be represented exactly as normalized floating-point numbers

# Write both in scientific format
# 2365.27   => 2.36527e3
# 0.0000512 => 5.12e-5
#
# Hence we have emin = -5, emax = 3 and p = 6


# Exercise 59 -------------------------------------------------------------

# Find the smallest normalized positive IEEE 754 64-bit floating-point number 
# epsilon for which 1 + epsilon != 1

# In IEEE 754 we have 11 digits for the exponent and 52 for the significand
# Hence, for the significand we need 10...0 (i.e. 1) and for the 2^e
# Hence, we get epsilon = 1 * 2^(-52)


# Exercise 60 -------------------------------------------------------------

# Find the smallest normalized positive IEEE 754 64-bit floating-point number 
# epsilon for which 1 − epsilon != 1

# The representation of 1 is significand (10...0) and e = 0
# The normalized number closest to 1 from below will be written as
# (1...1) and e = - 1, hence this is 1 - 2^-53
# So epsilon is 1 - (1 - 2^-53) = 2^-53


# Exercise 55 -------------------------------------------------------------

# Write functions which use a starting value of 1 and repeated division by 2 to 
# find the smallest numbers epsilon such that 1 + epsilon ! = 1 and 
# 1 - epsilon != 1, respectively. What are these numbers?

plus_epsilon <- 1
while(1 + plus_epsilon / 2 != 1) {
  plus_epsilon <- plus_epsilon / 2
}

minus_epsilon <- 1
while(1 - minus_epsilon / 2 != 1) {
  minus_epsilon <- minus_epsilon / 2
}

plus_epsilon  == .Machine$double.eps
minus_epsilon == .Machine$double.neg.eps


# Exercise 56 -------------------------------------------------------------

# Consider the IEEE 754 double-precision floating-point system. What are the 
# smallest positive normalized and the largest positive denormalized numbers? 
# What is the difference between them?

# The smallest positive normalized number is
# 2^(-1022)

# The largest positive denormalized number has significand (11...11) 
# Hence (0 + 1/2 + ... + 1/2^52) * 2^-1022 = (1-2^-52) * 2^-1022

# The difference between them is 
# 2^(-1022) - (1-2^-52) * 2^-1023 = 2^-52 * 2^-1023 = 2^-1074
