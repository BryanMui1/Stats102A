# Problem 1
# gcd(x, y), returns the greatest common divisor between x and y
gcd <- function(x, y) {
  if (y == 0) {
    return(abs(x))
  } else {
    return(gcd(y, (x %% y)))
  }
}

# gcdi(v), takes an integer vector v and return its greatest common divisor
gcdi <- function(v) {
  answer <- v[1]
  for (i in 2:(length(v))) {
    answer <- gcd(answer, v[i])
  }
  return(answer)
}

# Problem 2
# lcm(x, y), takes two integers, x, y and calculates the least common multiple
# the algorithm for lcm is |a * b|/gcd(a, b) according to Wikipedia
lcm <- function(x, y) {
  answer <- abs(x * y) / gcd(x, y)
  answer
}

# add_2_frac takes four integers as inputs(each frac numerator and denomiator), uses lcm to calculate the sum of two fractions
add_2_frac <- function(n1, d1, n2, d2) {
  denom <- lcm(d1, d2)
  numer <- ((denom / d1) * n1) + ((denom / d2) * n2)
  return(list(num = numer, denom = denom))
}

# Problem 3
# is_prime(x) takes in a vector x, returns a logical vector determining if each element is a prime number
is_prime <- function(x) {
  primes <- rep(TRUE, length(x))
  for (i in 1:length(x)) {
    if (x[i] <= 1) {
      primes[i] <- FALSE
    } else if (x[i] == 2 | x[i] == 3) {
      # number is prime
    } else if ((x[i] > 2) & ((x[i] %% 2) == 0)) {
      primes[i] <- FALSE
    } else {
      root <- floor(sqrt(x[i]))
      if (root < 5) {
        # number is prime
      } else {
        for (j in seq(from = 5, to = root, by = 2)) {
          if ((x[i] %% j) == 0) {
            primes[i] <- FALSE
          }
        }
      }
    }
  }
  return(primes)
}

# find(vec, value), helper function find, returns the index of the value if found in vector, otherwise returns 0
find <- function(vec, value) {
  if (length(vec) < 1) {return(0)}
  for (i in 1:length(vec)) {
    if (vec[i] == value) {
      return(i)
    }
  }
  return(0)
}

# get_factors(x), returns a vector of the unique prime factors of a number x, with their primes and corresponding exponents
get_factors <- function(x) {
  answer <- list(primes = c(), exponents = c())
  copy_x <- x
  incr <- 2
  while (copy_x > 1) {
    if ((copy_x %% incr) == 0) {
      # divide the number by the prime factor
      copy_x <- copy_x / incr
      print(copy_x)
      # check if the factor already exists in the list
      indx <- find(answer$primes, incr)
      if (indx > 0) {
        answer$exponents[indx] <- answer$exponents[indx] + 1
      } else {
        answer$primes <- c(answer$primes, incr)
        answer$exponents <- c(answer$exponents, 1)
      }
    } else {
      repeat {
        incr <- incr + 1
        if (is_prime(incr)) {
          break
        }
      }
    }
  }
  return(answer)
}
