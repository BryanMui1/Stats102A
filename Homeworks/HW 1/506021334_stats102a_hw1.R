# Problem 1
gcd <- function(x, y) {
  # gcd(x, y), returns the greatest common divisor(gcd) between x and y
  # Args: 
  #   x: integer
  #   y: integer
  # Return:
  #   integer
  # Error handling:
  if(!is.integer(x) || !is.integer(y)) {
    warning("Inputs are not integers, coercing x and y using as.integer...")
    x <- as.integer(x)
    y <- as.integer(y)
  }
  if (y == 0) {
    return(abs(x))
  } else {
    return(gcd(y, (x %% y)))
  }
}

gcdi <- function(v) {
  # gcdi(v), takes an integer vector v and return its greatest common divisor
  # Args: 
  #   v: integer-vector(any length)
  # Return:
  #   integer
  answer <- v[1]
  for (i in 2:(length(v))) {
    answer <- gcd(answer, v[i])
  }
  return(answer)
}

# Problem 2
lcm <- function(x, y) {
  # lcm(x, y), takes two integers, x, y and calculates the least common multiple
  # Args: 
  #   x: integer
  #   y: integer
  # Return:
  #   integer
  # the algorithm for lcm is |a * b|/gcd(a, b) according to Wikipedia
  answer <- abs(x * y) / gcd(x, y)
  answer
}

add_2_frac <- function(n1, d1, n2, d2) {
  # add_2_frac takes four integers as inputs(each frac numerator and denomiator), uses lcm to calculate the sum of two fractions
  # Args: 
  #   n1: integer(numerator1)
  #   d1: integer(denominator1)
  #   n2: integer(numerator2)
  #   d2: integer(denominator2)
  # Return:
  #   list of length 2(num = numerator(integer), denom = denominator(integer))
  denom <- lcm(d1, d2)
  numer <- ((denom / d1) * n1) + ((denom / d2) * n2)
  return(list(num = numer, denom = denom))
}

# Problem 3
is_prime <- function(x) {
  # is_prime(x) takes in a vector x, returns a logical vector determining if each element is a prime number
  # Args: 
  #   x: integer vector x(any length)
  # Return:
  #   logical vector(length x)
  # this solution detects that 1, 2, and 3 are prime, then checks if the number is even or tries to divide every odd number to the sqrt of the function
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

find <- function(vec, value) {
  # find(vec, value), helper function find(used in get_factors), returns the index of the value if found in vector, otherwise returns 0
  # Args: 
  #   vec: vector(any type, any length)
  #   value: any type
  # Return:
  #   integer
  if (length(vec) < 1) {return(0)}
  for (i in 1:length(vec)) {
    if (vec[i] == value) {
      return(i)
    }
  }
  return(0)
}

get_factors <- function(x) {
  # get_factors(x), returns a vector of the unique prime factors of a number x, with their primes and corresponding exponents
  # Args: 
  #   x: integer
  # Return:
  #   list of length 2(primes = integer-vector(any length),exponents = integer-vector(any length))
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
