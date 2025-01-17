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