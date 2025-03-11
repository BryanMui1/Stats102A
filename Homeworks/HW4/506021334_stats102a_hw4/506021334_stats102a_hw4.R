# constructor function
pqnumber <- function(sign, p, q, nums) {
  if(sign != 1 && sign != -1) {
    stop("Sign needs to be either -1 or 1")
  } else if(p %% 1 != 0) {
    stop("p needs to be an integer")
  } else if(q %% 1 != 0) {
    stop("q needs to be an integer")
  } else if(length(nums) != (p + q + 1) || sum(nums %% 1 != 0)) {
    stop("nums vector needs to be length p+q+1 and all values in nums have to be between 1-8")
  }
  return(structure(list(sign = sign, p = p, q = q, nums = nums), class="pqnumber"))
}

# predicate function
# checs if x is true or false
is_pqnumber <- function(x) {
  if(inherits(x, "pqnumber")) {
    return(TRUE)
  }
  return(FALSE)
}

# helper function
# converts function to decimal
to_decimal <- function(x) {
  if(!is_pqnumber(x)) {
    stop("object needs to be a pq number!")
  }
  power <- x$p * -1
  decimal <- 0
  # for every number, multiplied by the power in ascending order 
  for(i in x$nums) {
    decimal <- decimal + (i * 10^power)
    power <- power + 1
  }
  # return the result multiplied by sign
  return(decimal * x$sign)
}

# print() method
print.pqnumber <- function(x, DEC=FALSE) {
  if(DEC) {
    cat(to_decimal(x), "\n")
  }
  cat("sign = ", x$sign, "\n")
  cat("p = ", x$p, "\n")
  cat("q = ", x$q, "\n")
  cat("nums = ", x$nums, "\n")
}

# coercion functions
# uses x, p, and q to coerce a decimal into a pq function
as_pqnumber <- function(x, p, q) {
  if(x < 0) {
    sign <- -1
  } else {
    sign <- 1
  }
  x <- x * 10^p
  print(x)
  vec <- as.numeric(unlist(strsplit(as.character(abs(x)), "")))
  
  # reverse the list
  start <- 1
  end <- length(vec)
  while(start < end) {
    # swap the components
    temp <- vec[start]
    vec[start] <- vec[end]
    vec[end] <- temp
    start <- start + 1
    end <- end - 1
  }
  
  while(length(vec) < p + q + 1) {
    vec[length(vec) + 1] <- 0
  }
  
  pqnumber(sign = sign, p = p, q = q, nums = vec)
}

as_numeric <- function(x) {
  return(to_decimal(x))
}


#### 1B addition and subtraction

# helper funcs
# align_pq number, aligns properly p and q in order to perform addition
align_pqnumber <- function(x, p, q) {
  new_len <- p + q + 1
  aligned <- numeric(new_len)
  p_offset <- p - x$p
  
  for(i in 1:length(x$nums)) {
    pos <- p_offset + i
    aligned[pos] <- x$nums[i]
  }
  
  return(aligned)
}

# add_magnitude, adds without considering additional zeros
add_magnitude <- function(x, y, sign = 1) {
  q_max <- max(x$q, y$q) + 1
  p_max <- max(x$p, y$p)
  
  x_aligned <- align_pqnumber(x, q_max, p_max)
  y_aligned <- align_pqnumber(y, q_max, p_max)
  
  sum_digits <- x_aligned + y_aligned
  
  # carry over implementation
  carry <- 0
  for(i in 1:length(sum_digits)) {
    # add the previous carry
    sum_digits[i] <- sum_digits[i] + carry
    # generate carry for next digit
    if(sum_digits[i] > 9) {
      carry <- sum_digits[i] %% 10
      sum_digits[i] <- sum_digits[i] - 10
    } else { carry <- 0 }
  }
  
  # overflow
  if(sum_digits[length(sum_digits)] > 9) {
    if(max_p + 1 > 9) {
      warning("overflow occured, result exceeds maximum allowed p_value")
    }
    carry <- sum_digits[length(sum_digits)] %% 10
    sum_digits[length(sum_digits)] <- sum_digits[length(sum_digits)] %% 10
    sum_digits <- c(carry, sum_digits)
    max_p <- max_p + 1
  }
  
  # trim zeros
  result <- trim_zeros(sum_digits, p_max, q_max)
  
  pqnumber(sign, result$p, result$q, result$nums)
}

# trim zeros helper function: trims all excess zeros
trim_zeros <- function(digits, p, q) {
  first_nonzero <- which(digits != 0)[1]
  
  # return zero if all digits are 0
  if(is.na(first_nonzero)) {
    return(list(p = 0, q = 0, nums = 0))
  }
  
  last_nonzero <- max(which(digits != 0))
  
  trimmed_digits <- digits[first_nonzero:last_nonzero]
  
  new_p <- p - first_nonzero + 1
  new_q <- (last_nonzero - p - 1)
  
  if(new_p < 0) {
    warning("p is less than 0, underflow occured")
    new_p <- 0
  }
  if(new_q < 0) {
    warning("underflow occured, q would be negative")
    new_q <- 0
  }
  
  return(list(p = new_p, q = new_q, nums = trimmed_digits))
}

# subtract magnitude, implements subtraction by taking abs of x or y then handling carry
subtract_magnitude <- function(x, y) {
  x_val <- decimal_value(x)
  y_val <- decimal_value(y)
  abs_x <- abs(x_val)
  abs_y <- abs(y_val)
  
  if(abs_x == abs_y) {
    larger <- x
    smaller <- y
    result_sign <- x$sign
  } else if(abs_x > abs_y) {
    larger <- x
    smaller <- y
    result_sign <- x$sign
  } else {
    larger <- y
    smaller <- x
    result_sign <- -y$sign
  }
  
  p_max <- max(larger$p, smaller$p)
  q_max <- max(larger$q, smaller$q)
  
  larger_aligned <- align_pqnumber(larger, p_max, q_max)
  smaller_aligned <- align_pqnumber(smaller, p_max, q_max)
  
  diff_digits <- larger_aligned - smaller_aligned
  
  
  
  # borrowing implementation
  for(i in 1:length(diff_digits)) {
    # generate carry for next digit
    if(diff_digits[i] < 0) {
      diff_digits[i + 1] <- diff_digits[i + 1] - 1
      diff_digits[i] <- diff_digits[i] + 10
    }
  }
  
  # handle underflow
  if(diff_digits[1] < 0) {
    warning("underflow occured, result might be inaccurate ")
  }
  
  # trim zeros
  result <- trim_zeros(sum_digits, p_max, q_max)
  
  # create and return pq_number object 
  pqnumber(sign, result$p, result$q, result$nums)
}


# add function(implements x and y by hndling additional and subtraction)
add <- function(x, y) {
  integer_max <- as.integer(2^31-1)
  
  if(!is_pqnumber(x) || !is_pqnumber(y)) {
    warning("Both x and y must be pq numbers")
    return(NULL)
  }
  
  # handle addition
  if(x$sign == 1 && y$sign == 1) {
    return(add_magnitude(x, y, sign = 1))
  } else if(x$sign == -1 && y$sign == -1) {
    return(add_magnitude(x, y, sign = -1))
  } else if(x$sign == 1 && y$sign == -1) {
    y_pos <- y
    y_pos$sign <- -1
    return(subtract_magnitude(x, y))
  } else {
    return(subtract_magnitude(y, x))
  }
}


# subtract function(implements x and y by hndling additional and subtraction)
subtract <- function(x, y) {
  integer_max <- as.integer(2^31-1)
  
  if(!is_pqnumber(x) || !is_pqnumber(y)) {
    warning("Both x and y must be pq numbers")
    return(NULL)
  }
  
  # handle addition
  if(x$sign == 1 && y$sign == 1) {
    return(add_magnitude(x, y, sign = 1))
  } else if(x$sign == -1 && y$sign == -1) {
    return(add_magnitude(x, y, sign = -1))
  } else if(x$sign == 1 && y$sign == -1) {
    y_pos <- y
    y_pos$sign <- -1
    return(subtract_magnitude(x, y))
  } else {
    return(subtract_magnitude(y, x))
  }
}

## 1C multiplication
# convert the numbers to decimal, then multiply using decimal multiplication
multiply <- function(x, y) {
  dec_x <- to_decimal(x)
  dec_y <- to_decimal(y)
  return(as_pqnumber(dec_x * dec_y, p = 10, q = 10))
}
