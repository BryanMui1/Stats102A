library(tidyverse)

set.seed(506021334)

# 1a create a simulated dataset
simulate_data <- function() {
  # simulate_data() returns simulated data as a gradebook with 100 studenst
  # inputs: none 
  # outputs: a tibble(100 x 14), with UID, homeworks 1-5, and quizzes 1-7
  # Error handling: check function that the max is 100 and min is 0 for all scores in the gradebook
  # use floor and runif to generate random values
  UID <- sample(100000000:999999999, 100, replace=FALSE)
  Homework_1 <- sapply(round(rnorm(100, mean=75, sd=15)), function(x) min(x, 100))
  Homework_3 <- sapply(round(rnorm(100, mean=75, sd=15)), function(x) min(x, 100))
  Homework_4 <- sapply(round(rnorm(100, mean=75, sd=15)), function(x) min(x, 100))
  Homework_5 <- sapply(round(rnorm(100, mean=75, sd=15)), function(x) min(x, 100))
  Homework_2 <- sapply(round(rnorm(100, mean=75, sd=15)), function(x) min(x, 100))
  Quiz_1 <- sapply(round(rnorm(100, mean=75, sd=15)), function(x) min(x, 100))
  Quiz_2 <- sapply(round(rnorm(100, mean=75, sd=15)), function(x) min(x, 100))
  Quiz_3 <- sapply(round(rnorm(100, mean=75, sd=15)), function(x) min(x, 100))
  Quiz_4 <- sapply(round(rnorm(100, mean=75, sd=15)), function(x) min(x, 100))
  Quiz_5 <- sapply(round(rnorm(100, mean=75, sd=15)), function(x) min(x, 100))
  Quiz_6 <- sapply(round(rnorm(100, mean=75, sd=15)), function(x) min(x, 100))
  Quiz_7 <- sapply(round(rnorm(100, mean=75, sd=15)), function(x) min(x, 100))
  # Error handling: check that all variables have correct max and min
  data <- c(Homework_1, Homework_2, Homework_3, Homework_4, Homework_5,
            Quiz_1, Quiz_2, Quiz_3, Quiz_4, Quiz_5, Quiz_6, Quiz_7)
  if(max(data) > 100 || min(data) < 0) {
    warning("Data exceeds 100 or below 0, bounds are incorrect")
  }
  tibble(UID, Homework_1, Homework_2, Homework_3, Homework_4, Homework_5,
         Quiz_1, Quiz_2, Quiz_3, Quiz_4, Quiz_5, Quiz_6, Quiz_7)
}

# 1b replace 10% of the values with NA in HW 1, HW 5, and Quiz 3
replace_10_percent <- function(x) {
  # replace_10_percent(): given a numeric vector x, replace 10 percent of its values with NA
  # inputs: 
  #   numeric vector x(any length)
  # outputs: 
  #   numeric vector with length x
  # Error handling:
  if(!is.vector(x) | !is.numeric(x)) {
    warning("Vector is not numeric or input is not a vector, returning NULL")
    return(NULL)
  }
  indices <- sample(1:length(x), floor(length(x) * 0.10))
  x[indices] <- NA
  x
}

