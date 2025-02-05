library(tidyverse)

set.seed(506021334)

# 1a create a simulated dataset
simulate_data <- function() {
  # simulate_data() returns simulated data as a gradebook with 100 studenst
  # inputs: none 
  # outputs: a tibble(100 x 14), with UID, homeworks 1-5, and quizzes 1-7
  # Error handling: none, there are no parameters to the function
  # use floor and runif to generate random values
  UID <- as.numeric(100000000:(100000000+99))
  Homework1 <- floor(runif(n=100, min=0, max=100))
  Homework2 <- floor(runif(n=100, min=0, max=100))
  Homework3 <- floor(runif(n=100, min=0, max=100))
  Homework4 <- floor(runif(n=100, min=0, max=100))
  Homework5 <- floor(runif(n=100, min=0, max=100))
  Quiz1 <- floor(runif(n=100, min=0, max=100))
  Quiz2 <- floor(runif(n=100, min=0, max=100))
  Quiz3 <- floor(runif(n=100, min=0, max=100))
  Quiz4 <- floor(runif(n=100, min=0, max=100))
  Quiz5 <- floor(runif(n=100, min=0, max=100))
  Quiz6 <- floor(runif(n=100, min=0, max=100))
  Quiz7 <- floor(runif(n=100, min=0, max=100))
  tibble(UID, Homework1, Homework2, Homework3, Homework4, Homework5,
         Quiz1, Quiz2, Quiz3, Quiz4, Quiz5, Quiz6, Quiz7)
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

