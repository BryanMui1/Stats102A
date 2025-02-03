library(tidyverse)

set.seed(506021334)
# 1a create a simulated dataset
simulate_data <- function() {
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