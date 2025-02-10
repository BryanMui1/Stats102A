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

# compute(): helper for messy_impute(), imputes by cols using the correct function
compute <- function(x, center) {
  # compte():
  # inputs:
  #   x: vector to be imputed
  #   center: character, either mean, median, or min, function to be computed
  # ouputs:
  #   imputed value using mean, median, or min
  # Error handling: 
  #   Make sure all inputs are of correct type and value range:
  if(center != "mean" && center != "median" && center != "min") {
    warning("center has invalid function, function returns NULL")
    return(NULL)
  }
  if(center == "mean") {
    impute <- mean(x, na.rm=TRUE)
    x[is.na(x)] <- impute
  } else if(center == "median") {
    impute <- median(x, na.rm=TRUE)
    x[is.na(x)] <- impute
  } else {
    impute <- min(x, na.rm=TRUE)
    x[is.na(x)] <- impute
  }
  #print(impute)
  x
}

# compute_r(): helper for messy_impute(), imputes by rows using the correct function
compute_r <- function(x, center, hw_index, quiz_index) {
  # compute():
  # inputs:
  #   x: vector to be imputed
  #   center: character, either mean, median, or min, function to be computed
  # ouputs:
  #   imputed value using mean, median, or min
  # Error handling: 
  #   Make sure all inputs are of correct type and value range:
  if(center != "mean" && center != "median" && center != "min") {
    warning("center has invalid function, function returns NULL")
    return(NULL)
  }
  if(length(hw_index) != length(quiz_index)) {
    warning("indices are of the wrong lengths")
  }
  if(center == "mean") {
    impute_hw <- mean(x[hw_index], na.rm=TRUE)
    impute_quiz <- mean(x[quiz_index], na.rm=TRUE)
    x[is.na(x) & hw_index] <- impute_hw
    x[is.na(x) & quiz_index] <- impute_quiz
  } else if(center == "median") {
    impute_hw <- median(x[hw_index], na.rm=TRUE)
    impute_quiz <- median(x[quiz_index], na.rm=TRUE)
    x[is.na(x) & hw_index] <- impute_hw
    x[is.na(x) & quiz_index] <- impute_quiz
  } else {
    impute_hw <- min(x[hw_index], na.rm=TRUE)
    impute_quiz <- min(x[quiz_index], na.rm=TRUE)
    x[is.na(x) & hw_index] <- impute_hw
    x[is.na(x) & quiz_index] <- impute_quiz
  }
  #print(impute_hw)
  #print(impute_quiz)
  x
}

# 1c messy_impute() imputes using missing values in the gradebook 
messy_impute <- function(data, center="min", margin=1, trim=0) {
  # messy_impute():
  # inputs:
  #   data: gradebook tibble
  #   center: character obj: "mean, median, min", function to be imputed
  #   margin: integer: 1 or 2(column wise or row wise)
  #   trim: numeric(0-1) percentage, lower/upper percent of values to discard before imputation
  # outputs:
  #   tibble; gradebook with imputed values
  # Error handling: 
  #   Make sure all inputs are of correct type and within value range
  if(!is_tibble(data)) {
    warning("data is not a tibble, function returns NULL")
    return(NULL)
  } 
  if(center != "mean" && center != "median" && center != "min") {
    warning("center has invalid function, function returns NULL")
    return(NULL)
  }
  if(as.integer(margin) != 1 && as.integer(margin) != 2) {
    warning("invalid margin, function returns NULL")
    return(NULL)
  }
  if(!is.numeric(trim) || trim < 0 || trim > 1) {
    warning("trim is out of bounds or invalid, function returns NULL")
    return(NULL)
  }
  ncols <- dim(data)[2]
  nrows <- dim(data)[1]
  if(margin == 1) {
    # impute through columns
    for(i in 2:ncols) {
      data[[i]] <- compute(data[[i]], center)
    }
  } else {
    # impute through rows
    # find index of quizzes and homeworks
    col_names <- tolower(colnames(data))
    quiz_index <- sapply(col_names, function(x) return(!is.na(str_locate(x, "homework")[1])))
    hw_index <- sapply(col_names, function(x) return(!is.na(str_locate(x, "quiz")[1])))
    # iterate through rows and impute
    for(i in 1:nrows) {
      row <- as.vector(as.matrix(data[i, ]))
      # imputes the homework indices, and then the quizzes
      cat("row ", i, "\n")
      computation <- compute_r(row, center, hw_index, quiz_index)
      data[i, ] <- as.list(computation)
    }
  }
  data
}


# 1d tidy impute(): imputes missing values in a gradebook tidy object
tidy_impute <- function(data, center="min", margin=1, trim=0) {
  # tidy_impute():
  # inputs:
  #   data: gradebook tibble
  #   center: character obj: "mean, median, min", function to be imputed
  #   margin: integer: 1 or 2(column wise or row wise)
  #   trim: numeric(0-1) percentage, lower/upper percent of values to discard before imputation
  # outputs:
  #   tibble; gradebook with imputed values
  # Error handling: 
  #   Make sure all inputs are of correct type and within value range
  if(!is_tibble(data)) {
    warning("data is not a tibble, function returns NULL")
    return(NULL)
  } 
  if(center != "mean" && center != "median" && center != "min") {
    warning("center has invalid function, function returns NULL")
    return(NULL)
  }
  if(as.integer(margin) != 1 && as.integer(margin) != 2) {
    warning("invalid margin, function returns NULL")
    return(NULL)
  }
  if(!is.numeric(trim) || trim < 0 || trim > 1) {
    warning("trim is out of bounds or invalid, function returns NULL")
    return(NULL)
  }
  nrows <- dim(data)[1]
  uids <- data %>% 
    distinct(UID)
  na_rows <- is.na(data$Score)
  # print(na_rows)
  if(margin == 1) {
    # impute through columns(all students for that assignment)
    for(i in 1:nrows) {
      if(na_rows[i]) {
        asses <- data[i,2]
        num <- data[i,3]
        if(center == "mean") {
          data$Score[i] <- data %>%
            filter(Assesment == asses, Number == num) %>%
            select(Score) %>%
            summarize(mean(., na.rm=TRUE))
        } else if(center == "median") {
          data$Score[i] <- data %>%
            filter(Assesment == asses, Number == num) %>%
            select(Score) %>%
            summarize(mean(., na.rm=TRUE))
        } else {
          data$Score[i] <- data %>%
            filter(Assesment == asses, Number == num) %>%
            select(Score) %>%
            summarize(mean(., na.rm=TRUE))
        }
      }
    }
  } else {
    # impute through rows(other assignments for that student)
    for(i in 1:nrows) {
      if(na_rows[i]) {
        asses <- data[i,2]
        num <- data[i,3]
        if(center == "mean") {
          data$Score[i] <- data %>%
            filter(Assesment == asses, Number == num) %>%
            select(Score) %>%
            summarize(mean(., na.rm=TRUE))
        } else if(center == "median") {
          data$Score[i] <- data %>%
            filter(Assesment == asses, Number == num) %>%
            select(Score) %>%
            summarize(mean(., na.rm=TRUE))
        } else {
          data$Score[i] <- data %>%
            filter(Assesment == asses, Number == num) %>%
            select(Score) %>%
            summarize(mean(., na.rm=TRUE))
        }
      }
    }
  }
  data
}

