# Section 1
recipe <- read_csv("recipe.csv")
ingredient <- read_csv("ingredient.csv")
stock <- read_csv("stock.csv")
food_item <- read_csv("food_item.csv")

# 1a
# need to output calories, food type for turkey burger
a1 <- ingredient %>%
  left_join(food_item, by = c("food_item" = "item")) %>%
  filter(recipe == "Turkey Burger") %>%
  group_by(type) %>%
  summarise(sum(calories))

# 1b
b1 <- ingredient %>%
  group_by(recipe) %>%
  filter(food_item == "Tomato" | food_item == "Bread" | food_item == "Tomato Sauce") %>%
  filter(any(food_item == "Bread")) %>%
  summarize(sum_bread_and_tomato = sum(`weight (oz)`))

# 1c
c1 <- ingredient %>%
  rename(item = food_item) %>%
  right_join(food_item, by = "item") %>%
  rename(kcal_per_oz = calories) %>%
  mutate(kcal_total = kcal_per_oz * `weight (oz)`) %>%
  group_by(recipe) %>%
  summarize(total_calories = sum(kcal_total)) %>%
  filter(recipe == "Beef Burger" | recipe == "Turkey Burger")

# 1d
d1 <- food_item %>%
  rename(food_item=item) %>%
  left_join(stock, by="food_item") %>%
  rename(price=`price (US dollars per lb)`) %>%
  filter(type=="Vegetables", shop=="W-Mart" | shop=="Food warehouse") %>%
  group_by(food_item) %>%
  summarise(minprice=min(price)) %>%
  left_join(stock, by=c("food_item", "minprice" = "price (US dollars per lb)"))

# 1e
e1 <- ingredient %>%
  rename(item = food_item) %>%
  left_join(food_item, by = "item") %>%
  group_by(recipe) %>%
  filter(any(type == "Wheat product")) %>%
  mutate(kcal_calc = (`weight (oz)` * calories)) %>%
  summarize(total_kcal = sum(kcal_calc))

# Section 2

# 1a
pat_1_a <- "\\d"

# 1b
pat_1_b <- "\\A[[:alpha:]]{4}\\Z"

# 1c
pat_1_c <- "\\A[A-Z][a-z]+ [[:alpha:]]*[ ]*[A-Z][a-z]+\\Z"

# 2a
pat_2_a <- "^5[[:digit:]]{3}[ ]*([[:digit:]]{4}[ ]*){3}$"

# 2b
pat_2_b <- "^4[[:digit:]]{3}[ ]*([[:digit:]]{4}[ ]*){2}[[:digit:]]{1,4}$"

# 3a
pat_3_a <- "(?=.*[0-9])(?=.*[a-zA-Z]).{8,}"

# 3b
pat_3_b <- "(?=.*[0-9])(?=.*[a-z])(?=.*[A-Z])(?=.*[[:punct:]]).{8,}"

# 4a
pat_4_a <- "\\A[a-f]+\\Z"

# 4b
pat_4_b <- "\\A(...)(.*)\\1(.*)\\Z"

# 4c
pat_4_c <- "\\A(?!(..+)\\1+$)(.*)\\Z"
