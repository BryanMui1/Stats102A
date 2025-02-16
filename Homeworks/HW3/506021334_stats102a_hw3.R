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

pat_1_a <- "her"
