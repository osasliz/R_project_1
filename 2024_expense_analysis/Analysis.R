
library(data.table)
library(ggplot2)

my_data <- fread("2024Bankstatement.csv")
view(my_data)

# remove unnecessary columns
cols_remove <- c("Code", "Transaction type", "Notification")
my_data[, (cols_remove) := NULL]

# rename columns
names(my_data) <- c("Date", "Name", "DebitCredit", "Amount EUR")
glimpse(my_data)
str(my_data)

# lowercase column Name
#my_data[, Name := tolower(Name)]***when using data.table package but here we are using tidyverse

my_data <- my_data %>%
  mutate(Name = str_to_lower(Name))
print(my_data)

# reorder rows ascending Date
my_data <- my_data[order(Date)]
my_data

# remove credit transactions
new_data <- my_data %>% 
  filter(DebitCredit == "Debit")

# convert date from int to date format
str(new_data)

new_data[, Date := as.Date(as.character(Date), format = "%Y%m%d")]
new_data[, Month := month(Date)]
new_data[, Year := year(Date)]


#***********************
# Analysis
#***********************

# food expenses for the year
food_key <- c("lidl", "rewe", "dm drogeriemarkt", "neukauf lemler edeka", "mh muller handels gmbh", 
          "african bazaar", "penny", "mcdonalds")
food_pattern <- paste(food_key, collapse = "|")

new_data[grepl(food_pattern, Name), category := "food"]
view(new_data)

new_data_food <- new_data[category == "food"]
food_by_month <- new_data_food[, .(FoodExpense = sum(`Amount EUR`)), by=.(Month = paste0(Month, "-", Year))]
food_by_month[, Month := factor(Month, levels = Month)]

p <- ggplot(data = food_by_month, aes(x = Month, y = FoodExpense)) +
  geom_bar(stat = "identity", fill = "#40E0D0") +
  geom_hline(aes(yintercept = mean(FoodExpense)), linetype = "dashed")
p
