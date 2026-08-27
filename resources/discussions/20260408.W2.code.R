library(tidyverse)

# Create simple, toy dataset
demo_df <- tibble(sample_id = c("A-1", "A-2", "B-1", "B-2"),
                  score=c(10, 20, 30, 50),
                  max_score = 50,
                  ratio=c("1-10", "1-20", "20-30", "1-100"))
# View the dataframe to understand benefit of `glimpse` as compared to `summary`
summary(demo_df)
glimpse(demo_df)


# Answer question in class (if vectors change too when df column changes)
col1 <- c(1, 2)
col2 <- c(0, 0)
col2
tester <- tibble(col1, col2)
tester
tester <- tester %>% mutate(col2=col2 + 10)
tester$col2 # should be changed (10,10)
col2        # should remain unchanged, or (0,0)


# Splitting one strings in R
value <- "1-2"
typeof(value)
parts <- str_split(value, pattern="-")[[1]]  # use double brackets to extract actual contents
typeof(parts) # view updated type of parts --> character instead of list, as before
parts
first_elem <- parts[1] # having a character, we can extracthe first element
first_elem

elem <- str_split_i(value, pattern="-", 1)  # alternative splititng function that extracts contents for us
elem


# Apply splitting to a vector of strings (column) in R
demo_df <- demo_df %>%
  mutate(lower_value = str_split_i(ratio, "-", 1),
         upper_value = as.numeric(str_split_i(ratio, "-", 2))
         )
demo_df

# Use numeric columns to add percent column to df
demo_df %>% mutate(percent = (score / max_score) * 100)


# Work with penguins DF to see NAs and factor data type
install.packages("palmerpenguins")
library(palmerpenguins)
glimpse(penguins)

# Drop rows with NA
penguins <- penguins %>% drop_na()  # na.omit(penguins)
glimpse(penguins)

# Extended, sequenced chain of operations on penguins df to only see penguins whose mass > 5.2kg
penguins %>%
  select(species, island, body_mass_g) %>%
  mutate(body_mass_kg = body_mass_g / 1000) %>%
  filter(body_mass_kg > 5.2)

# AVOID THIS IF POSSIBLE - LOTS OF NESTED FUNCTIONS CAN BE HARD TO READ!!!!
filter(
  mutate(select(penguins, species, island, body_mass_g),
         body_mass_kg = body_mass_g / 1000),
  body_mass_kg > 5.2
)



# Most basic histogram in base package and in ggplot
hist(penguins$body_mass_g, breaks=50)
ggplot(penguins, aes(x=body_mass_g)) +
  geom_histogram(bins=50) +               # add visualized plot as layer
  labs(title="Distribution of Body Mass") # add title as additional layer

# Most basic boxplot in base package and in ggplot
boxplot(body_mass_g ~ species, data=penguins)
ggplot(penguins, aes(x=species, y=body_mass_g)) +
  geom_boxplot()