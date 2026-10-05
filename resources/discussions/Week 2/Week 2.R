library(tidyverse)
library(palmerpenguins) # sample dataset used for discussion

# Preview dataset
glimpse(penguins)

# Pipeline
arrange(filter(penguins, species == "Gentoo"), body_mass_g)

penguins |>
  filter(species == "Gentoo") |>
  arrange(body_mass_g)

# Row operations
penguins |>
  filter(species == "Adelie", body_mass_g > 4000) |>
  arrange(desc(body_mass_g))

# Column operations
penguins |>
  select(species, island, body_mass_g) |>
  mutate(body_mass_kg = body_mass_g / 1000)

# Group operations
penguins |>
  summarise(
    mean_mass = mean(body_mass_g, na.rm = TRUE),
    n         = n(),
    .by       = species
  )
penguins |> count(species, island)

# Exercise 1 solution
penguins |>
  filter(!is.na(sex)) |>
  summarise(
    mean_flipper = mean(flipper_length_mm, na.rm = TRUE),
    .by = c(species, sex)
  ) |>
  arrange(desc(mean_flipper))

# Pivot dataset
bills_long <- penguins |>
  mutate(id = row_number()) |>
  select(id, species, bill_length_mm, bill_depth_mm) |>
  pivot_longer(
    cols      = c(bill_length_mm, bill_depth_mm),
    names_to  = "measure",
    values_to = "mm"
  )

bills_wide <- bills_long |>
  pivot_wider(names_from = measure, values_from = mm)

# ggplot2
ggplot(penguins,
       aes(x = flipper_length_mm, y = body_mass_g,
           colour = species)) +
  geom_point() +
  labs(x = "Flipper length (mm)", y = "Body mass (g)")

penguins |>
  summarise(mean_mass = mean(body_mass_g, na.rm = TRUE),
            .by = species) |>
  ggplot(aes(x = species, y = mean_mass)) +
  geom_col(fill = "navy") +
  labs(x = NULL, y = "Mean body mass (g)")

# Exercise 2 solution
penguins |>
  filter(sex == "female") |>
  summarise(mean_flipper = mean(flipper_length_mm, na.rm = TRUE),
            .by = island) |>
  arrange(desc(mean_flipper)) |>
  ggplot(aes(x = fct_reorder(island, mean_flipper, .desc = TRUE),
             y = mean_flipper)) +
  geom_col(fill = "navy") +
  labs(x = "Island", y = "Mean flipper length (mm)")
