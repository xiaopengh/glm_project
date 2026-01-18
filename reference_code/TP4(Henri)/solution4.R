#########################################################################################################
################################### Scripte pour solution4.pdf ###################################
#########################################################################################################

## R Code 1------------------------------------------------------------------------------------------


options(scipen = 999, digits = 5)


## R Code 2------------------------------------------------------------------------------------------


library(broom)
library(performance)
library(parameters)
library(datawizard)
library(see)
library(effectsize)
library(insight)
library(correlation)
library(modelbased)
library(glue)
library(scales)
library(GGally)
library(ggpubr)
library(car)
library(lmtest)
library(multcomp)
library(rstatix)
library(matrixTests)
library(ggfortify)
library(qqplotr)
library(patchwork)
library(ggrepel)
library(gtsummary)
library(kableExtra)
library(openxlsx)
library(janitor)
library(marginaleffects)
library(collapse)
library(tidyverse)


## R Code 3------------------------------------------------------------------------------------------


source("helper_functions4.R")


## R Code 4------------------------------------------------------------------------------------------


pok <- read_csv("data_pokemon.csv", show_col_types = FALSE)


## R Code 5------------------------------------------------------------------------------------------


type_map3 <- list(
  elemental_env = c("Fire", "Water", "Grass", "Electric", "Ice", "Flying", "Poison"),
  physical_material = c("Bug", "Fighting", "Ground", "Rock", "Steel", "Normal"),
  mystical_supernatural = c("Psychic", "Ghost", "Dragon", "Fairy", "Dark")
)


## R Code 6------------------------------------------------------------------------------------------


pok <- pok |>
  mutate(
    typeg = case_when(
      type_1 %in% type_map3$elemental_env ~ "Elemental", # "Elemental",
      type_1 %in% type_map3$physical_material ~ "Physical", # "Physical",
      type_1 %in% type_map3$mystical_supernatural ~ "Mystical", # "Mystical",
      .default = NA_character_
    )
  ) |>
  mutate(second_type = ifelse(type_2 == "None", 0, 1) |> factor(labels = c("No", "Yes"))) |>
  mutate(typeg = fct_infreq(typeg), legendary = as.factor(legendary)) |>
  mutate(generation = factor(generation, labels = paste0("Gen", 1:6))) |>
  relabel(
    typeg = "Primary Type", second_type = "Secondary type",
    legendary = "Legendary", generation = "Pokemon generation",
    attack = "Attack power", speed = "Speed power", defense = "Defense power",
    hp = "Hit points (health)", sp_attack = "Special attack power",
    sp_def = "Special defense power", id = "ID", name = "Pokemon name"
  )


## R Code 7------------------------------------------------------------------------------------------


tab_freq1(pok, c("typeg", "second_type", "legendary", "generation"), digits = 1) |>
  kable(align = "l", padding = 2) |>
  row_spec(c(1, 5, 8, 11), bold = TRUE)


## R Code 8------------------------------------------------------------------------------------------


group_by(pok, second_type, typeg) |>
  summarise(m = mean(attack) |> round(1)) |>
  ggplot(aes(x = second_type, y = m, color = typeg, group = typeg, label = m)) +
  geom_line(linewidth = 0.75) +
  geom_point(size = 1.5) +
  geom_text_repel(size = 4.5, show.legend = FALSE, seed = 123) +
  scale_color_manual(values = c("black", "blue", "red")) +
  scale_x_discrete(expand = expansion(0.10)) +
  scale_y_continuous(expand = expansion(0.1)) +
  labs(x = vlabels(pok$second_type), y = vlabels(pok$attack), color = NULL) +
  theme_minimal(base_size = 14) +
  labs_pubr(18) +
  theme(legend.text = element_text(size = 14), legend.position = "top")


## R Code 9------------------------------------------------------------------------------------------


group_by(pok, typeg) |>
  group_modify(~ mean_by_group(.x, "attack", "second_type")) |>
  group_by(typeg) |>
  mutate(typeg = if_else(is.na(N), typeg, NA)) |>
  frename(vlabels(pok$typeg), cols = 1) |>
  kable(align = "l") |>
  row_spec(c(1, 4, 7), bold = TRUE)


## R Code 10------------------------------------------------------------------------------------------


group_by(pok, second_type, typeg) |>
  summarise(m = mean(attack) |> round(1)) |>
  ggplot(aes(x = typeg, y = m, color = second_type, group = second_type, label = m)) +
  geom_line(linewidth = 0.75) +
  geom_point(size = 1.5) +
  geom_text_repel(size = 4.5, show.legend = FALSE, seed = 123) +
  scale_color_manual(values = c("black", "blue", "red")) +
  scale_x_discrete(expand = expansion(0.10)) +
  scale_y_continuous(expand = expansion(0.1)) +
  labs(x = vlabels(pok$typeg), y = vlabels(pok$attack), color = vlabels(pok$second_type)) +
  theme_minimal(base_size = 14) +
  labs_pubr(18) +
  theme(legend.text = element_text(size = 12), legend.position = "top")


## R Code 11------------------------------------------------------------------------------------------


scatter_speed1 <- filter(pok, typeg == levels(typeg)[1]) |>
  scatter_plot("speed", "attack", nbreaks = 10, color = "grey50", ols_line = "black") +
  labs(title = levels(pok$typeg)[1])


## R Code 12------------------------------------------------------------------------------------------


scatter_speed2 <- filter(pok, typeg == levels(typeg)[2]) |>
  scatter_plot("speed", "attack", nbreaks = 10, color = "grey50", ols_line = "blue") +
  labs(title = levels(pok$typeg)[2])


## R Code 13------------------------------------------------------------------------------------------


scatter_speed3 <- filter(pok, typeg == levels(typeg)[3]) |>
  scatter_plot("speed", "attack", nbreaks = 10, color = "grey50", ols_line = "red") +
  labs(title = levels(pok$typeg)[3])


## R Code 14------------------------------------------------------------------------------------------


scatter_speed1 + scatter_speed2 + scatter_speed3 + plot_layout(axes = "collect") &
  coord_cartesian(xlim = c(0, 190), ylim = c(0, 190)) & labs_pubr(10) &
  theme(plot.title = element_text(hjust = 0.5))


## R Code 15------------------------------------------------------------------------------------------


mod_main1 <- lm(attack ~ second_type + typeg, data = pok)


model_parameters(mod_main1, ci_method = "residual", digits = 1) |>
  format_table(select = "{estimate} [{ci}]|{p}", digits = 1)


## R Code 16------------------------------------------------------------------------------------------


mod_interact1 <- lm(attack ~ second_type * typeg, data = pok)


model_parameters(mod_interact1, ci_method = "residual", digits = 1) |>
  format_table(select = "{estimate} [{ci}]|{p}", digits = 1)


## R Code 17------------------------------------------------------------------------------------------


anova(mod_main1, mod_interact1) |> as.data.frame()


## R Code 18------------------------------------------------------------------------------------------


Anova(mod_interact1, type = 3) |>
  as.data.frame() |>
  round(4)


## R Code 19------------------------------------------------------------------------------------------


linearHypothesis(
  mod_interact1,
  c("second_typeYes:typegPhysical = 0", "second_typeYes:typegMystical = 0")
) |>
  as.data.frame()


## R Code 20------------------------------------------------------------------------------------------


waldtest(mod_interact1, "second_type:typeg") |> as.data.frame()


## R Code 21------------------------------------------------------------------------------------------


mod_interact1bis1 <- lm(attack ~ typeg + second_type:typeg, data = pok)


model_parameters(mod_interact1bis1, ci_method = "residual", digits = 1) |>
  format_table(select = "{estimate} [{ci}]|{p}", digits = 1)


## R Code 22------------------------------------------------------------------------------------------


linearHypothesis(
  mod_interact1bis1,
  c(
    "typegElemental:second_typeYes = typegPhysical:second_typeYes",
    "typegElemental:second_typeYes = typegMystical:second_typeYes"
  )
) |>
  as_tibble()


## R Code 23------------------------------------------------------------------------------------------


mod_interact1bis2 <- lm(attack ~ second_type + typeg:second_type, data = pok)


model_parameters(mod_interact1bis2, ci_method = "residual", digits = 1) |>
  format_table(select = "{estimate} [{ci}]|{p}", digits = 1)


## R Code 24------------------------------------------------------------------------------------------


linearHypothesis(
  mod_interact1bis2,
  c(
    "second_typeNo:typegPhysical = second_typeYes:typegPhysical",
    "second_typeNo:typegMystical = second_typeYes:typegMystical"
  )
) |>
  as_tibble()


## R Code 25------------------------------------------------------------------------------------------


mod_main2 <- lm(attack ~ speed + typeg, data = pok)


model_parameters(mod_main2, ci_method = "residual", digits = 2) |>
  format_table(select = "{estimate} [{ci}]|{p}", digits = 2)


## R Code 26------------------------------------------------------------------------------------------


mod_interact2 <- lm(attack ~ speed * typeg, data = pok)


model_parameters(mod_interact2, ci_method = "residual", digits = 2) |>
  format_table(select = "{estimate} [{ci}]|{p}", digits = 2)


## R Code 27------------------------------------------------------------------------------------------


waldtest(mod_interact2, "speed:typeg") |> as.data.frame()


## R Code 28------------------------------------------------------------------------------------------


mod_interact2bis <- lm(attack ~ typeg + speed:typeg, data = pok)


model_parameters(mod_interact2bis, ci_method = "residual", digits = 2) |>
  format_table(select = "{estimate} [{ci}]|{p}", digits = 2)


## R Code 29------------------------------------------------------------------------------------------


linearHypothesis(
  mod_interact2bis,
  c(
    "typegElemental:speed = typegPhysical:speed",
    "typegElemental:speed = typegMystical:speed"
  )
) |> as.data.frame()
