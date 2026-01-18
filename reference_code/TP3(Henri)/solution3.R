#########################################################################################################
################################### Scripte pour solution3.pdf ###################################
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
library(gtsummary)
library(kableExtra)
library(openxlsx)
library(janitor)
library(marginaleffects)
library(collapse)
library(tidyverse)


## R Code 3------------------------------------------------------------------------------------------


source("helper_functions3.R")


## R Code 4------------------------------------------------------------------------------------------


pok <- read_csv("data_pokemon.csv", show_col_types = FALSE)


## R Code 5------------------------------------------------------------------------------------------


head(pok, n = 10)


## R Code 6------------------------------------------------------------------------------------------


table_type1_prop <- mutate(pok, type_1 = fct_infreq(type_1)) |>
  count(type_1) |>
  mutate(pct = 100 * n / sum(n))


table_type1_prop


## R Code 7------------------------------------------------------------------------------------------


table_type2_prop <- tabyl(pok, type_2) |>
  adorn_pct_formatting(digits = 2) |>
  arrange(desc(n)) |>
  as_tibble()


table_type2_prop


## R Code 8------------------------------------------------------------------------------------------


tabyl(pok, legendary) |>
  adorn_pct_formatting(digits = 2) |>
  as_tibble()


## R Code 9------------------------------------------------------------------------------------------


tabyl(pok, generation) |>
  adorn_pct_formatting(digits = 2) |>
  as_tibble()


## R Code 10------------------------------------------------------------------------------------------


select(pok, type_1) |>
  mutate(type_1 = fct_infreq(type_1)) |>
  tbl_summary(label = list(type_1 = "Pokemon primary type")) |>
  modify_header(all_stat_cols() ~ "**{level} (n={n})**") |>
  bold_labels() |>
  remove_footnote_header()


select(pok, type_2) |>
  mutate(type_2 = fct_infreq(type_2)) |>
  tbl_summary(label = list(type_2 = "Pokemon secondary type")) |>
  modify_header(all_stat_cols() ~ "**{level} (n={n})**") |>
  bold_labels() |>
  remove_footnote_header()


## R Code 11------------------------------------------------------------------------------------------


pok |>
  mutate(type_1 = fct_infreq(type_1)) |> # order by descending frequency
  ggplot(aes(x = type_1)) +
  geom_bar(fill = "dodgerblue", color = "black", width = 0.7) +
  labs(
    title = "Distribution of Primary Pokémon Types",
    x = "Primary Type", y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs_pubr()


## R Code 12------------------------------------------------------------------------------------------


pok |>
  mutate(type_2 = fct_infreq(type_2)) |> # order by descending frequency
  count(type_2) |> # get the count
  ggplot(aes(y = type_2, x = n, label = n)) +
  geom_col(fill = "dodgerblue", color = "black", width = 0.7) +
  geom_text(hjust = -0.2) +
  labs(
    title = "Distribution of Primary Pokémon Types",
    x = "Count", y = "Secondary Type"
  ) +
  theme_minimal() +
  labs_pubr()


## R Code 13------------------------------------------------------------------------------------------


type_map3 <- list(
  physical_material = c("Bug", "Fighting", "Ground", "Rock", "Steel", "Normal"),
  elemental_env = c("Fire", "Water", "Grass", "Electric", "Ice", "Flying", "Poison"),
  mystical_supernatural = c("Psychic", "Ghost", "Dragon", "Fairy", "Dark")
)


## R Code 14------------------------------------------------------------------------------------------


pok <- mutate(
  pok,
  type_group3 = case_when(
    type_1 %in% type_map3$physical_material ~ "2.Physical/Material",
    type_1 %in% type_map3$elemental_env ~ "1.Elemental/Environmental",
    type_1 %in% type_map3$mystical_supernatural ~ "3.Mystical/Supernatural",
    .default = NA_character_
  ),
  has_secondary_type = ifelse(type_2 == "None", 0, 1) |> factor(labels = c("No", "Yes"))
) |>
  mutate(type_group3 = as.factor(type_group3), legendary = as.factor(legendary)) |>
  mutate(generation = factor(generation, labels = paste0("Gen", 1:6))) |>
  relabel(
    type_group3 = "Primary Type", has_secondary_type = "Has a secondary type",
    legendary = "Legendary", generation = "Pokemon generation"
  )


## R Code 15------------------------------------------------------------------------------------------


tabyl(pok, type_group3) |>
  as_tibble()


## R Code 16------------------------------------------------------------------------------------------


tabyl(pok, type_1, type_group3) |>
  as_tibble()


## R Code 17------------------------------------------------------------------------------------------


tabyl(pok, has_secondary_type) |>
  as_tibble()


## R Code 18------------------------------------------------------------------------------------------


tabyl(pok, type_2, has_secondary_type) |>
  as_tibble()


## R Code 19------------------------------------------------------------------------------------------


tabyl(pok, type_group3, has_secondary_type) |>
  adorn_totals(where = c("row", "col")) |>
  adorn_percentages(denominator = "row") |> # or "col"
  adorn_pct_formatting(digits = 1) |>
  adorn_ns(position = "front") |>
  as_tibble()


## R Code 20------------------------------------------------------------------------------------------


count(pok, legendary)
count(pok, generation)


## R Code 21------------------------------------------------------------------------------------------


ggplot(pok, aes(x = type_group3, y = attack)) +
  geom_boxplot(linewidth = 0.25, median.linewidth = 0.75) +
  labs(y = "Pokemon power attack", x = vlabels(pok$type_group3)) +
  theme_bw(base_size = 14) +
  labs_pubr()


## R Code 22------------------------------------------------------------------------------------------


varcats <- c("type_group3", "has_secondary_type", "legendary", "generation")


## R Code 23------------------------------------------------------------------------------------------


boxplots <- select(pok, id, attack, all_of(varcats)) |>
  pivot_longer(all_of(varcats), names_to = "var") |>
  mutate(var = factor(var, levels = varcats, labels = vlabels(pok[, varcats]))) |>
  mutate(value = fct_relabel(value, \(x) str_trunc(x, 12))) |>
  ggplot(aes(x = attack, y = value)) +
  geom_boxplot(linewidth = 0.25, median.linewidth = 0.75) +
  facet_wrap(vars(var), scales = "free_y") +
  labs(x = "Pokemon power attack", y = NULL) +
  theme_bw(base_size = 14) +
  labs_pubr() +
  theme(
    strip.text = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 8, face = "bold")
  )


boxplots


## R Code 24------------------------------------------------------------------------------------------


mean_by_group(data = pok, x = "attack", by = "type_group3")


## R Code 25------------------------------------------------------------------------------------------


mean_by_group(pok, "attack", "has_secondary_type")


## R Code 26------------------------------------------------------------------------------------------


mean_by_group(pok, "attack", "legendary")


## R Code 27------------------------------------------------------------------------------------------


mean_by_group(pok, "attack", "generation", digits = 2)


## R Code 28------------------------------------------------------------------------------------------


split(pok, ~legendary) |>
  map(\(d) mean_by_group(d, x = "attack", by = "type_group3", digits = 2)) |>
  list_rbind(names_to = "Legendary Pokemon")


## R Code 29------------------------------------------------------------------------------------------


tabmeans <- map(varcats, \(by) mean_by_group(pok, "attack", by)) |>
  list_rbind()


tabmeans |>
  kable(align = "lcc", padding = 2) |>
  row_spec(c(1, 5, 8, 11), bold = TRUE, color = "white", background = "black")


## R Code 30------------------------------------------------------------------------------------------


# write.xlsx(tabmeans, "tabmeans.xlsx")


## R Code 31------------------------------------------------------------------------------------------


pok <- pok |>
  mutate(
    legend1 = ifelse(legendary == "Yes", 1, 0), legend0 = ifelse(legendary == "No", 1, 0)
  ) |>
  mutate(typeg1 = 1 * (type_group3 == "1.Elemental/Environmental")) |>
  mutate(typeg2 = 1 * (type_group3 == levels(type_group3)[2])) |>
  mutate(typeg3 = 1 * (type_group3 == levels(type_group3)[3]))


## R Code 32------------------------------------------------------------------------------------------


count(pok, legendary, legend0, legend1) |> as.data.frame()
count(pok, type_group3, typeg1, typeg2, typeg3) |> as.data.frame()


## R Code 33------------------------------------------------------------------------------------------


mod1 <- lm(attack ~ legend1, data = pok)


## R Code 34------------------------------------------------------------------------------------------


mod2 <- lm(attack ~ typeg2 + typeg3, data = pok)


## R Code 35------------------------------------------------------------------------------------------


mod3 <- lm(attack ~ legend1 + typeg2 + typeg3, data = pok)


## R Code 36------------------------------------------------------------------------------------------


model_parameters(mod1, ci_method = "residual", digits = 1)


## R Code 37------------------------------------------------------------------------------------------


model_parameters(mod2, ci_method = "residual", digits = 1)


## R Code 38------------------------------------------------------------------------------------------


model_parameters(mod3, ci_method = "residual", digits = 1)


## R Code 39------------------------------------------------------------------------------------------


compare_performance(
  mod1, mod2, mod3,
  metrics = c("AIC", "BIC", "R2", "R2_adj", "SIGMA", "RMSE")
)


## R Code 40------------------------------------------------------------------------------------------


linearHypothesis(mod3, c("typeg2", "typeg3"))


## R Code 41------------------------------------------------------------------------------------------


anova(mod1, mod3)


## R Code 42------------------------------------------------------------------------------------------


anova(mod2, mod3)


## R Code 43------------------------------------------------------------------------------------------


mod1bis <- lm(attack ~ legendary, data = pok)
model_parameters(mod1bis, digits = 1, include_reference = TRUE) |>
  format_table()


## R Code 44------------------------------------------------------------------------------------------


mod2bis <- lm(attack ~ type_group3, data = pok)
model_parameters(mod2bis, digits = 1, include_reference = TRUE) |>
  format_table()


## R Code 45------------------------------------------------------------------------------------------


mod3bis <- lm(attack ~ legendary + type_group3, data = pok)
model_parameters(mod3bis, digits = 1, include_reference = TRUE) |>
  format_table()


## R Code 46------------------------------------------------------------------------------------------


Anova(mod3bis, type = 3)


## R Code 47------------------------------------------------------------------------------------------


lm(attack ~ legendary + relevel(type_group3, ref = 3), data = pok) |>
  model_parameters() |>
  format_table(select = "{estimate} [{ci}]|{p}", digits = 1)


## R Code 48------------------------------------------------------------------------------------------


lm(attack ~ legendary + C(type_group3, base = 3), data = pok) |>
  model_parameters() |>
  format_table(select = "{estimate} [{ci}]|{p}", digits = 1)


## R Code 49------------------------------------------------------------------------------------------


lm(attack ~ legendary + C(type_group3, contr = sum), data = pok) |>
  model_parameters() |>
  format_table(select = "{estimate} [{ci}]|{p}", digits = 1)


## R Code 50------------------------------------------------------------------------------------------


model_parameters(mod3, digits = 3)


## R Code 51------------------------------------------------------------------------------------------


linearHypothesis(mod3, c("typeg2 = typeg3")) |> as_tibble()


## R Code 52------------------------------------------------------------------------------------------


glht(mod3, linfct = c("typeg2 - typeg3 = 0")) |>
  model_parameters(digits = 3, verbose = FALSE)


## R Code 53------------------------------------------------------------------------------------------


model_parameters(mod3bis, digits = 1, verbose = FALSE)


## R Code 54------------------------------------------------------------------------------------------


name_param <- find_parameters(mod3bis, flatten = TRUE)
name_param


## R Code 55------------------------------------------------------------------------------------------


linearHypothesis(mod3bis, glue("{name_param[3]} = {name_param[4]}")) |>
  as.data.frame()


## R Code 56------------------------------------------------------------------------------------------


C <- rbind("beta2 - beta3" = c(0, 0, 1, -1))
C


glht(mod3bis, linfct = C) |>
  model_parameters(digits = 3, verbose = FALSE)


## R Code 57------------------------------------------------------------------------------------------


model_parameters(mod3bis, digits = 1)


## R Code 58------------------------------------------------------------------------------------------


newdata_grid <- expand_grid(
  legendary = fct_unique(pok$legendary),
  type_group3 = fct_unique(pok$type_group3)
) |>
  arrange(legendary, type_group3)


newdata_grid


## R Code 59------------------------------------------------------------------------------------------


predictions <- predict(
  mod3bis,
  newdata = newdata_grid, se.fit = TRUE, interval = "confidence"
)


pred_means_tidy <- bind_cols(newdata_grid, predictions[["fit"]]) |>
  mutate(across(is.numeric, \(x) style_number(x, digits = 1))) |>
  mutate("Mean [95% CI]" = glue("{fit} [{lwr}, {upr}]")) |>
  select(Legendary = legendary, "Primary Type" = type_group3, "Mean [95% CI]")


## R Code 60------------------------------------------------------------------------------------------


pred_means_tidy


## R Code 61------------------------------------------------------------------------------------------


estimate_expectation(mod3bis, data = newdata_grid, ci = 0.95) |>
  rename(Legendary = 1, "Primary Type" = type_group3)


## R Code 62------------------------------------------------------------------------------------------


means <- estimate_expectation(mod3bis, by = c("type_group3", "legendary"), ci = 0.95) |>
  rename(Legendary = 1, "Primary Type" = type_group3)


means


## R Code 63------------------------------------------------------------------------------------------


predictors <- c("legendary", "type_group3")


## R Code 64------------------------------------------------------------------------------------------


mod3bis |>
  estimate_contrasts(contrast = predictors, comparison = "pairwise", p_adjust = "bonferroni") |>
  format_table(select = "{estimate} [{ci}]|{p}", digits = 1) |>
  kable(format = "pipe", align = "l")


## R Code 65------------------------------------------------------------------------------------------


resid_vs_fit(mod3bis, which = "res", jitter = TRUE)


## R Code 66------------------------------------------------------------------------------------------


resid_vs_var_factor(mod3bis, predictors, which = "res", fill = "red") +
  theme(axis.text.x = element_text(size = 7))


## R Code 67------------------------------------------------------------------------------------------


resid_vs_fit(mod3bis, which = "rstud", jitter = TRUE)


## R Code 68------------------------------------------------------------------------------------------


resid_vs_var_factor(mod3bis, predictors, which = "rstud", fill = "red", size = 1.5) +
  theme(axis.text.x = element_text(size = 7))


## R Code 69------------------------------------------------------------------------------------------


p1 <- resid_vs_order(mod3bis)


p2 <- resid_stand_hist(mod3bis)


p3 <- resid_stand_dens(mod3bis)


p4 <- resid_stand_qq(mod3bis)


## R Code 70------------------------------------------------------------------------------------------


p1 + p2 + p3 + p4
