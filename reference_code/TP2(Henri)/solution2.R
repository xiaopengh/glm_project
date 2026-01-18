#########################################################################################################
################################### Scripte pour solution2.pdf ###################################
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
library(rstatix)
library(matrixTests)
library(ggfortify)
library(qqplotr)
library(patchwork)
library(gtsummary)
library(kableExtra)
library(collapse)
library(tidyverse)


## R Code 3------------------------------------------------------------------------------------------


source("TP2(Henri)/helper_functions.R")


## R Code 4------------------------------------------------------------------------------------------


pok <- read_csv("data/data_pokemon.csv", show_col_types = FALSE) |>
  select(id, name, attack, speed, defense, hp, sp_attack, sp_def)


## R Code 5------------------------------------------------------------------------------------------


head(pok, n = 10)


## R Code 6------------------------------------------------------------------------------------------


pok <- pok |>
  relabel(
    attack = "Attack power", speed = "Speed power", defense = "Defense power",
    hp = "Hit points (health)", sp_attack = "Special attack power",
    sp_def = "Special defense power", id = "ID", name = "Pokemon name"
  )


## R Code 7------------------------------------------------------------------------------------------


namlab(pok, N = TRUE, Ndistinct = TRUE, class = TRUE)


## R Code 8------------------------------------------------------------------------------------------


vlabels(pok$attack)


## R Code 9------------------------------------------------------------------------------------------


vlabels(pok[c("speed", "defense")])


## R Code 10------------------------------------------------------------------------------------------


numeric_vars <- names(pok)[-c(1, 2)]
numeric_vars


## R Code 11------------------------------------------------------------------------------------------


predictors <- numeric_vars[-1]
predictors


## R Code 12------------------------------------------------------------------------------------------


tab_summary <- select(pok, all_of(numeric_vars)) |>
  tbl_summary(
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c(
      "{mean} ({sd})", "{median} ({p25}, {p75})", "{min}, {max}"
    ),
    digits = ~1
  ) |>
  bold_labels()


## R Code 13------------------------------------------------------------------------------------------


as_kable_extra(tab_summary, booktabs = TRUE, longtable = TRUE, linesep = "") |>
  kable_styling(
    position = "center", font_size = 10,
    latex_options = c("basic", "repeat_header")
  )


## R Code 14------------------------------------------------------------------------------------------


select(pok, id, attack:sp_def) |>
  pivot_longer(-id, names_to = "var") |>
  mutate(var = factor(var, levels = numeric_vars, labels = vlabels(pok[, numeric_vars]))) |>
  ggplot(aes(x = value)) +
  facet_wrap(vars(var)) +
  geom_histogram(fill = "dodgerblue", color = "black", bins = 20, linewidth = 0.5) +
  scale_y_continuous(expand = expansion(c(0, 0.05))) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(x = "Value", y = "Count") +
  theme_bw(base_size = 14) +
  theme(strip.text = element_text(size = 11, face = "bold")) +
  labs_pubr()


## R Code 15------------------------------------------------------------------------------------------


select(pok, -id, -name) |>
  relocate(attack, .after = last_col()) |>
  ggpairs(
    lower = list(
      continuous = wrap(
        "points",
        size = 1, shape = 21, fill = "white", color = "blue", alpha = 1
      )
    )
  ) +
  theme_bw(base_size = 14)


## R Code 16------------------------------------------------------------------------------------------


full_formula <- reformulate(termlabels = predictors, response = "attack")
full_formula


## R Code 17------------------------------------------------------------------------------------------


class(full_formula)


## R Code 18------------------------------------------------------------------------------------------


full_model <- lm(full_formula, data = pok)


## R Code 19------------------------------------------------------------------------------------------


summary(full_model)


## R Code 20------------------------------------------------------------------------------------------


model_parameters(full_model, pretty_names = FALSE)


## R Code 21------------------------------------------------------------------------------------------


null_model <- lm(attack ~ 1, data = pok)


## R Code 22------------------------------------------------------------------------------------------


global_test <- anova(null_model, full_model)
global_test
global_test$F
summary(full_model)[["fstatistic"]]


## R Code 23------------------------------------------------------------------------------------------


predictors


## R Code 24------------------------------------------------------------------------------------------


linearHypothesis(full_model, predictors)


## R Code 25------------------------------------------------------------------------------------------


C <- cbind(rep(0, 5), diag(nrow = 5))
betas <- coef(full_model)


# Design matrix
X <- model.matrix(full_model)


# Residual variance estimate (σ^2-hat)
sigma2_hat <- sigma(full_model)^2


# Number of restrictions
q <- nrow(C)


# Numerator: (C beta_hat)
C_beta <- C %*% betas


# Middle matrix: [C (X'X)^(-1) C']^(-1)
middle <- solve(C %*% solve(t(X) %*% X) %*% t(C))


# F-statistic
F_stat <- as.numeric(t(C_beta) %*% middle %*% C_beta / (q * sigma2_hat))


## R Code 26------------------------------------------------------------------------------------------


F_stat


## R Code 27------------------------------------------------------------------------------------------


# 95% quantile of F(q, n-r)
qf(0.95, q, df.residual(full_model), lower.tail = TRUE)


# p-value
pf(F_stat, q, df.residual(full_model), lower.tail = FALSE) |> label_scientific()()


## R Code 28------------------------------------------------------------------------------------------


glance(full_model)


## R Code 29------------------------------------------------------------------------------------------


model_performance(full_model)


## R Code 30------------------------------------------------------------------------------------------


AIC(full_model)
BIC(full_model)
r2(full_model, ci = 0.95) # {performance}
summary(full_model)[["r.squared"]]
summary(full_model)[["adj.r.squared"]]


## R Code 31------------------------------------------------------------------------------------------


res_model <- lm(attack ~ speed + defense + hp, data = pok)


## R Code 32------------------------------------------------------------------------------------------


anova(res_model, full_model) |> qTBL()


## R Code 33------------------------------------------------------------------------------------------


rss0 <- deviance(res_model)
df0 <- df.residual(res_model)
rss1 <- deviance(full_model)
df1 <- df.residual(full_model)
fstat <- ((rss0 - rss1) / (df0 - df1)) / (rss1 / df1)
fstat


## R Code 34------------------------------------------------------------------------------------------


pf(fstat, df0 - df1, df1, lower.tail = FALSE)


## R Code 35------------------------------------------------------------------------------------------


linearHypothesis(full_model, c("sp_attack = 0", "sp_def = 0"))


## R Code 36------------------------------------------------------------------------------------------


# Compare restricted vs full model
waldtest(res_model, full_model, test = "F")


# Test restrictions directly
waldtest(full_model, c("sp_attack", "sp_def"), test = "F")


## R Code 37------------------------------------------------------------------------------------------


grid1 <- select(pok, all_of(predictors)) |>
  get_datagrid(by = "speed = seq(30, 150, 40)", numerics = "integer")


grid1


## R Code 38------------------------------------------------------------------------------------------


estimate_expectation(full_model, data = grid1, ci = 0.95)


## R Code 39------------------------------------------------------------------------------------------


grid2 <- c(speed = 50, defense = 42, hp = 100, sp_attack = 135, sp_def = 60) |>
  as_tibble_row()


grid2


## R Code 40------------------------------------------------------------------------------------------


estimate_prediction(full_model, data = grid2, ci = 0.95)


## R Code 41------------------------------------------------------------------------------------------


augment_full <- augment(full_model)
head(augment_full)


## R Code 42------------------------------------------------------------------------------------------


resid_vs_predictors(model = full_model, predictors = predictors)


## R Code 43------------------------------------------------------------------------------------------


resid_stand_vs_predictors(model = full_model, predictors = predictors)


## R Code 44------------------------------------------------------------------------------------------


resid_stud_vs_predictors(full_model, predictors)


## R Code 45------------------------------------------------------------------------------------------


p1 <- resid_vs_order(full_model)


## R Code 46------------------------------------------------------------------------------------------


p2 <- resid_stand_hist(full_model)


## R Code 47------------------------------------------------------------------------------------------


p3 <- resid_stand_dens(full_model)


## R Code 48------------------------------------------------------------------------------------------


p4 <- resid_stand_qq(full_model)


## R Code 49------------------------------------------------------------------------------------------


p1 + p2 + p3 + p4


## R Code 50------------------------------------------------------------------------------------------


ncvTest(full_model)


## R Code 51------------------------------------------------------------------------------------------


durbinWatsonTest(full_model)


## R Code 52------------------------------------------------------------------------------------------


augment(full_model) |>
  shapiro_test(.std.resid)
