#########################################################################################################
################################### Scripte pour solution1.pdf ###################################
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
library(collapse)
library(tidyverse)


## R Code 3------------------------------------------------------------------------------------------


pok <- read_csv("data/data_pokemon.csv", show_col_types = FALSE)


## R Code 4------------------------------------------------------------------------------------------


glimpse(pok)


## R Code 5------------------------------------------------------------------------------------------


head(pok, n = 10)
slice(pok, 1:10)


## R Code 6------------------------------------------------------------------------------------------


select(pok, attack, speed, defense, hp) |>
  summary()


## R Code 7------------------------------------------------------------------------------------------


select(pok, attack, speed, defense, hp) |>
  descr(Ndistinct = TRUE, Qprobs = c(0.25, 0.5, 0.75)) |>
  as_tibble()


## R Code 8------------------------------------------------------------------------------------------


select(pok, attack, speed, defense, hp) |>
  describe_distribution(centrality = c("mean", "median"), quartiles = TRUE) |>
  as_tibble()


## R Code 9------------------------------------------------------------------------------------------


select(pok, attack, speed, defense, hp) |>
  get_summary_stats(show = c("n", "mean", "sd", "median", "q1", "q3"))


## R Code 10------------------------------------------------------------------------------------------


myfunctions <- list(
  # \(x) is a shortcut for function(x)
  n = length, nmiss = \(x) sum(is.na(x)), ndistinct = n_distinct,
  mean = mean, sd = sd, median = median
)


## R Code 11------------------------------------------------------------------------------------------


pok |>
  summarise(across(c("attack", "speed", "defense", "hp"), myfunctions)) |>
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", ".value"),
    names_sep = "_"
  )


## R Code 12------------------------------------------------------------------------------------------


ggplot(pok, aes(x = speed)) +
  geom_histogram(bins = 10, color = "black", fill = "dodgerblue") +
  scale_y_continuous(expand = expansion(c(0, 0.05))) +
  labs(x = "Speed", y = "Count") +
  theme_bw(base_size = 14) +
  labs_pubr()


## R Code 13------------------------------------------------------------------------------------------


pok |>
  ggplot(aes(x = speed, y = attack)) +
  geom_point(size = 2, shape = 21, fill = "dodgerblue", color = "black") +
  labs(x = "Speed (x)", y = "Attack (y)") +
  theme_bw(base_size = 14) +
  labs_pubr()


## R Code 14------------------------------------------------------------------------------------------


select(pok, attack, speed, defense, hp) |>
  pivot_longer(cols = c(speed, defense, hp)) |>
  ggplot(aes(x = value, y = attack)) +
  facet_wrap(vars(name), ncol = 1) +
  geom_point(size = 2, shape = 21, fill = "red", color = "black", alpha = 1) +
  labs(x = "Value (x)", y = "Attack (y)") +
  theme_bw(base_size = 14) +
  labs_pubr()



## R Code 15------------------------------------------------------------------------------------------


select(pok, speed, defense, hp) |>
  cor(method = "pearson")


## R Code 16------------------------------------------------------------------------------------------


select(pok, speed, defense, hp) |> correlation(method = "pearson")


## R Code 17------------------------------------------------------------------------------------------


select(pok, speed, defense, hp) |>
  correlation(method = "pearson") |>
  summary(redundant = TRUE)


## R Code 18------------------------------------------------------------------------------------------


select(pok, speed, defense, hp) |>
  correlation(method = "pearson") |>
  summary(redundant = TRUE) |>
  plot() +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom")


## R Code 19------------------------------------------------------------------------------------------


slr_speed <- lm(formula = attack ~ speed, data = pok)
slr_speed


## R Code 20------------------------------------------------------------------------------------------


slr_defense <- lm(attack ~ defense, data = pok)
slr_defense


## R Code 21------------------------------------------------------------------------------------------


slr_hp <- lm(attack ~ hp, data = pok)
slr_hp


## R Code 22------------------------------------------------------------------------------------------


class(slr_hp)
typeof(slr_hp)
names(slr_hp)


## R Code 23------------------------------------------------------------------------------------------


sum_lm_speed <- summary(slr_speed)
sum_lm_speed


## R Code 24------------------------------------------------------------------------------------------


class(sum_lm_speed)
typeof(sum_lm_speed)
names(sum_lm_speed)


## R Code 25------------------------------------------------------------------------------------------


coeftest(slr_speed)
coeftest(slr_defense)
coeftest(slr_hp)


## R Code 26------------------------------------------------------------------------------------------


confint(slr_speed, level = 0.95)


## R Code 27------------------------------------------------------------------------------------------


tidy(slr_defense, conf.int = TRUE, conf.level = 0.95)


## R Code 28------------------------------------------------------------------------------------------


model_parameters(slr_hp, ci = 0.95, ci_method = "residual", digits = 3)


## R Code 29------------------------------------------------------------------------------------------


betas_speed <- coef(slr_speed)
betas_speed


## R Code 30------------------------------------------------------------------------------------------


pok <- pok |>
  mutate(yhat_speed = betas_speed[1] + betas_speed[2] * speed) |>
  mutate(res_speed = attack - yhat_speed)


## R Code 31------------------------------------------------------------------------------------------


select(pok, id, name, attack, speed, yhat_speed, res_speed) |> head(10)


## R Code 32------------------------------------------------------------------------------------------


fitted(slr_speed)[1:10]
residuals(slr_speed)[1:10]


## R Code 33------------------------------------------------------------------------------------------


augment_speed <- augment(slr_speed)
head(augment_speed, 10)


## R Code 34------------------------------------------------------------------------------------------


autoplot(slr_speed, which = 1, ncol = 1, colour = "black", shape = 21, size = 2) +
  theme_bw(base_size = 14) +
  labs_pubr()


## R Code 35------------------------------------------------------------------------------------------


autoplot(slr_speed, which = 3, ncol = 1, colour = "black", shape = 21, size = 2) +
  theme_bw(base_size = 14) +
  labs_pubr()


## R Code 36------------------------------------------------------------------------------------------


tibble(
  fitted = fitted(slr_speed), rstudent = rstudent(slr_speed)
) |>
  ggplot(aes(x = fitted, y = rstudent)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(shape = 21, size = 2) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Fitted values", y = "Internally studentized residuals") +
  labs(title = "Studentized residuals vs Fitted values") +
  theme_bw(base_size = 14) +
  labs_pubr()


## R Code 37------------------------------------------------------------------------------------------


pok |>
  ggplot(aes(x = speed, y = res_speed)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(shape = 21, size = 2) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Speed", y = "Residuals") +
  labs(title = "Residuals vs Speed") +
  theme_bw(base_size = 14) +
  labs_pubr()


## R Code 38------------------------------------------------------------------------------------------


pok |>
  ggplot(aes(x = defense, y = res_speed)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(shape = 21, size = 2) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Defense", y = "speed Residuals") +
  labs(title = "Speed Residuals vs Defense") +
  theme_bw(base_size = 14) +
  labs_pubr()


## R Code 39------------------------------------------------------------------------------------------


pok |>
  ggplot(aes(x = id, y = res_speed)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(shape = 21, size = 2) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Observation order", y = "Residuals") +
  labs(title = "Residuals vs Observation order") +
  theme_bw(base_size = 14) +
  labs_pubr()


## R Code 40------------------------------------------------------------------------------------------


augment_speed |>
  ggplot(aes(x = .std.resid)) +
  geom_histogram(bins = 12, color = "black", fill = "dodgerblue") +
  scale_y_continuous(expand = expansion(c(0, 0.05))) +
  labs(x = "Standardized residuals", y = "Count") +
  theme_bw(base_size = 14) +
  labs_pubr()


## R Code 41------------------------------------------------------------------------------------------


ggdensity(augment_speed, x = ".std.resid", fill = "dodgerblue") +
  scale_x_continuous(limits = c(-4, 4)) +
  stat_overlay_normal_density(color = "red", linetype = 1, linewidth = 1) +
  scale_y_continuous(expand = expansion(c(0, 0.05))) +
  labs(x = "Standardized residuals", y = "Density") +
  theme_bw(base_size = 14) +
  labs_pubr()


## R Code 42------------------------------------------------------------------------------------------


augment_speed |>
  ggplot(aes(sample = .std.resid)) +
  stat_qq_band(alpha = 0.2, fill = "blue") + # du package {qqplotr}
  stat_qq_line(color = "red") + # version du package {qqplotr}
  stat_qq_point(size = 0.5) +
  labs(y = "Sample quantile", x = "Theoretical quantile") +
  theme_bw(base_size = 14) +
  labs_pubr()


## R Code 43------------------------------------------------------------------------------------------


bptest(slr_speed, studentize = FALSE)
ncvTest(slr_speed)


## R Code 44------------------------------------------------------------------------------------------


dwtest(slr_speed)
durbinWatsonTest(slr_speed)


## R Code 45------------------------------------------------------------------------------------------


shapiro.test(augment_speed[[".std.resid"]])


## R Code 46------------------------------------------------------------------------------------------


augment_speed |>
  shapiro_test(.std.resid)


## R Code 47------------------------------------------------------------------------------------------


select(augment_speed, .std.resid) |>
  col_jarquebera()


## R Code 48------------------------------------------------------------------------------------------


check_model(slr_speed, check = c("qq", "normality", "linearity", "homogeneity"), size_dot = 1)


## R Code 49------------------------------------------------------------------------------------------


grid_speed <- tibble(speed = seq(from = 30, to = 150, by = 40))


## R Code 50------------------------------------------------------------------------------------------


predict(slr_speed, newdata = grid_speed, interval = "confidence") |>
  as_tibble() |>
  mutate(speed = seq(30, 150, 40), .before = 1)


## R Code 51------------------------------------------------------------------------------------------


estimate_expectation(slr_speed, by = "speed = seq(30, 150, 40)", ci = 0.95) |>
  as_tibble()


## R Code 52------------------------------------------------------------------------------------------


predict(slr_speed, newdata = grid_speed, interval = "prediction") |>
  as_tibble() |>
  mutate(speed = seq(30, 150, 40), .before = 1)


## R Code 53------------------------------------------------------------------------------------------


estimate_prediction(slr_speed, by = "speed = seq(30, 150, 40)", ci = 0.95)


## R Code 54------------------------------------------------------------------------------------------


predict(slr_speed, newdata = select(pok, speed), interval = "confidence") |>
  as_tibble() |>
  bind_cols(select(pok, attack, speed)) |>
  ggplot(aes(x = speed, y = attack)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5, fill = "pink") +
  geom_point(size = 2, shape = 21, fill = "dodgerblue", color = "black") +
  geom_line(aes(y = fit), color = "black", linewidth = 0.5) +
  labs(x = "Speed (x)", y = "Attack (y)") +
  theme_bw(base_size = 14) +
  labs_pubr()


## R Code 55------------------------------------------------------------------------------------------


predict(slr_speed, newdata = select(pok, speed), interval = "prediction") |>
  as_tibble() |>
  bind_cols(select(pok, attack, speed)) |>
  ggplot(aes(x = speed, y = attack)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, fill = "pink") +
  geom_point(size = 2, shape = 21, fill = "dodgerblue", color = "black") +
  geom_line(aes(y = fit), color = "black", linewidth = 0.5) +
  labs(x = "Speed (x)", y = "Attack (y)") +
  theme_bw(base_size = 14) +
  labs_pubr()
