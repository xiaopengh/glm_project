#########################################################################################################
################################### Scripte pour solution5.pdf ###################################
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
library(pROC)
library(caret)
library(collapse)
library(tidyverse)


## R Code 3------------------------------------------------------------------------------------------


source("TP5(Henri)/helper_functions5.R")


## R Code 4------------------------------------------------------------------------------------------


base <- read_csv("data05.csv", show_col_types = FALSE) |>
  mutate(friendalc = factor(friendalc, levels = c("No", "Yes"))) |>
  mutate(binge = factor(binge, levels = c("No", "Yes"))) |>
  mutate(binge_bin = 1 * (binge == "Yes"), .after = binge) |>
  mutate(parentsurv = factor(parentsurv, levels = c("Low", "Medium", "High"))) |>
  relabel(
    friendalc = "Has a drinking friend", parentsurv = "Parental supervision",
    binge = "Binge drinking", age = "Age (years)", sensation = "Sensation seeking score",
    filmalc = "Movies with alcohol score"
  )


## R Code 5------------------------------------------------------------------------------------------


base


## R Code 6------------------------------------------------------------------------------------------


tab_freq1(base, c("binge", "friendalc", "parentsurv"), digits = 1) |>
  kable(align = "l", padding = 2) |>
  row_spec(c(1, 4, 7), bold = TRUE)


## R Code 7------------------------------------------------------------------------------------------


tab_summary <- select(base, age, sensation, filmalc) |>
  tbl_summary(
    type = list(age ~ "continuous2", all_continuous() ~ "continuous2"),
    statistic = all_continuous() ~ c(
      "{mean} ({sd})", "{median} ({p25}, {p75})", "{min}, {max}"
    ),
    digits = ~1
  ) |>
  bold_labels() |>
  as_kable_extra(booktabs = TRUE, linesep = "") |>
  kable_styling(position = "center", latex_options = "HOLD_position")


## R Code 8------------------------------------------------------------------------------------------


tab_summary


## R Code 9------------------------------------------------------------------------------------------


c("friendalc", "parentsurv") |>
  map_dfr(\(by) percent_by_group(base, "binge_bin", by, digits = 1)) |>
  rename("Binge drinking (%)" = Percentage) |>
  kable(align = "l", padding = 2) |>
  row_spec(c(1, 4), bold = TRUE)


## R Code 10------------------------------------------------------------------------------------------


tab_summary2 <- select(base, binge, age, sensation, filmalc) |>
  tbl_summary(
    by = binge,
    type = list(age ~ "continuous", all_continuous() ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    digits = ~1
  ) |>
  bold_labels() |>
  remove_footnote_header() |>
  as_kable_extra(booktabs = TRUE, linesep = "") |>
  kable_styling(position = "center", latex_options = "HOLD_position")


## R Code 11------------------------------------------------------------------------------------------


tab_summary2


## R Code 12------------------------------------------------------------------------------------------


set.seed(123) # for reproductibility


base <- base |>
  mutate(tag = rbinom(n(), size = 1, prob = 0.70))


## R Code 13------------------------------------------------------------------------------------------


tabyl(base, tag)


## R Code 14------------------------------------------------------------------------------------------


data_train <- filter(base, tag == 1)
nrow(data_train)


data_test <- filter(base, tag == 0)
nrow(data_test)


## R Code 15------------------------------------------------------------------------------------------


mod1 <- glm(
  binge_bin ~ age + sensation + filmalc + parentsurv + friendalc,
  data = data_train, family = binomial(link = "logit")
)


summary(mod1)


## R Code 16------------------------------------------------------------------------------------------


model_parameters(mod1, ci_method = "wald", digits = 2)


## R Code 17------------------------------------------------------------------------------------------


model_parameters(mod1, ci_method = "wald", digits = 2, exponentiate = TRUE)


## R Code 18------------------------------------------------------------------------------------------


Anova(mod1, type = 3)


## R Code 19------------------------------------------------------------------------------------------


lrtest(mod1, "parentsurv")


## R Code 20------------------------------------------------------------------------------------------


waldtest(mod1, "parentsurv")


## R Code 21------------------------------------------------------------------------------------------


grid1 <- select(data_train, find_predictors(mod1, flatten = TRUE)) |>
  get_datagrid(
    by = list(
      age = seq(13, 19, 0.5),
      parentsurv = levels(data_train$parentsurv),
      friendalc = levels(data_train$friendalc)
    ), numerics = "mean"
  )


print(as_tibble(grid1), n = 10)


## R Code 22------------------------------------------------------------------------------------------


expect_grid1 <- estimate_expectation(mod1, data = grid1, ci = 0.95) |>
  as_tibble()


print(expect_grid1, n = 30)


## R Code 23------------------------------------------------------------------------------------------


expect_grid1 |>
  ggplot(aes(x = age, y = Predicted, color = friendalc)) +
  facet_wrap(vars(parentsurv)) +
  geom_line() +
  geom_point(size = 1.25) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = pretty_breaks(n = 5)) +
  labs(x = "Age", color = "Drinking Friend") +
  theme_bw(base_size = 14) +
  labs_pubr(16) +
  theme(legend.position = "top")


## R Code 24------------------------------------------------------------------------------------------


pred_test <- augment(mod1, newdata = data_test, type.predict = "response")


print(pred_test, n = 20)


## R Code 25------------------------------------------------------------------------------------------


ggplot(pred_test, aes(x = .fitted)) +
  geom_histogram(fill = "dodgerblue", color = "black") +
  labs(y = "Count", x = "Fitted values") +
  scale_y_continuous(expand = expansion(c(0, 0.05))) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  theme_bw(base_size = 14) +
  labs_pubr(16)


## R Code 26------------------------------------------------------------------------------------------


confusion50 <- confusion_matrix(pred_test[[".fitted"]], pred_test[["binge_bin"]], 0.5)
confusion50


## R Code 27------------------------------------------------------------------------------------------


confusion40 <- confusion_matrix(pred_test[[".fitted"]], pred_test[["binge_bin"]], 0.4)
confusion40


## R Code 28------------------------------------------------------------------------------------------


acc(pred_test[[".fitted"]], pred_test[["binge"]], s = 0.5)


## R Code 29------------------------------------------------------------------------------------------


acc_data <- tibble(s = seq(0, 1, 0.01)) |>
  group_by(s) |>
  mutate(acc = acc(pred_test[[".fitted"]], pred_test[["binge"]], s = s)) |>
  ungroup()


print(acc_data, n = 5)


## R Code 30------------------------------------------------------------------------------------------


acc_max <- max(acc_data$acc)
acc_max


s_max <- acc_data[acc_data$acc == acc_max, ][["s"]][1]
s_max


## R Code 31------------------------------------------------------------------------------------------


ggplot(acc_data, aes(x = s, y = acc)) +
  geom_line(color = "black") +
  geom_hline(yintercept = acc_max, linetype = 2, linewidth = 0.5, color = "red") +
  geom_vline(xintercept = s_max, linetype = 2, linewidth = 0.5, color = "red") +
  scale_y_continuous(breaks = pretty_breaks(n = 10), expand = expansion(c(0, 0.05))) +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  coord_cartesian(ylim = c(0.2, 0.8)) +
  labs(y = "Accuracy", x = "Threshold s") +
  theme_bw(base_size = 14)


## R Code 32------------------------------------------------------------------------------------------


prec(pred_test[[".fitted"]], pred_test[["binge"]], s = 0.5)
npv(pred_test[[".fitted"]], pred_test[["binge"]], s = 0.5)


## R Code 33------------------------------------------------------------------------------------------


prec_npv_data <- tibble(s = seq(0, 1, 0.01)) |>
  group_by(s) |>
  mutate(prec = prec(pred_test[[".fitted"]], pred_test[["binge"]], s = s)) |>
  mutate(npv = npv(pred_test[[".fitted"]], pred_test[["binge"]], s = s)) |>
  ungroup()


prec_npv_data


## R Code 34------------------------------------------------------------------------------------------


rec(pred_test[[".fitted"]], pred_test[["binge"]], s = 0.5)


## R Code 35------------------------------------------------------------------------------------------


spec(pred_test[[".fitted"]], pred_test[["binge"]], s = 0.5)


## R Code 36------------------------------------------------------------------------------------------


rec_spec_data <- tibble(s = seq(0, 1, 0.01)) |>
  group_by(s) |>
  mutate(rec = rec(pred_test[[".fitted"]], pred_test[["binge"]], s = s)) |>
  mutate(spec = spec(pred_test[[".fitted"]], pred_test[["binge"]], s = s)) |>
  ungroup()


print(rec_spec_data, n = 20)


## R Code 37------------------------------------------------------------------------------------------


full_join(rec_spec_data, prec_npv_data) |>
  select(-spec, -npv) |>
  filter(s < 0.7) |>
  pivot_longer(-s) |>
  ggplot(aes(x = s, y = value, color = name)) +
  geom_line() +
  scale_y_continuous(breaks = pretty_breaks(n = 10), expand = expansion(c(0, 0.05))) +
  scale_x_continuous(breaks = seq(0, 1, 0.1), expand = expansion(c(0.05, 0.05))) +
  scale_color_manual(
    name = NULL, values = c("prec" = "blue", "rec" = "black"),
    labels = c("Precision (Positive Predictive Value)", "Recall (Sensitivity)")
  ) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 0.7)) +
  labs(y = "Value", x = "Threshold s") +
  theme_bw(base_size = 14) +
  theme(legend.position = "top", legend.key.width = unit(1, "cm"))


## R Code 38------------------------------------------------------------------------------------------


ggplot(rec_spec_data, aes(x = 1 - spec, y = rec)) +
  geom_line() +
  geom_line(aes(x = rec, y = rec), linetype = 2, color = "red") +
  scale_y_continuous(breaks = pretty_breaks(n = 10), expand = expansion(0.01)) +
  scale_x_continuous(breaks = pretty_breaks(n = 10), expand = expansion(0.01)) +
  labs(y = "Recall (sensitivity, tpr)", x = "1 - Specificity (fpr)") +
  theme_bw(base_size = 14) +
  theme(aspect.ratio = 1)


## R Code 39------------------------------------------------------------------------------------------


roc(pred_test, "binge", ".fitted") |>
  auc()


## R Code 40------------------------------------------------------------------------------------------


performance_roc(mod1, new_data = data_test)
