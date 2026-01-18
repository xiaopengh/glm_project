

resid_vs_predictors <- function(model, predictors, facet_ncol = 2, smooth_method = "loess") {
  aug_df <- augment(model)
  aug_df |>
    select("Fitted values" = .fitted, all_of(predictors), res = .resid) |>
    frename(vlabels(aug_df[, predictors]), cols = predictors) |>
    pivot_longer(- res, names_to = "name", values_to = "value") |>
    mutate(name = as_factor(name)) |>
    ggplot(aes(x = value, y = res)) +
    facet_wrap(vars(name), ncol = facet_ncol, scales = "free_x") +
    geom_point(size = 2, shape = 21, fill = "red", color = "black") +
    geom_smooth(method = smooth_method, se = FALSE) +
    labs(x = "Value", y = "Residuals") +
    theme_bw(base_size = 14) +
    labs_pubr()
}


resid_stand_vs_predictors  <- function(model, predictors, facet_ncol = 2, smooth_method = "loess") {
  aug_df <- augment(model)
  mutate(aug_df, res = sqrt(abs(.std.resid))) |> 
    select("Fitted values" = .fitted, all_of(predictors), res) |>
    frename(vlabels(aug_df[, predictors]), cols = predictors) |>
    pivot_longer(- res, names_to = "name", values_to = "value") |>
    mutate(name = as_factor(name)) |> 
    ggplot(aes(x = value, y = res)) +
    facet_wrap(vars(name), ncol = 2, scales = "free_x") +
    geom_point(size = 2, shape = 21,  fill = "red", color = "black", alpha = 1) +
    geom_smooth(method = smooth_method, se = FALSE)  +
    labs(x = "Value", y = expression(sqrt(abs("Standardized residuals")))) +
    theme_bw(base_size = 14) +
    labs_pubr()
}



resid_stud_vs_predictors  <- function(model, predictors, facet_ncol = 2, smooth_method = "loess") {
  aug_df <- augment(model)
  mutate(aug_df, res = rstudent(model)) |> 
    select("Fitted values" = .fitted, all_of(predictors), res) |>
    frename(vlabels(aug_df[, predictors]), cols = predictors) |>
    pivot_longer(- res, names_to = "name", values_to = "value") |>
    mutate(name = as_factor(name)) |> 
    ggplot(aes(x = value, y = res)) +
    facet_wrap(vars(name), ncol = 2, scales = "free_x") +
    geom_point(size = 2, shape = 21,  fill = "red", color = "black", alpha = 1) +
    geom_smooth(method = smooth_method, se = FALSE)  +
    labs(x = "Value", y = "Studentized Residuals") +
    theme_bw(base_size = 14) +
    labs_pubr()
}

resid_vs_order <- function(model, smooth_method = "loess") {
  tibble(res = resid(model), obs = seq_along(res)) |> 
    ggplot(aes(x = obs, y = res)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_point(shape = 21, size = 2) +
    geom_smooth(method = smooth_method, se = FALSE)  +
    scale_x_continuous(breaks = breaks_pretty(n = 5)) +
    labs(x = "Observation", y = "Residuals") +
    theme_bw(base_size = 14) + 
    labs_pubr()
}

resid_stand_hist <- function(model, bins = 12) {
  tibble(res = rstandard(model)) |> 
    ggplot(aes(x = res)) +
    geom_histogram(bins = bins, color = "black", fill = "dodgerblue") +
    scale_y_continuous(expand = expansion(c(0, 0.05)), breaks = breaks_pretty(n = 5)) +
    scale_x_continuous(breaks = breaks_pretty(n = 5)) +
    labs(x = "Standardized residuals", y = "Count") +
    theme_bw(base_size = 14) +
    labs_pubr()
}

resid_stand_dens <- function(model, bins = 12) {
  tibble(res = rstandard(model)) |> 
    ggdensity(x = "res", fill = "dodgerblue") +
    scale_x_continuous(limits = c(-4, 4)) +
    stat_overlay_normal_density(color = "red", linetype = 1, linewidth = 1) +
    scale_y_continuous(expand = expansion(c(0, 0.05)), breaks = breaks_pretty(n = 5)) +
    scale_x_continuous(breaks = breaks_pretty(n = 5)) +
    labs(x = "Standardized residuals", y = "Density") +
    theme_bw(base_size = 14) +
    labs_pubr()
}

resid_stand_qq <- function(model, bins = 12) {
  tibble(res = rstandard(model)) |> 
    ggplot(aes(sample = res)) +
    stat_qq_band(alpha = 0.2, fill = "blue") + # du package {qqplotr}
    stat_qq_line(color = "red") + # version du package {qqplotr}
    stat_qq_point(size = 0.5) +
    scale_y_continuous(breaks = breaks_pretty(n = 5)) +
    scale_x_continuous(breaks = breaks_pretty(n = 5)) +
    labs(y = "Sample quantile", x = "Theoretical quantile") +
    theme_bw(base_size = 14) +
    labs_pubr()
}


