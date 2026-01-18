


mean_and_sd <- function(x, digits = 1) {
  m <- style_number(fmean(x), digits = digits)  
  s <- style_number(fsd(x), digits = digits)
  glue("{m} ({s})") |> as.character()
}


mean_by_group <- function(data, x, by, digits = 1) {
  vlabx <- vlabels(data[[by]])
  labelx <- if (is.na(vlabx)) by else vlabx
  d <- select(data, all_of(c(x, by))) |>
    rename(y = 1, Variable = 2) |> 
    drop_na()
  tab <- group_by(d, Variable) |>
    summarise(N = n(), "Mean (SD)" = mean_and_sd(y, digits = digits)) |> 
    mutate(Variable = paste0("\U2000", Variable)) 
  bind_rows(c(Variable = labelx), tab)
}


tab_freq1 <- function(data, x, digits = 1) {
  df <- data[x]
  labels <- vlabels(df)
  map(df, \(v) fct_count(v, prop = TRUE)) |> 
    map(\(d) mutate(d, p = format_percent(p, digits = digits))) |> 
    map(\(d) mutate(d, f = paste0("\U2000", f))) |> 
    map2_dfr(labels, \(x, y)  bind_rows(c(f = y), x)) |> 
    set_names(c("Variable", "Count (n)", "Percent (%)"))
}

percent_by_group <- function(data, x, by, digits = 1) {
  vlabx <- vlabels(data[[by]])
  labelx <- if (is.na(vlabx)) by else vlabx
  d <- select(data, all_of(c(x, by))) |>
    rename(y = 1, Variable = 2) |> 
    drop_na()
  tab <- group_by(d, Variable) |>
    summarise(
      N = n(), 
      "Percentage" = label_percent(accuracy = 10^{-digits})(mean(y))
    ) |> 
    mutate(Variable = paste0("\U2000", Variable)) 
  bind_rows(c(Variable = labelx), tab)
}


confusion_matrix <- function(pred, true, s = 0.5) {
  predict <- 1 * (pred >= s)
  predict <- factor(predict, levels = c("0", "1"))
  out <- table(predict, true) 
  out[1, 1] <- paste0("TN = ", out[1, 1])
  out[1, 2] <- paste0("FN = ", out[1, 2])
  out[2, 1] <- paste0("FP = ", out[2, 1])
  out[2, 2] <- paste0("TP = ", out[2, 2])
  out
}

acc <- function(pred, true, s = 0.5) {
  mat <- confusion_matrix(pred, true, s) |> parse_number()
  dim(mat) <- c(2, 2)
  (mat[1, 1] + mat[2, 2]) / sum(mat)
}


prec <- function(pred, true, s = 0.5) {
  mat <- confusion_matrix(pred, true, s) |> parse_number()
  dim(mat) <- c(2, 2)
  mat[2, 2] / sum(mat[2, ])
}



npv <- function(pred, true, s = 0.5) {
  mat <- confusion_matrix(pred, true, s) |> parse_number()
  dim(mat) <- c(2, 2)
  mat[1, 1] / sum(mat[1, ])
}



rec <- function(pred, true, s = 0.5) {
  mat <- confusion_matrix(pred, true, s) |> parse_number()
  dim(mat) <- c(2, 2)
  mat[2, 2] / sum(mat[, 2])
}



spec <- function(pred, true, s = 0.5) {
  mat <- confusion_matrix(pred, true, s) |> parse_number()
  dim(mat) <- c(2, 2)
  mat[1, 1] / sum(mat[, 1])
}








