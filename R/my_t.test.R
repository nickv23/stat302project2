my_t.test <- function(x, alternative, mu) {
  #Calculate degrees of freedom and t values
  df <- length(x) - 1
  t_val <- (mean(x) - mu) / (sd(x) / sqrt(length(x)))

  #Determine the alt hypothesis to calculate correct p value, throw error if not valid alt hypothesis
  if (alternative == "less") {
    p_val <- pt(t_val, df, lower.tail = TRUE)
  } else if (alternative == "greater") {
    p_val <- pt(t_val, df, lower.tail = FALSE)
  } else if (alternative == "two.sided") {
    p_val <- 2 * pt(abs(t_val), df, lower.tail = FALSE)
  } else {
    stop("Please enter in a valid alternative hypothesis: 'greater', 'less', or 'two.sided'")
  }

  #Return list of necessary values
  answers <- list("test_stat" = t_val, "df" = df, "alternative" = alternative, "p_val" = p_val)
  return(answers)
}
