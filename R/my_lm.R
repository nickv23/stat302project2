my_lm <- function(formula, data) {

  # Set up x and y matrices, determine degrees of freedom
  x <- model.matrix(formula, data)
  y <- model.response(model.frame(formula, data))
  df <- nrow(x) - ncol(x)

  # Calculate estimates for covariates
  beta <- solve(t(x) %*% x) %*% t(x) %*% y

  #Calculate sigma squared for covariates
  sigma_sq <- sum((y - (x %*% beta))^2) / df

  #Calculate standard error for covariates
  temp <- sigma_sq * solve(t(x) %*% x)
  standard_error <- sqrt(diag(temp))

  #Calculate t values for covariates
  t_vals <- beta / standard_error

  #Calculate p values for covariates
  p_vals <- 2 * pt(abs(t_vals), df, lower.tail = FALSE)


  #Construct table of relevant coefficients for covariates
  answer_table <- data.frame("Estimate" = beta,
                             "Standard Error" = standard_error,
                             "T Value" = t_vals,
                             "Pr(>|t|)" = p_vals
  )
  return(answer_table)
}
