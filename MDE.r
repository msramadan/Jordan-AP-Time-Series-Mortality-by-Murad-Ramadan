# Compute minimum detectable effect (MDE) for a single coefficient
mde <- function(fit,
                         coef_name,
                         alpha = 0.05,
                         power = 0.80,
                         per_10_units = TRUE) {
  # Extract standard error of the coefficient
  se <- sqrt(vcov(fit)[coef_name, coef_name])
  
  # Z values
  z_alpha  <- qnorm(1 - alpha / 2)  # e.g., ~1.96 for alpha=0.05
  z_power  <- qnorm(power)         # e.g., ~0.84 for power=0.80
  
  # MDE on the log scale
  beta_mde <- (z_alpha + z_power) * se
  
  # If the model coefficient is per 10 units already, no rescaling needed.
  # If not, you can adjust later.
  if (per_10_units) {
    # Percent change per 10-unit increase
    mde_percent <- (exp(beta_mde) - 1) * 100
  } else {
    # If coef is per 1 unit, then MDE per 1 unit:
    mde_percent <- (exp(beta_mde) - 1) * 100
    # and per 10 units would be:
    mde_percent <- (exp(beta_mde * 10) - 1) * 100
  }
  
  return(mde_percent)
}
