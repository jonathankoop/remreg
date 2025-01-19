test_that("check variable selection for all ABR methods", {

  # Set seed for reproducibility
  set.seed(123)

  # Set up parameters and covariates
  parameters <- list(
    baseline = -5,
    send_z1 = 0.5,
    difference_z2 = 0,
    inertia = 0.4
  )
  covar <- data.frame(
    name = 1:10,
    time = 0,
    z1 = rnorm(10),
    z2 = rnorm(10)
  )

  # Generate relational event history
  edgelist <- generate_reh(parameters, covar, M = 50, directed = TRUE)

  # Estimate REMs
  result <- estimate_rems(edgelist, methods = c("mle", "abr_horseshoe", "abr_ridge", "abr_lasso"))

  # Select variables
  selected_variables <- select_variables(result, criterion_abr = "95% hdi",
                                         criterion_mle = "p<0.05")

  # Check for ABR Horseshoe
  if ("abr_horseshoe" %in% result$methods) {
    abr_horseshoe_nonzero <- rownames(result$coefs_abr_horseshoe[result$coefs_abr_horseshoe$nonzero == 1, ])
    expect_equal(selected_variables$abr_horseshoe, abr_horseshoe_nonzero)
  }

  # Check for ABR Ridge
  if ("abr_ridge" %in% result$methods) {
    abr_ridge_nonzero <- rownames(result$coefs_abr_ridge[result$coefs_abr_ridge$nonzero == 1, ])
    expect_equal(selected_variables$abr_ridge, abr_ridge_nonzero)
  }

  # Check for ABR Lasso
  if ("abr_lasso" %in% result$methods) {
    abr_lasso_nonzero <- rownames(result$coefs_abr_lasso[result$coefs_abr_lasso$nonzero == 1, ])
    expect_equal(selected_variables$abr_lasso, abr_lasso_nonzero)
  }
})
