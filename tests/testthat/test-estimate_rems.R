test_that("check MLE estimates and input for shrinkage", {

  # set seed for reproducibility
  set.seed(123)

  # Set up parameters and covariates
  parameters <- list(
    baseline = -5,
    send_z1 = 0.5,
    difference_z2 = 0.2,
    inertia = 0.3
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

  # Check MLE estimates
  expect_true("coefs_mle" %in% names(result))
  for (param in names(parameters)) {
    if (param %in% rownames(result$coefs_mle)) {
      mle_estimate <- result$coefs_mle[param,1]
      expect_true(abs(mle_estimate - parameters[[param]]) < 0.5)

      # Check consistency with ABRinput
      if ("abr_horseshoe" %in% names(result)) {
        abr_horseshoe_input <- result$coefs_abr_horseshoe[param,1]
        expect_equal(round(mle_estimate, 3), abr_horseshoe_input)
      }

      if ("abr_ridge" %in% names(result)) {
        abr_ridge_input <- result$coefs_abr_ridge[param,1]
        expect_equal(round(mle_estimate, 3), abr_ridge_input)
      }

      if ("abr_lasso" %in% names(result)) {
        abr_lasso_input <- result$coefs_abr_lasso[param,1]
        expect_equal(round(mle_estimate, 3), abr_lasso_input)
      }
    }
  }
})
