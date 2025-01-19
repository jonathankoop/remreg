test_that("evaluate_selection computes correct TDR and FDR", {
  # Create mock data
  selected <- list(
    mle = c("send_z1", "difference_z2"),
    abr_horseshoe = c("send_z1"),
    abr_ridge = c("send_z1", "difference_z2", "inertia"),
    abr_lasso = c("send_z1"),
    parameters = list(
      send_z1 = 0.5,
      difference_z2 = 0.2,
      inertia = 0,
      reciprocity = 0
    ),
    methods = c("mle", "abr_horseshoe", "abr_ridge", "abr_lasso")
  )

  # Evaluate selection performance
  performance <- evaluate_selection(selected)

  # Check TDR
  expect_equal(performance$tdr$mle, 2 / 2)
  expect_equal(performance$tdr$abr_horseshoe, 1 / 2)
  expect_equal(performance$tdr$abr_ridge, 2 / 2)
  expect_equal(performance$tdr$abr_lasso, 1 / 2)

  # Check FDR
  expect_equal(performance$fdr$mle, 0 / 2)
  expect_equal(performance$fdr$abr_horseshoe, 0 / 2)
  expect_equal(performance$fdr$abr_ridge, 1 / 2)
  expect_equal(performance$fdr$abr_lasso, 0 / 2)
})
