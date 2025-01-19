test_that("check that output format is correct for directed", {
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

  result <- generate_reh(
    parameters = parameters,
    covar = covar,
    M = 5,
    directed =  TRUE)

    expect_true("edgelist" %in% names(result))
    expect_equal(nrow(result$edgelist), 5)
    expect_true(all(
      c("time", "actor1", "actor2") %in% names(result$edgelist)
    ))
})

test_that("check that output format is correct for undirected", {
  parameters <- list(
    baseline = -5,
    difference_z2 = 0.2,
    inertia = 0.3
  )

  covar <- data.frame(
    name = 1:10,
    time = 0,
    z1 = rnorm(10),
    z2 = rnorm(10)
  )

  result <- generate_reh(
    parameters = parameters,
    covar = covar,
    M = 5,
    directed =  FALSE)

  expect_true("edgelist" %in% names(result))
  expect_equal(nrow(result$edgelist), 5)
  expect_true(all(
    c("time", "actor1", "actor2") %in% names(result$edgelist)
  ))
})
