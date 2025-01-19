test_that("baseline works", {
  parameters <- list(
    baseline = 1
  )

  formula <- generate_formula(parameters)

  expected_formula <- as.formula("~ 1")
  expect_true(all.equal(formula, expected_formula, ignore.environment = TRUE))
})

test_that("endogenous works", {
  parameters <- list(
    baseline = 1,
    inertia = 0.5
  )

  formula <- generate_formula(parameters)

  expected_formula <- as.formula("~ 1 + inertia(scaling = 'std')")
  expect_true(all.equal(formula, expected_formula, ignore.environment = TRUE))
})


test_that("unrecognized parameters are not added", {
  parameters <- list(
    baseline = -5,
    unknown_param = 1, # should be ignored
    send_z1 = 0.5
  )

  formula <- generate_formula(parameters)

  expected_formula <- as.formula("~ 1 + send('z1', scaling = 'std')")
  expect_true(all.equal(formula, expected_formula, ignore.environment = TRUE))
})

test_that("empty parameter list works", {
  parameters <- list()

  formula <- generate_formula(parameters)

  expected_formula <- as.formula("~ 1") # Only intercept
  expect_true(all.equal(formula, expected_formula, ignore.environment = TRUE))
})

test_that("all exogenous work", {
  parameters <- list(
    difference_x1 = 0.1,
    same_x2 = 0.2,
    send_x3 = 0.3,
    receive_x4 = 0.4,
    tie_x5 = 0.5,
    average_x6 = 0.6,
    minimum_x7 = 0.7,
    maximum_x8 = 0.8,
    event_x9 = 0.9,
    userStat_x10 = 1.0
  )

  formula <- generate_formula(parameters)

  expected_formula <- as.formula(
    "~ 1 + difference('x1', scaling = 'std') + same('x2') + send('x3', scaling = 'std') + receive('x4', scaling = 'std') + tie('x5', scaling = 'std') + average('x6', scaling = 'std') + minimum('x7', scaling = 'std') + maximum('x8', scaling = 'std') + event('x9') + userStat('x10')"
  )
  expect_true(all.equal(formula, expected_formula, ignore.environment = TRUE))
})


test_that("all endogenous work", {
  parameters <- list(
    inertia = 0.1,
    reciprocity = 0.2,
    indegreeSender = 0.3,
    indegreeReceiver = 0.4,
    outdegreeSender = 0.5,
    outdegreeReceiver = 0.6,
    totaldegreeDyad = 0.7,
    totaldegreeSender = 0.8,
    totaldegreeReceiver = 0.9,
    degreeMin = 1.0,
    degreeMax = 1.1,
    degreeDiff = 1.2,
    sp = 1.3,
    otp = 1.4,
    itp = 1.5,
    osp = 1.6,
    isp = 1.7,
    psABBA = 1.8,
    psABBY = 1.9,
    psABXA = 2.0,
    psABXB = 2.1,
    psABXY = 2.2,
    psABAY = 2.3,
    psABAB = 2.4,
    rrankSend = 2.5,
    rrankReceive = 2.6,
    recencySendSender = 2.7,
    recencySendReceiver = 2.8,
    recencyReceiveSender = 2.9,
    recencyReceiveReceiver = 3.0,
    recencyContinue = 3.1,
    FEtype = 3.2
  )

  formula <- generate_formula(parameters)
  expected_formula <- as.formula(
    "~ 1 + inertia(scaling = 'std') + reciprocity(scaling = 'std') + indegreeSender(scaling = 'std') + indegreeReceiver(scaling = 'std') + outdegreeSender(scaling = 'std') + outdegreeReceiver(scaling = 'std') + totaldegreeDyad(scaling = 'std') + totaldegreeSender(scaling = 'std') + totaldegreeReceiver(scaling = 'std') + degreeMin(scaling = 'std') + degreeMax(scaling = 'std') + degreeDiff(scaling = 'std') + sp(scaling = 'std') + otp(scaling = 'std') + itp(scaling = 'std') + osp(scaling = 'std') + isp(scaling = 'std') + psABBA() + psABBY() + psABXA() + psABXB() + psABXY() + psABAY() + psABAB() + rrankSend() + rrankReceive() + recencySendSender() + recencySendReceiver() + recencyReceiveSender() + recencyReceiveReceiver() + recencyContinue() + FEtype()"
  )

  expect_true(all.equal(formula, expected_formula, ignore.environment = TRUE))
})


