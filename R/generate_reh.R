#' Simulate a Relational Event History (REH)
#'
#' This function simulates a sequence of relational events based on a specified
#' statistical model derived from a relational event model (REM). It uses covariates,
#' a list of parameters, and the number of events to generate a relational event history
#' (REH) with associated statistics.
#'
#' @param parameters A named list of parameters for the model. Each parameter represents
#'   a specific effect or term in the tie-oriented model (e.g., `send_z1`, `difference_z2`,
#'   `inertia`). The parameter names must align with those supported by
#'   `remreg::generate_formula()`.
#' @param covar A data frame of covariates containing at least the following columns:
#'   - `name`: Numeric or character identifiers for actors.
#'   - `z1`: A numeric covariate representing some attribute of the actors.
#'   - `z2`: A numeric covariate representing another attribute of the actors.
#'   - `time`: The starting time for the simulation (default is usually `0`).
#'   Each row represents an actor in the network.
#' @param M An integer specifying the number of events to simulate.
#' @param directed A logical value indicating whether the network is directed
#'   (`TRUE`) or undirected (`FALSE`).
#'
#' @return A data frame representing the simulated event history. The data frame contains
#'   the following columns:
#'   - `time`: The time of each event.
#'   - `actor1`: The identifier of the first actor involved in the event.
#'   - `actor2`: The identifier of the second actor involved in the event.
#'
#' @details
#' The `generate_reh()` function simulates a series of relational events using the
#' specified model parameters and covariates. The simulation is based on a relational
#' event model (REM), a statistical framework for modeling and analyzing dynamic interactions
#' between actors over time.
#'
#' It performs the following steps:
#'
#' 1. **Formula Generation**: The function uses `remreg::generate_formula()` to
#'    create a formula for the `tie_effects` argument of `remstats::remstats()`.
#'
#' 2. **Initialization**: A dummy first event is created to initialize the simulation.
#'
#' 3. **Simulation Loop**: For each event:
#'    - The model parameters are applied to compute the event rates.
#'    - A new event is sampled based on the computed rates.
#'    - The event is added to the event history, and the model is updated.
#'
#' 4. **Endogeneity Updates**: The function computes endogeneity statistics for
#'    subsequent events using `remstats::remstats()`.
#'
#' **Standardization**: All effects that allow scaling are standardized by default
#' (`scaling = 'std'`), ensuring consistent computation of statistics.
#'
#' @note
#' This function is inspired by the work presented in the article:
#'
#' Lakdawala, R., Mulder, J., & Leenders, R. (2024). *Simulating Relational Event Histories--Why and How*.
#' arXiv preprint arXiv:2403.19329.
#'
#' @examples
#' # Define parameters
#' parameters <- list(
#'   baseline = -5,
#'   send_z1 = 0.5,
#'   difference_z2 = 0.2,
#'   inertia = 0.3
#' )
#'
#' # Create covariate data
#' covar <- data.frame(
#'   name = 1:50, time = 0,
#'   z1 = rnorm(n = 50, mean = 0, sd = 1),
#'   z2 = rnorm(n = 50, mean = 0, sd = 1)
#' )
#'
#' # Simulate 10 events
#' events <- generate_reh(parameters, covar, M = 10, directed = TRUE)
#' print(events$edgelist)
#'
#' @seealso
#' [remstats::remstats()] for calculating statistics for tie-oriented models.
#' [remreg::generate_formula()] for generating the formula used in this function.
#'
#' @references
#' Lakdawala, R., Mulder, J., & Leenders, R. (2024). *Simulating Relational Event Histories--Why and How*.
#' arXiv preprint arXiv:2403.19329.
#'
#' @export
generate_reh <- function (parameters, # list of parameters for the model
                          covar, #  data frame with id, time, and covariates
                          M, # number of events
                          directed = FALSE)
  # directed or undirected network
{
  # Generate the formula
  formula <- generate_formula(parameters)
  j <- 1
  t <- 0 # time of first event
  dummy <- data.frame(time = 1,
                      actor1 = 1,
                      actor2 = 2) # create dummy first event
  rehOut <- remify::remify(
    edgelist = dummy,
    model = "tie",
    actors = covar$name,
    directed = directed,
    origin = 0
  ) # remify the dummy event

  out <- remstats::remstats(reh = rehOut,
                            tie_effects = formula,
                            attr_actors  = covar) # compute statistics
  riskset <- attributes(out)$riskset # get the riskset
  adj <- matrix(0, 1, ncol = nrow(riskset)) # adjacency matrix


  param <- parameters

  for (i in 1:M) {
    beta <- lapply(param, function(x) {
      if (inherits(x, "function")) {
        x(t)
      }
      else {
        x
      }
    })
    logLambda <- out[dim(out)[1], , ] %*% unlist(beta) # log of the rate parameter

    lambda <- exp(logLambda) # rate parameter
    dt <- stats::rexp(1, sum(lambda)) # time to next event
    d <- sample(1:nrow(lambda), 1, prob = lambda / sum(lambda)) # next event
    if (i + ((j - 1) * M) == 1) {
      # initialize edgelist for the first event
      edgelist <- cbind(time = (t + dt),
                        actor1 = riskset[d, 1],
                        actor2 = riskset[d, 2])
    }
    else {
      # bind the new event to the edgelist for next events
      edgelist <- rbind(edgelist,
                        cbind(
                          time = (t + dt),
                          actor1 = riskset[d, 1],
                          actor2 = riskset[d, 2]
                        ))
    }
    edgelist <- as.data.frame(edgelist) # convert to data frame
    edgelist$time <- as.numeric(edgelist$time) # convert time to numeric
    t <- max(edgelist$time) # update time
    adj <- rbind(adj, adj[i + ((j - 1) * M), ]) # add a row to the adjacency matrix
    adj[i + 1 + ((j - 1) * M), d] <- adj[i + 1 + ((j - 1) * M), d] + 1 # update the adjacency matrix (1 for the event)
    cat(i + ((j - 1) * M), "\r") # print the current iteration
    if (i < M) {
      dummy$time <- dummy$time + t # update the time of the dummy event
      edgelistTemp <- rbind(edgelist, dummy) # create temp edgelist to calculate endo stats
      rehOut <- remify::remify(
        edgelist = edgelistTemp,
        model = "tie",
        actors = covar$name,
        directed = directed,
        origin = 0
      ) # remify the temp edgelist



      if (i + ((j - 1) * M) <= M) {
        out <- remstats::remstats(reh = rehOut,
                                  tie_effects = formula,
                                  attr_actors  = covar) # calculate endo stats
      }
      else{
        stop <-  i + ((j - 1) * M)
        start <- i + ((j - 1) * M) - M + 1

        out <- remstats::remstats(
          reh = rehOut,
          tie_effects = formula,
          attr_actors = covar,
          memory = "full",
          start = start,
          stop = stop
        )
      }
    }
  }

  return(list(
    edgelist = edgelist,
    parameters = parameters,
    covar = covar,
    directed = directed
  ))
}
