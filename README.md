<!-- README.md is generated from README.Rmd. Please edit that file -->

# remreg <a href="https://github.com/jonathankoop/remreg"><img src="man/figures/logo.svg" align="right" height="150"/></a>

## Checking Regularized Estimation of Relational Event Models

The `remreg` package provides tools to generate Relational Event History
(REH) data based on predefined effects and to estimate Relational Event
Models (REM) using Maximum Likelihood estimation and regularized
estimation techniques. The package is designed to facilitate the
evaluation of the performance of regularized estimation techniques in
the context of REMs. The package is developed by [Jonathan
Koop](https://jonathankoop.eu)

## Installation

You can install the development version of `remreg` from GitHub with:

``` r
install.packages("devtools")
devtools::install_github("jonathankoop/remreg")
```

Then, load the package:

``` r
library(remreg)
```

## Workflow

The `remreg` package is designed to follow a simple workflow lined out
in the following steps:

1.  **Set Covariates and their Parameters**: Define the covariates in
    the data.frame `covariates` and their parameters in the list
    `parameters`.
2.  **Generate Data**: Generate relational event history data based on
    the covariates and parameters using the `generate_reh()` function.
3.  **Estimate Models**: Estimate the REM using Maximum Likelihood
    estimation and regularized estimation techniques using the
    `estimate_rems()` function.
4.  **Select Variables**: Select predictor variables based on Maximum
    Likelihood estimation and the regularized estimation using the
    `select_variables()` function.
5.  **Evaluate Performance**: Evaluate the performance of the variable
    selection techniques using the `evaluate_selection()` function.

## Example

The following example demonstrates the workflow of the `remreg` package.
We first define the covariates and their parameters.

Here, it is important to mention that all effects, **also the zero
effects**, need to be defined in the `parameters` list. This is
necessary to ensure that these variables are considered later in the
estimation process.

``` r
# set seed for reproducibility
set.seed(1)

# Find suitable effects for directed networks
remstats::tie_effects(directed = TRUE)

# Define parameters
parameters <- list(
  baseline = -5,
  inertia = 0.5,
  
  difference_z1 = 0.5,

  
  # Zero effects to be checked
  difference_z2 = 0,
  reciprocity = 0,
  indegreeSender = 0
)

# Define covariates
covar <- data.frame(
  name = 1:10,
  time = 0,
  z1 = rnorm(10),
  z2 = rnorm(10)
)
```

Next, we generate relational event history data based on the covariates
and parameters.

``` r
events <- generate_reh(
  parameters = parameters,
  covar = covar,
  M = 500, # Number of events 
  directed = TRUE # directed network
)
```

Following the data generation, we estimate the REM using Maximum
Likelihood estimation and regularized estimation techniques.

``` r
results <- estimate_rems(edgelist = events,
                         methods = c("mle", "abr_horseshoe", 
                                     "abr_ridge", "abr_lasso"))
```

**Important Note:**

⚠️ **In the case of an error message indicating a singular matrix:**

- Ensure you do **not include effects that are linear combinations of
  others**.

- Consider **increasing the number of events** to make the estimation
  more stable.

- If no solution can be found, please consider [opening an issue on
  GitHub](https://github.com/jonathankoop/remreg/issues) for further
  assistance.

After estimating the models, we can select the variables based on
Maximum Likelihood estimation and the regularized estimation. Here,
criteria have to be defined to select the variables.

``` r
selected <- select_variables(results,
                             criterion_mle = "p<0.05",
                             criterion_abr = "95% hdi")
```

Finally, we evaluate the performance of the variable selection
techniques.

``` r
evaluation <- evaluate_selection(selected)
```

## Contact

For questions, suggestions, or issues, please open an [issue on
GitHub](https://github.com/jonathankoop/remreg/issues) or contact me via
[email](mailto:j.koop@uu.nl).
