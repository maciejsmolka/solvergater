
<!-- README.md is generated from README.Rmd. Please edit that file -->

# solvergater

<!-- badges: start -->

[![R build
status](https://github.com/maciejsmolka/solvergater/workflows/R-CMD-check/badge.svg)](https://github.com/maciejsmolka/solvergater/actions)
[![Codecov test
coverage](https://codecov.io/gh/maciejsmolka/solvergater/branch/master/graph/badge.svg)](https://codecov.io/gh/maciejsmolka/solvergater?branch=master)
<!-- badges: end -->

It is a common task to solve an optimization problem when the objective
is computed using an external application such as a
finite-element-method partial differential equation solver that gets its
input from the command line and writes its output to a number of files.
The `solvergater` package provides a simple R gateway to such external
apps.

## Installation

<!--
You can install the released version of solvergater from 
[CRAN](https://CRAN.R-project.org) with:
-->

<!--
``` r
install.packages("solvergater")
```
-->

<!--
And 
-->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("maciejsmolka/solvergater")
```

## Usage

Let us start with a basic example which shows you how to run an external
solver to compute the quantity of interest and its Jacobian matrix with
respect to the parameters. The example uses a mock solver provided with
the package.

``` r
library(solvergater)
rscript_path <- file.path(R.home(), "bin", "Rscript")
solver_path <- file.path(find.package("solvergater"), "exec", "fake_simple.R")
nparams <- 4
nqoi <- 5
solver_cmd <- paste(rscript_path, solver_path, nparams, nqoi)
s <- shell_solver(solver_cmd, nparams = nparams, qoi_file = "output_qoi",
                  jacobian_file = "output_jacobian", wd = tempdir())
run(s, c(20.11, 5, -1.2, 10.4), silent = TRUE)
#> $qoi
#> [1]      34.3100     539.0121    9380.8630  175874.8000 3413761.0000
#> 
#> $jacobian
#>            [,1] [,2]   [,3]      [,4]
#> [1,]      1.000    1  1.000     1.000
#> [2,]     40.220   10 -2.400    20.800
#> [3,]   1213.236   75  4.320   324.480
#> [4,]  32530.910  500 -6.912  4499.456
#> [5,] 817745.700 3125 10.368 58492.930
```

The basic use case for `solvergater` is the support of the optimization
problems with special attention paid for inverse problems. A simulated
problem of the latter type can be as follows.

First, we prepare some ‘observed data’ by running the solver at a given
point.

``` r
observed_data <- run(s, c(10, 10, 10, 10), silent = TRUE)$qoi
```

Then, as the objective we use the misfit between observed data and a
current proposed set of parameters. We can compute it in two ways.
First, we can obtain an objective function returning value and gradient
at the same time.

``` r
x <- c(10.5, 9.44, 10.21, 8.14)
obj <- objective(s, observed_data, silent = TRUE)
obj(x)
#> $value
#> [1] 2594156034
#> 
#> $gradient
#> [1] -6208209534 -4059190749 -5551351184 -2246937119
```

Second, we can obtain two separate functions for value and gradient.
This form is intended for the use in
[`stats::optim()`](https://rdrr.io/r/stats/optim.html).

``` r
solver_obj <- objective_functions(s, observed_data, silent = TRUE)
solver_obj$value(x)
#> [1] 2594156034
solver_obj$gradient(x)
#> [1] -6208209534 -4059190749 -5551351184 -2246937119
```

`objective_functions()` [memoises](https://memoise.r-lib.org/) internal
calls to an actual solver, so in the above example only a single solver
run is performed.

``` r
x1 <- c(0, 20, 0, 20)
solver_obj <- objective_functions(s, observed_data, silent = TRUE)
value_time <- system.time(solver_obj$value(x1))
gradient_time <- system.time(solver_obj$gradient(x1))
value_time
#>    user  system elapsed 
#>   0.292   0.094   0.466
gradient_time
#>    user  system elapsed 
#>   0.001   0.000   0.001
```
